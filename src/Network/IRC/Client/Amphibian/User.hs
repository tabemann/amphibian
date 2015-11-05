module Network.IRC.Client.Amphibian.User

       (User,
        new,
        subscribe,
        peek,
        recv,
        inject,
        start,
        stop,
        waitStop,
        message,
        waitMessage,
        notice,
        waitNotice,
        getConnectionManager)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               TMVar,
                               atomically,
                               orElse,
                               retry,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newBroadcastTChan,
                               peekTChan,
                               readTChan,
                               writeTChan,
                               dupTChan,
                               newEmptyTMVar,
                               putTMVar,
                               readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue,
                                      peekTQueue)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Functor ((<$>))

-- | Create a new user.
new :: ConnectionManager -> ConnectionManagerSubscription -> Nick -> STM User
new manager subscription nick = do
  async' <- newTVar Nothing
  nick' <- newTVar nick
  actions <- newTQueue,
  events <- newBroadcastTChan
  delayEvents <- newTVar True
  return $ User { userAsync = async',
                  userConnectionManager = manager,
                  userSubscription = subscription,
                  userNick = nick',
                  userActions = actions,
                  userEvents = events,
                  userDelayEvents = delayEvents }

-- | Subscribe to events from a user
subscribe :: User -> STM UserSubscription
subscribe user = do
  writeTVar (userDelayEvents user) False
  UserSubscription <$> dupTChan $ userEvents user

-- | Peek an event from a user.
peek :: UserSubscription -> STM UserEvent
peek (UserSubscription events) = peekTChan events

-- | Receive an event from a user.
recv :: UserSubscription -> STM UserEvent
recv (UserSubscription events) = readTChan events

-- | Inject a connection manager event.
inject :: User -> ConnectionManagerEvent -> STM ()
inject user event = writeTQueue (userInjectedEvents user) event

-- | Start handling events for a user.
start :: User -> AM ()
start user = do
  success <- liftIO . atomically $ do async' <- readTVar $ userAsync user
                                      case async' of
                                        Nothing -> do writeTVar (userAsync user) (Just undefined)
                                                      return True
                                        Just _ -> return False
  case success of
    True -> do
      registerUser user
      intf <- getInterface
      async' <- async $ runAM (runUser user) intf
      liftIO . atomically $ writeTVar (userAsync user) (Just async')
    False -> return ()

-- | Stop handling events for a user.
stop :: User -> STM UserStopResponse
stop user = do
  response <- UserStopResponse <$> newEmptyTMVar
  writeTQueue (userActions user) (UserStop response)
  return response

-- | Wait for response to stopping handling events for a user.
waitStop :: UserStopResponse -> STM (Either Error ())
waitStop (UserStopResponse response) = readTMVar response

-- | Send a message to a user.
message :: User -> MessageComment -> STM UserMessageResponse
message user comment = do
  response <- UserMessageResponse <$> newEmptyTMVar
  writeTQueue (userActions user) (UserMessage comment response)
  return response

-- | Wait for response to sending a message to a user.
waitMessage :: UserMessageResponse -> STM (Either Error ())
waitMessage (UserMessageResponse response) = readTMVar response

-- | Send a notice to a user.
notice :: User -> MessageComment -> STM UserNoticeResponse
notice user comment = do
  response <- UserNoticeResponse <$> newEmptyTMVar
  writeTQueue (userActions user) (UserNotice comment response)
  return response

-- | Wait for response to sending a notice to a user.
waitNotice :: UserNoticeResponse -> STM (Either Error ())
waitNotice (UserNoticeResponse response) = readTMVar response

-- | Run user actions and events.
runUser :: User -> AM ()
runUser user = do
  continue <- join . liftIO . atomically $ handleAction user `orElse` handleEvent user
  if continue
  then runUser user
  else return ()

-- | Handle action.
handleAction :: User -> STM (AM Bool)
handleAction user = do
  action <- readTQueue $ userActions user
  nick <- readTVar $ userNick user
  case action of
    UserMessage comment (UserMessageResponse response) -> do
      sendResponse <- liftIO . atomically $ do
        myNick <- CM.getNick $ userConnectionManager user
        sendResponse <- CM.send (userConnectionManager user) $
          IRCMessage { ircmPrefix = Nothing, ircmCommand = cmd_PRIVMSG, ircmParameters = [nick],
                       ircmComment = Just comment }
        writeTChan (userEvents user) (UserSelfMessage myNick comment)
        return selfResponse
      return $ do async . atomically $ do responseValue <- CM.waitSend sendResponse
                                          writeTMVar response responseValue
                  return True
    UserNotice comment (UserNoticeResponse response) -> do
      sendResponse <- liftIO . atomically $ do
        myNick <- CM.getNick $ userConnectionManager user
        sendResponse <- CM.send (userConnectionManager user) $
          IRCMessage { ircmPrefix = Nothing, ircmCommand = cmd_NOTICE, ircmParameters = [nick],
                       ircmComment = Just comment }
        writeTChan (userEvents user) (UserSelfNotice myNick comment)
        return sendResponse
      return $ do async . atomically $ do responseValue <- CM.waitSend sendResponse
                                          writeTMVar response responseValue
                  return True
    UserStop (UserStopResponse response) -> do
      writeTMVar response (Right ())
      return $ return False

-- | Handle event.
handleEvent :: User -> STM (AM Bool)
handleEvent user = do
  delayEvents <- readTVar $ userDelayEvents user
  if delayEvents
  then retry
  else return ()
  event <- do
    event <- peekTQueue $ userInjectedEvents user
    case event of
      Just event -> do
        readTQueue $ userInjectedEvents user
        return event
      Nothing -> CM.recv $ userSubscription user
  nick <- readTVar $ userNick user
  ownNick <- CM.getNick $ userConnectionManager user
  case event of
    ComaMessage message
      | ircmCommand message == cmd_PRIVMSG && (extractNick $ ircmPrefix message) == Just nick
        && ircmParameters message == [ownNick] ->
        case ircmComment message of
          Just comment -> do
            writeTChan (userEvents user) (UserRecvMessage nick comment)
            return $ return True
          Nothing -> return $ return True
      | ircmCommand message == cmd_NOTICE && (extractNick $ ircmPrefix message) == Just nick
        && ircmParameters message == [ownNick] ->
        case ircmComment message of
          Just comment -> do
            writeTChan (userEvents user) (UserRecvNotice nick comment)
            return $ return True
          Nothing -> return $ return True
      | ircmCommand message == cmd_NICK && (extractNick $ ircmPrefix message) == Just nick ->
        case ircmParameters message of
          [newNick] -> do
            writeTVar (userNick user) newNick
            writeTChan (userEvents user) (UserRecvNick nick newNick)
            return $ return True
          _ -> return $ return True
      | ircmCommand message == cmd_QUIT && (extractNick $ ircmPrefix message) == Just nick -> do
        writeTChan (userEvents user) (UserRecvQuit nick (ircmPrefix message) (ircmComment message))
        return $ return True
    ComaDisconected error -> do
      writeTChan (userEvents user) (UserDisconnected error)
      return $ return True
    _ -> return $ return True

-- | Get connection manager.
getConnectionManager :: User -> ConnectionManager
getConnectionManager user = userConnectionManager user

-- | Get nick.
getNick :: User -> STM Nick
getNick user = readTVar $ userNick user
