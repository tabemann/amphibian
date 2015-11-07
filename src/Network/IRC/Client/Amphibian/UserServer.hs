module Network.IRC.Client.Amphibian.UserServer

       (UserServer,
        UserServerStopResponse,
        new,
        start,
        stop,
        waitStop)

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

-- | Create a new user server.
new :: STM UserServer
new = do
  running <- newTVar Bool
  actions <- newTQueue
  return $ UserServer { usseRunning = running,
                        usseActions = actions }

-- | Start a user server
start :: UserServer -> AM ()
start server = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ usseRunning server
    if not running
    then do
      writeTVar (usseRunning server) True
      I.registerUserServer intf server
      return . async $ runAM (runServer server)
    else return $ return ()

-- | Stop user server.
stop :: UserServer -> STM UserServerStopResponse
stop server = do
  response <- UserServerStopResponse <$> newEmptyTMVar
  writeTVar (usseActions server) (UssaStop response)
  return response

-- | Wait for user server to stop.
waitStop :: UserServerStopResponse -> STM (Either Error ())
waitStop (UserServerStopResponse response) = readTMVar response

-- | Run user server.
runServer :: UserServer -> AM ()
runServer server = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    action <- readTQueue $ usseActions server
    case action of
      UssaStartUser user (UserStartResponse response) -> do
        active <- readTVar $ userActive user
        if not active
        then do
          writeTVar (userActive user) True
          I.registerUser intf user
          putTMVar response $ Right ()
          return $ do
            async $ runAM (runUser user) intf
            return True
        else do
          errorText <- I.lookupText intf $ T.pack "User is already started"
          putTMVar response . Left $ Error [errorText]
          return $ return True
      UssaStop -> do
        I.unregisterUserServer intf server
        return $ return False
  if continue
  then runServer server
  else return ()

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

