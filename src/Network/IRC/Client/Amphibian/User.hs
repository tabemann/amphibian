module Network.IRC.Client.Amphibian.User

       (User,
        new,
        subscribe,
        peek,
        recv,
        inject,
        start,
        waitStart,
        stop,
        waitStop,
        message,
        waitMessage,
        notice,
        waitNotice,
        getConnectionManager)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
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
import Data.Functor ((<$>))
import qualified Data.Text as T

-- | Create a new user.
new :: Interface -> ConnectionManager -> Nick -> STM User
new intf manager nick = do
  nick' <- newTVar nick
  subscription <- CM.subscribe manger
  actions <- newTQueue,
  events <- newBroadcastTChan
  delayEvents <- newTVar True
  active <- newTVar False
  return $ User { userInterface = intf,
                  userConnectionManager = manager,
                  userSubscription = subscription,
                  userNick = nick',
                  userActions = actions,
                  userEvents = events,
                  userDelayEvents = delayEvents,
                  userActive = active }

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

-- | Start a user thread.
start :: User -> STM UserStartResponse
start user = do
  response <- newEmptyTMVar
  let response' = UserStartResponse response
  userServer <- I.getUserServer $ userInterface user
  case userServer of
    Just userServer ->
      writeTQueue (usseActions userServer) $ UssaStartUser user response'
    Nothing -> do
      errorText <- I.lookupText (userInterface user) $ T.pack "User server is not registered"
      putTMVar response . Left $ Error [errorText]
  return response'

-- | Wait for response to starting user thread.
waitStart :: UserStartResponse -> STM (Either Error ())
waitStart (UserStartResponse response) = readTMVar response

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

-- | Get connection manager.
getConnectionManager :: User -> ConnectionManager
getConnectionManager user = userConnectionManager user

-- | Get nick.
getNick :: User -> STM Nick
getNick user = readTVar $ userNick user
