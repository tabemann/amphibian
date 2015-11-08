module Network.IRC.Client.Amphibian.ConnectionManager

       (ConnectionManager,
        ConnectionManagerSetup,
        ConnectionManagerStartResponse,
        ConnectionManagerStopResponse,
        ConnectionManagerConnectResponse,
        ConnectionManagerReconnectResponse,
        ConnectionManagerDisconnectResponse,
        ConnectionManagerStopResponse,
        isRegistered,
        isConnected,
        getNick,
        getSetup,
        new,
        start,
        stop,
        connect,
        reconnect,
        disconnect,
        send,
        waitStart,
        waitStop,
        waitConnect,
        waitReconnect,
        waitDisconnect,
        waitSend,
        subscribe,
        recv)

       where

import qualified Network.IRC.Client.Amphibian.Interface as I
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Data.Functor ((<$>))
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               TMVar,
                               atomically,
                               orElse,
                               newTVar,
                               writeTVar,
                               readTVar,
                               newBroadcastTChan,
                               writeTChan,
                               dupTChan,
                               newEmptyTMVar,
                               putTMVar
                               readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import qualified Data.Text as T
-- | Create a connection manager.
new :: Interface -> STM ConnectionManager
new intf actionGroup = do
  thread <- newTVar Nothing
  actions <- newTQueue
  events <- newBroadcastTChan
  stop <- newEmptyTMVar
  setup <- newTVar Nothing
  connection <- newTVar Nothing
  registered <- newTVar False
  host <- newTVar Nothing
  nick <- newTVar Nothing
  subscription <- newTVar Nothing
  motd <- newTVar []
  active <- newTVar False
  return $ ConnectionManager { comaInterface = intf,
                               comaAsync = thread,
                               comaActions = actions,
                               comaEvents = events,
                               comaStop = stop,
                               comaSetup = setup,
                               comaConnection = connection,
                               comaRegistered = registered,
                               comaHost = host,
                               comaNick = nick,
                               comaSubscription = subscription,
                               comaMotd = motd,
                               comaActive = active }

-- | Get whether a connection manager is connected.
isConnected :: ConnectionManager -> STM Bool
isConnected = (/= Nothing) <$> readTVar . comaConnection

-- | Get whether a connection manager is registered.
isRegistered :: ConnectionManager -> STM Bool
isRegistered = readTVar . comaRegistered

-- | Get nick.
getNick :: ConnectionManager -> STM (Maybe Nick)
getNick = readTVar . comaNick

-- | Set nick.
setNick :: ConnectionManager -> Nick -> STM ()
setNick manager nick = writeTVar (comaNick manager) (Just nick)

-- | Get setup.
getSetup :: ConnectionManager -> STM (Maybe ConnectionManagerSetup)
getSetup = readTVar . comaSetup

-- | Start a connection manager thread.
start :: ConnectionManager -> STM ConnectionManagerStartResponse
start manager = do
  response <- newEmptyTMVar
  let response' = ConnectionManagerStartResponse response
  managerServer <- I.getConnectionManagerServer $ comaInterface manager
  case managerServer of
    Just managerServer ->
      writeTQueue (cmseActions managerServer) $ CmsaStartConnectionManager manager response'
    Nothing -> do
      errorText <- I.lookupText (comaInterface manager) $ T.pack "Connection manager server is not registered"
      putTMVar response . Left $ Error [errorText]
  return response'

-- | Stop connection manager.
stop :: ConnectionManager -> STM (ConnectionManagerStopResponse)
stop manager = do
  stop <- ConnectionManagerStopResponse <$> newEmptyTMVar
  putTMVar (comaStop manager) stop
  return stop

-- | Connect to a server.
connect :: ConnectionManager -> ConnectionManagerSetup -> STM ConnectionManagerConnectResponse
connect manager setup = do
  response <- ConnectionManagerConnectResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaConnectNew setup response)
  return response

-- | Reconnect to a server.
reconnect :: ConnectionManager -> STM ConnectionManagerReconnectResponse
reconnect manager = do
  response <- ConnectionManagerReconnectResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaReconnect response)
  return response

-- | Disconnect from a server.
disconnect :: ConnectionManager -> STM ConnectionManagerDisconnectResponse
disconnect manager = do
  response <- ConnectionManagerDisconnectResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaDisconnect response)
  return response

-- | Send a message to a server.
send :: ConnectionManager -> IRCMessage ->
        STM ConnectionManagerSendResponse
send manager message = do
  response <- ConnectionManagerSendResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaSend message response)
  return response

-- | Wait for response to starting connection manager thread.
waitStart :: ConnectionManagerStartResponse -> STM (Either Error ())
waitStart (ConnectionManagerStartResponse response) = readTMVar response

-- | Wait for response to stopping connection manager thread.
waitStop :: ConnectionManagerStopResponse -> STM (Either Error ())
waitStop (ConnectionManagerStopResponse response) = readTMVar response

-- | Wait for a connect response.
waitConnect :: ConnectionManagerConnectResponse -> STM (Either Error ())
waitConnect (ConnectionManagerConnectResponse response) =
  readTMVar response

-- | Wait for a reconnect response.
waitReconnect :: ConnectionManagerReconnectResponse -> STM (Either Error ())
waitReconnect (ConnectionManagerReconnectResponse response) =
  readTMVar response

-- | Wait for a disconnect response.
waitDisconnect :: ConnectionManagerDisconnectResponse -> STM (Either Error ())
waitDisconnect (ConnectionManagerDisconnectResponse response) =
  readTMVar response

-- | Wait for a send message response.
waitSend :: ConnectionManagerMessageResponse -> STM (Either Error ())
waitsend (ConnectionManagerMessageResponse response) =
  readTMVar response

-- | Subscribe to connection manager events.
subscribe :: ConnectionManager -> STM ConnectionManagerSubscription
subscribe manager =
  ConnectionManagerSubscription <$> dupTChan $ comaEvents manager

-- | Receive an event from a connection manager.
recv :: ConnectionManagerSubscription -> STM ConnectionManagerEvent
recv (ConnectionManagerSubscription chan) = readTChan chan
