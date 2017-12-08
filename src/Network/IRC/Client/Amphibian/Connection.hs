-- Copyright (c) 2017, Travis Bemann
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- o Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- o Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- o Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Network.IRC.Client.Amphibian.Connection

  (Response(..),
   Error(..),
   Connection,
   ConnectionState(..),
   ConnectionEvent(..),
   ConnectionEventSub,
   newConnection,
   startConnection,
   stopConnection,
   connect,
   disconnect,
   sendData,
   subscribeConnection,
   recvConnection,
   tryRecvConnection,
   getConnectionState,
   getConnectionHostname,
   getConnectionAddress,
   getConnectionPort,
   isConnectionStateActive)

where

import Network.IRC.Client.Amphibian.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Data.Functor ((<$>))
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Exception (catch,
                          IOException,
                          SomeException)
import Control.Concurrent.STM (STM,
                               atomically,
                               orElse,
                               TVar,
                               newTVar,
                               readTVar,
                               writeTVar)
import Control.Concurrent.STM.TChan (TChan,
                                     newBroadcastTChan,
                                     dupTChan,
                                     writeTChan,
                                     readTChan,
                                     tryReadTChan)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue,
                                      tryReadTQueue,
                                      isEmptyTQueue)
import Control.Concurrent.STM.TMVar (TMVar,
                                     newEmptyTMVar,
                                     putTMVar,
                                     tryTakeTMVar,
                                     tryReadTMVar,
                                     takeTMVar,
                                     readTMVar)
import Text.Printf (printf)

-- | Connection state.
data ConnectionData = ConnectionData
  { connectionState :: ConnectionState,
    connectionHostname :: Maybe NS.HostName,
    connectionAddress :: Maybe NS.AddrInfo,
    connectionPort ::  Maybe NS.PortNumber,
    connectionSocket :: Maybe NS.Socket }

-- | Create a new connection.
newConnection :: STM Connection
newConnection = do
  running <- newTVar False
  actionQueue <- newTQueue
  eventQueue <- newBroadcastTChan
  return $ Connection
    { connectionRunning = running,
      connectionActionQueue = actionQueue,
      connectionEventQueue = eventQueue }

-- | Start execution of a thread for a connection.
startConnection :: Connection -> IO (Either Error ())
startConnection connection = do
  alreadyRunning <- atomically $ do
    running <- readTVar $ connectionRunning connection
    if not running
      then do writeTVar (connectionRunning connection) True
              return False
      else return True
  if not alreadyRunning
    then do let state = ConnectionData { connectionState = ConnectionStarted,
                                         connectionHostname = Nothing,
                                         connectionAddress = Nothing,
                                         connectionPort = Nothing,
                                         connectionSocket = Nothing }
            async $ runUnconnected connection state
            return $ Right ()
    else return . Left $ Error "connection already started"

-- | Stop execution of a thread for a connection.
stopConnection :: Connection -> STM (Response ())
stopConnection connection = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  if running
    then writeTQueue (connectionActionQueue connection)
         (StopConnection $ Response response)
    else putTMVar response . Left $ Error "connection not started"
  return $ Response response

-- | Connect a connection.
connect :: Connection -> NS.HostName -> NS.PortNumber -> STM (Response ())
connect connection hostname port = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  if running
    then writeTQueue (connectionActionQueue connection)
         (Connect hostname port $ Response response)
    else putTMVar response . Left $ Error "connection not started"
  return $ Response response

-- | Disconnect a connection.
disconnect :: Connection -> STM (Response ())
disconnect connection = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  if running
    then writeTQueue (connectionActionQueue connection)
         (Disconnect $ Response response)
    else putTMVar response . Left $ Error "connection not started"
  return $ Response response

-- | Send data to a connection.
sendData :: Connection -> B.ByteString -> STM (Response ())
sendData connection bytes = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  if running
    then writeTQueue (connectionActionQueue connection)
         (SendData bytes $ Response response)
    else putTMVar response . Left $ Error "connection not started"
  return $ Response response

-- | Subscribe to a connection.
subscribeConnection :: Connection -> STM ConnectionEventSub
subscribeConnection connection =
  ConnectionEventSub <$> dupTChan (connectionEventQueue connection)

-- | Receive an event from a connection.
recvConnection :: ConnectionEventSub -> STM ConnectionEvent
recvConnection (ConnectionEventSub sub) = readTChan sub

-- | Try to receive an event from a connection.
tryRecvConnection :: ConnectionEventSub -> STM (Maybe ConnectionEvent)
tryRecvConnection (ConnectionEventSub sub) = tryReadTChan sub

-- | Get the state of a connection.
getConnectionState :: Connection -> STM (Response ConnectionState)
getConnectionState connection = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (connectionActionQueue connection) $
         GetConnectionState response'
    else putTMVar response $ Right ConnectionNotStarted
  return response'

-- | Get the hostname of a connection.
getConnectionHostname :: Connection -> STM (Response (Maybe NS.HostName))
getConnectionHostname connection = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (connectionActionQueue connection) $
         GetConnectionHostname response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Get the address of a connection.
getConnectionAddress :: Connection -> STM (Response (Maybe NS.AddrInfo))
getConnectionAddress connection = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (connectionActionQueue connection) $
         GetConnectionAddress response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Get the port of a connection.
getConnectionPort :: Connection -> STM (Response (Maybe NS.PortNumber))
getConnectionPort connection = do
  running <- readTVar $ connectionRunning connection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (connectionActionQueue connection) $
         GetConnectionPort response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Test whether a state is the connection being active
isConnectionStateActive :: ConnectionState -> Bool
isConnectionStateActive ConnectionNotStarted = False
isConnectionStateActive ConnectionStarted = False
isConnectionStateActive ConnectionFindingAddr = True
isConnectionStateActive (ConnectionNoAddrFound _) = False
isConnectionStateActive ConnectionLookupCanceled = False
isConnectionStateActive ConnectionFindingName = True
isConnectionStateActive (ConnectionNoNameFound _) = True
isConnectionStateActive ConnectionReverseLookupCanceled = False
isConnectionStateActive ConnectionConnecting = True
isConnectionStateActive ConnectionConnected = True
isConnectionStateActive (ConnectionConnectingFailed _) = False
isConnectionStateActive ConnectionConnectingCanceled = False
isConnectionStateActive ConnectionDisconnected = False
isConnectionStateActive (ConnectionDisconnectError _) = False
isConnectionStateActive ConnectionDisconnectedByPeer = False
isConnectionStateActive (ConnectionRecvError _) = False
isConnectionStateActive (ConnectionSendError _) = False

-- | Run an unconnected connection.
runUnconnected :: Connection -> ConnectionData -> IO ()
runUnconnected outer connection = do
  action <- atomically . readTQueue $ connectionActionQueue outer
  case action of
    Connect hostname port response ->
      runLookupAddress outer connection hostname port response
    StopConnection (Response response) -> do
      atomically $ do
        writeTChan (connectionEventQueue outer) ConnectionStopped
        putTMVar response (Right ())
        clearActions outer connection
    Disconnect (Response response) -> do
      atomically . putTMVar response . Left . Error $ "not connected"
      runUnconnected outer connection
    SendData _ (Response response) -> do
      atomically . putTMVar response . Left . Error $ "not connected"
      runUnconnected outer connection
    GetConnectionState (Response response) -> do
      atomically . putTMVar response . Right $ connectionState connection
      runUnconnected outer connection
    GetConnectionHostname (Response response) -> do
      atomically . putTMVar response $ Right Nothing
      runUnconnected outer connection
    GetConnectionPort (Response response) -> do
      atomically . putTMVar response $ Right Nothing
      runUnconnected outer connection
    GetConnectionAddress (Response response) -> do
      atomically . putTMVar response $ Right Nothing
      runUnconnected outer connection

-- | Run looking up a hostname for a connection.
runLookupAddress :: Connection -> ConnectionData -> NS.HostName ->
                    NS.PortNumber -> Response () ->
                    IO ()
runLookupAddress outer connection hostname port
  connectResponse@(Response connectResponse') = do
  resultVar <- atomically newEmptyTMVar
  lookup <- async $ doLookup resultVar hostname port
  handleMessages connection lookup resultVar
  where doLookup resultVar hostname port = do
          result <- do
            let hints = NS.defaultHints { NS.addrFlags = [NS.AI_NUMERICSERV],
                                          NS.addrSocketType = NS.Stream }
            catch (Right <$> NS.getAddrInfo (Just hints) (Just hostname)
                    (Just $ show port))
                  (\e -> return . Left . T.pack $ show (e :: IOException))
          atomically $ putTMVar resultVar result
        handleMessages connection lookup resultVar = do
          lookupOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue outer))
          case lookupOrAction of
            Right (Right resultVar) -> do
              let address = resultVar !! 0
                  connection' =
                    connection { connectionHostname = Just hostname,
                                 connectionPort = Just port,
                                 connectionAddress = Just address,
                                 connectionState = ConnectionFindingName }
              atomically $
                writeTChan (connectionEventQueue outer) (FoundAddr address)
              runLookupName outer connection' connectResponse
            Right (Left errorText) -> do
              let connection' =
                    connection { connectionState =
                                   ConnectionNoAddrFound $ Error errorText }
              atomically $ do
                writeTChan (connectionEventQueue outer)
                  (NoAddrFound $ Error errorText)
                putTMVar connectResponse' . Left . Error $ errorText
                clearActions outer connection'
              runUnconnected outer connection'
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left $ Error "already connecting"
              handleMessages connection lookup resultVar
            Left (Disconnect (Response response)) -> do
              cancel lookup
              let connection' =
                    connection { connectionState = ConnectionLookupCanceled }
              atomically $ do
                writeTChan (connectionEventQueue outer) LookupCanceled
                putTMVar connectResponse' . Left $ Error "lookup canceled"
                putTMVar response $ Right ()
              runUnconnected outer connection'
            Left (SendData _ (Response response)) -> do
              atomically . putTMVar response . Left $ Error "not connected"
              handleMessages connection lookup resultVar
            Left (StopConnection (Response response)) -> do
              cancel lookup
              let connection' =
                    connection { connectionState = ConnectionLookupCanceled }
              atomically $ do
                writeTVar (connectionRunning outer) False
                writeTChan (connectionEventQueue outer) LookupCanceled
                writeTChan (connectionEventQueue outer) ConnectionStopped
                putTMVar response $ Right ()
                clearActions outer connection'
            Left (GetConnectionState (Response response)) -> do
              atomically . putTMVar response $ Right ConnectionFindingAddr
              handleMessages connection lookup resultVar
            Left (GetConnectionHostname (Response response)) -> do
              atomically . putTMVar response . Right $ Just hostname
              handleMessages connection lookup resultVar
            Left (GetConnectionPort (Response response)) -> do
              atomically . putTMVar response . Right $ Just port
              handleMessages connection lookup resultVar
            Left (GetConnectionAddress (Response response)) -> do
              atomically . putTMVar response $ Right Nothing
              handleMessages connection lookup resultVar

-- | Run reverse looking up a name for a connection.
runLookupName :: Connection -> ConnectionData -> Response () -> IO ()
runLookupName outer connection connectResponse@(Response connectResponse') = do
  resultVar <- atomically newEmptyTMVar
  let address = case connectionAddress connection of
                  Just address -> address
                  Nothing -> error "impossible"
  lookup <- async . doLookup resultVar $ NS.addrAddress address
  handleMessages connection lookup resultVar
  where doLookup resultVar address = do
          result <- do
            catch (Right <$> actuallyDoLookup address)
                  (\e -> return . Left . T.pack $ show (e :: IOException))
          atomically $ putTMVar resultVar result
        actuallyDoLookup address = do
          (Just hostname, _) <- NS.getNameInfo [] True False address
          return hostname
        handleMessages connection lookup resultVar = do
          lookupOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue outer))
          case lookupOrAction of
            Right (Right hostname) -> do
              let connection' =
                    connection { connectionHostname = Just hostname,
                                 connectionState = ConnectionConnecting }
              atomically $ do
                writeTChan (connectionEventQueue outer)
                  (FoundName hostname)
              runConnecting outer connection' connectResponse
            Right (Left errorText) -> do
              let connection' =
                    connection { connectionState =
                                 ConnectionNoNameFound $ Error errorText }
              atomically $ do
                writeTChan (connectionEventQueue outer)
                  (NoNameFound $ Error errorText)
              runConnecting outer connection' connectResponse
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connecting"
              handleMessages connection lookup resultVar
            Left (Disconnect (Response response)) -> do
              cancel lookup
              let connection' =
                    connection { connectionState =
                                 ConnectionReverseLookupCanceled }
              atomically $ do
                writeTChan (connectionEventQueue outer) ReverseLookupCanceled
                putTMVar connectResponse' . Left . Error $
                  "reverse lookup canceled"
                putTMVar response $ Right ()
              runUnconnected outer connection'
            Left (SendData _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "not connected"
              handleMessages connection lookup resultVar
            Left (StopConnection (Response response)) -> do
              cancel lookup
              let connection' =
                    connection { connectionState = ConnectionLookupCanceled }
              atomically $ do
                writeTVar (connectionRunning outer) False
                writeTChan (connectionEventQueue outer) LookupCanceled
                writeTChan (connectionEventQueue outer) ConnectionStopped
                putTMVar response $ Right ()
                clearActions outer connection'
            Left (GetConnectionState (Response response)) -> do
              atomically . putTMVar response $ Right ConnectionFindingName
              handleMessages connection lookup resultVar
            Left (GetConnectionHostname (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionHostname connection
              handleMessages connection lookup resultVar
            Left (GetConnectionPort (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionPort connection
              handleMessages connection lookup resultVar
            Left (GetConnectionAddress (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionAddress connection
              handleMessages connection lookup resultVar

-- | Run connecting to a host.
runConnecting :: Connection -> ConnectionData -> Response () -> IO ()
runConnecting outer connection (Response connectResponse) = do
  resultVar <- atomically newEmptyTMVar
  connecting <- async $ doConnecting connection resultVar
  handleMessages connection connecting resultVar
  where doConnecting connection resultVar = do
          result <- catch
            (Right <$> actuallyDoConnecting connection)
            (\e -> return . Left . T.pack $ show (e :: IOException))
          atomically $ putTMVar resultVar result
        actuallyDoConnecting connection = do
          case connectionAddress connection of
            Just address -> do
              socket <- NS.socket (NS.addrFamily address)
               (NS.addrSocketType address) (NS.addrProtocol address)
              NS.connect socket $ NS.addrAddress address
              return socket
            Nothing -> error "impossible"
        handleMessages connection connecting resultVar = do
          connectOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue outer))
          case connectOrAction of
            Right (Right socket) -> do
              let connection' =
                    connection { connectionSocket = Just socket,
                                 connectionState = ConnectionConnected }
              atomically $ do
                writeTChan (connectionEventQueue outer) Connected
                putTMVar connectResponse $ Right ()
              runConnected outer connection'
            Right (Left errorText) -> do
              let connection' =
                    connection { connectionState =
                                 ConnectionConnectingFailed $ Error errorText }
              atomically $ do
                writeTChan (connectionEventQueue outer)
                  (ConnectingFailed $ Error errorText)
                putTMVar connectResponse . Left $ Error errorText
                clearActions outer connection'
              runUnconnected outer connection'
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connecting"
              handleMessages connection connecting resultVar
            Left (Disconnect (Response response)) -> do
              cancel connecting
              let connection' =
                    connection { connectionState =
                                 ConnectionConnectingCanceled }
              atomically $ do
                writeTChan (connectionEventQueue outer) ConnectingCanceled
                putTMVar connectResponse . Left $ Error "connect canceled"
                putTMVar response $ Right ()
              runUnconnected outer connection'
            Left (SendData _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "not connected"
              handleMessages connection connecting resultVar
            Left (StopConnection (Response response)) -> do
              cancel connecting
              let connection' =
                    connection { connectionState =
                                   ConnectionConnectingCanceled }
              atomically $ do
                writeTVar (connectionRunning outer) False
                writeTChan (connectionEventQueue outer) ConnectingCanceled
                writeTChan (connectionEventQueue outer) ConnectionStopped
                putTMVar connectResponse . Left $ Error "connect canceled"
                putTMVar response $ Right ()
                clearActions outer connection'
            Left (GetConnectionState (Response response)) -> do
              atomically . putTMVar response $ Right ConnectionConnecting
              handleMessages connection connecting resultVar
            Left (GetConnectionHostname (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionHostname connection
              handleMessages connection connecting resultVar
            Left (GetConnectionPort (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionPort connection
              handleMessages connection connecting resultVar
            Left (GetConnectionAddress (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionAddress connection
              handleMessages connection connecting resultVar

-- | Run being connected to a host.
runConnected :: Connection -> ConnectionData -> IO ()
runConnected outer connection = do
  case connectionSocket connection of
    Just socket -> do
      resultVar <- atomically newEmptyTMVar
      receiving <- async $ doReceiving socket resultVar
      handleMessages connection receiving resultVar
    Nothing -> error "impossible"
  where doReceiving socket resultVar = do
          result <- catch
            (Right <$> NSB.recv socket 4096)
            (\e -> do
                catch (Right <$> NS.shutdown socket NS.ShutdownBoth)
                  (\e -> return . Left . T.pack $ show (e :: IOException)) >>
                  return ()
                return . Left . T.pack $ show (e :: IOException))
          case result of
            Right bytes
              | B.null bytes -> atomically . putTMVar resultVar $ Right ()
              | otherwise -> do
                  atomically . writeTChan (connectionEventQueue outer) $
                    RecvData bytes
                  doReceiving socket resultVar
            Left errorText -> atomically . putTMVar resultVar $ Left errorText
        handleMessages connection receiving resultVar = do
          closeOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue outer))
          case closeOrAction of
            Right (Right ()) -> do
              let connection' =
                    connection { connectionSocket = Nothing,
                                 connectionState =
                                   ConnectionDisconnectedByPeer }
              atomically $ do
                writeTChan (connectionEventQueue outer)
                  DisconnectedByPeer
                clearActions outer connection'
              runUnconnected outer connection'
            Right (Left errorText) -> do
              let connection' =
                    connection { connectionSocket = Nothing,
                                 connectionState =
                                   ConnectionRecvError $ Error errorText }
              atomically $ do
                writeTChan (connectionEventQueue outer)
                  (RecvError $ Error errorText)
                clearActions outer connection'
              runUnconnected outer connection'
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connected"
              handleMessages connection receiving resultVar
            Left (Disconnect (Response response)) -> do
              doDisconnect connection receiving $ \result connection ->
                case result of
                  Right () -> do
                    let connection' =
                          connection { connectionState =
                                         ConnectionDisconnected }
                    atomically $ do
                      writeTChan (connectionEventQueue outer) Disconnected
                      putTMVar response $ Right ()
                    runUnconnected outer connection'
                  Left errorText -> do
                    let connection' =
                          connection { connectionState =
                                         ConnectionDisconnectError $
                                         Error errorText }
                    atomically $ do
                      writeTChan (connectionEventQueue outer)
                        (DisconnectError $ Error errorText)
                      putTMVar response . Left $ Error errorText
                    runUnconnected outer connection'
            Left (SendData bytes (Response response)) -> do
              let socket = case connectionSocket connection of
                             Just socket -> socket
                             Nothing -> error "impossible"
              result <- catch
                (Right <$> NSB.sendAll socket bytes)
                (\e -> return . Left . T.pack $ show (e :: IOException))
              case result of
                Right () -> do
                  atomically . putTMVar response $ Right ()
                  handleMessages connection receiving resultVar
                Left errorText -> do
                  catch (Right <$> NS.shutdown socket NS.ShutdownBoth)
                    (\e -> return . Left . T.pack $ show (e :: IOException)) >>
                    return ()
                  cancel receiving
                  let connection' =
                        connection { connectionSocket = Nothing,
                                     connectionState =
                                       ConnectionSendError $ Error errorText }
                  atomically $ do
                    writeTChan (connectionEventQueue outer)
                      (SendError $ Error errorText)
                    putTMVar response . Left $ Error errorText
                  runUnconnected outer connection'
            Left (StopConnection (Response response)) -> do
              doDisconnect connection receiving $ \result connection -> do
                case result of
                  Right () -> do
                    let connection' =
                          connection { connectionState =
                                         ConnectionDisconnected }
                    atomically $ do
                      writeTVar (connectionRunning outer) False
                      writeTChan (connectionEventQueue outer) Disconnected
                      writeTChan (connectionEventQueue outer)
                        ConnectionStopped
                      putTMVar response $ Right ()
                      clearActions outer connection'
                  Left errorText -> do
                    let connection' =
                          connection { connectionState =
                                         ConnectionDisconnectError $
                                         Error errorText }
                    atomically $ do
                      writeTVar (connectionRunning outer) False
                      writeTChan (connectionEventQueue outer)
                        (DisconnectError $ Error errorText)
                      putTMVar response . Left $ Error errorText
                      writeTChan (connectionEventQueue outer)
                        ConnectionStopped
                      clearActions outer connection'
            Left (GetConnectionState (Response response)) -> do
              atomically . putTMVar response $ Right ConnectionConnected
              handleMessages connection receiving resultVar
            Left (GetConnectionHostname (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionHostname connection
              handleMessages connection receiving resultVar
            Left (GetConnectionPort (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionPort connection
              handleMessages connection receiving resultVar
            Left (GetConnectionAddress (Response response)) -> do
              atomically . putTMVar response . Right $
                connectionAddress connection
              handleMessages connection receiving resultVar
        doDisconnect connection receiving handleResult = do
          let socket = case connectionSocket connection of
                         Just socket -> socket
                         Nothing -> error "impossible"
          result <- catch
            (Right <$> NS.shutdown socket NS.ShutdownBoth)
            (\e -> return . Left . T.pack $ show (e :: IOException))
          cancel receiving
          let connection' =
                connection { connectionSocket = Nothing }
          handleResult result connection'

-- | Clear actions.
clearActions :: Connection -> ConnectionData -> STM ()
clearActions outer connection = do
  action <- tryReadTQueue $ connectionActionQueue outer
  case action of
    Just (Connect _ _  (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer connection
    Just (Disconnect (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer connection
    Just (SendData _ (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer connection
    Just (StopConnection (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer connection
    Just (GetConnectionState (Response response)) -> do
      putTMVar response . Right $ connectionState connection
      clearActions outer connection
    Just (GetConnectionHostname (Response response)) -> do
      putTMVar response . Right $ connectionHostname connection
      clearActions outer connection
    Just (GetConnectionPort (Response response)) -> do
      putTMVar response . Right $ connectionPort connection
      clearActions outer connection
    Just (GetConnectionAddress (Response response)) -> do
      putTMVar response . Right $ connectionAddress connection
      clearActions outer connection
    Nothing -> return ()
