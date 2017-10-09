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
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THEfffffff
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
   isConnectionActive,
   areConnectionActionsPending,
   getConnectionError)

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

-- | Create a new connection.
newConnection :: STM Connection
newConnection = do
  state <- newTVar ConnectionNotStarted
  hostname <- newEmptyTMVar
  address <- newEmptyTMVar
  port <- newEmptyTMVar
  socket <- newEmptyTMVar
  actionQueue <- newTQueue
  eventQueue <- newBroadcastTChan
  return $ Connection
    { connectionState = state,
      connectionHostname = hostname,
      connectionAddress = address,
      connectionPort = port,
      connectionSocket = socket,
      connectionActionQueue = actionQueue,
      connectionEventQueue = eventQueue }

-- | Start execution of a thread for a connection.
startConnection :: Connection -> IO (Either Error ())
startConnection connection = do
  alreadyRunning <- atomically $ do
    state <- readTVar $ connectionState connection
    if state == ConnectionNotStarted
      then do writeTVar (connectionState connection) ConnectionStarted
              tryTakeTMVar (connectionHostname connection) >> return ()
              tryTakeTMVar (connectionAddress connection) >> return ()
              tryTakeTMVar (connectionPort connection) >> return ()
              tryTakeTMVar (connectionSocket connection) >> return ()
              clearActions connection
              return False
      else return True
  if not alreadyRunning
    then do async $ runUnconnected connection
            return $ Right ()
    else return . Left . Error $ "connection already started"

-- | Stop execution of a thread for a connection.
stopConnection :: Connection -> STM (Response ())
stopConnection connection = do
  state <- readTVar $ connectionState connection
  response <- newEmptyTMVar
  if state /= ConnectionNotStarted
    then writeTQueue (connectionActionQueue connection)
         (StopConnection $ Response response)
    else putTMVar response . Left $ Error "connection not started")
  return $ Response response

-- | Connect a connection.
connect :: Connection -> NS.HostName -> NS.PortNumber -> STM (Response ())
connect connection hostname port = do
  state <- readTVar $ connectionState connection
  response <- newEmptyTMVar
  if state /= ConnectionNotStarted
    then writeTQueue (connectionActionQueue connection)
         (Connect hostname port $ Response response)
    else putTMVar response . Left $ Error "connection not started")
  return $ Response response

-- | Disconnect a connection.
disconnect :: Connection -> STM (Response ())
disconnect connection = do
  state <- readTVar $ connectionState connection
  response <- newEmptyTMVar
  if state /= ConnectionNotStarted
    then writeTQueue (connectionActionQueue connection)
         (Disconnect $ Response response)
    else putTMVar response . Left $ Error "connection not started")
  return $ Response response

-- | Send data to a connection.
sendData :: Connection -> B.ByteString -> STM (Response ())
sendData connection bytes = do
  state <- readTVar $ connectionState connection
  response <- newEmptyTMVar
  if state /= ConnectionNotStarted
    then writeTQueue (connectionActionQueue connection)
         (SendData bytes $ Response response)
    else putTMVar response . Left $ Error "connection not started")
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
getConnectionState :: Connection -> STM ConnectionState
getConnectionState = readTVar . connectionState

-- | Get the hostname of a connection.
getConnectionHostname :: Connection -> STM (Maybe NS.HostName)
getConnectionHostname = tryReadTMVar . connectionHostname

-- | Get the address of a connection.
getConnectionAddress :: Connection -> STM (Maybe NS.AddrInfo)
getConnectionAddress = tryReadTMVar . connectionAddress

-- | Get the port of a connection.
getConnectionPort :: Connection -> STM (Maybe NS.PortNumber)
getConnectionPort = tryReadTMVar . connectionPort

-- | Get whether a connection is active.
isConnectionActive :: Connection -> STM Bool
isConnectionActive connection = do
  state <- readTVar $ connectionState connection
  case state of
    ConnectionNotStarted -> return False
    ConnectionStarted -> return False
    ConnectionFindingAddr -> return True
    ConnectionNoAddrFound _ -> return False
    ConnectionLookupCanceled -> return False
    ConnectionFindingName -> return True
    ConnectionNoNameFound _ -> return True
    ConnectionReverseLookupCanceled -> return False
    ConnectionConnecting -> return True
    ConnectionConnected -> return True
    ConnectionConnectingFailed _ -> return False
    ConnectionConnectingCanceled -> return False
    ConnectionDisconnected -> return False
    ConnectionDisconnectError _ -> return False
    ConnectionDisconnectedByPeer -> return False
    ConnectionRecvError _ -> return False
    ConnectionSendError _ -> return False

-- | Get whether connection actions are pending.
areConnectionActionsPending :: Connection -> STM Bool
areConnectionActionsPending = isEmptyTQueue . connectionActionQueue

-- | Get connection error text.
getConnectionError :: Connection -> STM (Maybe Error)
getConnectionError connection = do
  state <- readTVar $ connectionState connection
  case state of
    ConnectionNotStarted -> return Nothing
    ConnectionStarted -> return Nothing
    ConnectionFindingAddr -> return Nothing
    ConnectionNoAddrFound failure -> return $ Just failure
    ConnectionLookupCanceled -> return Nothing
    ConnectionFindingName -> return Nothing
    ConnectionNoNameFound failure -> return $ Just failure
    ConnectionReverseLookupCanceled -> return Nothing
    ConnectionConnecting -> return Nothing
    ConnectionConnected -> return Nothing
    ConnectionConnectingFailed failure -> return $ Just failure
    ConnectionConnectingCanceled -> return Nothing
    ConnectionDisconnected -> return Nothing
    ConnectionDisconnectError failure -> return $ Just failure
    ConnectionDisconnectedByPeer -> return Nothing
    ConnectionRecvError failure -> return $ Just failure
    ConnectionSendError failure -> return $ Just failure

-- | Run an unconnected connection.
runUnconnected :: Connection -> IO ()
runUnconnected connection = do
  action <- atomically . readTQueue $ connectionActionQueue connection
  case action of
    Connect hostname port response ->
      runLookupAddress connection hostname port response
    StopConnection (Response response) -> do
      atomically $ do
        writeTChan (connectionEventQueue connection) ConnectionStopped
        writeTVar (connectionState connection) ConnectionNotStarted
        putTMVar response (Right ())
        clearActions connection
    Disconnect (Response response) -> do
      atomically . putTMVar response . Left . Error $ "not connected"
      runUnconnected connection
    SendData _ (Response response) -> do
      atomically . putTMVar response . Left . Error $ "not connected"
      runUnconnected connection

-- | Run looking up a hostname for a connection.
runLookupAddress :: Connection -> NS.HostName -> NS.PortNumber -> Response () ->
                    IO ()
runLookupAddress connection hostname port connectResponse = do
  atomically $ do writeTVar (connectionState connection) ConnectionFindingAddr
                  tryTakeTMVar (connectionHostname connection) >> return ()
                  tryTakeTMVar (connectionAddress connection) >> return ()
                  tryTakeTMVar (connectionPort connection) >> return ()
                  tryTakeTMVar (connectionSocket connection) >> return ()
  resultVar <- atomically newEmptyTMVar
  lookup <- async $ doLookup resultVar hostname port
  handleMessages connection lookup resultVar hostname port connectResponse
  where doLookup resultVar hostname port = do
          result <- do
            let hints = NS.defaultHints { NS.addrFlags = [NS.AI_NUMERICSERV],
                                          NS.addrSocketType = NS.Stream }
            catch (Right <$> NS.getAddrInfo (Just hints) (Just hostname)
                    (Just $ show port))
                  (\e -> return . Left . T.pack $ show (e :: IOException))
          atomically $ putTMVar resultVar result
        handleMessages connection lookup resultVar hostname port
          connectResponse@(Response connectResponse') = do
          lookupOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue connection))
          case lookupOrAction of
            Right (Right resultVar) -> do
              let address = resultVar !! 0
              atomically $ do
                putTMVar (connectionHostname connection) hostname
                putTMVar (connectionPort connection) port
                putTMVar (connectionAddress connection) address
                writeTVar (connectionState connection) ConnectionFindingName
                writeTChan (connectionEventQueue connection) (FoundAddr address)
              runLookupName connection connectResponse
            Right (Left errorText) -> do
              atomically $ do
                writeTVar (connectionState connection)
                  (ConnectionNoAddrFound $ Error errorText)
                writeTChan (connectionEventQueue connection)
                  (NoAddrFound $ Error errorText)
                putTMVar connectResponse' . Left . Error $ errorText
                clearActions connection
              runUnconnected connection
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connecting"
              handleMessages connection lookup resultVar hostname port
                connectResponse
            Left (Disconnect (Response response)) -> do
              cancel lookup
              atomically $ do
                writeTVar (connectionState connection)
                  ConnectionLookupCanceled
                writeTChan (connectionEventQueue connection) LookupCanceled
                putTMVar connectResponse' . Left . Error $ "lookup canceled"
                putTMVar response $ Right ()
              runUnconnected connection
            Left (SendData _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "not connected"
              handleMessages connection lookup resultVar hostname port
                connectResponse
            Left (StopConnection (Response response)) -> do
              cancel lookup
              atomically $ do
                writeTVar (connectionState connection) ConnectionNotStarted
                writeTChan (connectionEventQueue connection) LookupCanceled
                writeTChan (connectionEventQueue connection) ConnectionStopped
                putTMVar response $ Right ()
                clearActions connection

-- | Run reverse looking up a name for a connection.
runLookupName :: Connection -> Response () -> IO ()
runLookupName connection connectResponse = do
  (resultVar, address) <- atomically $ do
    resultVar <- newEmptyTMVar
    address <- NS.addrAddress <$> readTMVar (connectionAddress connection)
    return (resultVar, address)
  lookup <- async $ doLookup resultVar address
  handleMessages connection lookup resultVar connectResponse
  where doLookup resultVar address = do
          result <- do
            catch (Right <$> actuallyDoLookup address)
                  (\e -> return . Left . T.pack $ show (e :: IOException))
          atomically $ putTMVar resultVar result
        actuallyDoLookup address = do
          (Just hostname, _) <- NS.getNameInfo [] True False address
          return hostname
        handleMessages connection lookup resultVar
          connectResponse@(Response connectResponse') = do
          lookupOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue connection))
          case lookupOrAction of
            Right (Right hostname) -> do
              atomically $ do
                (tryTakeTMVar $ connectionHostname connection) >> return ()
                putTMVar (connectionHostname connection) hostname
                writeTVar (connectionState connection) ConnectionConnecting
                writeTChan (connectionEventQueue connection)
                  (FoundName hostname)
              runConnecting connection connectResponse
            Right (Left errorText) -> do
              atomically $ do
                writeTVar (connectionState connection)
                  (ConnectionNoNameFound $ Error errorText)
                writeTChan (connectionEventQueue connection)
                  (NoNameFound $ Error errorText)
              runConnecting connection connectResponse
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connecting"
              handleMessages connection lookup resultVar
                connectResponse
            Left (Disconnect (Response response)) -> do
              cancel lookup
              atomically $ do
                writeTVar (connectionState connection)
                  ConnectionReverseLookupCanceled
                writeTChan (connectionEventQueue connection)
                  ReverseLookupCanceled
                putTMVar connectResponse' . Left . Error $
                  "reverse lookup canceled"
                putTMVar response $ Right ()
              runUnconnected connection
            Left (SendData _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "not connected"
              handleMessages connection lookup resultVar
                connectResponse
            Left (StopConnection (Response response)) -> do
              cancel lookup
              atomically $ do
                writeTVar (connectionState connection) ConnectionNotStarted
                writeTChan (connectionEventQueue connection) LookupCanceled
                writeTChan (connectionEventQueue connection) ConnectionStopped
                putTMVar response $ Right ()
                clearActions connection

-- | Run connecting to a host.
runConnecting :: Connection -> Response () -> IO ()
runConnecting connection response = do
  resultVar <- atomically newEmptyTMVar
  connecting <- async $ doConnecting connection resultVar
  handleMessages connection connecting resultVar response
  where doConnecting connection resultVar = do
          result <- catch
            (Right <$> actuallyDoConnecting connection)
            (\e -> return . Left . T.pack $ show (e :: IOException))
          atomically $ putTMVar resultVar result
        actuallyDoConnecting connection = do
          address <- atomically . readTMVar $ connectionAddress connection
          socket <- NS.socket (NS.addrFamily address)
            (NS.addrSocketType address) (NS.addrProtocol address)
          NS.connect socket $ NS.addrAddress address
          return socket
        handleMessages connection connecting resultVar
          connectResponse@(Response connectResponse') = do
          connectOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue connection))
          case connectOrAction of
            Right (Right socket) -> do
              atomically $ do
                putTMVar (connectionSocket connection) socket
                writeTVar (connectionState connection) ConnectionConnected
                writeTChan (connectionEventQueue connection) Connected
                putTMVar connectResponse' $ Right ()
              runConnected connection
            Right (Left errorText) -> do
              atomically $ do
                writeTVar (connectionState connection)
                  (ConnectionConnectingFailed $ Error errorText)
                writeTChan (connectionEventQueue connection)
                  (ConnectingFailed $ Error errorText)
                putTMVar connectResponse' . Left $ Error errorText
                clearActions connection
              runUnconnected connection
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connecting"
              handleMessages connection connecting resultVar connectResponse
            Left (Disconnect (Response response)) -> do
              cancel connecting
              atomically $ do
                writeTVar (connectionState connection)
                  ConnectionConnectingCanceled
                writeTChan (connectionEventQueue connection) ConnectingCanceled
                putTMVar connectResponse' . Left . Error $ "connect canceled"
                putTMVar response $ Right ()
              runUnconnected connection
            Left (SendData _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "not connected"
              handleMessages connection connecting resultVar connectResponse
            Left (StopConnection (Response response)) -> do
              cancel connecting
              atomically $ do
                writeTVar (connectionState connection) ConnectionNotStarted
                writeTChan (connectionEventQueue connection) ConnectingCanceled
                writeTChan (connectionEventQueue connection) ConnectionStopped
                putTMVar response $ Right ()
                clearActions connection

-- | Run being connected to a host.
runConnected :: Connection -> IO ()
runConnected connection = do
  resultVar <- atomically newEmptyTMVar
  receiving <- async $ doReceiving connection resultVar
  handleMessages connection receiving resultVar
  where doReceiving connection resultVar = do
          socket <- atomically . readTMVar $ connectionSocket connection
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
                  atomically . writeTChan (connectionEventQueue connection) $
                    RecvData bytes
                  doReceiving connection resultVar
            Left errorText -> atomically . putTMVar resultVar $ Left errorText
        handleMessages connection receiving resultVar = do
          closeOrAction <- atomically $
            (Right <$> takeTMVar resultVar) `orElse`
            (Left <$> readTQueue (connectionActionQueue connection))
          case closeOrAction of
            Right (Right ()) -> do
              atomically $ do
                takeTMVar $ connectionSocket connection
                writeTVar (connectionState connection)
                  ConnectionDisconnectedByPeer
                writeTChan (connectionEventQueue connection)
                  DisconnectedByPeer
                clearActions connection
              runUnconnected connection
            Right (Left errorText) -> do
              atomically $ do
                takeTMVar $ connectionSocket connection
                writeTVar (connectionState connection)
                  (ConnectionRecvError $ Error errorText)
                writeTChan (connectionEventQueue connection)
                  (RecvError $ Error errorText)
                clearActions connection
              runUnconnected connection
            Left (Connect _ _ (Response response)) -> do
              atomically . putTMVar response . Left . Error $
                "already connected"
              handleMessages connection receiving resultVar
            Left (Disconnect (Response response)) -> do
              doDisconnect connection receiving $ \result ->
                case result of
                  Right () -> do
                    writeTVar (connectionState connection)
                      ConnectionDisconnected
                    writeTChan (connectionEventQueue connection) Disconnected
                    putTMVar response $ Right ()
                  Left errorText -> do
                    writeTVar (connectionState connection)
                      (ConnectionDisconnectError $ Error errorText)
                    writeTChan (connectionEventQueue connection)
                      (DisconnectError $ Error errorText)
                    putTMVar response . Left $ Error errorText
              runUnconnected connection
            Left (SendData bytes (Response response)) -> do
              socket <- atomically . readTMVar $ connectionSocket connection
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
                  atomically $ do
                    takeTMVar $ connectionSocket connection
                    writeTVar (connectionState connection)
                      (ConnectionSendError $ Error errorText)
                    writeTChan (connectionEventQueue connection)
                      (SendError $ Error errorText)
                    putTMVar response . Left $ Error errorText
                  runUnconnected connection
            Left (StopConnection (Response response)) -> do
              doDisconnect connection receiving $ \result -> do
                writeTVar (connectionState connection) ConnectionNotStarted
                case result of
                  Right () -> do
                    writeTChan (connectionEventQueue connection) Disconnected
                    putTMVar response $ Right ()
                    clearActions connection
                  Left errorText -> do
                    writeTVar (connectionState connection)
                      (ConnectionDisconnectError $ Error errorText)
                    writeTChan (connectionEventQueue connection)
                      (DisconnectError $ Error errorText)
                    putTMVar response . Left $ Error errorText
                    writeTChan (connectionEventQueue connection)
                      ConnectionStopped
                    clearActions connection
        doDisconnect connection receiving handleResult = do
          socket <- atomically . readTMVar $ connectionSocket connection
          result <- catch
            (Right <$> NS.shutdown socket NS.ShutdownBoth)
            (\e -> return . Left . T.pack $ show (e :: IOException))
          cancel receiving
          case result of
            Right () -> do
              atomically $ do
                takeTMVar $ connectionSocket connection
                handleResult result
            Left errorText -> do
              atomically $ do
                takeTMVar $ connectionSocket connection
                handleResult result

-- | Clear actions.
clearActions :: Connection -> STM ()
clearActions connection = do
  action <- tryReadTQueue $ connectionActionQueue connection
  case action of
    Just action -> do
      let response = case action of
            Connect _ _ (Response response) -> response
            Disconnect (Response response) -> response
            SendData _ (Response response) -> response
            StopConnection (Response response) -> response
      putTMVar response . Left $ Error "canceled"
      clearActions connection
    Nothing -> return ()
