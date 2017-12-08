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

{-# LANGUAGE OverloadedStrings, OverloadedLists, RecordWildCards #-}

module Network.IRC.Client.Amphibian.IRCConnection

  (Response(..),
   Error(..),
   IRCConnection,
   IRCConnectionState(..),
   IRCConnectionEvent(..),
   IRCConnectionEventSub,
   IRCMessage(..),
   newIRCConnection,
   startIRCConnection,
   stopIRCConnection,
   connectIRC,
   disconnectIRC,
   sendIRCMessage,
   subscribeIRCConnection,
   recvIRCConnection,
   tryRecvIRCConnection,
   getIRCConnectionState,
   getIRCConnectionHostname,
   getIRCConnectionAddress,
   getIRCConnectionPort,
   isIRCConnectionStateActive)

where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Connection
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Network.Socket as NS
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid (mempty,
                    mappend)
import Data.List (init,
                  last)
import Data.Functor (fmap,
                     (<$>))
import Data.Foldable (foldl')
import Control.Monad ((=<<),
                      forM_)
import Data.Sequence ((|>))
import Data.Word (Word8)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Exception (catch,
                          IOException,
                          SomeException)
import Control.Concurrent.STM (STM,
                               atomically,
                               orElse,
                               retry,
                               TVar,
                               newTVar,
                               readTVar,
                               writeTVar)
import Control.Concurrent.STM.TChan (TChan,
                                     newBroadcastTChan,
                                     dupTChan,
                                     writeTChan,
                                     tryReadTChan,
                                     readTChan)
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

-- | IRC connection state.
data IRCConnectionData = IRCConnectionData
  { ircConnectionConnection :: Connection,
    ircConnectionState :: IRCConnectionState,
    ircConnectionHostname :: Maybe NS.HostName,
    ircConnectionAddress :: Maybe NS.AddrInfo,
    ircConnectionPort :: Maybe NS.PortNumber,
    ircConnectionEventSub :: ConnectionEventSub,
    ircConnectionBuffer :: B.ByteString }

-- | Create a new IRC connection.
newIRCConnection :: STM IRCConnection
newIRCConnection = do
  running <- newTVar False
  actionQueue <- newTQueue
  eventQueue <- newBroadcastTChan
  return IRCConnection { ircConnectionRunning = running,
                         ircConnectionActionQueue = actionQueue,
                         ircConnectionEventQueue = eventQueue }

-- | Start an IRC connection.
startIRCConnection :: IRCConnection -> IO (Either Error ())
startIRCConnection ircConnection = do
  alreadyRunning <- atomically $ do
    running <- readTVar $ ircConnectionRunning ircConnection
    if not running
      then do writeTVar (ircConnectionRunning ircConnection) True
              return False
      else return True
  if not alreadyRunning
    then do
      connection <- atomically newConnection
      eventSub <- atomically $ subscribeConnection connection
      result <- startConnection connection
      case result of
        Right () -> do
          let state = IRCConnectionData { ircConnectionConnection = connection,
                                          ircConnectionState =
                                            IRCConnectionStarted,
                                          ircConnectionHostname = Nothing,
                                          ircConnectionAddress = Nothing,
                                          ircConnectionPort = Nothing,
                                          ircConnectionEventSub = eventSub,
                                          ircConnectionBuffer = B.empty }
          async $ runUnconnected ircConnection state
          return $ Right ()
        failure -> return failure
    else return . Left $ Error "connection already started"

-- | Stop an IRC connection.
stopIRCConnection :: IRCConnection -> STM (Response ())
stopIRCConnection ircConnection = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         StopIRCConnection response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Connect an IRC connection.
connectIRC :: IRCConnection -> NS.HostName -> NS.PortNumber -> STM (Response ())
connectIRC ircConnection hostname port = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         ConnectIRC hostname port response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Disconnect an IRC connection.
disconnectIRC :: IRCConnection -> STM (Response ())
disconnectIRC ircConnection = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         DisconnectIRC response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Send a message to an IRC connection.
sendIRCMessage :: IRCConnection -> IRCMessage -> STM (Response ())
sendIRCMessage ircConnection message = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         SendIRCMessage message response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Subscribe to an IRC connection.
subscribeIRCConnection :: IRCConnection -> STM IRCConnectionEventSub
subscribeIRCConnection ircConnection =
  IRCConnectionEventSub <$> dupTChan (ircConnectionEventQueue ircConnection)

-- | Receive an event from an IRC connection.
recvIRCConnection :: IRCConnectionEventSub -> STM IRCConnectionEvent
recvIRCConnection (IRCConnectionEventSub sub) = readTChan sub

-- | Try to receive an event from an IRC connection.
tryRecvIRCConnection :: IRCConnectionEventSub -> STM (Maybe IRCConnectionEvent)
tryRecvIRCConnection (IRCConnectionEventSub sub) = tryReadTChan sub

-- | Get the state of an IRC connection.
getIRCConnectionState :: IRCConnection -> STM (Response IRCConnectionState)
getIRCConnectionState ircConnection = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         GetIRCConnectionState response'
    else putTMVar response $ Right IRCConnectionNotStarted
  return response'

-- | Get the hostname of an IRC connection.
getIRCConnectionHostname :: IRCConnection -> STM (Response (Maybe NS.HostName))
getIRCConnectionHostname ircConnection = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         GetIRCConnectionHostname response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Get the address of an IRC connection.
getIRCConnectionAddress :: IRCConnection -> STM (Response (Maybe NS.AddrInfo))
getIRCConnectionAddress ircConnection = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         GetIRCConnectionAddress response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Get the port of an IRC connection.
getIRCConnectionPort :: IRCConnection -> STM (Response (Maybe NS.PortNumber))
getIRCConnectionPort ircConnection = do
  running <- readTVar $ ircConnectionRunning ircConnection
  response <- newEmptyTMVar
  let response' = Response response
  if running
    then writeTQueue (ircConnectionActionQueue ircConnection) $
         GetIRCConnectionPort response'
    else putTMVar response . Left $ Error "connection not started"
  return response'

-- | Get whether an IRC connection state is active.
isIRCConnectionStateActive :: IRCConnectionState -> Bool
isIRCConnectionStateActive IRCConnectionNotStarted = False
isIRCConnectionStateActive IRCConnectionStarted = False
isIRCConnectionStateActive IRCConnectionFindingAddr = True
isIRCConnectionStateActive (IRCConnectionNoAddrFound _) = False
isIRCConnectionStateActive IRCConnectionLookupCanceled = False
isIRCConnectionStateActive IRCConnectionFindingName = True
isIRCConnectionStateActive (IRCConnectionNoNameFound _) = True
isIRCConnectionStateActive IRCConnectionReverseLookupCanceled = False
isIRCConnectionStateActive IRCConnectionConnecting = True
isIRCConnectionStateActive IRCConnectionConnected = True
isIRCConnectionStateActive (IRCConnectionConnectingFailed _) = False
isIRCConnectionStateActive IRCConnectionConnectingCanceled = False
isIRCConnectionStateActive IRCConnectionDisconnected = False
isIRCConnectionStateActive (IRCConnectionDisconnectError _) = False
isIRCConnectionStateActive IRCConnectionDisconnectedByPeer = False
isIRCConnectionStateActive (IRCConnectionRecvError _) = False
isIRCConnectionStateActive (IRCConnectionSendError _) = False

-- | Run an unconnected IRC connection.
runUnconnected :: IRCConnection -> IRCConnectionData -> IO ()
runUnconnected outer ircConnection = do
  let connection = ircConnectionConnection ircConnection
  actionOrEvent <- atomically $ do
    (Right <$> (readTQueue $ ircConnectionActionQueue outer)) `orElse`
      (Left <$> (recvConnection $ ircConnectionEventSub ircConnection))
  ircConnection <- updateState ircConnection
  case actionOrEvent of
    Right (ConnectIRC hostname port response) -> do
      let ircConnection' =
            ircConnection { ircConnectionBuffer = B.empty,
                            ircConnectionHostname = Just hostname,
                            ircConnectionPort = Just port,
                            ircConnectionAddress = Nothing,
                            ircConnectionState = IRCConnectionFindingAddr }
      (atomically $ connect connection hostname port) >> return ()
      runConnecting outer ircConnection' response
    Right (DisconnectIRC (Response response)) -> do
      atomically . putTMVar response . Left $ Error "not active"
      runUnconnected outer ircConnection
    Right (SendIRCMessage _ (Response response)) -> do
      atomically . putTMVar response . Left $ Error "not connected"
      runUnconnected outer ircConnection
    Right (StopIRCConnection (Response response)) -> do
      atomically $ do
        writeTVar (ircConnectionRunning outer) False
        putTMVar response $ Right ()
        clearActions outer ircConnection
    Right (GetIRCConnectionState (Response response)) -> do
      atomically . putTMVar response . Right $ ircConnectionState ircConnection
      runUnconnected outer ircConnection
    Right (GetIRCConnectionHostname (Response response)) -> do
      atomically . putTMVar response . Right $
        ircConnectionHostname ircConnection
      runUnconnected outer ircConnection
    Right (GetIRCConnectionAddress (Response response)) -> do
      atomically . putTMVar response . Right $
        ircConnectionAddress ircConnection
      runUnconnected outer ircConnection
    Right (GetIRCConnectionPort (Response response)) -> do
      atomically . putTMVar response . Right $ ircConnectionPort ircConnection
      runUnconnected outer ircConnection
    Left (RecvData _) -> runUnconnected outer ircConnection
    Left event -> do
      atomically . writeTChan (ircConnectionEventQueue outer) $
        ircConnectionEventOfConnectionEvent event
      runUnconnected outer ircConnection

-- | Run an connecting IRC connection.
runConnecting :: IRCConnection -> IRCConnectionData -> Response () -> IO ()
runConnecting outer ircConnection
  connectResponse'@(Response connectResponse) = do
  let connection = ircConnectionConnection ircConnection
  actionOrEvent <- atomically $ do
    (Right <$> (readTQueue $ ircConnectionActionQueue outer)) `orElse`
      (Left <$> (recvConnection $ ircConnectionEventSub ircConnection))
  ircConnection <- updateState ircConnection
  case actionOrEvent of
    Right (ConnectIRC hostname port (Response response)) -> do
      atomically . putTMVar response . Left $ Error "already connecting"
      runConnecting outer ircConnection connectResponse'
    Right (DisconnectIRC (Response response)) -> do
      response' <- atomically $ disconnect connection
      result <- atomically $ getResponse response'
      ircConnection <- updateState ircConnection
      atomically $ putTMVar response result
      runUnconnected outer ircConnection
    Right (SendIRCMessage _ (Response response)) -> do
      atomically . putTMVar response . Left $ Error "not connected"
      runConnecting outer ircConnection connectResponse'
    Right (StopIRCConnection (Response response)) -> do
      response' <- atomically . stopConnection $
                   ircConnectionConnection ircConnection
      result <- atomically $ getResponse response'
      case result of
        Right () -> return ()
        Left (Error errorText) -> displayError errorText
      atomically $ do
        writeTVar (ircConnectionRunning outer) False
        putTMVar response $ Right ()
        clearActions outer ircConnection
    Right (GetIRCConnectionState (Response response)) -> do
      atomically . putTMVar response . Right $ ircConnectionState ircConnection
      runConnecting outer ircConnection connectResponse'
    Right (GetIRCConnectionHostname (Response response)) -> do
      atomically . putTMVar response . Right $
        ircConnectionHostname ircConnection
      runConnecting outer ircConnection connectResponse'
    Right (GetIRCConnectionAddress (Response response)) -> do
      atomically . putTMVar response . Right $
        ircConnectionAddress ircConnection
      runConnecting outer ircConnection connectResponse'
    Right (GetIRCConnectionPort (Response response)) -> do
      atomically . putTMVar response . Right $ ircConnectionPort ircConnection
      runConnecting outer ircConnection connectResponse'
    Left (RecvData _) -> runUnconnected outer ircConnection
    Left event -> do
      atomically . writeTChan (ircConnectionEventQueue outer) $
        ircConnectionEventOfConnectionEvent event
      case event of
        NoAddrFound failure -> do
          atomically . putTMVar connectResponse $ Left failure
          runUnconnected outer ircConnection
        FoundAddr address -> do
          let ircConnection' =
                ircConnection { ircConnectionAddress = Just address }
          runConnecting outer ircConnection' connectResponse'
        FoundName hostname -> do
          let ircConnection' =
                ircConnection { ircConnectionHostname = Just hostname }
          runConnecting outer ircConnection' connectResponse'
        ConnectingFailed failure -> do
          atomically . putTMVar connectResponse $ Left failure
          runUnconnected outer ircConnection
        Connected -> do
          atomically . putTMVar connectResponse $ Right ()
          runConnected outer ircConnection
        _ -> runConnecting outer ircConnection connectResponse'

-- | Run an connected IRC connection.
runConnected :: IRCConnection -> IRCConnectionData -> IO ()
runConnected outer ircConnection = do
  let connection = ircConnectionConnection ircConnection
  actionOrEvent <- atomically $ do
    (Right <$> (readTQueue $ ircConnectionActionQueue outer)) `orElse`
      (Left <$> (recvConnection $ ircConnectionEventSub ircConnection))
  ircConnection <- updateState ircConnection
  case actionOrEvent of
    Right (ConnectIRC hostname port (Response response)) -> do
      atomically . putTMVar response . Left $ Error "already connected"
      runConnected outer ircConnection
    Right (DisconnectIRC (Response response)) -> do
      response' <- atomically $ disconnect connection
      result <- atomically $ getResponse response'
      ircConnection <- updateState ircConnection
      atomically $ putTMVar response result
      runUnconnected outer ircConnection
    Right (SendIRCMessage message (Response response)) -> do
      response' <- atomically . sendData connection $ formatIRCMessage message
      result <- atomically $ getResponse response'
      ircConnection <- updateState ircConnection
      atomically $ putTMVar response result
      runConnected outer ircConnection
    Right (StopIRCConnection (Response response)) -> do
      response' <- atomically . stopConnection $
                   ircConnectionConnection ircConnection
      result <- atomically $ getResponse response'
      case result of
        Right () -> return ()
        Left (Error errorText) -> displayError errorText
      atomically $ do
        writeTVar (ircConnectionRunning outer) False
        putTMVar response $ Right ()
        clearActions outer ircConnection
    Right (GetIRCConnectionState (Response response)) -> do
      atomically . putTMVar response . Right $ ircConnectionState ircConnection
      runConnected outer ircConnection
    Right (GetIRCConnectionHostname (Response response)) -> do
      atomically . putTMVar response . Right $
        ircConnectionHostname ircConnection
      runConnected outer ircConnection
    Right (GetIRCConnectionAddress (Response response)) -> do
      atomically . putTMVar response . Right $
        ircConnectionAddress ircConnection
      runConnected outer ircConnection
    Right (GetIRCConnectionPort (Response response)) -> do
      atomically . putTMVar response . Right $ ircConnectionPort ircConnection
      runConnected outer ircConnection
    Left (RecvData bytes) -> do
      let oldBuffer = ircConnectionBuffer ircConnection
          ircConnection' =
            ircConnection { ircConnectionBuffer = B.append oldBuffer bytes }
      ircConnection'' <- atomically $ parseIRCMessages outer ircConnection'
      runConnected outer ircConnection''
    Left event -> do
      atomically . writeTChan (ircConnectionEventQueue outer) $
        ircConnectionEventOfConnectionEvent event
      case event of
        DisconnectedByPeer -> runUnconnected outer ircConnection
        SendError _ -> runUnconnected outer ircConnection
        RecvError _ -> runUnconnected outer ircConnection
        _ -> runConnected outer ircConnection

-- | Update the state for an IRC connection.
updateState :: IRCConnectionData -> IO IRCConnectionData
updateState ircConnection = do
  response <- atomically . getConnectionState $
              ircConnectionConnection ircConnection
  result <- atomically $ getResponse response
  case result of
    Right state ->
      return $ ircConnection { ircConnectionState =
                                 ircConnectionStateOfConnectionState state }
    Left (Error errorText) -> do
      displayError errorText
      return ircConnection

-- | Convert an connection state to an IRC connection state.
ircConnectionStateOfConnectionState :: ConnectionState -> IRCConnectionState
ircConnectionStateOfConnectionState ConnectionNotStarted =
  IRCConnectionNotStarted
ircConnectionStateOfConnectionState ConnectionStarted =
  IRCConnectionStarted
ircConnectionStateOfConnectionState ConnectionFindingAddr =
  IRCConnectionFindingAddr
ircConnectionStateOfConnectionState (ConnectionNoAddrFound failure) =
  IRCConnectionNoAddrFound failure
ircConnectionStateOfConnectionState ConnectionLookupCanceled =
  IRCConnectionLookupCanceled
ircConnectionStateOfConnectionState ConnectionFindingName =
  IRCConnectionFindingName
ircConnectionStateOfConnectionState (ConnectionNoNameFound failure) =
  IRCConnectionNoNameFound failure
ircConnectionStateOfConnectionState ConnectionReverseLookupCanceled =
  IRCConnectionReverseLookupCanceled
ircConnectionStateOfConnectionState ConnectionConnecting =
  IRCConnectionConnecting
ircConnectionStateOfConnectionState ConnectionConnected =
  IRCConnectionConnected
ircConnectionStateOfConnectionState (ConnectionConnectingFailed failure) =
  IRCConnectionConnectingFailed failure
ircConnectionStateOfConnectionState ConnectionConnectingCanceled =
  IRCConnectionConnectingCanceled
ircConnectionStateOfConnectionState ConnectionDisconnected =
  IRCConnectionDisconnected
ircConnectionStateOfConnectionState (ConnectionDisconnectError failure) =
  IRCConnectionDisconnectError failure
ircConnectionStateOfConnectionState ConnectionDisconnectedByPeer =
  IRCConnectionDisconnectedByPeer
ircConnectionStateOfConnectionState (ConnectionRecvError failure) =
  IRCConnectionRecvError failure
ircConnectionStateOfConnectionState (ConnectionSendError failure) =
  IRCConnectionSendError failure

-- | Convert a connection event to an IRC connection event.
ircConnectionEventOfConnectionEvent :: ConnectionEvent -> IRCConnectionEvent
ircConnectionEventOfConnectionEvent (FoundAddr addr) = IRCFoundAddr addr
ircConnectionEventOfConnectionEvent (NoAddrFound failure) =
  IRCNoAddrFound failure
ircConnectionEventOfConnectionEvent LookupCanceled = IRCLookupCanceled
ircConnectionEventOfConnectionEvent (FoundName hostname) =
  IRCFoundName hostname
ircConnectionEventOfConnectionEvent (NoNameFound failure) =
  IRCNoNameFound failure
ircConnectionEventOfConnectionEvent ReverseLookupCanceled =
  IRCReverseLookupCanceled
ircConnectionEventOfConnectionEvent (ConnectingFailed failure) =
  IRCConnectingFailed failure
ircConnectionEventOfConnectionEvent Connected = IRCConnected
ircConnectionEventOfConnectionEvent ConnectingCanceled = IRCConnectingCanceled
ircConnectionEventOfConnectionEvent Disconnected = IRCDisconnected
ircConnectionEventOfConnectionEvent (DisconnectError failure) =
  IRCDisconnectError failure
ircConnectionEventOfConnectionEvent DisconnectedByPeer = IRCDisconnectedByPeer
ircConnectionEventOfConnectionEvent (SendError failure) = IRCSendError failure
ircConnectionEventOfConnectionEvent (RecvError failure) = IRCRecvError failure
ircConnectionEventOfConnectionEvent ConnectionStopped = IRCConnectionStopped

-- | Parse IRC messages.
parseIRCMessages :: IRCConnection -> IRCConnectionData -> STM IRCConnectionData
parseIRCMessages outer ircConnection = do
  let parts = B.split (byteOfChar '\n') $ ircConnectionBuffer ircConnection
  forM_ (init parts) $ \part -> do
    let part' = case B.stripSuffix (encodeUtf8 "\r") part of
          Just part -> part
          Nothing -> part
        (prefix, rest) = case B.stripPrefix (encodeUtf8 ":") part' of
          Just part ->
            let (prefix, rest) = splitOnSpaces part
            in (Just prefix, rest)
          Nothing -> (Nothing, Just part')
    case rest of
      Just rest ->
        let (command, rest') = splitOnSpaces rest
            (params, coda) =
              case rest' of
                Just rest' -> parseIRCParams rest' S.empty
                Nothing -> (S.empty, Nothing)
            ircMessage = IRCMessage { ircMessagePrefix = prefix,
                                      ircMessageCommand = command,
                                      ircMessageParams = params,
                                      ircMessageCoda = coda }
        in writeTChan (ircConnectionEventQueue outer) $
           IRCRecvMessage ircMessage
      Nothing -> return ()
  return $ ircConnection { ircConnectionBuffer = last parts }
  where parseIRCParams bytes params =
          if B.length bytes > 0
          then if B.head bytes == byteOfChar ':'
               then (params, Just $ B.tail bytes)
               else
                 let (param, rest) = splitOnSpaces bytes
                 in case rest of
                   Just rest -> parseIRCParams rest (params |> param)
                   Nothing -> (params |> param, Nothing)
          else (params, Nothing)
          
-- | Format an IRC message.
formatIRCMessage :: IRCMessage -> B.ByteString
formatIRCMessage IRCMessage{..} =
  let prefix = case ircMessagePrefix of
        Just prefix ->
          (BB.charUtf8 ':' `mappend` BB.byteString prefix)
          `mappend` BB.charUtf8 ' '
        Nothing -> mempty
      params = fmap (\param -> BB.charUtf8 ' ' `mappend` BB.byteString param)
        ircMessageParams
      coda = case ircMessageCoda of
        Just coda -> BB.stringUtf8 " :" `mappend` BB.byteString coda
        Nothing -> mempty
      includingCommand = prefix `mappend` BB.byteString ircMessageCommand
      includingParams = foldl' mappend includingCommand params
      includingCoda = includingParams `mappend` coda
      includingNewline = includingCoda `mappend` BB.stringUtf8 "\r\n"
  in BL.toStrict $ BB.toLazyByteString includingNewline

clearActions :: IRCConnection -> IRCConnectionData -> STM ()
clearActions outer ircConnection = do
  action <- tryReadTQueue $ ircConnectionActionQueue outer
  case action of
    Just (ConnectIRC _ _  (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer ircConnection
    Just (DisconnectIRC (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer ircConnection
    Just (SendIRCMessage _ (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer ircConnection
    Just (StopIRCConnection (Response response)) -> do
      putTMVar response . Left $ Error "canceled"
      clearActions outer ircConnection
    Just (GetIRCConnectionState (Response response)) -> do
      putTMVar response . Right $ ircConnectionState ircConnection
      clearActions outer ircConnection
    Just (GetIRCConnectionHostname (Response response)) -> do
      putTMVar response . Right $ ircConnectionHostname ircConnection
      clearActions outer ircConnection
    Just (GetIRCConnectionPort (Response response)) -> do
      putTMVar response . Right $ ircConnectionPort ircConnection
      clearActions outer ircConnection
    Just (GetIRCConnectionAddress (Response response)) -> do
      putTMVar response . Right $ ircConnectionAddress ircConnection
      clearActions outer ircConnection
    Nothing -> return ()
