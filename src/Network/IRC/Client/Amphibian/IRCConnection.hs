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
   isIRCConnectionActive,
   areIRCConnectionActionsPending,
   getIRCConnectionError)

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
-- | Create a new IRC connection.
newIRCConnection :: STM IRCConnection
newIRCConnection = do
  connection <- newConnection
  state <- newTVar IRCConnectionNotStarted
  buffer <- newTVar B.empty
  actionQueue <- newTQueue
  eventQueue <- newBroadcastTChan
  return IRCConnection { ircConnectionConnection = connection,
                         ircConnectionState = state,
                         ircConnectionBuffer = buffer,
                         ircConnectionActionQueue = actionQueue,
                         ircConnectionEventQueue = eventQueue }

-- | Start an IRC connection.
startIRCConnection :: IRCConnection -> IO (Either Error ())
startIRCConnection ircConnection = do
  result <- startConnection $ ircConnectionConnection ircConnection
  case result of
    Right () -> do
      async $ runStateTracker ircConnection ConnectionNotStarted
      eventSub <- atomically . subscribeConnection $
        ircConnectionConnection ircConnection
      async $ runEventTracker ircConnection eventSub
      async $ runActions ircConnection
      return $ Right ()
    failure -> return failure

-- | Stop an IRC connection.
stopIRCConnection :: IRCConnection -> STM (Response ())
stopIRCConnection ircConnection = do
  innerState <- getConnectionState $ ircConnectionConnection ircConnection
  response <- newEmptyTMVar
  if innerState /= ConnectionNotStarted
    then writeTQueue (ircConnectionActionQueue ircConnection)
         (StopIRCConnection $ Response response)
    else putTMVar response (Left . Error $ "connection not started")
  return $ Response response

-- | Connect an IRC connection.
connectIRC :: IRCConnection -> NS.HostName -> NS.PortNumber -> STM (Response ())
connectIRC ircConnection hostname port = do
  innerState <- getConnectionState $ ircConnectionConnection ircConnection
  response <- newEmptyTMVar
  if innerState /= ConnectionNotStarted
    then writeTQueue (ircConnectionActionQueue ircConnection)
         (ConnectIRC hostname port $ Response response)
    else putTMVar response (Left . Error $ "connection not started")
  return $ Response response

-- | Disconnect an IRC connection.
disconnectIRC :: IRCConnection -> STM (Response ())
disconnectIRC ircConnection = do
  innerState <- getConnectionState $ ircConnectionConnection ircConnection
  response <- newEmptyTMVar
  if innerState /= ConnectionNotStarted
    then writeTQueue (ircConnectionActionQueue ircConnection)
         (DisconnectIRC $ Response response)
    else putTMVar response (Left . Error $ "connection not started")
  return $ Response response

-- | Send a message to an IRC connection.
sendIRCMessage :: IRCConnection -> IRCMessage -> STM (Response ())
sendIRCMessage ircConnection message = do
  innerState <- getConnectionState $ ircConnectionConnection ircConnection
  response <- newEmptyTMVar
  if innerState /= ConnectionNotStarted
    then writeTQueue (ircConnectionActionQueue ircConnection)
         (SendIRCMessage message $ Response response)
    else putTMVar response (Left . Error $ "connection not started")
  return $ Response response

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

-- | Get the state of an IRC connection
getIRCConnectionState :: IRCConnection -> STM IRCConnectionState
getIRCConnectionState = readTVar . ircConnectionState

-- | Get the hostname of an IRC connection.
getIRCConnectionHostname :: IRCConnection -> STM (Maybe NS.HostName)
getIRCConnectionHostname = getConnectionHostname . ircConnectionConnection

-- | Get the address of an IRC connection.
getIRCConnectionAddress :: IRCConnection -> STM (Maybe NS.AddrInfo)
getIRCConnectionAddress = getConnectionAddress . ircConnectionConnection

-- | Get the port of an IRC connection.
getIRCConnectionPort :: IRCConnection -> STM (Maybe NS.PortNumber)
getIRCConnectionPort = getConnectionPort . ircConnectionConnection

-- | Get whether an IRC connection is active.
isIRCConnectionActive :: IRCConnection -> STM Bool
isIRCConnectionActive = isConnectionActive . ircConnectionConnection

-- | Get whether IRC connection actions are pending.
areIRCConnectionActionsPending :: IRCConnection -> STM Bool
areIRCConnectionActionsPending ircConnection = do
  pending <- areConnectionActionsPending $ ircConnectionConnection ircConnection
  if not pending
    then isEmptyTQueue $ ircConnectionActionQueue ircConnection
    else return False

-- | Get IRC connection error text.
getIRCConnectionError :: IRCConnection -> STM (Maybe Error)
getIRCConnectionError = getConnectionError . ircConnectionConnection

-- | Track connection state.
runStateTracker :: IRCConnection -> ConnectionState -> IO ()
runStateTracker ircConnection oldState = do
  state <- atomically $ do
    newState <- getConnectionState $ ircConnectionConnection ircConnection
    if newState /= oldState
      then do writeTVar (ircConnectionState ircConnection)
                (ircConnectionStateOfConnectionState newState)
              return newState
      else retry
  if state /= ConnectionNotStarted
    then runStateTracker ircConnection state
    else return ()

-- | Track connection events.
runEventTracker :: IRCConnection -> ConnectionEventSub -> IO ()
runEventTracker ircConnection sub = do
  continue <- atomically $ do
    event <- recvConnection sub
    case event of
      RecvData bytes -> do
        oldBuffer <- readTVar $ ircConnectionBuffer ircConnection
        writeTVar (ircConnectionBuffer ircConnection) (B.append oldBuffer bytes)
        parseIRCMessages ircConnection
        return True
      ConnectionStopped -> do
        writeTChan (ircConnectionEventQueue ircConnection) IRCConnectionStopped
        return False
      _ -> do
        writeTChan (ircConnectionEventQueue ircConnection)
          (ircConnectionEventOfConnectionEvent event)
        return True
  if continue
    then runEventTracker ircConnection sub
    else return ()

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

-- | Run an unconnected IRC connection.
runActions :: IRCConnection -> IO ()
runActions ircConnection = do
  let connection = ircConnectionConnection ircConnection
  action <- atomically . readTQueue $ ircConnectionActionQueue ircConnection
  case action of
    ConnectIRC hostname port (Response response) -> do
      output <- atomically $ do
        active <- isConnectionActive $ connection
        if not active
          then do writeTVar (ircConnectionBuffer ircConnection) B.empty
                  Right <$> connect connection hostname port
          else return . Left $ Error "already active"
      case output of
        Right response' -> do
          (async . atomically $ putTMVar response =<< getResponse response') >>
            return ()
        Left failure -> atomically . putTMVar response $ Left failure
      runActions ircConnection
    DisconnectIRC (Response response) -> do
      output <- atomically $ do
        active <- isConnectionActive $ connection
        if active
          then Right <$> disconnect connection
          else return . Left $ Error "not active"
      case output of
        Right response' -> do
          (async . atomically $ putTMVar response =<< getResponse response') >>
            return ()
        Left failure -> atomically . putTMVar response $ Left failure
      runActions ircConnection
    SendIRCMessage message (Response response) -> do
      output <- atomically $ do
        state <- getConnectionState $ connection
        if state == ConnectionConnected
          then Right <$> sendData connection (formatIRCMessage message)
          else return . Left $ Error "not connected"
      case output of
        Right response' -> do
          (async . atomically $ putTMVar response =<< getResponse response') >>
            return ()
        Left failure -> atomically . putTMVar response $ Left failure
      runActions ircConnection
    StopIRCConnection (Response response) -> do
      output <- atomically $ do
        state <- getConnectionState $ connection
        if state /= ConnectionNotStarted
          then Right <$> stopConnection connection
          else return . Left $ Error "not started"
      case output of
        Right response' -> do
          (async . atomically $ putTMVar response =<< getResponse response') >>
            return ()
        Left failure -> atomically . putTMVar response $ Left failure

-- | Parse IRC messages.
parseIRCMessages :: IRCConnection -> STM ()
parseIRCMessages ircConnection = do
  buffer <- readTVar $ ircConnectionBuffer ircConnection
  let parts = B.split (byteOfChar '\n') buffer
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
        in writeTChan (ircConnectionEventQueue ircConnection) $
           IRCRecvMessage ircMessage
      Nothing -> return ()
  writeTVar (ircConnectionBuffer ircConnection) $ last parts
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
