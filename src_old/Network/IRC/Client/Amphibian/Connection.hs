-- Copyright (c) 2015, Travis Bemann
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
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Client.Amphibian.Connection

       (Connection,
        ConnectionConnectResponse,
        ConnectionSendResponse,
        ConnectionCloseResponse,
        ConnectionEvent(..),
        new,
        connect,
        send,
        close,
        subscribe,
        recv,
        peek,
        waitConnect,
        waitSend,
        waitClose)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import Data.Functor ((<$>))
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (STM,
                               TMVar,
                               atomically,
                               orElse,
                               newEmptyTMVar,
                               newEmptyTMVarIO,
                               putTMVar,
                               readTMVar,
                               newTVar,
                               writeTVar,
                               readTVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.STM.TChan (TChan,
                                     newBrodcastTChan,
                                     readTChan,
                                     writeTChan,
                                     peekTChan)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)

-- | Create a connection.
new :: STM Connection
new = do
  actions <- newTQueue
  events <- newBroadcastTChan
  connected <- newTVar False
  recvData <- newTVar B.empty
  return Connection { connActions = actions,
                      connEvents = events,
                      connConnected = connected,
                      connRecvData = recvData }

-- | Connect to a socket.
connect :: Connection -> HostName -> Port ->
           AM ConnectionConnectResponse
connect connection host port = do
  success <- liftIO . atomically $ do
    isConnected <- readTVar (connConnected connection)
    if isConnected
      then return False
      else do writeTVar (connConnected connection) True
              return True
  response <- liftIO $ newEmptyTMVarIO
  case success of
    True -> do
      intf <- getInterface
      async $ runAM (runConnection host port connection response) intf
    False ->
      liftIO . atomically $ do
        let error = Either ["connection already connected"]
        putTMVar response (ConnectionConnectResponse error)
        writeTChan (connEvents connection) (ConnConnectError error)
  return response

-- | Send data to a connection.
send :: Connection -> IRCMessage -> STM ConnectionSendResponse
send connection message = do
  response <- ConnectionSendResponse <$> newEmptyTMVar
  writeTQueue (connActions connection) (ChnaSend message response)
  return response

-- | Close a connection.
close :: Connection -> STM ConnectionCloseResponse
close connection = do
  response <- ConnectionCloseResponse <$> newEmptyTMVar
  writeTQueue (connActions connection) (ChnaClose response)
  return response

-- | Subscribe to a connection
subscribe :: Connection -> STM ConnectionSubscription
subscribe connection = ConnectionSubscription $ dupTChan (connEvents connection)

-- | Receive an event from a connection subscription.
recv :: ConnectionSubscription -> STM ConnectionEvent
recv (ConnectionSubscription subscription) = readTChan subscription

-- | Peek an event from a connection subscription.
peek :: ConnectionSubscription -> STM (Maybe ConnectionEvent)
peek (ConnectionSubscription subscription) = peekTChan subscription

-- | Wait for a connection connect to complete.
waitConnect :: ConnectionConnectResponse -> STM (Either Error ())
waitConnect (ConnectionConnectResponse response) =
  readTMVar response

-- | Wait for a connection send to complete.
waitSend :: ConnectionSendResponse -> STM (Either Error ())
waitSend (ConnectionSendResponse response) =
  readTMVar response

-- | Wait for a connection close to complete.
waitClose :: ConnectionCloseResponse -> STM (Either Error ())
waitClose (ConnectionCloseResponse response) =
  readTMVar response

-- | Actually connect to a socket and carry out its functionality.
runConnection :: HostName -> Port -> Connection ->
                 ConnectionConnectResponse -> AM ()
runConnection host port connection (ConnectionConnnectResponse response) = do
  intf <- getInterface
  addr <- lookupAddress host port connection
  output <- case addr of
    Right addr -> do
      actualHost <- reverseLookupAddress addr connection
      let actualHost' = case actualHost of
        Right actualHost -> actualHost
        Left _ -> host
      liftIO . atomically $ writeTChan (connEvents connection) (ConnConnecting actualHost' port)
      sock <- liftIO $ S.socket S.AF_INET6 S.Stream S.defaultProtocol
      status <- liftIO $ catch (do S.connect sock (S.addrAddress addr)
                                   return $ Right sock)
      (\(e :: IOException) -> Left (Error [T.pack $ show e]))
      liftIO . atomically $ do
        case status of
          Right _ -> writeTChan (connEvents connection) (ConnConnected actualHost' port)
          Left error -> writeTChan (connEvents connection) (ConnConnectFailed error)
      return status
    Left error -> return $ Left error
  case output of
    Right sock -> do
      liftIO . atomically $ putTMVar response (Right ())
      disconnected <- newEmptyTMVarIO
      recvMessagesAsync <-
        async $ runAM (recvMessages connection sock disconnected) intf
      handleActions connection sock recvMessagesAsync disconnected
    Left error -> liftIO . atomically $ putTMVar response (Left error)

-- | Lookup address.
lookupAddress :: HostName -> Port -> Connection -> AM (Either Error AddrInfo)
lookupAddress host port connection = do
  liftIO . atomically $
    writeTChan (connEvents connection) (ConnLookupAddress host)
  let hints = S.defaultHints { addrFlags = [S.AI_CANONNAME, S.AI_NUMERICSERV] }
  addrs <- liftIO $ catch (Right <$> S.getAddrInfo (Just hints) (Just host)
                           (Just (show port)))
           (\(e :: IOException) -> Left $ Error [T.pack $ show e])
  case addrs of
    Right (addrs :: _) ->
      liftIO . atomically $
        writeTChan (connEvents connection) (ConnFoundAddress addr)
      return $ Right addr
    Left error ->
      liftIO . atomically $
        writeTChan (connEvents connection) (ConnLookupAddressFailed error)
      return $ Left error
    _ -> error "Network.Socket.getAddrInfo returned no addresses without \
               \raising an exception"

-- | Reverse lookup address.
reverseLookupAddress :: SockAddr -> Connection -> AM (Either Error String)
reverseLookupAddress addr connection = do
  host <- liftIO . catch (do (host, service) <- S.getNameInfo [] True False addr
                             case host of
                               Just host -> return $ Right host
                               None -> return . Left $ Error ["host not returned"])
          (\(e :: IOException) -> Left $ Error [T.pack $ show e])
  case host of
    Right host ->
      liftIO . atomically $
        writeTChan (connEvents connection) (ConnFoundHostname host)
      return $ Right host
    Left error ->
      liftIO . atomically $
        writeTChan (connEvents connection) (ConnReverseLookupFailed error)
      return $ Left error

-- | Connection socket actions.
handleActions :: Connection -> Socket -> Async () -> TMVar () -> AM ()
handleActions connection sock recvMessagesAsync disconnected = do
  input <- liftIO . atomically $
           Left <$> readTMVar disconnected `orElse`
           Right <$> readTQueue (connActions connection)
  case input of
    Left _ -> return ()
    Right (ConnSend message (ConnectionSendResponse response)) -> do
      success <- handleSend connection sock recvMessagesAsync message
                 response
      if success
         then handleActions connection sock recvMessagesAsync
              disconnected
         else return ()
    Right (ConnClose (ConnectionCloseResponse response)) ->
      handleClose connection sock recvMessagesAsync response

-- | Connection socket send.
handleSend :: Connection -> Socket -> Async () -> IRCMessage ->
              TMVar (Either Error ()) -> AM ()
handleSend connection sock recvMessagesAsync message response = do
  liftIO $ catch (do SB.sendAll sock (formatMessage message)
                     atomically $ putTMVar response (Right ())
                     return True)
    (\(e :: IOException) -> do
      cancel recvMessagesAsync
      catch (do S.shutdown sock S.ShutdownBoth)
        (\(e :: IOException) -> return ())
      atomically . putTMVar response . Left $ Error [T.pack $ show e]
      S.close sock
      return False)

-- | Connection socket close.
handleClose :: Connection -> Socket -> Async () ->
                     TMVar (Either Error ()) -> AM ()
handleClose connection sock recvMessagesAsync response = do
  liftIO $ cancel recvMessagesAsync
  liftIO $ catch (do S.shutdown sock S.ShutdownBoth
                     atomically $ do
                       writeTChan (connEvents connection)
                         (ConnDisconnected (Right ()))
                       putTMVar response (Right ())
                     S.close sock)
    (\(e :: IOException) ->
      atomically $ do
        writeTChan (connEvents connection) . ConnDisconnected . Left $ Error [T.pack $ show e]
        putTMVar response . Left $ Error [T.pack $ show e]
      S.close sock)

-- | Generate socket events.
recvMessages :: Connection -> Socket -> TMVar () -> AM ()
recvMessages connection sock disconnected = do
  intf <- getInterface
  continue <- liftIO $ catch
              (do recvData <- SB.recv sock 4096
                  if B.length recvData > 0
                    then do atomically $
                              oldRecvData <- readTVar $ connReadData connection
                              writeTVar $ B.append oldRecvData recvData
                            runAM (parseRecvData connection) intf
                            return True
                    else do atomically $ do
                              writeTChan (connEvents connection)
                                (ConnDisconnected (Right ()))
                              putTMVar disconnected ()
                            S.close sock
                            return False)
              (\(e :: IOException) -> do
                catch (S.shutdown sock S.ShutdownBoth)
                  (\(e :: IOException) -> return ())
                atomically $ do
                  writeTChan (connEvents connection) . ConnDisconnected . Left $ Error [T.pack $ show e]
                  putTMVar disconnected ()
                S.close sock
                return False)
  if continue
    then recvMessages connection sock disconnected
    else return ()

-- | Parse received data into IRC messages.
parseRecvData :: Connection -> AM ()
parseRecvData connection = do
  lines <- BC.lines <$> liftIO . atomically . readTVar $ connRecvData connection
  let lines' = map (\xs -> let (start, end) = BC.unsnoc xs
                                   in if end == '\r'
                                      then start
                                      else xs) (init lines)
  mapM_ (\xs ->
            liftIO . atomically . writeTChan (connEvents connection) $
              case parseMessage xs of
                Just message -> ConnMessage message
                Nothing -> ConnMalformed xs)
  writeTVar (connRecvData connection) (last lines)

-- | Parse an IRC message.
parseMessage :: B.ByteString -> Maybe IRCMessage
parseMessage line =
  case BC.uncons line of
    Just (':', line') ->
      case nextItem line' of
        (prefix, Just rest) ->
          case parseMainMessage rest of
            Just (command, parameters, comment) ->
              Just (IRCMessage { ircmPrefix = Just prefix,
                                 ircmCommand = command,
                                 ircmParameters = parameters,
                                 ircmComment = comment })
            Nothing -> Nothing
        _ -> Nothing
    Just (_, _) ->
      case parseMainMessage line of
        Just (command, parameters, comment) ->
          Just (IRCMessage { ircmPrefix = Nothing,
                             ircmCommand = command,
                             ircmParameters = parameters,
                             ircmComment = comment })
        Nothing -> Nothing
    Nothing -> Nothing

-- | Parse the main body of an IRC message.
parseMainMessage :: B.ByteString -> Maybe (MessageCommand, [MessageParameter],
                                           Maybe MessageComment)
parseMainMessage line =
  case nextItem line of
    (command, Just rest) ->
      let (parameters, comment) = extractParameters rest in
      Just (command, parameters, comment)
    (command, Nothing) ->
      if command /= B.empty
      then Just (command, [], Nothing)
      else Nothing

-- | Extract parameters and comment from an IRC message.
extractParameters :: B.ByteString -> ([MessageParameter], Maybe MessageComment)
extractParameters line = extractParameters' line []
  where extractParameters' line params =
          if B.length line > 0
          then
            if BC.head line == ':'
            then (reverse params, Just $ B.tail line)
            else case nextItem line of
              (param, Just rest) -> extractParameters' rest (param : params)
              (param, Nothing) -> (reverse $ param : params, Nothing)
          else (reverse params, Nothing)

-- | Get next item.
nextItem :: B.ByteString -> (B.ByteString, Maybe B.ByteString)
nextItem =
  let (start, rest) = BC.split (/= ' ')
  in if rest /= B.empty
     then (start, Just $ B.tail rest)
     else (start, Nothing)

-- | Format a message as a ByteString.
formatMessage :: IRCMessage -> B.ByteString
formatMessage message = 
  let prefix = maybe [] (\prefix -> [B.concat [":", prefix]]) $ ircmPrefix message
      comment = maybe [] (\comment -> [B.concat [":", comment]]) $ ircmComment message
  in BC.intercalate (BC.singleton ' ') $ prefix ++ [ircmCommand message] ++ ircmParameters message ++ comment
      
