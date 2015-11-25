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

module Network.IRC.Client.Amphibian.ConnectionManagerServer

       (ConnectionManagerServer,
        ConnectionManagerServerStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import qualified Network.IRC.Client.Amphibian.Connection as C
import qualified Network.IRC.Client.Amphibian.User as U
import qualified Network.IRC.Client.Amphibian.Interface as I
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Commands
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Ctcp
import Network.IRC.Client.Amphibian.Monad
import Data.Functor ((<$>))
import Control.Monad (mapM,
                      mapM_,
                      join)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
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
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.Text as T

-- | Create a new plugin server.
new :: STM ConnectionManagerServer
new = do
  running <- newTVar Bool
  actions <- newTQueue
  return $ ConnectionManagerServer { cmseRunning = running,
                                     cmseActions = actions }

-- | Start a manager server
start :: ConnectionManagerServer -> AM ()
start server = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ cmseRunning server
    if not running
    then do
      writeTVar (cmseRunning server) True
      I.registerConnectionManagerServer intf server
      return . async $ runAM (runServer server)
    else return $ return ()

-- | Stop plugin server.
stop :: ConnectionManagerServer -> STM ConnectionManagerServerStopResponse
stop server = do
  response <- ConnectionManagerServerStopResponse <$> newEmptyTMVar
  writeTVar (cmseActions server) (PlsaStop response)
  return response

-- | Wait for plugin server to stop.
waitStop :: ConnectionManagerServerStopResponse -> STM (Either Error ())
waitStop (ConnectionManagerServerStopResponse response) = readTMVar response

-- | Run plugin server.
runServer :: ConnectionManagerServer -> AM ()
runServer server = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    action <- readTQueue $ cmseActions server
    case action of
      PlsaStartConnectionManager manager (ConnectionManagerStartResponse response) -> do
        active <- readTVar $ comaActive manager
        if not active
        then do
          writeTVar (comaActive manager) True
          I.registerConnectionManager intf manager
          putTMVar response $ Right ()
          return $ do
            async $ runAM (runConnectionManager manager) intf
            return True
        else do
          errorText <- I.lookupText intf "Connection manager is already started"
          putTMVar response . Left $ Error [errorText]
          return $ return True
      PlsaStop -> do
        I.unregisterConnectionManagerServer intf server
        return $ return False
  if continue
  then runServer server
  else return ()

-- | Connection manager thread body.
runConnectionManager :: ConnectionManager -> AM ()
runConnectionManager manager =
  continue <- join . atomically $
    handleAction manager intf `orElse`
    handleEvent manager intf
  if continue
    then connectionManager manager
    else return ()

-- | Handle action for connection manager.
handleAction :: ConnectionManager -> STM (AM Bool)
handleAction manager = do
  action <- readTQueue $ comaActions manager
  case action of
    ComaStop (ConnectionManagerStopResponse response) -> do
      I.unregisterConnectionManager (comaInterface manager) manager
      writeTVar (comaActive manager) False
      return $ return True
    ComaConnectNew setup (ConnectionManagerConnectResponse response) -> do
      writeTVar (comaSetup manager) $ Just setup
      return $ do
        result <- doConnect manager
        liftIO . atomically $ putTMVar response result
        return True
    ComaReconnect (ConnectManagerReconnectResponse response) ->
      return $ do
        result <- doReconnect manager
        liftIO . atomically $ putTMVar response result
        return True
    ComaDisconnect (ConnectionManagerDisconnectResponse response) ->
      return $ do
        result <- doDisconnect manager
        liftIO . atomically $ putTMVar response result
        return True
    ComaSend message (ConnectionManagerSendResponse response) ->
      return $ do
        result <- doSend manager message
        liftIO . atomically $ putTMVar response result
        return True
    
-- | Handle event for connection manager.
handleEvent :: ConnectionManager -> STM (AM Bool)
handleEvent manager = do
  subscription <- readTVar $ comaSubscription manager
  connection <- readTVar $ comaConnection manager
  case subscription of
    (subscription : rest) -> do
      event <- C.recv subscription
      case event of
        ConnLookupAddress _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnFoundAddress _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnLookupAddressFailed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          writeTVar (comaConnection manager) Nothing
          writeTVar (comaSubscription manager) rest
          return $ return True
        ConnFoundHostname hostName -> do
          writeTChan (comaHost manager) (Just hostName) 
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnReverseLookupFailed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnConnecting _ _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnConnected _ _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ do
            intf <- getInterface
            async $ runAM (register manager) intf
            return True
        ConnConnectFailed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          writeTVar (comaConnection manager) Nothing
          writeTVar (comaSubscription manager) rest
          return $ return True
        ConnDisconnected _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          writeTVar (comaConnection manager) Nothing
          writeTVar (comaRegistered manager) False
          writeTVar (comaHost manager) Nothing
          writeTVar (comaSubscription manager) rest
          return $ return True
        ConnMessage message -> do
          writeTChan (comaEvents manager) (convertEvent event)
          handleMessage manager message
        ConnMalformed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True 
    None -> return $ return True

-- | Handle message.
handleMessage :: ConnectionManager -> IRCMessage -> STM (AM Bool)
handleMessage manager message@(IRCMessage { ircmCommand = command })
  | command == cmd_NICK = handleNick manager message
  | command == cmd_PRIVMSG = handlePrivMsg manager message
  | command == cmd_NOTICE = handleNotice manager message
  | command == cmd_PING = handlePing manager message
  | otherwise = return $ return True

-- | Handle nick message.
handleNick :: ConnectionManager -> IRCMessage -> STM (AM Bool)
handleNick manager message = do
  currentNick <- getNick manager
  case (extractNick $ ircmPrefix message, ircmParameters message) of
   (Just oldNick, [newNick])
     | oldNick == currentNick -> do
       setNick manager newNick
       writeTChan (comaEvents manager) $ ComaRecvSelfNick oldNick newNick
     | otherwise ->
       writeTChan (comaEvents manager) $ ComaRecvNick oldNick newNick
   _ -> return ()
  return $ return True

-- | Handle PRIVMSG message.
handlePrivMsg :: ConnectionManager -> IRCMessage -> STM (AM Bool)
handlePrivMsg manager message = do
  currentNick <- getNick manager
  isCtcp <- case (extractNick $ ircmPrefix prefix, ircmParameters message, ircmComment message) of
    (Just fromNick, [dest], Just comment) ->
      case checkCtcp comment of
        Just comment -> do
          writeTChan (comaEvents manager) $ ComaRecvCtcpRequest fromNick (parseChannelNameOrNick dest) comment
          return True
        Nothing -> return False
    _ -> return false
  if not isCtcp
  then
    case (extractNick $ ircmPrefix prefix, ircmParameters message) of
      (Just fromNick, [nick]) | nick == currentNick -> do
        return $ do
          intf <- getInterface
          liftIO . atomically $ do
            users <- I.getUsers intf
            nicks <- mapM U.getNick users
            if fromNick `notElem` nicks
            then do
              user <- U.new intf manager fromNick
              U.inject user $ ComaMessage message
              U.start user
            else return ()
          return True
      _ -> return $ return True
  else return $ return True

-- | Handle NOTICE message.
handleNotice :: ConnectionManager -> IRCMessage -> STM (AM Bool)
handleNotice manager message = do
  currentNick <- getNick manager
  isCtcp <- case (extractNick $ ircmPrefix prefix, ircmParameters message, ircmComment message) of
    (Just fromNick, [dest], Just comment) ->
      case checkCtcp comment of
        Just comment -> do
          writeTChan (comaEvents manager) $ ComaRecvCtcpReply fromNick (parseChannelNameOrNick dest) comment
          return True
        Nothing -> return False
    _ -> return False
  if not isCtcp
  then
    case (extractNick $ ircmPrefix message, ircmParameters message, ircmComment message) of
      (Just fromNick, [nick], Just comment) | nick == currentNick -> do
        return $ do
          intf <- getInterface
          liftIO . atomically $ do
            users <- I.getUsers intf
            nicks <- mapM U.getNick users
            if fromNick `notElem` nicks
            then writeTChan (comaEvents manager) (ComaRecvNotice fromNick comment)
            return True
      _ -> return $ return True
  else return $ return True

-- | Handle a PING message.
handlePing :: ConnectionManager -> IRCMessage -> STM (AM Bool)
handlePing manager message = do
  case ircmComment message of
    Just comment ->  do
      C.send connection $ IRCMessage { ircmPrefix = Nothing,
                                       ircmCommand = cmd_PONG,
                                       ircmParameters = [comment],
                                       ircmComment = Nothing }
      return $ return True
    _ -> return $ return True

-- | Register with a server.
register :: ConnectionManager -> AM ()
register manager = do
  connection <- fromJust <$> liftIO . atomically . readTVar $ comaConnection manager
  continue <- 
  case continue of
    True -> do
      continue <- registerNick manager
      case continue of
        True -> registerUser manager
    False -> return ()

-- | Password registration delay (in microseconds).
passwordRegistrationDelay :: Int
passwordRegistrationDelay = 1000000
  
-- | Register a password with the server.
registerPassword :: ConnectionManager -> AM Bool
registerPassword manager =
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection -> do
      setup <- liftIO . atomically . readTVar $ comaSetup manager
      case comaPassword setup of
        Just password -> do
          subscription <- liftIO . atomically $ C.subscribe connection
          liftIO . atomically . C.send connection $
            IRCMessage { ircmPrefix = Nothing,
                         ircmCommand = cmd_PASS,
                         ircmParameters = [password],
                         ircmComment = Nothing }
          liftIO $ threadDelay passwordDelay
          event <- liftIO . atomically $ C.peek subscription
          case event of
            Just (ConnMessage (IRCCommand { ircmCommand = command })) ->
              | command == err_PASSWDMISMATCH -> do
                liftIO . atomically $ writeTChan (comaEvents manager) (ComaPasswordMismatch password)
                doDisconnect manager
                return False
              | command == err_YOUREBANNEDCREEP -> do
                liftIO . atomically $ writeTChan (comaEvents manager) ComaBannedFromServer
                doDisconnect manager
                return False
            _ -> return True
        Nothing -> return True
    Nothing -> return False

-- | Nick registration delay (in microseconds).
nickRegistrationDelay :: Int
nickRegistrationDelay = 1000000

-- | Register a nick with the server.
registerNick :: ConnectionManager -> AM Bool
registerNick manager = do
  storedNicks <- comaAllNicks <$> liftIO . atomically . readTVar $ comaSetup manager
  nick <- liftIO . atomically . readTVar $ comaNick manager
  let nicks = unique $ case nick of
    Just nick -> nick : storedNicks
    Nothing -> storedNicks
  registerCurrentNick manager nicks

-- | Register a particular nick with the server.
registerCurrentNick :: ConnectionManager -> [Nick] -> AM Bool
registerCurrentNick manager nicks@(nick : _) = do
  liftIO . atomically $ writeTChan (comaEvents manager) (ComaAttemptingNick nick)
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection -> do
      subscription <- liftIO . atomically $ C.subscribe connection
      liftIO . atomically . C.send connection $
        IRCMessage { ircmPrefix = Nothing,
                     ircmCommand = cmd_NICK,
                     ircmParameters = [nick],
                     ircmComment = Nothing }
      liftIO $ threadDelay nickRegistrationDelay
      event <- C.peek subscription
      case event of
        Just (ConnMessage (IRCCommand { ircmCommand = command, ircmComment = comment })) ->
          | err_NICKNAMEINUSE -> registerCurrentNick manager (nextNick nicks)
          | err_ERRONEUSNICKNAME ->
              case nicks of
                [nick] -> do
                  liftIO . atomically $ writeTChan (comaEvents manager) (ComaMalformedNick nick)
                (_ : next) -> registerCurrentNick manager next
          | err_NICKCOLLISION -> registerCurrentNick manager (nextNick nicks)
          | err_UNAVAILRESOURCE -> registerCurrentNick manager (nextNick nicks)
          | err_RESTRICTED -> do
              setNick manager nick
              return True
          | err_YOUREBANNEDCREEP -> do
              liftIO . atomically $ writeTChan (comaEvents manager) (ComaBannedFromServer comment)
              doDisconnect manager
              return False
        _ -> do
          liftIO . atomically $ setNick manager nick
          return True
    _ -> return False

-- | Get the next nick.
nextNick :: [Nick] -> [Nick]
nextNick [nick] = [B.append nick (BC.singleton '_')]
nextNick (_ : next) = next

-- | User registration delay (in microseconds).
userRegistrationDelay :: Int
userRegistrationDelay = 1000000

-- | Register a user with the server.
registerUser :: ConnectionManager -> AM ()
registerUser manager = do
  setup <- liftIO . atomically . readTVar $ comaSetup manager
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection ->
      subscription <- liftIO . atomically $ C.subscribe connection
      liftIO . atomically . C.send connection $
        IRCMessage { ircmPrefix = Nothing,
                     ircCommand = cmd_USER,
                     ircParameters = [comaUserName setup,
                                      convertUserMode $ comaMode setup,
                                      BC.singleton '*'],
                     ircComment = comaName setup }
      liftIO $ threadDelay userRegistrationDelay
      event <- liftIO . atomically $ C.recv subscription
      case event of
        ConnMessage message@(IRCMessage { ircmCommand = command }) ->
          | command == err_YOUREBANNEDCREEP -> do
             liftIO . atomically $ writeChan (comaEvents manager) ComaBannedFromServer
             doDisconnect
          | command == rpl_WELCOME ->
             liftIO . atomically $ do
               writeTVar (comaRegistered manager) True
               writeTChan (comaEvents manager) (ComaWelcome $ ircmComment message)
          | command == rpl_MOTDSTART ->
            case ircmComment message of
              Just comment -> liftIO . atomically $ writeTVar (comaMotd manager) [comment]
              Nothing -> liftIO . atomically $ writeTVar (comaMotd manager) []
          | command == rpl_MOTD ->
            case ircmComment message of
              Just comment -> liftIO . atomically $ do
                motd <- readTVar $ comaMotd manager
                writeTVar (comaMotd manager) (comment : motd)
              Nothing -> return ()
          | command == rpl_ENDOFMOTD ->
            case ircmComment message of
              Just comment -> liftIO . atomically $ do
                motd <- readTVar $ comaMotd manager
                writeTVar (comaMotd manager) []
                writeTChan (comaEvents manager) (ComaMotd . reverse $ comment : motd)
              Nothing -> liftIO . atomically $ do
                motd <- readTVar $ comaMotd manager
                writeTVar (comaMotd manager) []
                writeTChan (comaEvents manager) (ComaMotd $ reverse motd)
        _ -> liftIO . atomically $ writeTChan (comaEvents manager) ComaRegistrationFailed
    Nothing -> return ()

-- | Convert a user mode to a single message parameter.
convertUserMode :: [UserMode] -> MessageParameter
convertUserMode mode =
  let wallopsMode = if elem (BC.singleton 'w') mode then 4 else 0
      invisibleMode = if elem (BC.singleton 'i') mode then 8 else 0 in
  BC.pack . show $ wallopsMode + invisibleMode

-- | Actually connect to a connection.
doConnect :: ConnectionManager -> AM (Either Error ())
doConnect manager = do
  setup <- liftIO . atomically . readTVar $ comaSetup manager
  case setup of
    Just setup -> do
      connection <- liftIO . atomically . readTVar $ comaConnection manager
      case connection of
        Nothing -> do
          connection <- liftIO . atomically $ do
            connection <- C.new
            writeTVar (comaConnection manager) (Just connection)
            subscription <- C.subscribe connection
            oldSubscription <- readTVar $ comaSubscription manager
            writeTVar (comaSubscription manager) (oldSubscription ++ [subscription])
            return connection
          response <- liftIO $ C.connect connection (comaOriginalHost setup) (comaPort setup)
          liftIO . atomically $ C.waitConnect response
        Just _ -> do
          errorText <- lookupText $ T.pack "Already connected to server"
          return . Left $ Error [errorText]
  Nothing -> do
    errorText <- lookupText $ T.pack "Never connected to server"
    return . Left $ Error [errorText]

-- | Actually reconnect to a connection.
doReconnect :: ConnectionManager -> AM (Either Error ())
doReconnect manager = do
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  result <- case connection of
    Just connection -> doDisconnect manager
    Nothing -> return $ Right ()
  case result of ->
    Right () -> doConnect manager
    Left error -> return $ Left error

-- | Actually disconnect from a connection.
doDisconnect :: ConnectionManager -> AM (Either Error ())
doDisconnect manager = do
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection ->
      response <- liftIO . atomically $ C.disconnect connection
      result <- liftIO . atomically $ do C.waitDisconnect response
      liftIO . atomically $ writeTVar (comaConnection manager) Nothing
      return result
    Nothing -> do
      errorText <- lookupText $ T.pack "not connected to server"
      return . Left $ Error [errorText]

-- | Actually send a message to a connection.
doSend :: ConnectionManager -> IRCMessage -> AM (Either Error ())
doSend manager message = do
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection -> do
      response <- liftIO . atomically $ C.send connection message
      response' <- liftIO . atomically $ do C.waitSend response
      nick <- liftIO . atomically . readTVar $ comaNick manager
      case (ircmParameters message, ircmComment message) of
       ([dest], Just comment) ->
         case checkCtcp comment of
          Just comment
            | ircmCommand message == cmd_PRIVMSG ->
              liftIO. atomically . writeTChan (comaEvents manager) $
              ComaSelfCtcpRequest nick (parseChannelNameOrNick dest) comment
            | ircmCommand message == cmd_NOTICE ->
              liftIO . atomically . writeTChan (comaEvents manager) $
              ComaSelfCtcpReply nick (parseChannelNameOrNick dest) comment
            | otherwise -> return ()
          _
            | ircmCommand message == cmd_PRIVMSG ->
              liftIO. atomically . writeTChan (comaEvents manager) $
              ComaSelfMessage nick (parseChannelNameOrNick dest) comment
            | ircmCommand message == cmd_NOTICE ->
                liftIO . atomically . writeTChan (comaEvents manager) $
                ComaSelfNotice nick (parseChannelNameOrNick dest) comment
            | otherwise -> return ()
        _ ->
      return response'
    Nothing -> do
      errorText <- lookupText $ T.pack "not connected to server"
      return . Left $ Error [errorText]

-- | Convert events.
convertEvent :: ConnectionEvent -> ConnectionManagerEvent
convertEvent (ConnLookupAddress hostname) = ComaLookupAddress hostname
convertEvent (ConnFoundAddress addr) = ComaFoundAddress addr
convertEvent (ConnLookupAddressFailed error) = ComaLookupAddressFailed error
convertEvent (ConnFoundHostname hostname) = ComaFoundHostname hostname
convertEvent (ConnReverseLookupFailed error) = ComaReverseLookupFailed error
convertEvent (ConnConnecting hostname port) = ComaConnecting hostname port
convertEvent (ConnConnected hostname port) = ComaConnected hostname port
convertEvent (ConnConnectFailed error) = ComaConnectFailed error
convertEvent (ConnDisconnected error) = ComaDisconnected error
convertEvent (ConnMessage message) = ComaMessage message
convertEvent (ConnMalformed bytes) = ComaMalformed bytes

-- | Handle close for connection manager.
handleClose :: ConnectionManager -> ConnectionManagerState -> STM (AM (ConnectionManagerState, Bool))
handleClose manager state = do
  ConnectionManagerStopResponse response <- readTMVar $ comaStop manager
  handle <- readTVar $ comaHandle manager
  putTMVar response ()
  return $ do
    case comaHandle state of
      Just handle -> closeConnectionHandle handle
      Nothing -> return ()
    return (comaHandle { comaHandle = Nothing }, True)
