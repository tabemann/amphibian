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

module Network.IRC.Client.Amphibian.ChannelServer

       (ChannelServer,
        ChannelServerStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Commands
import Network.IRC.Client.Amphibian.Utility
import Data.Functor ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad as M
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar,
                               TChan,
                               atomically,
                               orElse,
                               newTVar,
                               writeTVar,
                               readTVar,
                               newEmptyTMVar,
                               putTMVar,
                               readTMVar,
                               newBroadcastTChan,
                               dupTChan,
                               writeTChan,
                               peekTChan,
                               readTChan)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Time.Clock (UTCTime,
                        getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime,
                              posixSecondsToUTCTime)

-- | Create a new channel server.
new :: STM ChannelServer
new = do
  running <- newTVar Bool
  actions <- newTQueue
  return $ ChannelServer { chseRunning = running,
                           chseActions = actions }

-- | Start a channel server
start :: ChannelServer -> AM ()
start server = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ chseRunning server
    if not running
    then do
      writeTVar (chseRunning server) True
      I.registerChannelServer intf server
      return . async $ runAM (runServer server)
    else return $ return ()

-- | Stop channel server.
stop :: ChannelServer -> STM ChannelServerStopResponse
stop server = do
  response <- ChannelServerStopResponse <$> newEmptyTMVar
  writeTVar (chseActions server) (ChsaStop response)
  return response

-- | Wait for channel server to stop.
waitStop :: ChannelServerStopResponse -> STM (Either Error ())
waitStop (ChannelServerStopResponse response) = readTMVar response

-- | Run channel server.
runServer :: ChannelServer -> AM ()
runServer server = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    action <- readTQueue $ chseActions server
    case action of
      ChsaStartChannel channel (ChannelStartResponse response) -> do
        active <- readTVar $ chanActive channel
        if not active
        then do
          writeTVar (chanActive channel) True
          I.registerChannel intf channel
          putTMVar response $ Right ()
          return $ do
            async $ runAM (runChannel channel) intf
            return True
        else do
          errorText <- I.lookupText intf "Channel is already started"
          putTMVar response . Left $ Error [errorText]
          return $ return True
      ChsaStop -> do
        I.unregisterChannelServer intf server
        return $ return False
  if continue
  then runServer server
  else return ()

-- | Run a channel.
runChannel :: Channel -> AM ()
runChannel channel = do
  continue <- M.join . liftIO . atomically $ handleActions channel `orElse` handleEvents channel
  if continue
  then runChannel channel
  else return ()

-- | Handle actions.
handleActions :: Channel -> STM (AM Bool)
handleActions channel = do
  action <- readTQueue $ chanActions channel
  case action of
    ChanStop response -> doStop channel response
    ChanJoin response -> doJoin channel response
    ChanPart comment response -> doPart channel comment response
    ChanMessage comment response -> doMessage channel comment response
    ChanNotice comment response -> doNotice channel comment response
    ChanSetTopic comment response -> doSetTopic channel comment response

-- | Carry out stop.
doStop :: Channel -> ChannelStopResponse -> STM (AM Bool)
doStop channel (ChannelStopResponse response) = do
  I.unregisterChannel (chanInterface channel) channel
  writeTVar (chanActive channel) False
  putTMVar response $ Right ()
  return $ return False

-- | Carry out join.
doJoin :: Channel -> ChannelJoinResponse -> STM (AM Bool)
doJoin channel (ChannelJoinResponse response) = do
  autoJoin <- readTVar $ chanAutoJoin channel
  if not autJoin
    then do writeTVar (chanAutoJoin channel) True
            name <- readTVar $ chanName channel
            key <- readTVar $ chanKey channel
            let parameters = maybe [name] (\key -> [name, key]) key
            sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                                   ircmCommand = cmd_JOIN,
                                                                                   ircmParameters = parameters,
                                                                                   ircmComment = Nothing }
            let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                                 putTMVar response errorResponse
            return $ do intf <- getInterface
                        liftIO . async $ runAM relay intf
                        return True
    else return $ do errorText <- lookupText "already joined channel"
                     liftIO . atomically . putTMVar response . Left $ Error [errorText]
                     return True

-- | Carry out part.
doPart :: Channel -> Maybe MessageComment -> ChannelPartResponse -> STM (AM Bool)
doPart channel comment (ChannelPartResponse response) = do
  autoJoin <- readTVar $ chanAutoJoin channel
  if autoJoin
  then do
    writeTVar (chanAutoJoin channel) False
    writeTVar (chanJoined channel) False
    name' <- readTVar $ chanName channel
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_PART,
                                                                           ircmParameters = [name],
                                                                           ircmComment = comment }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <- lookupText "not joined channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out message.
doMessage :: Channel -> MessageComment -> ChannelMessageResponse -> STM (AM Bool)
doMessage channel comment (ChannelMessageResponse response) = do
  joined <- readTVar $ chanJoined channel
  if joined
  then do
    nick <- CM.getNick $ chanConnectionManager channel
    name' <- readTVar $ chanName channel
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_PRIVMSG,
                                                                           ircmParameters = [name].
                                                                           ircmComment = Just comment }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <-lookupText "not in channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out notice.
doNotice :: Channel -> MessageComment -> ChannelNoticeResponse -> STM (AM Bool)
doNotice channel comment (ChannelNoticeResponse response) = do
  joined <- readTVar $ chanJoined channel
  if joined
  then do
    nick <- CM.getNick $ chanConnectionManager channel
    name' <- readTVar $ chanName chanel
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_NOTICE,
                                                                           ircmParameters = [name].
                                                                           ircmComment = Just comment }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <-lookupText "not in channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out set topic.
doSetTopic :: Channel -> MessageComment -> ChannelSetTopicResponse -> STM (AM Bool)
doSetTopic channel comment (ChannelSetTopicResponse response) = do
  joined <- readTVar $ chanJoined channel
  if joined
  then do
    name' <- readTVar $ chanName channel
    sendResponse < CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                          ircmCommand = cmd_TOPIC,
                                                                          ircmParameters = [name],
                                                                          ircmComment = Just comment }
    writeTVar (chanTopic channel) (Just comment)
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <- lookupText "not in channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Handle events.
handleEvents :: Channel -> STM (AM Bool)
handleEvents channel = do
  autoJoin <- readTVar $ chanAutoJoin channel
  if autoJoin
    then do event <- CM.recv $ chanConnectionManager channel
            case event of
             ComaMessage message@(IRCMessage { ircmCommand = command })
               | command == cmd_JOIN -> handleJoin channel message
               | command == cmd_PART -> handlePart channel message
               | command == cmd_PRIVMSG -> handlePrivmsg channel message
               | command == cmd_NOTICE -> handleNotice channel message
               | command == cmd_TOPIC -> handleTopic channel messsage
               | command == rpl_NAMREPLY -> handleRplNamreply channel message
               | command == rpl_ENDOFNAMES -> handleRplEndofnames channel message
               | command == rpl_NOTOPIC -> handleRplNotopic channel message
               | command == rpl_TOPIC -> handleRplTopic channel message
               | command == rpl_TOPICWHOTIME -> handleRplTopicWhoTime channel message
               | command == cmd_QUIT -> handleQuit channel message
             ComaRecvNick oldNick newNick -> handleRecvNick channel oldNick newNick
             ComaRecvCtcpRequest nick dest comment -> handleRecvCtcpRequest channel nick dest comment
             ComaRecvCtcpReply nick dest comment -> handleRecvCtcpReply channel nick dest comment
             ComaRecvSelfNick oldNick newNick -> handleRecvSelfNick channel oldNick newNick
             ComaSelfMessage nick dest comment -> handleSelfMessage channel nick dest comment
             ComaSelfNotice nick dest comment -> handleSelfNotice channel nick dest comment
             ComaSelfCtcpRequest nick dest comment -> handleSelfCtcpRequest channel nick dest comment
             ComaSelfCtcpReply nick dest comment -> handleSelfCtcpReply channel nick dest comment
             ComaDisconnected error -> writeTChan (chanEvents channel) (ChanDisconnected error)
             _ -> return $ return True
  else return $ return True

-- | Handle JOIN message.
handleJoin :: Channel -> IRCMessage -> STM (AM Bool)
handleJoin channel message = do
  currentNick <- CM.getNick $ chanConnectionManager channel
  name' <- readTVar $ chanName channel
  case (extractNick $ ircmPrefix message, ircmComment message) of
    (Just nick, Just channelName)
      | nick == currentNick && channelName == name' -> do
        response <- readTVar $ chanJoinResponse channel
        case response of
          Just response -> do
            putTMVar response (Right ())
          Nothing -> return ()
        writeTVar (chanJoined channel) True
        writeTChan (chanEvents channel) ChanJoined
      | channelName == chanName channel ->
        writeTChan (chanEvents channel) (ChanRecvJoin nick (ircPrefix message))
        CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                               ircmCommand = cmd_NAMES,
                                                               ircmParameters = [chanName channel],
                                                               ircmComment = Nothing }
    _ -> return ()
  return $ return True

-- | Handle PART message.
handlePart :: Channel -> IRCMessage -> STM (AM Bool)
handlePart channel message = do
  currentNick <- CM.getNick $ chanConnectionManager channel
  name' <- readTVar $ chanName channel
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | nick == currentNick && name == channelName ->
        writeTChan (chanEvents channel) (ChanParted $ ircmComment comment)
      | name == channelName ->
        writeTChan (chanEvents channel) (ChanRecvPart nick (ircmPrefix prefix) (ircmComment comment))
        nicks <- filter (\(knownNick, _) -> knownNick /= nick) <$> readTVar $ chanNames channel
        writeTVar (chanNames channel) nicks
    _ -> return ()
  return $ return True

-- | Handle PRIVMSG message.
handlePrivmsg :: Channel -> IRCMessage -> STM (AM Bool)
handlePrivmsg channel message = do
  name' <- readTVar $ chanName channel
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | channelName == name' -> writeTChan (chanEvents channel) (ChanRecvMessage nick (ircmComment message))
    _ -> return ()
  return $ return True

-- | Handle NOTICE message.
handleNotice :: Channel -> IRCMessage -> STM (AM Bool)
handleNotice channel message = do
  name' <- readTVar $ chanName channel
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | channelName == name' -> writeTChan (chanEvents channel) (ChanRecvNotice nick (ircmComment message))
    _ -> return ()
  return $ return True

-- | Handle TOPIC message.
handleTopic :: Channel -> IRCMessage -> STM (AM Bool)
handleTopic channel message = do
  name' <- readTVar $ chanName channel
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | channelName == name' -> do
        return $ do
          time <- liftIO getCurrentTime
          liftIO . atomically $ do
            writeTVar (chanTopic channel) (Just $ ircmComment message)
            writeTVar (chanTopicUser channel) (Just $ ircmPrefix message)
            writeTVar (chanTopicTime channel) (Just time)
            writeTChan (chanEvents channel) (ChanRecvTopic nick $ ircmComment message)
            return True
    _ -> return $ return True

-- | Handle rpl_NAMREPLY message.
handleRplNamrply :: Channel -> IRCMessage -> STM (AM Bool)
handleRplNamrply channel message = do
  name' <- readTVar $ chanName channel
  let parameters = ircmParameters message
  case fromEnd 1 parameters of
    Just name | name == name' -> do
      case fromEnd 2 parameters of
        Just channelType
          | channelType == BUTF8.fromString "=" -> writeTVar (chanType channel) ChanPublic
          | channelType == BUTF8.fromString "*" -> writeTVar (chanType channel) ChanPrivate
          | channelType == BUTF8.fromString "@" -> writeTVar (chanType channel) ChanSecret
        _ -> return ()
      case ircmComment message of
        Just comment -> do
          let names = splitNames comment
          oldNames <- readTVar $ chanNames channel
          writeTVar (chanNames channel) (names ++ oldNames)
        Nothing -> return ()
    _ -> return ()
  return $ return True
 
-- | Split names in a rpl_NAMREPLY message
splitNames :: MessageComment -> [(Nick, UserStatus)]
splitNames comment = splitNames' comment []
  where splitNames' comment names =
          let (name, rest) = BUTF8.break (== ' ') comment in
          let names = if BUTF8.length name > 0
                      then case BUTF8.uncons name of
                             Just ('@', name) -> (name, UserOp) : names
                             Just ('%', name) -> (name, UserHalfOp) : names
                             Just ('+', name) -> (name, UserVoice) : names
                             _ -> (name, UserNormal) : names
                      else names
              rest = BUTF8.drop 1 rest in
          if BUTF8.length rest > 0
          then splitNames' rest names
          else reverse names

-- | Handle rpl_ENDOFNAMES message.
handleRplEndofnames :: Channel -> IRCMessage -> STM (AM Bool)
handleRplEndofnames channel message = do
  case fromEnd 1 $ ircmParameters message of
    Just name | name == chanName channel -> do
      names <- readTVar $ chanNamesAccum channel
      writeTVar (chanNames channel) names
      writeTVar (chanNamesAccum channel) []
      channelType <- readTVar $ chanType channel
      writeTChan (chanEvents channel) (ChanType channelType)
      writeTChan (chanEvents channel) (ChanNames names)
    _ -> return ()
  return $ return True

-- | Handle rpl_NOTOPIC message.
handleRplNotopic :: Channel -> IRCMessage -> STM (AM Bool)
handleRplNotopic channel message = do
  case fromEnd 1 $ ircmParameters message of
    Just name | name == chanName channel -> do
      writeTVar (chanTopic channel) Nothing
      writeTChan (chanEvents channel) ChanNoTopic
    _ -> return ()
  return $ return True

-- | Handle rpl_TOPIC message.
handleRplTopic :: Channel -> IRCMessage -> STM (AM Bool)
handleRplTopic channel message = do
  case (fromEnd 1 $ ircmParameters message, ircmComment message) of
    (Just name, Just comment) | name == chanName channel -> do
      writeTVar (chanTopic channel) comment
      writeTChan (chanEvents channel) (ChanTopic comment)
    _ -> return ()
  return $ return True

-- | Handle rpl_TOPICWHOTIME message.
handleRplTopicWhoTime :: Channel -> IRCMessage -> STM (AM Bool)
handleRplTopicWhoTime channel message = do
  case ircmParameters message of
    [name, user, timeString] | name == chanName channel -> do
      case readMaybe timeString :: Maybe Integer of
        Just time -> do
          let time' = posixSecondsToUTCTime $ realToFrac time
          writeTVar (chanTopicUser channel) user
          writeTVar (chanTopicTime channel) time'
          writeTChan (chanEvents channel) (ChanTopicWhoTime user time')
        _ -> return ()
    _ -> return ()
  return $ return True

-- | Handle QUIT message.
handleQuit :: Channel -> IRCMessage -> STM (AM Bool)
handleQuit channel message = do
  currentNick <- CM.getNick $ chanConnectionManager channel
  names <- readTVar $ chanNames channel
  case extractNick $ ircmPrefix message of
    Just nick
      | nick /= currentNick && nick `elem` map (\(knownNick, _) -> knownNick) names ->
        writeTChan (chanEvents channel) (ChanRecvQuit nick (ircmPrefix prefix) (ircmComment comment))
        names' <- filter (\(knownNick, _) -> knownNick /= nick) <$> names
        writeTVar (chanNames channel) names'
    _ -> return ()
  return $ return True

-- | Handle received CTCP request message.
handleRecvCtcpRequest :: Channel -> Nick -> ChannelNameOrNick -> MessageComment -> STM (AM Bool)
handleRecvCtcpRequest channel nick dest comment = do
  name' <- readTVar $ chanName channel
  if dest == CnonChannelName name'
  then writeTChan (chanEvents channel) (ChanRecvCtcpRequest nick comment)
  else return ()
  return $ return True

-- | Handle received CTCP reply message.
handleRecvCtcpReply :: Channel -> Nick -> ChannelNameOrNick -> MessageComment -> STM (AM Bool)
handleRecvCtcpReply channel nick dest comment = do
  name' <- readTVar $ chanName channel
  if dest == CnonChannelName name'
  then writeTChan (chanEvents channel) (ChanRecvCtcpReply nick comment)
  else return ()
  return $ return True

-- | Handle received nick message.
handleRecvNick :: Channel -> Nick -> Nick -> STM (AM Bool)
handleRecvNick channel oldNick newNick = do
  inChannel <- isNickInChannel channel oldNick
  if inChannel
    then do oldNicks <- readTVar $ chanNames channel
            let newNicks = map (\(nick, status) -> (if nick == oldNick then newNick else nick, status)) oldNicks
            writeTVar (chanNames channel) newNicks
            writeTChan (chanEvents channel) (ChanRecvNick oldNick newNick)
    else return ()
  return $ return True

-- | Handle received self nick message.
handleRecvSelfNick :: Channel -> Nick -> Nick -> STM (AM Bool)
handleRecvSelfNick channel oldNick newNick = do
  oldNicks <- readTVar $ chanNames channel
  let newNicks = map (\(nick, status) -> (if nick == oldNick then newNick else nick, status)) oldNicks
  writeTVar (chanNames channel) newNicks
  writeTChan (chanEvents channel) (ChanRecvSelfNick oldNick newNick)
  return $ return True

-- | Handle self message message.
handleSelfMessage :: Channel -> Nick -> ChannelNameOrNick -> MessageComment -> STM (AM Bool)
handleSelfMessage channel nick dest comment = do
  name' <- readTVar $ chanName channel
  if dest == CnonChannelName name'
  then writeTChan (chanEvents channel) (ChanSelfMessage nick comment)
  else return ()
  return $ return True

-- | Handle self notice message.
handleSelfNotice :: Channel -> Nick -> ChannelNameOrNick -> MessageComment -> STM (AM Bool)
handleSelfNotice channel message = do
  name' <- readTVar $ chanName channel
  if dest == CnonChannelName name'
  then writeTChan (chanEvents channel) (ChanSelfNotice nick comment)
  else return ()
  return $ return True

-- | Handle self CTCP request message.
handleSelfCtcpRequest :: Channel -> Nick -> ChannelNameOrNick -> MessageComment -> STM (AM Bool)
handleSelfCtcpRequest channel nick dest comment = do
  name' <- readTVar $ chanName channel
  if dest == CnonChannelName name'
  then writeTChan (chanEvents channel) (ChanSelfCtcpRequest nick comment)
  else return ()
  return $ return True

-- | Handle self CTCP reply message.
handleSelfCtcpReply :: Channel -> Nick -> ChannelNameOrNick -> MessageComment -> STM (AM Bool)
handleSelfCtcpReply channel nick dest comment = do
  name' <- readTVar $ chanName channel
  if dest == CnonChannelName name'
  then writeTChan (chanEvents channel) (ChanSelfCtcpReply nick comment)
  else return ()
  return $ return True

-- | Get the item in a position from the end of a list
fromEnd :: Int -> [a] -> Maybe a
fromEnd position list
  | length list < position = Nothing
  | otherwise = case drop ((length list) - position) list of
                  item : _ -> Just item
                  _ -> Nothing

-- | Get whether a nick is in a channel.
isNickInChannel :: Channel -> Nick -> STM Bool
isNickInChannel channel nick = elem nick <$> map fst <$> readTVar $ chanNames channel
