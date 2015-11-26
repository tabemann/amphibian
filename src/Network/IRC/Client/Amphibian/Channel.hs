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

module Network.IRC.Client.Amphibian.Channel

       (Channel,
        ChannelName,
        ChannelKey,
        ChannelTopic,
        UserStatus,
        ChannelEvent,
        ChannelJoinResponse,
        ChannelPartResponse,
        ChannelMessageResponse,
        ChannelNoticeResponse,
        new,
        getConnectionManager,
        getName,
        getTopic,
        getTopicUser,
        getTopicTime,
        getType,
        getNames,
        getAutoJoin,
        getJoined,
        start,
        stop,
        waitStop,
        subscribe,
        peek,
        recv,
        join,
        waitJoin,
        part,
        waitPart,
        message,
        waitMessage,
        notice,
        waitNotice,
        setTopic,
        waitSetTopic)
       where

import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Commands
import Network.IRC.Client.Amphibian.Utility
import Data.Functor ((<$>))
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
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | Create channel.
new :: Interface -> ConnectionManager -> ChannelName -> Maybe ChannelKey -> STM Channel
new intf manager name key = do
  actions <- newTQueue
  events <- newBroadcastTChan
  name' <- newTVar name
  key <- newTVar key
  topic <- newTVar Nothing
  topicUser <- newTVar Nothing
  topicTime <- newTVar Nothing
  channelType <- newTVar ChanPublic
  names <- newTVar []
  namesAccum <- newTVar []
  autoJoin <- newTVar False
  joined <- newTVar False
  joinResponse <- newTVar Nothing
  active <- newTVar False
  return $ Channel { chanInterface = interface,
                     chanActions = actions,
                     chanEvents = events,
                     chanName = name',
                     chanConnectionManager = manager,
                     chanKey = key,
                     chanTopic = topic,
                     chanTopicUser = topicUser,
                     chanTopicTime = topicTime,
                     chanType = channelType,
                     chanNames = names,
                     chanNamesAccum = namesAccum,
                     chanAutoJoin = autoJoin,
                     chanJoined = joined,
                     chanJoinResponse = joinResponse,
                     chanActive = active }

-- | Get connection manager
getConnectionManager :: Channel -> ConnectionManager
getConnectionManager = chanConnectionManager

-- | Get channel name.
getName :: Channel -> STM ChannelName
getName = readTVar . chanName

-- | Get channel topic.
getTopic :: Channel -> STM (Maybe ChannelTopic)
getTopic = readTVar . chanTopic

-- | Get channel topic user.
getTopicUser :: Channel -> STM (Maybe FullUser)
getTopicUser = readTVar . chanTopicUser

-- | Get channel topic time.
getTopicTime :: Channel -> STM UTCTime
getTopicTime = readTVar . chanTopicTime

-- | Get channel type.
getType :: Channel -> STM ChannelType
getType = readTVar . chanType

-- | Get channel names.
getNames :: Channel -> STM [(Nick, UserStatus)]
getNames = readTVar . chanNames

-- | Get whether the channel will be auto-joined.
getAutoJoin :: Channel -> STM Bool
getAutoJoin = readTVar . chanAutoJoin

-- | Get whether the channel is joined.
getJoined :: Channel -> STM Bool
getJoined = readTVar . chanJoined

-- | Start a channel thread.
start :: Channel -> STM ChannelStartResponse
start channel = do
  response <- newEmptyTMVar
  let response' = ChannelStartResponse response
  channelServer <- I.getChannelServer $ chanInterface channel
  case channelServer of
    Just channelServer ->
      writeTQueue (chseActions channelServer) $ ChsaStartChannel channel response'
    Nothing -> do
      errorText <- I.lookupText (chanInterface channel) "Channel server is not registered"
      putTMVar response . Left $ Error [errorText]
  return response'

-- | Wait for response to starting channel thread.
waitStart :: ChannelStartResponse -> STM (Either Error ())
waitStart (ChannelStartResponse response) = readTMVar response

-- | Stop a channel.
stop :: Channel -> STM ChannelStopResponse
stop channel = do
  response <- ChannelStopResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanStop response)
  return response

-- | Wait for a response to stopping a channel.
waitStop :: ChannelStopResponse -> STM (Either Error ())
waitStop (ChannelStopResponse response) = readTMVar response

-- | Subscribe to events for a channel.
subscribe :: Channel -> STM ChannelSubscription
subscribe channel = ChannelSubscription <$> dupTChan $ chanEvents channel

-- | Peek an event from a channel subscription.
peek :: ChannelSubscription -> STM ChannelEvent
peek (ChannelSubscription events) = peekTChan events

-- | Receive an event from a channel subscription
recv :: ChannelSubscripton -> STM ChannelEvent
recv (ChannelSubscription events) = readTChan events

-- | Join a channel.
join :: Channel -> STM ChannelJoinResponse
join channel = do
  response <- ChannelJoinResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanJoin response)
  return response

-- | Wait for a response to joining a channel.
waitJoin :: ChannelJoinResponse -> STM (Either Error ())
waitJoin (ChannelJoinResponse response) = readTMVar response

-- | Part from a channel.
part :: Channel -> Maybe MessageComment -> STM ChannelPartResponse
part channel comment = do
  response <- ChannelPartResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanPart comment response)
  return response

-- | Wait for a response to parting from a channel.
waitPart :: ChannelPartResponse -> STM (Either Error ())
waitPart (ChannelPartResponse response) = readTMVar response

-- | Send a message to a channel.
message :: Channel -> MessageComment -> STM ChannelMessageResponse
message channel comment = do
  response <- ChannelMessageResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanMessage comment response)
  return response

-- | Wait for a response to sending a message to a channel.
waitMessage :: ChannelMessageResponse -> STM (Either Error ())
waitMessage (ChannelMessageResponse response) = readTMVar response

-- | Send a notice to a channel.
notice :: Channel -> MessageComment -> STM ChannelNoticeResponse
notice channel comment = do
  response <- ChannelNoticeResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanNotice comment response)
  return response

-- | Wait for a response to sending a notice to a channel.
waitNotice :: ChannelNoticeResponse -> STM (Either Error ())
waitNotice (ChannelNoticeResponse response) = readTMVar response

-- | Set the topic of a channel.
setTopic :: Channel -> MessageComment -> STM ChannelSetTopicResponse
setTopic channel comment = do
  response <- ChannelSetTopicResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanSetTopic comment response)
  return response

-- | Wait for a response to setting the topic of a channel.
waitSetTopic :: ChannelSetTopicResponse -> STM (Either Error ())
waitSetTopic (ChannelSetTopicResponse response) = readTMVar response
