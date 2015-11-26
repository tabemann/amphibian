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

module Network.IRC.Client.Amphibian.ChannelDisplay

       (ChannelDisplay,
        ChannelDisplayStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Ctcp
import Network.IRC.Client.Amphibian.Commands
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.Channel as C
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.FrameMessage as FM
import Control.Concurrrent.STM (STM,
                                TVar,
                                TMVar,
                                atomically,
                                orElse,
                                retry,
                                newTVar,
                                readTVar,
                                writeTVar,
                                newEmptyTMVar,
                                putTMVar,
                                readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Data.List (find)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8

-- | Create a connection display.
new :: Interface -> STM ChannelDisplay
new intf = do
  running <- newTVar False
  actions <- newTQUeue
  interfaceSubscription <- I.subscribe intf
  frames <- newTVar []
  allFrames <- newTVar []
  return $ ChannelDisplay { chdiRunning = running,
                            chdiActions = actions,
                            chdiInterface = interface,
                            chdiInterfaceSubscription = interfaceSubscription,
                            chdiFrames = frames,
                            chdiAllFrames = allFrames }

-- | Start connection display.
start :: ChannelDisplay -> AM ()
start display = do
  join . liftIO . atomically $ do
    running <- readTVar $ chdiRunning display
    if not running
    then do
      writeTVar (chdiRunning display) True
      return $ do
        intf <- getInterface
        async (runAM (runDisplay display) intf)
    else return $ return ()

-- | Stop connection display.
stop :: ChannelDisplay -> STM ChannelDisplayStopResponse
stop display = do
  response <- ChannelDisplayStopResponse <$> newEmptyTMVar
  writeTQueue (codaActions display) (CodaStop response)
  return response

-- | Wait for response to stopping connection display.
waitStop :: ChannelDisplayStopResponse -> STM (Either Error ())
waitStop (ChannelDisplayStopResponse response) = readTMVar response

-- | Run connection display.
runChannelDisplay :: ChannelDisplay -> AM ()
runChannelDisplay display = do
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ chdiFrames display
    allFrames <- readTVar $ chdiAllFrames display
    handleAction display `orElse` handleInterfaceEvent display `orElse`
      (foldr (\frame y -> handleFrameEvent display frame `orElse` y) retry allFrames) `orElse`
      (foldr (\mapping y -> handleChannelEvent display mapping `orElse` y) retry frames) 
  if continue
  then runChannelDisplay display
  else liftIO . atomically $ writeTVar (chdiRunning display) False

-- | Handle connection display action.
handleAction :: ChannelDisplay -> STM (AM Bool)
handleAction display = do
  action <- readTQueue $ chdiActions display
  case action of
    CodaStop (ChannelDisplayStopResponse response) -> do
      writeTVar (chdiRunning display) False
      writeTMVar response (Right ())
      return $ return False

-- | Handle interface event.
handleInterfaceEvent :: ChannelDisplay -> STM (AM Bool)
handleInterfaceEvent display = do
  event <- I.recv $ chdiInterfaceSubscription display
  case event of
    IntfFrameRegistered frame -> do
      allFrames <- readTVar $ chdiAllFrames display
      if frame `notElem` map chdfFrame allFrames
      then do
        frameSubscription <- F.subscribeInput frame
        let frame' = ChannelDisplayFrame { chdfFrame = frame,
                                           chdfSubscription = frameSubscription }
        writeTVar (chdiAllFrames display) (frame' : allFrames)
      else return ()
      mapping <- F.getMapping frame
      case mapping of
        FrmaChannel channel -> addMappedFrame display frame channel
        _ -> return $ return True
    IntfFrameUnregistered frame -> do
      allFrames <- readTVar $ chdiAllFrames display
      if frame `elem` map chdfFrame allFrames
      then writeTVar (chdiAllFrames display) (filter (\frame' -> chdfFrame frame' /= frame) allFrames)
      else return ()
      removeMappedFrame display frame
    _ -> return $ return True

-- | Handle frame event.
handleFrameEvent :: ChannelDisplay -> ChannelDisplayFrame -> STM (AM Bool)
handleFrameEvent display frame = do
  event <- F.recvInput $ chdfSubscription frame
  case event of
   FievMapping mapping -> do
     case mapping of
      FrmaChannel channel -> addMappedFrame display (chdfFrame frame) channel
      _ -> removeMappedFrame display $ chdfFrame frame
   _ -> return $ return True

-- | Add frame to mapped frames.
addMappedFrame :: ChannelDisplay -> Frame -> Channel -> STM (AM Bool)
addMappedFrame display frame channel = do
  frames <- readTVar $ chdiFrames display
  case find (\mapping -> cdfmFrame mapping == frame) frames of
   Nothing -> addMappedFrame' display frame channel frames
   Just mapping
     | cdfmChannel mapping /= channel ->
         let frames' = filter (\mapping -> cdfmFrame mapping /= frame) frames in
         addMappedFrame' display frame channel frames'
     | otherwise -> return $ return True
  where addMappedFrame' display frame channel frames' = do
          subscription <- C.subscribe channel
          let mapping = ChannelDisplayFrameMapping { cdfmFrame = frame,
                                                     cdfmChannel = channel,
                                                     cdfmSubscription = subscription }
          writeTVar (chdiFrames display) (mapping : frames)
          name <- C.getName channel
          let manager = C.getConnectionManager channel
          nick <- CM.getNick manager
          setup <- CM.getSetup manager
          return $ do
            FM.setNick frame nick
            FM.setName frame name
            FM.setTitle frame (Just $ comaServerName setup) (Just name)
            return True

-- | Remove frame from mapped frames.
removeMappedFrame :: ChannelDisplay -> Frame -> STM (AM Bool)
removeMappedFrame display frame = do
  frames <- readTVar $ chdiFrames display
  if frame `elem` map cdfmFrame frames
    then do writeTVar (chdiFrames display) (filter (\mapping -> cdfmFrame mapping /= frame) frames)
            F.setNick frame Nothing
            F.setName frame Nothing
            return $ do
              FM.setTitle frame Nothing Nothing
              return True
    else return $ return True

-- | Handle connection event.
handleChannelEvent :: ChannelDisplay -> ChannelDisplayFrameMapping -> STM (AM Bool)
handleChannelEvent display mapping = do
  event <- C.recv $ cdfmSubscription mapping
  let frame = cdfmFrame mapping
      channel = cdfmChannel mapping
  case event of
   ChanDisconnected (Left error) ->
     return $ do
       FM.disconnectErrorMessage frame error
       return True
   ChanDisconnected (Right ()) ->
     return $ do
       FM.disconnectMessage frame
       return True
   ChanJoined ->
     return $ do
       FM.joinedMessage frame $ C.getName channel
       return True
   ChanParted (Just comment) ->
     return $ do
       FM.partedCommentMessage frame (C.getName channel) comment
       return True
   ChanParted Nothing ->
     return $ do
       FM.partedMessage frame channel
       return True
   ChanNoTopic ->
     return $ do
       FM.noTopicMessage frame (C.getName channel)
       return True
   ChanTopic topic ->
     return $ do
       FM.topicMessage frame (C.getName channel) topic
       return True
   ChanTopicWhoTime fullName time ->
     return $ do
       FM.topicWhoTimeMessage frame (C.getName channel) fullName time
       return True
   ChanNames names ->
     return $ do
       FM.namesMessage frame names
        return True
   ChanRecvJoin nick fullName ->
     return $ do
       FM.recvJoinMessage frame (C.getname channel) nick fullName
       return True
   ChanRecvPart nick fullName (Just comment) ->
     return $ do
       FM.recvPartCommentMessage frame (C.getName channel) nick fullName comment
       FM.namesMessage frame (C.getNames channel)
       return True
   ChanRecvPart nick fullName Nothing ->
     return $ do
       FM.recvPartMessage frame (C.getName channel) nick fullName
       FM.namesMessage frame (C.getNames channel)
       return True
   ChanRecvMessage nick comment ->
     return $ do
       FM.recvMessageMessage frame nick comment FrmtChannel
       return True
   ChanRecvNotice nick comment ->
     return $ do
       FM.recvNoticeMessage frame nick comment FrmtChannel FrtaSpecific
       return True
   ChanRecvNick oldNick newNick ->
     return $ do
       FM.recvNickMessage frame oldNick newNick
       return True
   ChanRecvTopic nick topic ->
     return $ do
       FM.recvTopicMessage frame nick topic
       return True
   ChanRecvQuit nick fullName (Just comment) ->
     return $ do
       FM.recvQuitCommentMessage frame nick fullName comment
       FM.namesMessage frame (C.getNames channel)
       return True
   ChanRecvQuit nick fullName Nothing ->
     return $ do
       FM.recvQuitMessage frame nick fullName
       FM.namesMessage frame (C.getNames channel)
       return True
   ChanRecvCtcpRequest nick comment ->
     case parseCtcp comment of
      Just (command, Just comment)
        | command == ctcp_ACTION ->
            return $ do
              FM.recvActionMessage frame nick comment FrmtChannel
              return True
      _ -> return $ return True
   ChanRecvSelfNick oldNick newNick ->
     return $ do
       FM.setNick frame newNick
       FM.recvSelfNickMessage frame oldNick newNick
       return True
   ChanSelfMessage nick comment ->
     return $ do
       FM.selfMessageMessage frame nick comment
       return True
   ChanSelfNotice nick comment ->
     return $ do
       FM.selfNoticeMessage frame nick comment
       return True
   ChanSelfCtcpRequest nick comment ->
     case parseCtcp comment of
      Just (command, Just comment)
        | command == ctcp_ACTION ->
            return $ do
              FM.selfActionMessage frame nick comment FrmtChannel
              return True
      _ -> return $ return True
   _ -> return $ return True
