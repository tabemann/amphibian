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

module Network.IRC.Client.Amphibian.ConnectionDisplay

       (ConnectionDisplay,
        ConnectionDisplayStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
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
import Data.Functor ((<$>))
import Data.List (find)

-- | Create a connection display.
new :: Interface -> STM ConnectionDisplay
new intf = do
  running <- newTVar False
  actions <- newTQUeue
  interfaceSubscription <- I.subscribe intf
  allFrames <- newTVar []
  frames <- newTVar []
  return $ ConnectionDisplay { codiRunning = running,
                               codiActions = actions,
                               codiInterface = interface,
                               codiInterfaceSubscription = interfaceSubscription,
                               codiAllFrames = allFrames,
                               codiFrames = frames }

-- | Start connection display.
start :: ConnectionDisplay -> AM ()
start display = do
  join . liftIO . atomically $ do
    running <- readTVar $ codiRunning display
    if not running
    then do
      writeTVar (codiRunning display) True
      return $ do
        intf <- getInterface
        async (runAM (runDisplay display) intf)
    else return $ return ()

-- | Stop connection display.
stop :: ConnectionDisplay -> STM ConnectionDisplayStopResponse
stop display = do
  response <- ConnectionDisplayStopResponse <$> newEmptyTMVar
  writeTQueue (codaActions display) (CodaStop response)
  return response

-- | Wait for response to stopping connection display.
waitStop :: ConnectionDisplayStopResponse -> STM (Either Error ())
waitStop (ConnectionDisplayStopResponse response) = readTMVar response

-- | Run connection display.
runConnectionDisplay :: ConnectionDisplay -> AM ()
runConnectionDisplay display = do
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ codiFrames display
    allFrames <- readTVar $ codiALlFrames display
    handleAction display `orElse` handleInterfaceEvent display `orElse`
      (foldr (\frame y -> handleFrameEvent display frame `orElse` y) retry allFrames) `orElse`
      (foldr (\mapping y -> handleConnectionEvent display mapping `orElse` y) retry frames) 
  if continue
  then runConnectionDisplay display
  else liftIO . atomically $ writeTVar (codiRunning display) False

-- | Handle connection display action.
handleAction :: ConnectionDisplay -> STM (AM Bool)
handleAction display = do
  action <- readTQueue $ codiActions display
  case action of
    CodaStop (ConnectionDisplayStopResponse response) -> do
      writeTVar (codiRunning display) False
      writeTMVar response (Right ())
      return $ return False

-- | Handle interface event.
handleInterfaceEvent :: ConnectionDisplay -> STM (AM Bool)
handleInterfaceEvent display = do
  event <- I.recv $ codiInterfaceSubscription display
  case event of
   IntfFrameRegistered frame -> do
     allFrames <- readTVar $ codiAllFrames display
     if frame `notElem` map cdfrFrame allFrames
       then do frameSubscription <- F.subscribeInput frame
               let frame' = ChannelDisplayFrame { cdfrFrame = frame,
                                                  cdrSubscription = frameSubscription }
               writeTVar (codiAllFrames display) (frame' : allFrames)
       else return ()
     mapping <- F.getMapping frame
     case mapping of
      FrmaConnectionManager manager -> addMappedFrame display frame manager
      _ -> return $ return True
   IntfFrameUnregistered frame -> do
     allFrames <- readTVar $ codiAllFrames
     if frame `elem` map cdfrFrame allFrames
       then writeTVar (codiAllFrames display) (filter (\frame' -> cdfrFrame frame' /= frame) allFrames)
       else return ()
     removeMappedFrame display frame
   _ -> return $ return True

-- | Handle frame event.
handleFrameEvent :: ConnectionDisplay -> ConnectionDisplayFrame -> STM (AM Bool)
handleFrameEvent display frame = do
  event <- F.recvInput $ cdfrSubscription frame
  case event of
    FievMapping mapping -> do
      case mapping of
        FrmaConnectionManager manager -> addMappedFrame display (cdfrFrame frame) manager
        _ -> removeMappedFrame display $ cdfrFrame frame
    _ -> return $ return True

-- | Add frame to mapped frames.
addMappedFrame :: ConnectionDisplay -> Frame -> ConnectionManager -> STM (AM Bool)
addMappedFrame display frame manager = do
  frames <- readTVar $ codiFrames display
  case find (\mapping -> codfFrame mapping == frame) frames of
   Nothing -> addMappedFrame' display frame manager frames
   Just mapping
     | codfConnectionManager mapping /= manager ->
       let frames' = filter (\mapping -> codfFrame mapping /= frame) frames in
       addMappedFrame' display frame manager frames'
     | otherwise = return $ return True
  where addMappedFrame' display frame maanger frames = do
          subscription <- CM.subscribe manager
          let mapping = ConnectionDisplayFrameMapping { codfFrame = frame,
                                                        codfConnectionManager = manager,
                                                        codfSubscription = subscription }
          writeTVar (codiFrames display) (mapping : frames)
          nick <- CM.getNick manager
          setup <- CM.getSetup manager
          F.setName frame . Just $ comaServerName setup
          return $ do
            FM.setNick frame nick
            FM.setTitle frame (Just $ comaServerName setup) Nothing
            return True

-- | Remove frame from mapped frames.
removeMappedFrame :: ConnectionDisplay -> Frame -> STM (AM Bool)
removeMappedFrame display frame = do
  frames <- readTVar $ codiFrames display
  if frame `elem` map codfFrame frames
    then do writeTVar (codiFrames display) (filter (\mapping -> codfFrame mapping /= frame) frames)
            F.setNick frame Nothing
            F.setName frame Nothing
            return $ do
              FM.setTitle frame Nothing Nothing
              return True
    else return $ return True

-- | Handle connection event.
handleConnectionEvent :: ConnectionDisplay -> ConnectionDisplayFrameMapping -> STM (AM Bool)
handleConnectionEvent display mapping = do
  event <- CM.recv $ codfSubscription mapping
  let frame = codfFrame mapping
      manager = codfConnectionManager mapping
  case event of
   ComaLookupAddress hostName ->
     return $ do
       FM.lookupAddressMessage frame hostName
       return True
   ComaLookupAddressFailed error ->
     return $ do
       FM.lookupAddressFailedMessage frame error
       return True
   ComaReverseLookupFailed error ->
     return $ do
       FM.reverseLookupFailedMessage frame error
       return True
   ComaConnecting hostName port ->
     return $ do
       FM.connectingMessage frame hostName port
       return True
   ComaConnected hostName port ->
     return $ do
       FM.connectedMessage frame hostName port
       return True
   ComaConnectFailed error ->
     return $ do
       FM.connectFailedMessage error
       return True
   ComaDisconnected (Left error) ->
     return $ do
       FM.disconnectErrorMessage frame error
       return True
   ComaDisconnected (Right ()) ->
     return $ do
       FM.disconnectMessage frame
       return True
   ComaPasswordMismatch password ->
     return $ do
       FM.passwordMismatchMessage frame password
       return True
   ComaBannedFromServer (Just comment) ->
     return $ do
       FM.bannedFromServerCommentMessage frame comment
       return True
   ComaBannedFromServer Nothing ->
     return $ do
       FM.bannedFromServerMessage frame
       returnTrue
   ComaWelcome (Just comment) ->
     return $ do
       FM.welcomeCommentMessage frame comment
       return True
   ComaWelcome Nothing -> do
     nick <- CM.getNick manager
     return $ do
       FM.welcomeMessage frame nick
       return True
   ComaAttemptingNick nick ->
     return $ do
       FM.attemptingNickMessage frame nick
       return True
   ComaMalformedNick nick ->
     return $ do
       FM.malformedNickMessage frame nick
       return True
   ComaRecvNotice nick comment ->
     return $ do
       FM.recvNoticeMessage frame nick comment True FrmtPrivate FrtaLastFocused
       return True
   ComaRecvSelfNick oldNick newNick ->
     return $ do
       FM.setNick frame newNick
       FM.recvSelfNickMessage frame oldNick newNick
       return True
   ComaMotd lines ->
     return $ do
       FM.motdMessage frame lines
       return True
   _ -> return $ return True    
