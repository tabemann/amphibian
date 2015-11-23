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

module Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindowServer

       (VtyWindowServer,
        VtyWindowServerStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend as VF
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar,
                               atomically,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newEmptyTMVar,
                               readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.Async (Async,
                                 async)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Sequence ((<|))

-- | Create a new window server.
new :: VtyFrontend -> STM VtyWindowServer
new vtyFrontend = do
  interfaceSubscription <- I.subscribe $ vtfrInterface vtyFrontend
  running <- newTVar Bool
  actions <- newTQueue
  return $ VtyWindowServer { vtwsFrontend = frontend,
                             vtwsInterfaceSubscription = interfaceSubscription,
                             vtwsRunning = running,
                             vtwsActions = actions }

-- | Start a window server
start :: VtyWindowServer -> AM ()
start server = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ vtwsRunning server
    if not running
    then do
      writeTVar (vtwsRunning server) True
      I.registerVtyWindowServer intf server
      return . async $ runAM (runServer server)
    else return $ return ()

-- | Stop window server.
stop :: VtyWindowServer -> STM VtyWindowServerStopResponse
stop server = do
  response <- VtyWindowServerStopResponse <$> newEmptyTMVar
  writeTVar (vtwsActions server) (VtwsStop response)
  return response

-- | Wait for window server to stop.
waitStop :: VtyWindowServerStopResponse -> STM (Either Error ())
waitStop (VtyWindowServerStopResponse response) = readTMVar response

-- | Run window server.
runServer :: VtyWindowServer -> AM ()
runServer server = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ handleAction server `orElse` handleInterface server
  if continue
  then runServer server
  else return ()

-- | Handle window server action.
handleAction :: VtyWindowServer -> STM (AM Bool)
handleAction server = do
  action <- readTQueue $ vtwsActions server
  case action of
   VtwsStop -> do
     VF.unregisterWindowServer (vtwsFrontend server) server
     return $ return False

-- | Handle interface event.
handleInterface :: VtyWIndowServer -> STM (AM Bool)
handleInterface server = do
  event <- I.recv $ vtwsInterfaceSubscription server
  intf <- vtfrInterface $ vtwsFrontend server
  case event of
   IntfFrameRegistered frame -> do
     window <- newWindow (vtwsFrontend server) frame
     VF.registerWindow (vtwsFrontend server) window
     return $ do
       async $ runAM (runWindow user) intf
       return True
   _ -> return $ return True
     
-- | Create a new window.
newWindow :: VtyFrontend -> Frame -> STM VtyWindow
newWindow vtyFrontend frame = do
  subscription <- F.subscribeOutput frame
  active <- newTVar True
  bufferLines <- newTVar S.empty
  bufferPosition <- newTVar VtbpDynamic
  prevInputBufffer <- newTVar S.empty
  prevInputPosition <- newTVar (-1)
  inputText <- newTVar ST.empty
  inputCursorPosition <- newTVar 0
  inputVisiblePosition <- newTVar 0
  return $ VtyWindow { vtwiFrontend = vtyFrontend,
                       vtwiFrame = frame,
                       vtwiSubscription = subscription,
                       vtwiActive = active,
                       vtwiBufferLines = bufferLines,
                       vtwiBufferPosition = bufferPosition,
                       vtwiPrevInputBuffer = prevInputBuffer,
                       vtwiPrevInputPosition = prevInputPosition,
                       vtwiInputText = inputText,
                       vtwiInputCursorPosition = inputCursorPosition,
                       vtwiInputVisiblePosition = inputVisiblePosition }

-- | Run window.
runWindow :: VtyWindow -> AM ()
runWindow vtyWindow = do
  continue <- join . liftIO . atomically $ do
    event <- F.recvOutput $ vtwiSubscription vtyWindow
    case event of
     FoevTopic _ -> return $ do
       VF.redraw $ vtwiFrontend vtyWindow
       return True
     FoevUsers _ -> return $ do
       VF.redraw $ vtwiFrontend vtyWindow
       return True
     FoevNick _ -> return $ do
       VF.redraw $ vtwiFrontend vtyWindow
       return True
     FoevTitle _ -> return $ do
       VF.redraw $ vtwiFrontend vtyWindow
     FoevLine line -> do
       bufferLines <- readTVar $ vtwiBufferLines vtyWindow
       let bufferLines' = line <| bufferLines
       writeTVar (vtwiBufferLines vtyWindow) bufferLines'
       bufferPosition <- readTVar $ vtwiBufferPosition vtyWindow
       let bufferPosition' = case bufferPosition of
             VtbpFixed position -> VtbpFixed $ position + 1
             VtbpDynamic -> VtbpDynamic
       writeTVar (vtwiBufferPosition vtyWindow) bufferPosition'
       return $ do
         VF.redraw $ vtwiFrontend vtyWindow
         return True
     FoevNotifications _ -> return $ do
       VF.redraw $ vtwiFrontend vtyWindow
       return True
     FoevMapping _ -> return $ do
       VF.redraw $ vtwiFrontend vtyWindow
       return True
     FoevClose -> do
       count <- length <$> VF.getWindows $ vtwiFrontend vtyWindow
       if count > 1
         then do F.closed $ vtwiFrame vtyWindow
                 VF.unregisterWindow (vtwiFrontend vtyWindow) vtyWindow
                 return $ do
                   VF.redraw $ vtwiFrontend vtyWindow
                   return True
         else return $ return True
     _ -> return $ return True
  if continue
    then runWindow vtyWindow
    else return ()
