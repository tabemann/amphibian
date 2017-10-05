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

module Network.IRC.Client.Amphibian.Frontend.Vty.VtyKeys

       (installHandlers)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend as VF
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow as VW
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Control.Concurrent.STM (STM,
                               atomically)
import qualified Graphics.Vty.Input as VI
import Data.Char (toUpper,
                  isControl)

-- | Install key handlers for the Vty frontend.
installHandlers :: VtyFrontend -> STM ()
installHandlers vtyFrontend = do
  VF.registerUnmappedKeyHandler vtyFrontend handleUnmapped
  VF.registerKeyHandler vtyFrontend VI.KEnter [] handleEnter
  VF.registerKeyHandler vtyFrontend VI.KLeft [] handleLeft
  VF.registerKeyHandler vtyFrontend VI.KRight [] handleRight
  VF.registerKeyHandler vtyFrontend VI.KUp [] handleUp
  VF.registerKeyHandler vtyFrontend VI.KDown [] handleDown
  VF.registerKeyHandler vtyFrontend VI.KPageUp [] handlePageUp
  VF.registerKeyHandler vtyFrontend VI.KPageDown [] handlePageDown
  VF.registerKeyHandler vtyFrontend (VI.KChar 'p') [VI.MCtrl] handlePrevWindow
  VF.registerKeyHandler vtyFrontend (VI.KChar 'n') [VI.MCtrl] handleNextWindow

-- | Handle unmapped key.
handleUnmapped :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleUnmapped vtyFrontend (VI.KChar char) []
  | not $ isControl char =
      join . liftIO . atomically $ do
        currentWindow <- VF.getCurrentWindow vtyFrontend
        case currentWindow of
         Just currentWindow -> do
           VW.enterChar currentWindow char
           return $ VF.redraw vtyFrontend
         Nothing -> return $ return ()
handleUnmapped vtyFrontend (VI.KChar char) [VI.MShift]
  | not $ isControl char =
      join . liftIO . atomically $ do
        currentWindow <- VF.getCurrentWindow vtyFrontend
        case currentWindow of
         Just currentWindow -> do
           VW.enterChar currentWindow $ toUpper char
           return $ VF.redraw vtyFrontend
         Nothing -> return $ return ()
handleUnmapped _ _ _ = return ()

-- | Handle enter key.
handleLeft :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleLeft vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.enterLine currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle left key.
handleLeft :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleLeft vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.moveLeft currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle right key.
handleRight :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleRight vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.moveRight currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle up key.
handleUp :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleUp vtyFrontend _ - = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.prevInput currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle down key.
handleDown :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleDown vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.nextInput currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle page up key.
handlePageUp :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handlePageUp vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.scrollPrev currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle page down key.
handlePageDown :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handlePageDown vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.scrollNext currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle previous window.
handlePrevWindow :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handlePrevWindow vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    VF.prevWindow vtyFrontend
    return $ VF.redraw vtyFrontend
  return True

-- | Handle next window.
handleNextWindow :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleNextWindow vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    VF.nextWindow vtyFrontend
    return $ VF.redraw vtyFrontend
  return True
