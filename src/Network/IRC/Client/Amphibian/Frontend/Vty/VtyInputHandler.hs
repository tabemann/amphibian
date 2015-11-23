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

module Network.IRC.Client.Amphibian.Frontend.Vty.VtyInputHandler

       (installHandlers)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import qualified Network.IRC.Client.Amphibian.InputDispatcher as ID
import qualified Network.IRC.Client.Amphibian.FrameMessage as FM
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend as VF
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow as VW
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Data.Char (isSpace)

-- | Install handlers.
installHandlers :: VtyFrontend -> STM ()
installHandlers vtyFrontend = do
  dispatcher <- I.getInputDispatcher $ VF.getInterface vtyFrontend
  case dispatcher of
    Just dispatcher -> do
      ID.registerCommandHandler dispatcher "window" (windowHandler vtyFrontend)
    Nothing -> return ()

-- Install window command handler.
windowHandler :: VtyFrontend -> Frame -> Text -> StyledText -> AM Bool
windowHandler vtyFrontend frame command argument = do
  case ST.break isSpace $ T.strip argument of
   (innerCommand, innerArgument)
     | innerCommand == "prev" -> do
         liftIO . atomically $ VF.prevWindow vtyFrontend
         liftIO $ VF.redraw vtyFrontend
     | innerCommand == "next" -> do
         liftIO . atomically $ VF.nextWindow vtyFrontend
         liftIO $ VF.redraw vtyFrontend
     | innerCommand == "close" -> do
         liftIO . atomically $ do
           currentWindow <- VF.getCurrentWindow vtyFrontend
           VW.close currentWindow
         liftIO $ VF.redraw vtyFrontend
     | _ -> FM.unknownCommandMessage frame $ T.concat [command, " ", innerCommand]
  
