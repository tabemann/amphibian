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

module Network.IRC.Client.Amphibian.Bootstrap

  (bootstrap)

  where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Default
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.PluginServer as PS
import qualified Network.IRC.Client.Amphibian.Frontend as F
import qualified Network.IRC.Client.Amphibian.InputDispatcher as ID
import qualified Network.IRC.Client.Amphibian.InputHandler as IH
import qualified Network.IRC.Client.Amphibian.CtcpDispatcher as CD
import qualified Network.IRC.Client.Amphibian.CtcpHandler as CH
import qualified Network.IRC.Client.Amphibian.Plugin as P
import qualified Network.IRC.Client.Amphibian.ConnectionDisplay as CoD
import qualified Network.IRC.Client.Amphibian.ChannelDisplay as ChD
import qualified Network.IRC.Client.Amphibian.UserDisplay as UD
import Control.Monad.IO.CLass (liftIO)
import Control.Concurrent.STM (STM,
                               atomically,
                               retry)
import Data.Functor ((<$>))
import Data.Text as T
import System.IO (FilePath)
import System.FilePath ((</>))
import System.Directory (doesFileExist)

-- | Bootstrap Amphibian.
bootstrap :: IO ()
bootstrap = do
  config <- getDefaultConfig
  intf <- atomically $ I.new config
  intfSubscription <- I.subscribe intf
  runAM continueBootstrap intf
  waitInterfaceExit intfSubscription

-- | Continue bootstrapping Amphibian.
continueBootstrap :: AM ()
continueBootstrap = do
  intf <- getInterface
  pluginServer <- liftIO . atomically $ PS.new
  PS.start pluginServer
  liftIO . atomically $ do
    frontend <- F.new
    F.start frontend
  inputDispatcher <- liftIO . atomically $ ID.new intf
  ID.start inputDispatcer
  ctcpDispatcher <- liftIO . atomically $ CD.new intf
  CD.start ctcpDispatcher
  connectionDisplay <- liftIO . atomically $ CoD.new intf
  CoD.start connectionDisplay
  channelDisplay <- liftIO . atomically $ ChD.new intf
  ChD.start channelDisplay
  userDisplay <- liftIO . atomically $ UD.new intf
  UD.start userDisplay
  liftIO . atomically $ do
    IH.installHandlers intf
    CH.installHandlers intf
  runConfigPlugin
  

-- | Run configuration plugin.
runConfigPlugin :: AM ()
runConfigPlugin = do
  config <- getConfig
  let configPluginPath = confConfigDir config </> confConfigPluginName config
  configPluginExists <- doesFileExist configPluginPath
  if configPluginExists
  then do
    intf <- getInterface
    (subscription, startResponse) <- liftIO . atomically $ do
      plugin <- P.new intf configPluginPath Nothing False
      subscription <- P.subscribeOut plugin
      startResponse <- P.start plugin
      return (subscription, startResponse)
    startResponse' <- liftIO . atomically $ P.waitStart startResponse
    updateConfig (\conf -> conf { confPluginError = startResponse' })
    case startResponse' of
      Right () -> liftIO . atomically $ waitExit subscription
      Left error -> return ()
  else return ()

-- | Wait until configuration plugin exits
waitExit :: PluginOutputSubscription -> STM ()
waitExit subscription = do
  event <- P.recvOutput subscription
  case event of
    PoevExit -> return ()
    _ -> retry
  
-- | Wait until interface indicates exit
waitInterfaceExit :: InterfaceSubscription -> IO ()
waitInterfaceExit subscription = do
  continue <- atomically $ do
    event <- I.recv subscription
    case event of
     IntfExit -> return False
     _ -> return True
  if continue
    then waitInterfaceExit subscription
    else return ()
   
