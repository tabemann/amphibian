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

module Network.IRC.Client.Amphibian.Monad

       (AM,
        Interface,
        runAM,
        getInterface,
        getConfig,
        setConfig,
        updateConfig,
        getConnectionConfig,
        setConnectionConfig,
        updateConnectionConfig,
        lookupText,
        getConnectionManagers,
        registerConnectionManager,
        unregisterConnectionManager,
        getChannels,
        registerChannel,
        unregisterChannel,
        getUsers,
        registerUser,
        unregisterUser,
        getPlugins,
        registerPlugin,
        unregisterPlugin,
        getFrontend,
        registerFrontend,
        unregisterFrontend,
        getInputDispatcher,
        registerInputDispatcher,
        unregisterInputDispatcher,
        getCtcpDispatcher,
        registerCtcpDispatcher,
        unregisterCtcpDispatcher,
        getPluginServer,
        registerPluginServer,
        unregisterPluginServer,
        getConnectionManagerServer,
        registerConnectionManagerServer,
        unregisterConnectionManagerServer,
        getChannelServer,
        registerChannelServer,
        unregisterChannelServer)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import Control.Monad.State.Strict (runAM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Data.HashMap.Strict (HashMap,
                            lookup)
import qualified Data.Text as T

-- | Run Amphibian monad.
runAM :: AM a -> Interface -> IO a
runAM monad interface = runReaderT monad interface

-- | Get interface.
ɡetInterface :: AM Interface
getInterface = AM ask

-- | Get configuration.
getConfig :: AM Config
getConfig = do
  intf <- getInterface
  liftIO . atomically $ I.getConfig intf

-- | Set configuration.
setConfig :: Config -> AM ()
setConfig config = do
  intf <- getInterface
  liftIO . atomically $ I.setConfig intf config

-- | Update configuration
updateConfig :: (Config -> Config) -> AM ()
updateConfig update = do
  intf <- getInterface
  liftIO . atomically $ do
    config <- I.getConfig intf
    I.setConfig intf (update config)

-- | Get connection configuration.
getConnectionConfig :: ConnectionManager -> AM (Maybe ConnectionConfig)
getConnectionConfig manager = do
  intf <- getInterface
  liftIO . atomically $ I.getConnectionConfig intf manager

-- | Set connection configuration
setConnectionConfig :: ConnectionManager -> ConnectionConfig -> AM ()
setConnectionConfig manager config = do
  intf <- getInterface
  liftIO . atomically $ I.setConnectionConfig intf config

-- | Update connection configuration
updateConnectionConfig :: (ConnectionConfig -> ConnectionConfig) -> ConnectionManager -> AM ()
updateConnectionConfig update manager = do
  intf <- getInterface
  liftIO . atomically $ do
    config <- I.getConnectionConfig intf manager
    I.setConnectionConfig intf manager (update config)

-- | Look up text, and return the provided key text if no text is found.
lookupText :: T.Text -> AM T.Text
lookupText key = do
  intf <- getInterface
  liftIO . atomically $ I.lookupText intf key

-- | Get connection managers.
getConnectionManagers :: AM [ConnectionManager]
getConnectionManagers = do
  intf <- getInterface
  liftIO . atomically $ I.getConnectionManagers intf

-- | Register connection manager.
registerConnectionManager :: ConnectionManager -> AM ()
registerConnectionManager manager = do
  intf <- getInterface
  liftIO . atomically $ I.registerConnectionManager intf manager

-- | Unregister connection manager.
unregisterConnectionManager :: ConnectionManager -> AM ()
unregisterConnectionManager manager = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterConnectionManager intf manager

-- | Get channels.
getChannels :: AM [Channel]
getChannels = do
  intf <- getInterface
  liftIO . atomically $ I.getChannels intf

-- | Register channel.
registerChannel :: Channel -> AM ()
registerChannel channel = do
  intf <- getInterface
  liftIO . atomically $ I.registerChannel intf channel

-- | Unregister channel.
unregisterChannel :: Channel -> AM ()
unregisterChannel channel = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterChannel intf channel

-- | Get users.
getUsers :: AM [User]
getUsers = do
  intf <- getInterface
  liftIO . atomically $ I.getUsers intf

-- | Register user.
registerUser :: User -> AM ()
registerUser user = do
  intf <- getInterface
  liftIO . atomically $ I.registerUser intf user

-- | Unregister user.
unregisterUser :: User -> AM ()
unregisterUser user = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterUser intf user

-- | Get frames.
getFrames :: AM [Frame]
getFrames = do
  intf <- getInterface
  liftIO . atomically $ I.getFrames intf

-- | Register frame.
registerFrame :: Frame -> AM ()
registerFrame frame = do
  intf <- getInterface
  liftIO . atomically $ I.registerFrame intf frame

-- | Unregister frame.
unregisterFrame :: Frame -> AM ()
unregisterFrame frame = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterFrame intf frame

-- | Get plugins.
getPlugins :: AM [Plugin]
getPlugins = do
  intf <- getInterface
  liftIO . atomically $ I.getPlugins intf

-- | Register plugin.
registerPlugin :: Plugin -> AM ()
registerPlugin plugin = do
  intf <- getInterface
  liftIO . atomically $ I.registerPlugin intf plugin

-- | Unregister plugin.
unregisterPlugin :: Plugin -> AM ()
unregisterPlugin plugin = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterPlugin intf plugin

-- | Get frontend.
getFrontend :: AM (Maybe Frontend)
getFrontend = do
  intf <- getInterface
  liftIO . atomically $ I.getFrontend intf

-- | Register frontend.
registerFrontend :: Frontend -> AM ()
registerFrontend frontend = do
  intf <- getInterface
  liftIO . atomically $ I.registerFrontend intf frontend

-- | Unregister frontend.
unregisterFrontend :: Frontend -> AM ()
unregisterFrontend frontend = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterFrontend intf frontend

-- | Get input dispatcher.
getInputDispatcher :: AM (Maybe InputDispatcher)
getInputDispatcher = do
  intf <- getInterface
  liftIO . atomically $ I.getInputDispatcher intf

-- | Register input dispatcher.
registerInputDispatcher :: InputDispatcher -> AM ()
registerInputDispatcher dispatcher = do
  intf <- getInterface
  liftIO . atomically $ I.registerInputDispatcher intf dispatcher

-- | Unregister input dispatcher.
unregisterInputDispatcher :: InputDispatcher -> AM ()
unregisterInputDispatcher dispatcher = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterInputDispatcher intf dispatcher

-- | Get CTCP dispatcher.
getCtcpDispatcher :: AM (Maybe CtcpDispatcher)
getCtcpDispatcher = do
  intf <- getInterface
  liftIO . atomically $ I.getCtcpDispatcher intf

-- | Register CTCP dispatcher.
registerCtcpDispatcher :: CtcpDispatcher -> AM ()
registerCtcpDispatcher dispatcher = do
  intf <- getInterface
  liftIO . atomically $ I.registerCtcpDispatcher intf dispatcher

-- | Unregister CTCP dispatcher.
unregisterCtcpDispatcher :: CtcpDispatcher -> AM ()
unregisterCtcpDispatcher dispatcher = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterCtcpDispatcher intf dispatcher

-- | Get plugin server.
getPluginServer :: AM (Maybe PluginServer)
getPluginServer = do
  intf <- getInterface
  liftIO . atomically $ I.getPluginServer intf

-- | Register plugin server.
registerPluginServer :: PluginServer -> AM ()
registerPluginServer server = do
  intf <- getInterface
  liftIO . atomically $ I.registerPluginServer intf server

-- | Unregister plugin server.
unregisterPluginServer :: PluginServer -> AM ()
unregisterPluginServer server = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterPluginServer intf server

-- | Get connection manager server.
getConnectionManagerServer :: AM (Maybe ConnectionManagerServer)
getConnectionManagerServer = do
  intf <- getInterface
  liftIO . atomically $ I.getConnectionManagerServer intf

-- | Register connection manager server.
registerConnectionManagerServer :: ConnectionManagerServer -> AM ()
registerConnectionManagerServer server = do
  intf <- getInterface
  liftIO . atomically $ I.registerConnectionManagerServer intf server

-- | Unregister connection manager server.
unregisterConnectionManagerServer :: ConnectionManagerServer -> AM ()
unregisterConnectionManagerServer server = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterConnectionManagerServer intf server

-- | Get channel server.
getChannelServer :: AM (Maybe ChannelServer)
getChannelServer = do
  intf <- getInterface
  liftIO . atomically $ I.getChannelServer intf

-- | Register channel server.
registerChannelServer :: ChannelServer -> AM ()
registerChannelServer server = do
  intf <- getInterface
  liftIO . atomically $ I.registerChannelServer intf server

-- | Unregister channel server.
unregisterChannelServer :: ChannelServer -> AM ()
unregisterChannelServer server = do
  intf <- getInterface
  liftIO . atomically $ I.unregisterChannelServer intf server
