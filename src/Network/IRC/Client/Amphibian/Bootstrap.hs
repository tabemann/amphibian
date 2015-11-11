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
  runAM continueBootstrap intf

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
  