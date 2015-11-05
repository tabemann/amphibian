module Network.IRC.Client.Amphibian.Plugin

       (Plugin,
        PluginSubscription,
        new,
        start,
        stop,
        exit,
        getPath,
        getEntryPoint,
        getPrecompiled,
        subscribe,
        recv,
        peek)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.Interface as I
import Data.Functor ((<$>))
import System.Eval.Haskell (eval_)
import System.Plugins.Make (make,
                            MakeStatus(..))
import System.FilePath (takeDirectory,
                        takeBaseName)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               atomically,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newBroadcastTChan,
                               dupTChan,
                               readTChan,
                               peekTChan,
                               writeTChan)
import qualified Data.Text as T


-- This should be compiled with -optl=-Wl,--export-dynamic

-- | Create a plugin.
new :: Interface -> T.Text -> Maybe T.Text -> Bool -> STM Plugin
new interface path entryPoint precompiled = do
  actions <- newBroadcastChan,
  running <- newTVar False
  return $ Plugin { plugPath = path,
                    plugEntryPoint = entryPoint,
                    plugPrecompiled = precompiled,
                    plugInterface = interface,
                    plugActions = actions,
                    plugRunning = running }

-- | Start a plugin.
start :: Plugin -> AM (Either Error ())
start plugin = do
  join . liftIO . atomically $ do
    running <- readTVar $ plugRunning plugin
    if not running
    then do
      writeTVar (plugRunning plugin) True
      return $ do
        compileOutput <- if not plugPrecompiled plugin
                         then compile plugin
                         else return $ Right ()
        case compileOutput of
          Right () -> do
            loadOutput <- load plugin
            case loadOutput of
              Right () -> do
                registerPlugin plugin
                return $ Right ()
              Left error -> do
                liftIO . atomically $ writeTVar (plugRunning plugin) False
                return $ Left error
          Left error -> do
            liftIO . atomically $ writeTVar (plugRunning plugin) False
            return $ Left error
    else return $ return (Right ())

-- | Stop a plugin.
stop :: Plugin -> STM ()
stop plugin = writeTChan (plugActions plugin) PlugStop

-- | Exit a plugin.
exit :: Plugin -> STM ()
exit plugin = do
  I.unregisterPlugin (pluginInterface plugin) plugin
  writeTVar (plugRunning plugin) False

-- | Get plugin path.
getPath :: Plugin -> T.Text
getPath = plugPath

-- | Get plugin entry point.
getEntryPoint :: Plugin -> Maybe T.Text
getEntryPoint = plugEntryPoint

-- | Get whether plugin is precompiled.
getPrecompiled :: Plugin -> Bool
getPrecompiled = plugPrecompiled

-- | Subscribe to a plugin action.
subscribe :: Plugin -> STM PluginSubscription
subscribe plugin = PluginSubscription <$> dupTChan $ plugActions plugin

-- | Receive a plugin action.
recv :: PluginSubscription -> STM PluginAction
recv (PluginSubscription subscription) = readTChan subscription

-- | Peek a plugin action.
peek :: PluginSubscription -> STM PluginAction
peek (PluginSubscription subscription) = peekTChan subscription

-- | Compile a plugin.
compile :: Plugin -> AM (Either Error ())
compile path = do
  let path' = T.unpack $ plugPath plugin
  compileOptions <- map T.unpack <$> confScriptCompileOptions <$> liftIO getConfig
  status <- liftIO $ make path' compileOptions
  case status of
    MakeSuccess _ _ -> return (Right ())
    MakeFailure errors -> return . Left . Error $ map T.pack errors  

-- | Load a plugin.
load :: Plugin -> AM (Either Error ())
load plugin = do
  let path' = T.unpack $ plugPath plugin
  conf <- liftIO getConfig
  let entryPoint' =
    case plugEntryPoint plugin of
      Just entryPoint -> T.unpack entryPoint
      Nothing -> T.unpack $ confDefaultScriptEntry conf
  let base = takeBaseName path'
      dir = takeDirectory path' in
  do intf <- interface
     result <- liftIO $ eval_ (base ++ "." ++ entryPoint' ++ " :: Plugin -> AM ()") ["Prelude", base]
               (map T.unpack $ confScriptEvalOptions conf) [] [dir] :: AM (Either [String] (Maybe (Plugin -> AM ())))
     case result of
       Right (Just action) -> do
         plugin action
         return (Right ())
       Right Nothing -> do
         errorText <- lookupText $ T.pack "Unable to check the type of the plugin"
         return . Left $ Error [errorText]
       Left errors -> return . Left . Error $ map T.pack errors
