module Network.IRC.Client.Amphibian.Plugin

       (PluginServer,
        PluginServerStopResponse,
        new,
        start,
        stop)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.Interface as I
import Data.Functor ((<$>))
import Control.Monad (join,
                      foldM,
                      mapM_)
import System.Eval.Haskell (eval_)
import System.Plugins.Make (make,
                            MakeStatus(..))
import System.FilePath (takeDirectory,
                        takeBaseName)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar,
                               TChan,
                               atomically,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newEmptyTMVar,
                               putTMVar,
                               readTMVar,
                               newBroadcastTChan,
                               dupTChan,
                               readTChan,
                               peekTChan,
                               writeTChan)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.Async (Async,
                                 async)
import qualified Data.Text as T

-- | Create a new plugin server.
new :: STM PluginServer
new = do
  running <- newTVar Bool
  actions <- newTQueue
  return $ PluginServer { plseRunning = running,
                          plseActions = actions }

-- | Start a plugin server
start :: PluginServer -> AM ()
start server = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ plseRunning server
    if not running
    then do
      writeTVar (plseRunning server) True
      I.registerPluginServer intf server
      return . async $ runAM (runServer server)
    else return $ return ()

-- | Stop plugin server.
stop :: PluginServer -> STM PluginServerStopResponse
stop server = do
  response <- PluginServerStopResponse <$> newEmptyTMVar
  writeTVar (plseActions server) (PlsaStop response)
  return response

-- | Wait for plugin server to stop.
waitStop :: PluginServerStopResponse -> STM (Either Error ())
waitStop (PluginServerStopResponse response) = readTMVar response

-- | Run plugin server.
runServer :: PluginServer -> AM ()
runServer server = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    action <- readTQueue $ plseActions server
    case action of
      PlsaStartPlugin plugin (PluginStartResponse response) -> do
        active <- readTVar $ plugActive plugin
        if not active
        then do
          writeTVar (plugActive plugin) True
          I.registerPlugin intf plugin
          putTMVar response $ Right ()
          return $ do
            async $ runAM (runPlugin plugin) intf
            return True
        else do
          errorText <- I.lookupText intf $ T.pack "Plugin is already started"
          putTMVar response . Left $ Error [errorText]
          return $ return True
      PlsaStop -> do
        I.unregisterPluginServer intf server
        return $ return False
  if continue
  then runServer server
  else return ()

-- | Run a plugin.
runPlugin :: Plugin -> AM ()
runPlugin plugin = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    action <- readTQueue $ plugActions plugin
    case action of
      PlugRun response -> handleRun plugin intf response
      PlugStop response -> handleStop plugin intf response
  if continue
  then runPlugin plugin
  else return ()

-- | Handle running a plugin.
handleRun :: Plugin -> Interface -> PluginRunResponse -> STM (AM Bool)
handleRun plugin intf (PluginRunResponse response) = do
  running <- readTVar $ plugRunning plugin
  if not running
  then do
    writeTVar (plugRunning plugin) True
    writeTVar (plugQuitting plugin) False
    return $ do
      async $ runAM (tryExecute plugin response) intf
      return True
  else do
    errorText <- I.lookupText intf $ T.pack "Plugin is already running"
    putTMVar response $ Error [errorText]
    return $ return True
  where tryExecute plugin = do
          result <- execute plugin
          liftIO . atomically $ do
            case result of
              Left _ -> writeTVar (plugRunning plugin) False
              Right () -> writeTChan (plugInputEvents plugin) PoevStart
            putTMVar response result

-- | Handle stopping a plugin.
handleStop :: Plugin -> Interface -> PluginStopResponse -> STM (AM Bool)
handleStop plugin intf (PluginStopResponse response) = do
  running <- readTVar $ plugRunning plugin
  if running
  then do
    quitting <- readTVar $ plugQuitting plugin
    if not quitting
    then do
      writeTVar (plugQuitting plugin) True
      writeTChan (plugInputEvents plugin) PievQuit
    else return ()
  else return ()
  I.unregisterPlugin intf plugin
  writeTVar (plugActive plugin) False
  return $ return False

-- | Start a plugin.
execute :: Plugin -> AM (Either Error ())
execute plugin = do
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
              Right () -> return $ Right ()
              Left error -> do
                liftIO . atomically $ writeTVar (plugRunning plugin) False
                return $ Left error
          Left error -> do
            liftIO . atomically $ writeTVar (plugRunning plugin) False
            return $ Left error
    else return $ return (Right ())

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
         action plugin
         return (Right ())
       Right Nothing -> do
         errorText <- lookupText $ T.pack "Unable to check the type of the plugin"
         return . Left $ Error [errorText]
       Left errors -> return . Left . Error $ map T.pack errors
