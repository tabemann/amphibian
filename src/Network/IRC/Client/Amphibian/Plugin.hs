module Network.IRC.Client.Amphibian.Plugin

       (Plugin,
        PluginInputEvent,
        PluginOutputEvent,
        PluginRunResponse,
        PluginStopResponse,
        PluginInputSubscription,
        PluginOutputSubscription,
        new,
        start,
        waitStart,
        stop,
        waitStop,
        run,
        waitRun,
        quit,
        exit,
        getPath,
        getEntryPoint,
        getPrecompiled,
        getRunning,
        getActive,
        getQuitting,
        subscribeInput,
        recvInput,
        peekInput,
        subscribeOutput,
        recvOutput,
        peekOutput)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.Interface as I
import Data.Functor ((<$>))
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
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import qualified Data.Text as T


-- This should be compiled with -optl=-Wl,--export-dynamic

-- | Create a plugin.
new :: Interface -> FilePath -> Maybe T.Text -> Bool -> STM Plugin
new interface path entryPoint precompiled = do
  actions <- newTQueue
  inputEvents <- newBroadcastTChan
  outputEvents <- newBroadcastTChan
  running <- newTVar False
  active <- newTVar False
  quitting <- newTVar False
  return $ Plugin { plugPath = path,
                    plugEntryPoint = entryPoint,
                    plugPrecompiled = precompiled,
                    plugInterface = interface,
                    plugActions = actions,
                    plugInputEvents = inputEvents,
                    plugOutputEvents = outputEvents,
                    plugRunning = running,
                    plugActive = active,
                    plugQuitting = quitting }

-- | Start a plugin thread.
start :: Plugin -> STM PluginStartResponse
start plugin = do
  response <- newEmptyTMVar
  let response' = PluginStartResponse response
  pluginServer <- I.getPluginServer $ plugInterface plugin
  case pluginServer of
    Just pluginServer ->
      writeTQueue (plseActions pluginServer) $ PlsaStartPlugin plugin response'
    Nothing -> do
      errorText <- I.lookupText (plugInterface plugin) $ T.pack "Plugin server is not registered"
      putTMVar response . Left $ Error [errorText]
  return response'

-- | Wait for response to starting plugin thread.
waitStart :: PluginStartResponse -> STM (Either Error ())
waitStart (PluginStartResponse response) = readTMVar response

-- | Stop a plugin thread.
stop :: Plugin -> STM PluginStopResponse
stop plugin = do
  response <- PluginStopResponse <$> newEmptyTMVar
  writeTQueue (plugActions plugin) (PlugStop response)
  return response

-- | Wait for response to stopping plugin thread.
waitStop :: PluginStopResponse -> STM (Either Error ())
waitStop (PluginStopResponse response) = readTMVar response

-- | Stop a plugin.
stop :: Plugin -> STM ()
stop plugin = writeTChan (plugActions plugin) PlugStop

-- | Run a plugin.
run :: Plugin -> STM PluginRunResponse
run plugin = do
  response <- PluginRunResponse <$> newEmptyTMVar
  writeTQueue (plugActions plugin) (PlugRun response)
  return response

-- | Wait for response to running plugin.
waitRun :: PluginRunResponse -> STM (Either Error ())
waitRun (PluginRunResponse response) = readTMVar response

-- | Quit a plugin.
quit :: Plugin -> STM (Either Error ())
quit plugin = do
  running <- readTVar $ plugRunning plugin
  if running
  then do
    quitting <- readTVar $ plugQuitting plugin
    if not quitting
    then do
      writeTVar (plugQuitting plugin) True
      writeTChan (plugInputEvents plugin) PievQuit
      return $ Right ()
    else return $ Right ()
  else do
    errorText <- I.lookupText (plugInterface plugin) $ T.pack "Plugin is not running"
    return . Left $ Error [errorText]

-- | Exit a plugin.
exit :: Plugin -> STM (Either Error ())
exit plugin = do
  running <- readTVar $ plugRunning plugin
  if running
  then do
    writeTVar (plugRunning plugin) False
    writeTChan (pluginOutputEvents plugin) PoevExit
    return $ Right ()
  else do
    errorText <- I.lookupText (plugInterface plugin) $ T.pack "Plugin is not running"
    return . Left $ Error [errorText]

-- | Get plugin path.
getPath :: Plugin -> T.Text
getPath = plugPath

-- | Get plugin entry point.
getEntryPoint :: Plugin -> Maybe T.Text
getEntryPoint = plugEntryPoint

-- | Get whether plugin is precompiled.
getPrecompiled :: Plugin -> Bool
getPrecompiled = plugPrecompiled

-- | Get whether plugin is running.
getRunning :: Plugin -> STM Bool
getRunning = readTVar . plugRunning

-- | Get whether plugin is active.
getActive :: Plugin -> STM Bool
getActive = readTVar . plugActive

-- | Get whether plugin is quitting.
getQuitting :: Plugin -> STM Bool
getQuitting = readTVr . plugQuitting

-- | Subscribe to plugin input events.
subscribeInput :: Plugin -> STM PluginInputSubscription
subscribeInput plugin = PluginInputSubscription <$> dupTChan $ plugInputEvents plugin

-- | Receive a plugin input event.
recvInput :: PluginInputSubscription -> STM PluginInputEvent
recvInput (PluginInputSubscription subscription) = readTChan subscription

-- | Peek a plugin input event.
peekInput :: PluginInputSubscription -> STM PluginInputEvent
peekInput (PluginInputSubscription subscription) = peekTChan subscription

-- | Subscribe to plugin output events.
subscribeOutput :: Plugin -> STM PluginOutputSubscription
subscribeOutput plugin = PluginOutputSubscription <$> dupTChan $ plugOutputEvents plugin

-- | Receive a plugin output event.
recvOutput :: PluginOutputSubscription -> STM PluginOutputEvent
recvOutput (PluginOutputSubscription subscription) = readTChan subscription

-- | Peek a plugin output event.
peekOutput :: PluginOutputSubscription -> STM PluginOutputEvent
peekOutput (PluginOutputSubscription subscription) = peekTChan subscription
