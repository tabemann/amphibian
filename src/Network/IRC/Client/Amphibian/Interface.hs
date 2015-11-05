module Network.IRC.Client.Amphibian.Interface

       (Interface,
        InterfaceSubscription,
        InterfaceEvent(..),
        newInterface,
        subscribe,
        peek,
        recv,
        getConfig,
        setConfig,
        getConnectionConfig,
        registerConnectionConfig,
        unregisterConnectionConfig,
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
        getFrames,
        registerFrame,
        unregisterFrame
        getPlugins,
        registerPlugin,
        unregisterPlugin)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Language
import Network.IRC.Client.Amphibian.Encoding
import Network.IRC.Client.Amphibian.Config (Config)
import Network.IRC.Client.Amphibian.Connection (Connection)
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               atomically,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newBroadcastTChan,
                               readTChan,
                               peekTChan,
                               writeTChan,
                               dupTChan)
import Data.Functor ((<$>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.Locale (TimeLocale,
                      defaultTimeLocale)

-- | Create a new interface.
newInterface :: Config -> STM Interface
newInterface config = do
  textMap <- newTVar HM.empty
  availableLanguages' <- newTVar availableLanguages
  availableEncodings' <- newTVar availableEncodings
  config' <- newTVar config
  connections <- newTVar []
  channels <- newTVar []
  users <- newTVar []
  frames <- newTVar []
  plugins <- newTVar []
  inputDispatcher <- newTVar Nothing
  events <- newBroadcastTChan
  return $ Interface { intfTextMap = textMap,
                       intfAvailableLanguages = availableLanguages',
                       intfAvailableEncodings = availableEncodings',
                       intfConfig = config',
                       intfConnections = connections,
                       intfChannels = channels,
                       intfUsers = users,
                       intfFrames = frames,
                       intfPlugins = plugins,
                       intfInputDispatcher = inputDispatcher,
                       intfEvents = events }

-- | Subscribe to events from an interface.
subscribe :: Interface -> STM InterfaceSubscription
subscribe intf = InterfaceSubscription <$> dupTChan $ intfEvents intf

-- | Peek an event from an interface subscription.
peek :: InterfaceSubscription -> STM InterfaceEvent
peek (InterfaceSubscription subscription) = peekTChan subscription

-- | Receive an event from an interface subscription.
recv :: InterfaceSubscription -> STM InterfaceEvent
recv (InterfaceSubscription subscription) = readTChan subscription

-- | Get configuration.
getConfig :: Interface -> STM Config
getConfig = readTVar . intfConfig

-- | Set configuration.
setConfig :: Interface -> Config -> STM ()
setConfig intf config = do
  writeTVar (intfConfig intf) config
  writeTChan (intfEvents intf) (IntfConfigSet config)

-- | Get connection configuration.
getConnectionConfig :: Interface -> ConnectionManager -> STM (Maybe ConnectionConfig)
getConnectionConfig intf manager =
  config' <- lookup manager <$> readTVar $ intfConnectionConfigs intf
  case config' of
    Just config' -> Just <$> readTVar config'
    Nothing -> return Nothing

-- | Set connection configuration.
setConnectionConfig :: Interface -> ConnectionManager -> ConnectionConfig -> STM ()
setConnectionConfig intf manager config = do
  config' <- lookup manager <$> readTVar $ intfConnectionConfigs intf
  case config' of
    Just config' -> do writeTVar config' config
                       writeTChan (intfEvents intf) (IntfConnectionConfigSet manager config)
    _ -> do config' <- newTVar config
            writeTVar (intfConnectionConfigs intf) ((manager, config') : configs)
            writeTChan (intfEvents intf) (IntfConnectionConfigSet manager config)

-- | Look up text, and return the provided key if no text is found.
lookupText :: Interface -> T.Text -> STM T.Text
lookupText intf key = do
  language <- confLanguage <$> readTVar $ intfConfig intf
  textMap <- readTVar $ intfTextMap intf
  return $ case HM.lookup language textMap of
    Just languageTextMap ->
      case HM.lookup key languageTextMap of
        Just text -> text
        Nothing -> key
    Nothing -> key

-- | Get connection managers.
getConnectionManagers :: Interface -> STM [ConnectionManager]
getConnectionManagers = readTVar . intfConnectionManagers

-- | Register connection manager.
registerConnectionManager :: Interface -> ConnectionManager -> STM ()
registerConnectionManager intf manager = do
  managers <- readTVar $ intfConnectionManagers intf
  if manager `notElem` managers
  then do
    writeTVar (intfConnectionManagers intf) (manager : managers)
    writeTChan (intfEvents intf) (IntfConnectionManagerRegistered manager)
  else return ()

-- | Unregister connection manager.
unregisterConnectionManager :: Interface -> ConnectionManager -> STM ()
unregisterConnectionManager intf manager = do
  managers <- readTVar $ intfConnectionManagers intf
  if manager `elem` managers
  then do
    configs <- readTVar $ intfConnectionConfigs intf
    writeTVar (intfConnectionConfigs intf) (filter (\(manager', _) -> manager != manager') configs)
    writeTVar (intfConnectionManagers intf) (filter (/= manager) managers)
    writeTChan (intfEvents intf) (IntfConnectionManagerUnregistered manager)
  else return ()

-- | Get channels.
getChannels :: Interface -> STM [Channel]
getChannels = readTVar . intfChannels

-- | Register channel.
registerChannel :: Interface -> Channel -> STM ()
registerChannel intf channel = do
  channels <- readTVar $ intfChannels intf
  if channel `notElem` channels
  then do 
    writeTVar (intfChannels intf) (channel : channels)
    writeTChan (intfEvents intf) (IntfChannelRegistered channel)
  else return ()

-- | Unregister user.
unregisterChannel :: Interface -> Channel -> STM ()
unregisterChannel intf channel = do
  channels <- readTVar $ intfChannels intf
  if channel `elem` channels
  then do
    writeTVar (intfChannels intf) (filter (/= channel) channels)
    writeTChan (intfEvents intf) (IntfChannelUnregistered channel)
  else return ()

-- | Get users.
getUsers :: Interface -> STM [User]
getUsers = readTVar . intfUsers

-- | Register user.
registerUser :: Interface -> User -> STM ()
registerUser intf user = do
  users <- readTVar $ intfUsers intf
  if user `notElem` users
  then do
    writeTVar (intfUsers intf) (user : users)
    writeTChan (intfEvents intf) (IntfUserRegistered user)
  else return ()

-- | Unregister user.
unregisterUser :: Interface -> User -> STM ()
unregisterUser intf user = do
  users <- readTVar $ intfUsers intf
  if user `elem` users
  then do
    writeTVar (intfUsers intf) (filter (/= user) users)
    writeTChan (intfEvents intf) (IntfUserUnregistered user)
  else return ()

-- | Get frames.
getFrames :: Interface -> STM [User]
getFrames = readTVar . intfFrames

-- | Register frame.
registerFrame :: Interface -> Frame -> STM ()
registerFrame intf frame = do
  frames <- readTVar $ intfFrames intf
  if frame `notElem` frames
  then do
    writeTVar (intfFrames intf) (frame : frames)
    writeTChan (intfEvents intf) (IntfFrameRegistered frame)
  else return ()

-- | Unregister frame.
unregisterFrame :: Interface -> Frame -> STM ()
unregisterfFrame intf frame = do
  frames <- readTVar $ intfFrames intf
  if frame `elem` frames
  then do
    writeTVar (intfFrames intf) (filter (/= frame) frames)
    writeTChan (intfEvents intf) (IntfFrameUnregistered frame)
  else return ()

-- | Get plugins.
getPlugins :: Interface -> STM [User]
getPlugins = readTVar . intfPlugins

-- | Register plugin.
registerPlugin :: Interface -> Plugin -> STM ()
registerPlugin intf plugin = do
  plugins <- readTVar $ intfPlugins intf
  if plugin `notElem` plugins
  then do
    writeTVar (intfPlugins intf) (plugin : plugins)
    writeTChan (intfEvents intf) (IntfPluginRegistered plugin)
  else return ()

-- | Unregister plugin.
unregisterPlugin :: Interface -> Plugin -> STM ()
unregisterfPlugin intf plugin = do
  plugins <- readTVar $ intfPlugins intf
  if plugin `elem` plugins
  then do
    writeTVar (intfPlugins intf) (filter (/= plugin) plugins)
    writeTChan (intfEvents intf) (IntfPluginUnregistered plugin)
  else return ()

-- | Get input dispatcher.
getInputDispatcher :: Interface -> STM (Maybe InputDispatcher)
getInputDispatcher = readTVar . intfInputDispatcher

-- | Register input dispatcher.
registerInputDispatcher :: Interface -> InputDispatcher -> STM ()
registerInputDispatcher intf dispatcher = do
  currentDispatcher <- readTVar $ intfInputDispatcher intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfInputDispatcher intf) (Just dispatcher)
      writeTChan (intfEvents intf) (IntfInputDispatcherRegistered dispatcher)
    _ -> return ()

-- | Unregister input dispatcher.
unregisterInputDispatcher :: Interface -> InputDispatcher -> STM ()
unregisterInputDispatcher intf dispatcher = do
  currentDispatcher <- readTVar $ intfInputDispatcher intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == dispatcher -> do
      writeTVar (intfInputDispatcher intf) Nothing
      writeTChan (intfEvents intf) (IntfInputDispatcherUnregistered dispatcher)
    _ -> return ()
