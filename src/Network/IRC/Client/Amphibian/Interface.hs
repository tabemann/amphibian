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
        unregisterChannelServer,
        getUserServer,
        registerUserServer,
        unregisterUserServer)

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
  frontend <- newTVar Nothing
  inputDispatcher <- newTVar Nothing
  ctcpDispatcher <- newTVar Nothing
  pluginServer <- newTVar Nothing
  connectionManagerServer <- newTVar Nothing
  channelServer <- newTVar Nothing
  userServer <- newTVar Nothing
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
                       intfFrontend = frontend,
                       intfInputDispatcher = inputDispatcher,
                       intfCtcpDispatcher = ctcpDispatcher,
                       intfPluginServer = pluginServer,
                       intfConnectionManagerServer = connectionManagerServer,
                       intfChannelServer = channelServer,
                       intfUserServer = userServer,
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

-- | Get frontend.
getFrontend :: Interface -> STM (Maybe Frontend)
getFrontend = readTVar . intfFrontend

-- | Register frontend.
registerFrontend :: Interface -> Frontend -> STM ()
registerFrontend intf frontend = do
  currentDispatcher <- readTVar $ intfFrontend intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfFrontend intf) (Just frontend)
      writeTChan (intfEvents intf) (IntfFrontendRegistered frontend)
    _ -> return ()

-- | Unregister frontend.
unregisterFrontend :: Interface -> Frontend -> STM ()
unregisterFrontend intf frontend = do
  currentDispatcher <- readTVar $ intfFrontend intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == frontend -> do
      writeTVar (intfFrontend intf) Nothing
      writeTChan (intfEvents intf) (IntfFrontendUnregistered frontend)
    _ -> return ()

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

-- | Get CTCP dispatcher.
getCtcpDispatcher :: Interface -> STM (Maybe CtcpDispatcher)
getCtcpDispatcher = readTVar . intfCtcpDispatcher

-- | Register CTCP dispatcher.
registerCtcpDispatcher :: Interface -> CtcpDispatcher -> STM ()
registerCtcpDispatcher intf dispatcher = do
  currentDispatcher <- readTVar $ intfCtcpDispatcher intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfCtcpDispatcher intf) (Just dispatcher)
      writeTChan (intfEvents intf) (IntfCtcpDispatcherRegistered dispatcher)
    _ -> return ()

-- | Unregister CTCP dispatcher.
unregisterCtcpDispatcher :: Interface -> CtcpDispatcher -> STM ()
unregisterCtcpDispatcher intf dispatcher = do
  currentDispatcher <- readTVar $ intfCtcpDispatcher intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == dispatcher -> do
      writeTVar (intfCtcpDispatcher intf) Nothing
      writeTChan (intfEvents intf) (IntfCtcpDispatcherUnregistered dispatcher)
    _ -> return ()

-- | Get plugin server.
getPluginServer :: Interface -> STM (Maybe PluginServer)
getPluginServer = readTVar . intfPluginServer

-- | Register plugin server.
registerPluginServer :: Interface -> PluginServer -> STM ()
registerPluginServer intf server = do
  currentDispatcher <- readTVar $ intfPluginServer intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfPluginServer intf) (Just server)
      writeTChan (intfEvents intf) (IntfPluginServerRegistered server)
    _ -> return ()

-- | Unregister plugin server.
unregisterPluginServer :: Interface -> PluginServer -> STM ()
unregisterPluginServer intf server = do
  currentDispatcher <- readTVar $ intfPluginServer intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == server -> do
      writeTVar (intfPluginServer intf) Nothing
      writeTChan (intfEvents intf) (IntfPluginServerUnregistered server)
    _ -> return ()

-- | Get connection manager server.
getConnectionManagerServer :: Interface -> STM (Maybe ConnectionManagerServer)
getConnectionManagerServer = readTVar . intfConnectionManagerServer

-- | Register connection manager server.
registerConnectionManagerServer :: Interface -> ConnectionManagerServer -> STM ()
registerConnectionManagerServer intf server = do
  currentDispatcher <- readTVar $ intfConnectionManagerServer intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfConnectionManagerServer intf) (Just server)
      writeTChan (intfEvents intf) (IntfConnectionManagerServerRegistered server)
    _ -> return ()

-- | Unregister connection manager server.
unregisterConnectionManagerServer :: Interface -> ConnectionManagerServer -> STM ()
unregisterConnectionManagerServer intf server = do
  currentDispatcher <- readTVar $ intfConnectionManagerServer intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == server -> do
      writeTVar (intfConnectionManagerServer intf) Nothing
      writeTChan (intfEvents intf) (IntfConnectionManagerServerUnregistered server)
    _ -> return ()

-- | Get channel server.
getChannelServer :: Interface -> STM (Maybe ChannelServer)
getChannelServer = readTVar . intfChannelServer

-- | Register channel server.
registerChannelServer :: Interface -> ChannelServer -> STM ()
registerChannelServer intf server = do
  currentDispatcher <- readTVar $ intfChannelServer intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfChannelServer intf) (Just server)
      writeTChan (intfEvents intf) (IntfChannelServerRegistered server)
    _ -> return ()

-- | Unregister channel server.
unregisterChannelServer :: Interface -> ChannelServer -> STM ()
unregisterChannelServer intf server = do
  currentDispatcher <- readTVar $ intfChannelServer intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == server -> do
      writeTVar (intfChannelServer intf) Nothing
      writeTChan (intfEvents intf) (IntfChannelServerUnregistered server)
    _ -> return ()

-- | Get user server.
getUserServer :: Interface -> STM (Maybe UserServer)
getUserServer = readTVar . intfUserServer

-- | Register user server.
registerUserServer :: Interface -> UserServer -> STM ()
registerUserServer intf server = do
  currentDispatcher <- readTVar $ intfUserServer intf
  case currentDispatcher of
    Nothing -> do
      writeTVar (intfUserServer intf) (Just server)
      writeTChan (intfEvents intf) (IntfUserServerRegistered server)
    _ -> return ()

-- | Unregister user server.
unregisterUserServer :: Interface -> UserServer -> STM ()
unregisterUserServer intf server = do
  currentDispatcher <- readTVar $ intfUserServer intf
  case currentDispatcher of
    Just currentDispatcher | currentDispatcher == server -> do
      writeTVar (intfUserServer intf) Nothing
      writeTChan (intfEvents intf) (IntfUserServerUnregistered server)
    _ -> return ()
