module Network.IRC.Client.Amphibian.CtcpHandler

       (CtcpHandler,
        CtcpHandlerStopResponse.
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Commands
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Control.Concurrrent.STM (STM,
                                TVar,
                                TMVar,
                                atomically,
                                orElse,
                                retry,
                                newTVar,
                                readTVar,
                                writeTVar,
                                newEmptyTMVar,
                                putTMVar,
                                readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Monad ((=<<))
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Time.Clock (UTCTime,
                        getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (TimeLocale)


-- | Create a CTCP handler.
new :: Interface -> STM CtcpHandler
new intf = do
  running <- newTVar False
  actions <- newTQueue
  subscription <- I.subscribe intf
  connectionManagers <- newTVar []
  return $ CtcpHandler { cthaInterface = interface,
                         cthaRunning = running,
                         cthaActions = actions,
                         cthaSubscription = subscription,
                         cthaConnectionManagers = connectionManagers }

-- | Start a CTCP handler.
start :: CtcpHandler -> AM ()
start handler = do
  join . liftIO . atomically $ do
    running <- readTVar $ cthaRunning handler
    if not running
    then do
      writeTVar (cthaRunning handler) True
      return $ do
        intf <- getInterface
        async $ runAM (runCtcpHandler handler) intf
    else return $ return ()

-- | Stop a CTCP handler.
stop :: CtcpHandler -> STM CtcpHandlerStopResponse
stop handler = do
  response <- CtcpHandlerStopResponse <$> newEmptyTMVar
  writeTQueue (cthaActions handler) $ CthaStop response
  return response

-- | Wait for a CTCP handler to stop.
waitStop :: CtcpHandlerStopResponse -> STM (Either Error ())
waitStop (CtcpHandlerStopResponse response) = readTMVar response

-- | Run a CTCP handler.
runCtcpHandler :: CtcpHandler -> AM ()
runCtcpHandler handler = do
  continue <- join . liftIO . atomically $ do
    mappings <- readTVar $ cthaConnectionManagers handler
    handleAction handler `orElse` handleInterface handler `orElse`
      foldr (\mapping y -> handleMapping handler mapping `orElse` y) retry mappings
  if continue
  then runCtcpHandler handler
  else return ()

-- | Handle a CTCP handler action.
handleAction :: CtcpHandler -> STM (AM Bool)
handleAction handler = do
  action <- readTQueue $ cthaActions handler
  case action of
    CthaStop (CtcpHandlerStopResponse response) -> do
      putTMVar response $ Right ()
      return $ return False

-- | Handle a CTCP handler interface event.
handleInterface :: CtcpHandler -> STM (AM Bool)
handleInterface handler = do
  event <- I.recv $ cthaSubscription handler
  case event of
    IntfConnectionManagerRegistered manager -> do
      mappings <- readTVar $ cthaConnectionManagers handler
      if not any (\mapping -> cthmConnectionManager mapping == manager)
      then do
        subscription <- CM.subscribe manager
        let mapping = CtcpHandlerMapping { cthmConnectionManager = manager,
                                           cthmSubscription = subscription }
        writeTVar (cthaConnectionManagers handler) (mapping : mappings)
      else return ()
    IntfConnectionManagerUnregistered manager -> do
      mappings <- readTVar $ cthaConnectionManagers handler
      writeTVar (cthaConnectionManagers) $ filter (\mapping -> cthmConnectionManager /= manager) mappings
    _ -> return ()
  return $ return True
  
-- | Handle a CTCP handler mapping.
handleMapping :: CtcpHandler -> CtcpHandlerMapping -> STM (AM Bool)
handleMapping handler mapping = do
  event <- CM.recv $ cthmSubscription mapping
  let manager = cthmConnectionManager mapping
      intf = cthaInterface handler
  case event of
    ComaRecvCtcpRequest nick _ comment ->
      case parseCtcp comment of
        Just (command, argument)
          | command == ctcp_FINGER && argument == Nothing-> handleFinger intf manager nick
          | command == ctcp_VERSION && argument == Nothing -> handleVersion intf manager nick
          | command == ctcp_SOURCE && argument == Nothing -> handleSource intf manager nick
          | command == ctcp_USERINFO && argument == Nothing -> handleUserInfo intf manager nick
          | command == ctcp_CLIENTINFO && argument == Nothing -> handleClientInfo intf manager nick
          | command == ctcp_PING -> handlePing intf manager nick argument
          | command == ctcp_TIME && argument == Nothing -> handleTime intf manager nick
          | otherwise -> handleUnsupported intf manager nick argument
    _ -> return $ return True

-- | Handle FINGER CTCP request.
handleFinger :: Interface -> ConnectionManager -> Nick -> STM (AM Bool)
handleFinger intf manager nick = do
  name <- comaName <$> CM.getSetup manager
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_FINGER . Just $
                                   B.append (BC.singleton ':') name }
  return $ return True

-- | Handle VERSION CTCP request.
handleVersion :: Interface -> ConnectionManager -> Nick -> STM (AM Bool)
handleVersion intf manager nick = do
  version <- encode intf manager =<< confCtcpVersion <$> I.getConfig intf
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_VERSION $ Just version }
  return $ return True

-- | Handle SOURCE CTCP request.
handleSource :: Interface -> ConnectionManager -> Nick -> STM (AM Bool)
handleSource intf manager nick = do
  source <- encode intf manager =<< confCtcpSource <$> I.getConfig intf
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_SOURCE $ Just source }
  return $ return True

-- | Handle USERINFO CTCP request.
handleUserInfo :: Interface -> ConnectionManager-> Nick -> STM (AM Bool)
handleUserInfo intf manager nick = do
  userInfo <- encode intf manager =<< cocoCtcpUserInfo <$> I.getConnectionConfig intf manager
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_USERINFO . Just $
                                   B.append (BC.singleton ':') userInfo }
  return $ return True

-- | Handle CLIENTINFO CTCP request.
handleClientInfo :: Interface -> ConnectionManager -> Nick -> STM (AM Bool)
handleClientInfo intf manager nick = do
  clientInfo <- encode intf manager =<< confCtcpUserInfo <$> I.getConfig intf
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_CLIENTINFO . Just $
                                   B.append (BC.singleton ':') clientInfo }
  return $ return True

-- | Handle PING CTCP request.
handlePing :: Interface -> ConnectionManager -> Nick -> Maybe CtcpArgument -> STM (AM Bool)
handlePing intf manager nick (Just argument) = do
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_PING $ Just argument }
  return $ return True
handlePing intf manager nick Nothing = do
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just $ formatCtcp ctcp_PING Nothing }
  return $ return True

-- | Handle TIME CTCP request.
handleTime :: Interface -> ConnectionManager -> Nick -> STM (AM Bool)
handleTime intf manager nick = do
  return $ do
    time <- liftIO getCurrentTime
    liftIO . atomically $
      timeLocale <- confTimeLocale <$> I.getConfig intf
      let timeData = encode intf manager . T.pack $ formatTime timeLocale "%c" time
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick]
                                     ircmComment = Just . formatCtcp ctcp_TIME $ Just timeData }
    return True

-- | Handle unsupported CTCP request.
handleUnsupported :: Interface -> ConnectionManager -> Nick -> Maybe CtcpArgument -> STM (AM Bool)
handleUnsupported intf manager nick argument = do
  let errorMessage = encode intf manager $ I.lookupText intf "Unknown/unsupported CTCP command"
  let comment = case argument of
    Just argument ->
      formatCtcp ctcp_ERRMSG (B.concat [command, BC.singleton ' ', argument, BC.pack " :", errorMesage])
    Nothing ->
      formatCtcp ctcp_ERRMSG (B.concat [command, BC.pack " :", errorMessage])
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_ERRMSG $ Just comment }
  return $ return True

-- | Encode text sent to a connection manager.
encode :: Interface -> ConnectionManager -> T.Text -> STM B.ByteString
encode intf manager text = do
  config <- I.getConnectionConfig intf manager
  case config of
    Just config -> return $ (encoEncoder $ cocoEncoding config) text
    Nothing -> return B.empty
  Nothing -> return B.empty
