module Network.IRC.Client.Amphibian.CtcpDispatcher

       (CtcpDispatcher,
        CtcpDispatcherStopResponse,
        CtcpDispatcherRequestHandler,
        new,
        start,
        stop,
        waitStop,
        registerRequestHandler,
        unregisterRequestHandler)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Ctcp
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
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

-- | Create a CTCP dispatcher.
new :: Interface -> STM CtcpDispatcher
new intf = do
  running <- newTVar False
  actions <- newTQueue
  subscription <- I.subscribe intf
  connectionManagers <- newTVar []
  requestHandlers = newTVar HM.empty
  return $ CtcpDispatcher { ctdiInterface = interface,
                            ctdiRunning = running,
                            ctdiActions = actions,
                            ctdiSubscription = subscription,
                            ctdiConnectionManagers = connectionManagers }

-- | Start a CTCP dispatcher.
start :: CtcpDispatcher -> AM ()
start dispatcher = do
  join . liftIO . atomically $ do
    running <- readTVar $ ctdiRunning dispatcher
    if not running
    then do
      writeTVar (ctdiRunning dispatcher) True
      I.registerCtcpDispatcher (ctdiInterface dispatcher) dispatcher
      return $ do
        intf <- getInterface
        async $ runAM (runCtcpDispatcher dispatcher) intf
    else return $ return ()

-- | Stop a CTCP dispatcher.
stop :: CtcpDispatcher -> STM CtcpDispatcherStopResponse
stop dispatcher = do
  response <- CtcpDispatcherStopResponse <$> newEmptyTMVar
  writeTQueue (ctdiActions dispatcher) $ CthaStop response
  return response

-- | Wait for a CTCP dispatcher to stop.
waitStop :: CtcpDispatcherStopResponse -> STM (Either Error ())
waitStop (CtcpDispatcherStopResponse response) = readTMVar response

-- | Register a request handler with an input dispatcher.
registerRequestHandler :: CtcpDispatcher -> CtcpCommand ->
                          (ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool) ->
                          STM CtcpDispatcherRequestHandler
registerRequestHandler dispatcher command handler = do
  handler' <- newTVar handler
  let handler = CtcpDispatcherRequestHandler { cdrhDispatcher = dispatcher,
                                               cdrhCommand = command,
                                               cdrhHandler = handler' }
  handlerMap <- readTVar $ ctdiRequestHandlers dispatcher
  let handlers = HM.lookupDefault [] command handlerMap
  writeTVar (ctdiRequestHandlers dispatcher) (HM.insert request (handler : handler) handlerMap)
  return handler

-- | Unregister a request handler from an input dispatcher.
unregisterRequestHandler :: CtcpDispatcherRequestHandler -> STM ()
unregisterRequestHandler handler = do
  let dispatcher = cdrhDispatcher handler
  handlerMap <- readTVar $ ctdiRequestHandlers dispatcher
  writeTVar (ctdiRequestHandlers dispatcher) (HM.adjust (filter (/= handler)) (cdrhCommand handler) handlerMap)

-- | Run a CTCP dispatcher.
runCtcpDispatcher :: CtcpDispatcher -> AM ()
runCtcpDispatcher dispatcher = do
  continue <- join . liftIO . atomically $ do
    mappings <- readTVar $ ctdiConnectionManagers dispatcher
    handleAction dispatcher `orElse` handleInterface dispatcher `orElse`
      foldr (\mapping y -> handleMapping dispatcher mapping `orElse` y) retry mappings
  if continue
  then runCtcpDispatcher dispatcher
  else return ()

-- | Handle a CTCP dispatcher action.
handleAction :: CtcpDispatcher -> STM (AM Bool)
handleAction dispatcher = do
  action <- readTQueue $ ctdiActions dispatcher
  case action of
    CthaStop (CtcpDispatcherStopResponse response) -> do
      I.unregisterCtcpHandler (ctdiInterface dispatcher) dispatcher
      putTMVar response $ Right ()
      return $ return False

-- | Handle a CTCP dispatcher interface event.
handleInterface :: CtcpDispatcher -> STM (AM Bool)
handleInterface dispatcher = do
  event <- I.recv $ ctdiSubscription dispatcher
  case event of
    IntfConnectionManagerRegistered manager -> do
      mappings <- readTVar $ ctdiConnectionManagers dispatcher
      if not any (\mapping -> ctdmConnectionManager mapping == manager)
      then do
        subscription <- CM.subscribe manager
        let mapping = CtcpDispatcherMapping { ctdmConnectionManager = manager,
                                           ctdmSubscription = subscription }
        writeTVar (ctdiConnectionManagers dispatcher) (mapping : mappings)
      else return ()
    IntfConnectionManagerUnregistered manager -> do
      mappings <- readTVar $ ctdiConnectionManagers dispatcher
      writeTVar (ctdiConnectionManagers) $ filter (\mapping -> ctdmConnectionManager /= manager) mappings
    _ -> return ()
  return $ return True
  
-- | Handle a CTCP dispatcher mapping.
handleMapping :: CtcpDispatcher -> CtcpDispatcherMapping -> STM (AM Bool)
handleMapping dispatcher mapping = do
  event <- CM.recv $ ctdmSubscription mapping
  let manager = ctdmConnectionManager mapping
      intf = ctdiInterface dispatcher
  case event of
    ComaRecvCtcpRequest nick _ comment ->
      case parseCtcp comment of
        Just (command, argument) -> dispatchRequest intf manager nick command argument
          | request == ctcp_FINGER && argument == Nothing-> handleFinger intf manager nick
          | request == ctcp_VERSION && argument == Nothing -> handleVersion intf manager nick
          | request == ctcp_SOURCE && argument == Nothing -> handleSource intf manager nick
          | request == ctcp_USERINFO && argument == Nothing -> handleUserInfo intf manager nick
          | request == ctcp_CLIENTINFO -> handleClientInfo intf manager nick
          | request == ctcp_PING -> handlePing intf manager nick argument
          | request == ctcp_TIME && argument == Nothing -> handleTime intf manager nick
          | otherwise -> handleUnsupported intf manager nick argument
    _ -> return $ return True

-- | Dispatch request.
dispatchRequest :: CtcpDispatcher -> ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> STM (AM Bool)
dispatchRequest dispatcher manager nick command argument = do
  requestHandlers <- HM.lookup command <$> readTVar $ ctdiRequestHandlers dispatcher
  case commandHandlers of
    [] -> return $ do
            handleUnsupported manager nick command argument
            return True
    requestHandlers -> return $ dispatchRequest' requestHandlers manager nick command argument
  where dispatchRequest' (handler : rest) manager nick command argument = do
          handled <- (cdrhHandler handler) manager nick command argument
          if not handled
          then dispatchRequest' rest manager nick command argument
          else return True
        dispatchRequest' [] manager nick command argument = do
          handleUnsupported manager nick command argument
          return True

-- | Handle unsupported CTCP request.
handleUnsupported :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM ()
handleUnsupported manager nick command argument = do
  intf <- getInterface
  let errorMessage = encode intf manager <$> lookupText "Unknown/unsupported CTCP request"
  let comment = case argument of
    Just argument ->
      formatCtcp ctcp_ERRMSG (B.concat [command, BC.singleton ' ', argument, BC.pack " :", errorMesage])
    Nothing ->
      formatCtcp ctcp_ERRMSG (B.concat [command, BC.pack " :", errorMessage])
  liftIO . atomically $ CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_ERRMSG $ Just comment }
  return ()

-- | Encode text sent to a connection manager.
encode :: Interface -> ConnectionManager -> T.Text -> STM B.ByteString
encode intf manager text = do
  config <- I.getConnectionConfig intf manager
  case config of
    Just config -> return $ (encoEncoder $ cocoEncoding config) text
    Nothing -> return B.empty
  Nothing -> return B.empty
