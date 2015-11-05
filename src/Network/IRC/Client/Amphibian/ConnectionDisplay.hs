module Network.IRC.Client.Amphibian.ConnectionDisplay

       (ConnectionDisplay,
        ConnectionDisplayStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.FrameMessage as FM
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
import Data.Functor ((<$>))

-- | Create a connection display.
new :: Interface -> STM ConnectionDisplay
new intf = do
  running <- newTVar False
  actions <- newTQUeue
  interfaceSubscription <- I.subscribe intf
  frames <- newTVar []
  return $ ConnectionDisplay { codiRunning = running,
                               codiActions = actions,
                               codiInterface = interface,
                               codiInterfaceSubscription = interfaceSubscription,
                               codiFrames = frames }

-- | Start connection display.
start :: ConnectionDisplay -> AM ()
start display = do
  join . liftIO . atomically $ do
    running <- readTVar $ codiRunning display
    if not running
    then do
      writeTVar (codiRunning display) True
      return $ do
        intf <- getInterface
        async (runAM (runDisplay display) intf)
    else return $ return ()

-- | Stop connection display.
stop :: ConnectionDisplay -> STM ConnectionDisplayStopResponse
stop display = do
  response <- ConnectionDisplayStopResponse <$> newEmptyTMVar
  writeTQueue (codaActions display) (CodaStop response)
  return response

-- | Wait for response to stopping connection display.
waitStop :: ConnectionDisplayStopResponse -> STM (Either Error ())
waitStop (ConnectionDisplayStopResponse response) = readTMVar response

-- | Run connection display.
runConnectionDisplay :: ConnectionDisplay -> AM ()
runConnectionDisplay display = do
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ codiFrames display
    handleAction display `orElse` handleInterfaceEvent display `orElse`
      (foldr1 (\mapping y -> handleConnectionEvent display mapping `orElse` y) frames) 
  if continue
  then runConnectionDisplay display
  else liftIO . atomically $ writeTVar (codaRunning display) False

-- | Handle connection display action.
handleAction :: ConnectionDisplay -> STM (AM Bool)
handleAction display = do
  action <- readTQueue $ codiActions display
  case action of
    CodaStop (ConnectionDisplayStopResponse response) -> do
      writeTMVar response (Right ())
      return $ return False

-- | Handle interface event.
handleInterfaceEvent :: ConnectionDisplay -> STM (AM Bool)
handleInterfaceEvent display = do
  event <- I.recv $ codiInterfaceSubscription display
  case event of
    IntfFrameRegistered frame -> do
      mapping <- F.getMapping frame
      case mapping of
        FrmaConnectionManager manager ->
          frames <- readTVar $ codiFrames display
          if frame `notElem` map codfFrame frames
          then do
            subscription <- CM.subscribe manager
            let mapping = ConnectionDisplayFrameMapping { codfFrame = frame,
                                                          codfConnectionManager = manager,
                                                          codfSubscription = subscription }
            writeTVar (codiFrames display) (mapping : frames)
          else return ()
        _ -> return ()
      return $ return True
    IntfFrameUnregistered frame -> do
      frame <- readTVar $ codiFrames display
      if frame `elem` map codfFrame frames
      then writeTVar (codiFrames display) (filter (\mapping -> codfFrame mapping /= frame) frames)
      else return ()
      return $ return True
    _ -> return $ return True

-- | Handle connection event.
handleConnectionEvent :: ConnectionDisplay -> ConnectionDisplayFrameMapping -> STM (AM Bool)
handleConnectionEvent display mapping = do
  event <- CM.recv $ codfSubscription mapping
  let frame = codfFrame mapping
      manager = codfConnectionManager mapping
  case event of
    ComaLookupAddress hostName ->
      return $ do
        FM.lookupAddressMessage frame hostName
        return True
    ComaLookupAddressFailed error ->
      return $ do
        FM.lookupAddressFailedMessage frame error
        return True
    ComaReverseLookupFailed error ->
      return $ do
        FM.reverseLookupFailedMessage frame error
        return True
    ComaConnecting hostName port ->
      return $ do
        FM.connectingMessage frame hostName port
        return True
    ComaConnected hostName port ->
      return $ do
        FM.connectedMessage frame hostName port
        return True
    ComaConnectFailed error ->
      return $ do
        FM.connectFailedMessage error
        return True
    ComaDisconnected (Left error) ->
      return $ do
        FM.disconnectErrorMessage frame error
        return True
    ComaDisconnected (Right ()) ->
      return $ do
        FM.disconnectMessage frame
        return True
    ComaPasswordMismatch password ->
      return $ do
        FM.passwordMismatchMessage frame password
        return True
    ComaBannedFromServer (Just comment) ->
      return $ do
        FM.bannedFromServerCommentMessage frame comment
        return True
    ComaBannedFromServer Nothing ->
      return $ do
        FM.bannedFromServerMessage frame
        returnTrue
    ComaWelcome (Just comment) ->
      return $ do
       FM.welcomeCommentMessage frame comment
       return True
    ComaWelcome Nothing -> do
      nick <- CM.getNick manager
      return $ do
        FM.welcomeMessage frame nick
        return True
    ComaAttemptingNick nick ->
      return $ do
        FM.attemptingNickMessage frame nick
        return True
    ComaMalformedNick nick ->
      return $ do
        FM.malformedNickMessage frame nick
        return True
    ComaRecvNotice nick comment ->
      return $ do
        FM.recvNoticeMessage frame nick comment True FrmtPrivate FrtaLastFocused
        return True
    ComaMotd lines ->
      return $ do
        FM.motdMessage frame lines
        return True
    _ -> return $ return True    
