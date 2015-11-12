module Network.IRC.Client.Amphibian.UserDisplay

       (UserDisplay,
        UserDisplayStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Ctcp
import Network.IRC.Client.Amphibian.Commands
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.User as U
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
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8

-- | Create a connection display.
new :: Interface -> STM UserDisplay
new intf = do
  running <- newTVar False
  actions <- newTQUeue
  interfaceSubscription <- I.subscribe intf
  allFrames <- newTVar []
  frames <- newTVar []
  return $ UserDisplay { usdiRunning = running,
                         usdiActions = actions,
                         usdiInterface = interface,
                         usdiInterfaceSubscription = interfaceSubscription,
                         usdiAllFrames = allFrames,
                         usdiFrames = frames }

-- | Start connection display.
start :: UserDisplay -> AM ()
start display = do
  join . liftIO . atomically $ do
    running <- readTVar $ usdiRunning display
    if not running
    then do
      writeTVar (usdiRunning display) True
      return $ do
        intf <- getInterface
        async (runAM (runDisplay display) intf)
    else return $ return ()

-- | Stop connection display.
stop :: UserDisplay -> STM UserDisplayStopResponse
stop display = do
  response <- UserDisplayStopResponse <$> newEmptyTMVar
  writeTQueue (codaActions display) (CodaStop response)
  return response

-- | Wait for response to stopping connection display.
waitStop :: UserDisplayStopResponse -> STM (Either Error ())
waitStop (UserDisplayStopResponse response) = readTMVar response

-- | Run connection display.
runUserDisplay :: UserDisplay -> AM ()
runUserDisplay display = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ usdiFrames display
    allFrames <- readTVar $ usdiAllFrames display
    handleAction display `orElse` handleInterfaceEvent display `orElse`
      (foldr (\frame y -> handleFrameEvent display frame `orElse` y) retry allFrames) `orElse`
      (foldr (\mapping y -> handleUserEvent display mapping `orElse` y) retry frames) 
  if continue
  then runUserDisplay display
  else liftIO . atomically $ writeTVar (usdiRunning display) False

-- | Handle connection display action.
handleAction :: UserDisplay -> STM (AM Bool)
handleAction display = do
  action <- readTQueue $ usdiActions display
  case action of
    CodaStop (UserDisplayStopResponse response) -> do
      writeTMVar response (Right ())
      return $ return False

-- | Handle interface event.
handleInterfaceEvent :: UserDisplay -> STM (AM Bool)
handleInterfaceEvent display = do
  event <- I.recv $ usdiInterfaceSubscription display
  case event of
    IntfFrameRegistered frame -> do
      allFrames <- readTVar $ usdiAllFrames display
      if frame `notElem` map usdfFrame allFrames
      then do
        frameSubscription <- F.subscribeInput frame
        let frame' = UserDisplayFrame { usdfFrame = frame,
                                        usdfSubscription = frameSubscription }
        writeTVar (usdiAllFrames display) (frame' : allFrames)
      else return ()
      mapping <- F.getMapping frame
      case mapping of
        FrmaUser _ -> addMappedFrame display frame
        _ -> return ()
      return $ return True
    IntfFrameUnregistered frame -> do
      allFrames <- readTVar $ usdiAllFrames display
      if frame `elem` map usdfFrame allFrames
      then writeTVar (usdiAllFrames display) (filter (\frame' -> usdfFrame frame' /= frame) allFrames)
      else return ()
      removeMappedFrame display frame
      return $ return True
    _ -> return $ return True

-- | Handle frame event.
handleFrameEvent :: UserDisplay -> UserDisplayFrame -> STM (AM Bool)
handleFrameEvent display frame = do
  event <- F.recvInput $ usdfSubscription frame
  case event of
    FievMapping mapping -> do
      case mapping of
        FrmaUser _ -> addMappedFrame display $ usdfFrame frame
        _ -> removeMappedFrame display $ usdfFrame frame
      return $ return True
    _ -> return $ return True

-- | Add frame to mapped frames.
addMappedFrame :: UserDisplay -> Frame -> STM ()
addMappedFrame display frame = do
  frames <- readTVar $ usdiFrames display
  if frame `notElem` map udfmFrame frames
  then do
    subscription <- U.subscribe channel
    let mapping = UserDisplayFrameMapping { udfmFrame = frame,
                                            udfmUser = channel,
                                            udfmSubscription = subscription }
    writeTVar (usdiFrames display) (mapping : frames)
  else return ()

-- | Remove frame from mapped frames.
removeMappedFrame :: UserDisplay -> Frame -> STM ()
removeMappedFrame display frame = do
  frames <- readTVar $ usdiFrames display
  if frame `elem` map udfmFrame frames
  then writeTVar (usdiFrames display) (filter (\mapping -> udfmFrame mapping /= frame) frames)
  else return ()

-- | Handle connection event.
handleUserEvent :: UserDisplay -> UserDisplayFrameMapping -> STM (AM Bool)
handleUserEvent display mapping = do
  event <- U.recv $ udfmSubscription mapping
  let frame = udfmFrame mapping
      channel = udfmUser mapping
  case event of
    UserDisconnected (Left error) ->
      return $ do
        disconnectErrorMessage frame error
        return True
    UserDisconnected (Right ()) ->
      return $ do
        disconnectMessage frame
        return True
    UserRecvMessage nick comment ->
      return $ do
        recvMessageMessage frame nick comment FrmtUser
        return True
    UserRecvNotice nick comment ->
      return $ do
        recvNoticeMessage frame nick comment FrmtUser FrtaSpecific
        return True
    UserRecvNick oldNick newNick ->
      return $ do
        recvNickMessage frame oldNick newNick
        return True
    UserRecvQuit nick fullName (Just comment) ->
      return $ do
        recvQuitCommentMessage frame nick fullName comment
        return True
    UserRecvQuit nick fullName Nothing ->
      return $ do
        recvQuitMessage frame nick fullName
        return True
    UserRecvCtcpRequest nick comment ->
      case parseCtcp comment of
        Just (command, Just comment) | command == ctcp_ACTION -> do
          return $ do
            FM.recvActionMessage frame nick comment FrmtPrivate
            return True
        _ -> return $ return True
    UserSelfMessage nick comment ->
      return $ do
        selfMessageMessage frame nick comment
        return True
    UserSelfNotice nick comment ->
      return $ do
        selfNoticeMessage frame nick comment
        return True
    UserSelfCtcpRequest nick comment ->
      case parseCtcp comment of
        Just (command, Just comment) | command == ctcp_ACTION -> do
          return $ do
            FM.selfActionMessage frame nick comment FrmtPrivate
            return True
        _ -> return $ return True
    _ -> return $ return True