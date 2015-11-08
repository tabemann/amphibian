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
  frames <- newTVar []
  return $ UserDisplay { usdiRunning = running,
                         usdiActions = actions,
                         usdiInterface = interface,
                         usdiInterfaceSubscription = interfaceSubscription,
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
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ usdiFrames display
    handleAction display `orElse` handleInterfaceEvent display `orElse`
      (foldr1 (\mapping y -> handleUserEvent display mapping `orElse` y) retry frames) 
  if continue
  then runUserDisplay display
  else liftIO . atomically $ writeTVar (codaRunning display) False

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
      mapping <- F.getMapping frame
      case mapping of
        FrmaUser channel ->
          frames <- readTVar $ usdiFrames display
          if frame `notElem` map usdfFrame frames
          then do
            subscription <- U.subscribe channel
            let mapping = UserDisplayFrameMapping { usdfFrame = frame,
                                                    usdfUser = channel,
                                                    usdfSubscription = subscription }
            writeTVar (usdiFrames display) (mapping : frames)
          else return ()
        _ -> return ()
      return $ return True
    IntfFrameUnregistered frame -> do
      frame <- readTVar $ usdiFrames display
      if frame `elem` map usdfFrame frames
      then writeTVar (usdiFrames display) (filter (\mapping -> usdfFrame mapping /= frame) frames)
      else return ()
      return $ return True
    _ -> return $ return True

-- | Handle connection event.
handleUserEvent :: UserDisplay -> UserDisplayFrameMapping -> STM (AM Bool)
handleUserEvent display mapping = do
  event <- U.recv $ usdfSubscription mapping
  let frame = usdfFrame mapping
      channel = usdfUser mapping
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