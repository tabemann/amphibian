module Network.IRC.Client.Amphibian.ChannelDisplay

       (ChannelDisplay,
        ChannelDisplayStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.Channel as C
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
new :: Interface -> STM ChannelDisplay
new intf = do
  running <- newTVar False
  actions <- newTQUeue
  interfaceSubscription <- I.subscribe intf
  frames <- newTVar []
  return $ ChannelDisplay { chdiRunning = running,
                            chdiActions = actions,
                            chdiInterface = interface,
                            chdiInterfaceSubscription = interfaceSubscription,
                            chdiFrames = frames }

-- | Start connection display.
start :: ChannelDisplay -> AM ()
start display = do
  join . liftIO . atomically $ do
    running <- readTVar $ chdiRunning display
    if not running
    then do
      writeTVar (chdiRunning display) True
      return $ do
        intf <- getInterface
        async (runAM (runDisplay display) intf)
    else return $ return ()

-- | Stop connection display.
stop :: ChannelDisplay -> STM ChannelDisplayStopResponse
stop display = do
  response <- ChannelDisplayStopResponse <$> newEmptyTMVar
  writeTQueue (codaActions display) (CodaStop response)
  return response

-- | Wait for response to stopping connection display.
waitStop :: ChannelDisplayStopResponse -> STM (Either Error ())
waitStop (ChannelDisplayStopResponse response) = readTMVar response

-- | Run connection display.
runChannelDisplay :: ChannelDisplay -> AM ()
runChannelDisplay display = do
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ chdiFrames display
    handleAction display `orElse` handleInterfaceEvent display `orElse`
      (foldr1 (\mapping y -> handleChannelEvent display mapping `orElse` y) retry frames) 
  if continue
  then runChannelDisplay display
  else liftIO . atomically $ writeTVar (codaRunning display) False

-- | Handle connection display action.
handleAction :: ChannelDisplay -> STM (AM Bool)
handleAction display = do
  action <- readTQueue $ chdiActions display
  case action of
    CodaStop (ChannelDisplayStopResponse response) -> do
      writeTMVar response (Right ())
      return $ return False

-- | Handle interface event.
handleInterfaceEvent :: ChannelDisplay -> STM (AM Bool)
handleInterfaceEvent display = do
  event <- I.recv $ chdiInterfaceSubscription display
  case event of
    IntfFrameRegistered frame -> do
      mapping <- F.getMapping frame
      case mapping of
        FrmaChannel channel ->
          frames <- readTVar $ chdiFrames display
          if frame `notElem` map chdfFrame frames
          then do
            subscription <- C.subscribe channel
            let mapping = ChannelDisplayFrameMapping { chdfFrame = frame,
                                                       chdfChannel = channel,
                                                       chdfSubscription = subscription }
            writeTVar (chdiFrames display) (mapping : frames)
          else return ()
        _ -> return ()
      return $ return True
    IntfFrameUnregistered frame -> do
      frame <- readTVar $ chdiFrames display
      if frame `elem` map chdfFrame frames
      then writeTVar (chdiFrames display) (filter (\mapping -> chdfFrame mapping /= frame) frames)
      else return ()
      return $ return True
    _ -> return $ return True

-- | Handle connection event.
handleChannelEvent :: ChannelDisplay -> ChannelDisplayFrameMapping -> STM (AM Bool)
handleChannelEvent display mapping = do
  event <- C.recv $ chdfSubscription mapping
  let frame = chdfFrame mapping
      channel = chdfChannel mapping
  case event of
    ChanDisconnected (Left error) ->
      return $ do
        disconnectErrorMessage frame error
        return True
    ChanDisconnected (Right ()) ->
      return $ do
        disconnectMessage frame
        return True
    ChanJoined ->
      return $ do
        joinedMessage frame $ C.getName channel
        return True
    ChanParted (Just comment) ->
      return $ do
        partedCommentMessage frame (C.getName channel) comment
        return True
    ChanParted Nothing ->
      return $ do
        partedMessage frame channel
        return True
    ChanNoTopic ->
      return $ do
        noTopicMessage frame (C.getName channel)
        return True
    ChanTopic topic ->
      return $ do
        topicMessage frame (C.getName channel) topic
        return True
    ChanTopicWhoTime fullName time ->
      return $ do
        topicWhoTimeMessage frame (C.getName channel) fullName time
        return True
    ChanNames names ->
      return $ do
        namesMessage frame names
        return True
    ChanRecvJoin nick fullName ->
      return $ do
        recvJoinMessage frame (C.getname channel) nick fullName
        return True
    ChanRecvPart nick fullName (Just comment) ->
      return $ do
        recvPartCommentMessage frame (C.getName channel) nick fullName comment
        namesMessage frame (C.getNames channel)
        return True
    ChanRecvPart nick fullName Nothing ->
      return $ do
        recvPartMessage frame (C.getName channel) nick fullName
        namesMessage frame (C.getNames channel)
        return True
    ChanRecvMessage nick comment ->
      return $ do
        recvMessageMessage frame nick comment FrmtChannel
        return True
    ChanRecvNotice nick comment ->
      return $ do
        recvNoticeMessage frame nick comment FrmtChannel FrtaSpecific
        return True
    ChanRecvNick oldNick newNick ->
      return $ do
        recvNickMessage frame oldNick newNick
        return True
    ChanRecvTopic nick topic ->
      return $ do
        recvTopicMessage frame nick topic
        return True
    ChanRecvQuit nick fullName (Just comment) ->
      return $ do
        recvQuitCommentMessage frame nick fullName comment
        namesMessage frame (C.getNames channel)
        return True
    ChanRecvQuit nick fullName Nothing ->
      return $ do
        recvQuitMessage frame nick fullName
        namesMessage frame (C.getNames channel)
        return True
    ChanSelfMessage nick comment ->
      return $ do
        selfMessageMessage frame nick comment
        return True
    ChanSelfNotice nick comment ->
      return $ do
        selfNoticeMessage frame nick comment
        return True