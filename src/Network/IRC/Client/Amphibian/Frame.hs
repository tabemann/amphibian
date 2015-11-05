module Network.IRC.Client.Amphibian.Frame

       (Frame,
        FrameOutputSubscription,
        FrameInputSubscription,
        FrameOutputEvent,
        FrameInputEvent,
        new,
        subscribeOutput,
        subscribeInput,
        peekOutput,
        peekInput,
        recvOutput,
        recvInput,
        getConnectionManager,
        getMapping,
        setMapping,
        getTopic,
        setTopic,
        inputTopic,
        getUsers,
        setUsers,
        inputLine,
        outputLine,
        outputLineLastFocused,
        getNick,
        setNick,
        getName,
        setName,
        getTitle,
        setTitle
        hasFocus,
        setFocus,
        getLastFocus,
        setLastFocus
        getNotifications,
        notify,
        notifyLastFocused)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Interface as I
import Network.IRC.Client.Amphibian.Channel as C
import Network.IRC.Client.Amphibian.User as U
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newBroadcastTChan,
                               peekTChan,
                               readTChan,
                               writeTChan,
                               dupTChan)
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.ByteString as B

-- | Create a new frame.
new :: Maybe Frame -> FrameMapping -> T.Text -> T.Text -> T.Text -> STM Frame
new parent mapping nick name title = do
  inputEvents <- newBroadcastTChan
  outputEvents <- newBroadcastTChan
  mapping' <- newTVar mapping
  topic <- newTVar Nothing
  users <- newTVar Nothing
  nick' <- newTVar nick
  name' <- newTVar name
  title' <- newTVar title
  parent' <- newTVar parent
  children <- newTVar []
  focus <- newTVar False
  lastFocus <- newTVar Nothing
  notifications <- newTVar []
  let frame = Frame { framInputEvents = inputEvents,
                      framOutputEvents = outputEvents,
                      framMapping = mapping',
                      framTopic = topic,
                      framUsers = users,
                      framNick = nick'
                      framName = name'
                      framTitle = title',
                      framParent = parent',
                      framChildren = children,
                      framFocus = focus,
                      framLastFocus = lastFocus,
                      framNotifications = notifications }
  case parent of
    Just parent -> do
      parentNotifications <- readTVar $ framChildren parent
      writeTVar (framChildren parent) (frame : parentNotifications)
    Nothing -> return ()
  return frame

-- | Subscribe to output events from a frame.
subscribeOutput :: Frame -> STM FrameOutputSubscription
subscribeOutput frame = FrameOutputSubscription <$> dupTChan $ framOutputEvents frame

-- | Subscribe to input events from a frame.
subscribeInput :: Frame -> STM FrameInputSubscription
subscribeInput frame = FrameInputSubscription <$> dupTChan $ frameInputEvents frame

-- | Peek output events from a frame.
peekOutput :: FrameOutputSubscription -> STM FrameOutputEvent
peekOutput (FrameOutputSubscription subscription) = peekTChan subscription

-- | Peek input events from a frame.
peekInput :: FrameInputSubscription -> STM FrameInputEvent
peekInput (FrameInputSubscription subscription) = peekTChan subscription 

-- | Receive output events from a frame.
recvOutput :: FrameOutputSubscription -> STM FrameOutputEvent
recvOutput (FrameOutputSubscription subscription) = readTChan subscription

-- | Receive input events from a frame.
recvInput :: FrameInputSubscription -> STM FrameInputEvent
recvInput (FrameInputSubscription subscription) = readTChan subscription 

-- | Get connection manager for a frame.
getConnectionManager :: Frame -> STM (Maybe ConnectionManager)
getConnectionManager frame = do
  mapping <- readTVar $ framMapping frame
  case mapping of
    FrmaConnectionManager manager -> Just manager
    FrmaChannel channel -> Just <$> C.getConnectionManager channel
    FrmaUser user -> Just <$> U.getConnectionManager user
    FrmaNone -> Nothing

-- | Get mapping for a frame.
getMapping :: Frame -> STM FrameMapping
getMapping = readTVar . framMapping

-- | Set mapping for a frame.
setMapping :: Frame -> FrameMapping -> STM ()
setMapping frame mapping = do
  writeTVar (framMapping frame) mapping
  writeTChan (framInputEvents frame) (FievMapping mapping)
  writeTChan (framOutputEvents frame) (FoevMapping mapping)

-- | Get topic for a frame.
getTopic :: Frame -> STM (Maybe T.Text)
getTopic = readTVar . framTopic

-- | Set topic for a frame.
setTopic :: Frame -> Maybe T.Text -> STM ()
setTopic frame topic = do
  writeTVar (framTopic frame) topic
  writeTChan (framOutputEvents frame) (FoevTopic topic)

-- | Input topic for a frame.
inputTopic :: Frame -> T.Text -> STM ()
inputTopic frame topic = do
  writeTVar (framTopic frame) (Just topic)
  writeTChan (framInputEvents frame (FievTopic topic)

-- | Get users for a frame.
getUsers :: Frame -> STM (Maybe [(T.Text, UserStatus)])
getUsers = readTVar . framUsers

-- | Set users for a frame.
setUsers :: Frame -> Maybe [(T.Text, UserStatus)] -> STM ()
setUsers frame users = do
  writeTVar (framUsers frame) users
  writeTChan (framOutputEvents frame) (FoevUsers users)

-- | Input a line from a frame.
inputLine :: Frame -> StyledText -> STM ()
inputLine frame line = writeTChan (framInputEvents frame) (FievLine line)

-- | Output a line to a frame.
outputLine :: Frame -> FrameLine -> STM ()
outputLine frame line = writeTChan (framOutputEvents frame) (FoevLine line)

-- | Output a line to a frame or child of a frame that was focused last.
outputLineLastFocused :: Frame -> FrameLine -> STM ()
outputLineLastFocused frame line = do
  targetFrame <- readTVar $ framLastFocus frame
  case targetFrame of
    Just targetFrame -> outputLine targetFrame line
    Nothing -> outputLine frame line

-- | Get the nick of a frame.
getNick :: Frame -> STM T.Text
getNick = readTVar . frameNick

-- | Set the nick of a frame.
setNick :: Frame -> T.Text -> STM ()
setNick frame nick = do
  writeTVar (framNick frame) nick
  writeTChan (framOutputEvents frame) (FoevNick nick)

-- | Get the name of a frame.
getName :: Frame -> STM T.Text
getName = readTVar . frameName

-- | Set the name of a frame.
setName :: Frame -> T.Text -> STM ()
setName frame name = do
  writeTVar (framName frame) name
  writeTChan (framOutputEvents frame) (FoevName name)

-- | Get the title of a frame.
getTitle :: Frame -> STM T.Text
getTitle = readTVar . frameTitle

-- | Set the title of a frame.
setTitle :: Frame -> T.Text -> STM ()
setTitle frame title = do
  writeTVar (framTitle frame) title
  writeTChan (framOutputEvents frame) (FoevTitle title)

-- | Get whether a frame has focus.
hasFocus :: Frame -> STM Bool
hasFocus = readTVar . framFocus

-- | Set whether a frame has focus.
setFocus :: Frame -> Bool -> STM ()
setFocus frame focus = do
  writeTVar (framFocus frame) focus
  writeTChan (framInputEvents frame) (FievFocus focus)
  if focus
  then do writeTVar (framNotifications frame) []
          writeTChan (framOutputEvents frame) (FoevNotifications [])
          setLastFocus frame frame
  else return ()

-- | Get the frame with last focus.
getLastFocus :: Frame -> STM (Maybe Frame)
getLastFocus = readTVar . framLastFocus

-- | Set the frame with last focus.
setLastFocus :: Frame -> Frame -> STM ()
setLastFocus frame focusedFrame = do
  writeTVar (framLastFocus frame) (Just focusedFrame)
  parent <- readTVar $ framParent frame
  case parent of
    Just parent -> setLastFocus parent focusedFrame
    Nothing -> return ()

-- | Get notifications for a frame.
getNotifications :: Frame -> STM [FrameNotifications]
getNotifications = readTVar . frameNotifications

-- | Notify a frame.
notify :: Frame -> [FrameNotifications] -> STM ()
notify frame notifications = do
  oldNotifications <- readTVar $ framNotifications frame
  let newNotifications = unique $ notifications ++ oldNotifications
  writeTVar (framNotifications frame) newNotifications
  writeTChan (framOutputEvents frame) (FoevNotifications newNotifications)

-- | Notify a frame or child of a frame that was focused last.
notifyLastFocused :: Frame -> [FrameNotifications] -> STM ()
notifyLastFocused frame notifications = do
  lastFocus <- getLastFocus frame
  case lastFocus of
    Just lastFocus -> notify lastFocus notifications
    Nothing -> notify frame notifications
