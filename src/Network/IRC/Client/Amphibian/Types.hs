-- Copyright (c) 2017, Travis Bemann
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- o Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- o Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- o Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Network.IRC.Client.Amphibian.Types

  (Response(..),
   Error(..),
   Connection(..),
   ConnectionState(..),
   ConnectionAction(..),
   ConnectionEvent(..),
   ConnectionEventSub(..),
   IRCConnection(..),
   IRCConnectionState(..),
   IRCConnectionAction(..),
   IRCConnectionEvent(..),
   IRCConnectionEventSub(..),
   IRCMessage(..),
   Window(..),
   WindowState(..),
   WindowAction(..),
   WindowEvent(..),
   WindowEventSub(..),
   Tab(..),
   TabState(..),
   TabEvent(..),
   TabEventSub(..),
   TabUser(..),
   TabUserState(..),
   TabUserEvent(..),
   TabUserEventSub(..),
   UserType(..),
   Session(..),
   SessionState(..),
   Channel(..),
   ChannelState(..),
   User(..),
   Mode(..),
   Delay(..),
   ClientTabSubtype(..),
   ClientWindow(..),
   Notification(..),
   ClientTab(..),
   Client(..),
   ClientTaggedEvent(..),
   Settings(..),
   Log(..),
   History(..),
   Style(..),
   StyleAndColor(..))
   
where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Network.Socket as NS
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.Word (Word8)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TMVar (TMVar)
import System.IO (Handle)

-- | Responses
newtype Response a = Response (TMVar (Either Error a))

-- | Errors
newtype Error = Error T.Text
                deriving (Eq, Show)

-- | Connection type
data Connection = Connection
  { connectionState :: TVar ConnectionState,
    connectionHostname :: TMVar NS.HostName,
    connectionAddress :: TMVar NS.AddrInfo,
    connectionPort :: TMVar NS.PortNumber,
    connectionSocket :: TMVar NS.Socket,
    connectionActionQueue :: TQueue ConnectionAction,
    connectionEventQueue :: TChan ConnectionEvent }

-- | Connection state
data ConnectionState = ConnectionNotStarted
                     | ConnectionStarted
                     | ConnectionFindingAddr
                     | ConnectionNoAddrFound Error
                     | ConnectionLookupCanceled
                     | ConnectionFindingName
                     | ConnectionNoNameFound Error
                     | ConnectionReverseLookupCanceled
                     | ConnectionConnecting
                     | ConnectionConnected
                     | ConnectionConnectingFailed Error
                     | ConnectionConnectingCanceled
                     | ConnectionDisconnected
                     | ConnectionDisconnectError Error
                     | ConnectionDisconnectedByPeer
                     | ConnectionRecvError Error
                     | ConnectionSendError Error
                     deriving (Eq, Show)

-- | Connection action type
data ConnectionAction = Connect NS.HostName NS.PortNumber (Response ())
                      | Disconnect (Response ())
                      | SendData B.ByteString (Response ())
                      | StopConnection (Response ())

-- | Connection event type
data ConnectionEvent = FoundAddr NS.AddrInfo
                     | NoAddrFound Error
                     | LookupCanceled
                     | FoundName NS.HostName
                     | NoNameFound Error
                     | ReverseLookupCanceled
                     | ConnectingFailed Error
                     | Connected
                     | ConnectingCanceled
                     | Disconnected
                     | DisconnectError Error
                     | DisconnectedByPeer
                     | SendError Error
                     | RecvError Error
                     | RecvData B.ByteString
                     | ConnectionStopped
                     deriving (Eq, Show)

-- | Connection event subscription type
newtype ConnectionEventSub = ConnectionEventSub (TChan ConnectionEvent)

-- | IRC connection type
data IRCConnection = IRCConnection
  { ircConnectionConnection :: Connection,
    ircConnectionState :: TVar IRCConnectionState,
    ircConnectionBuffer :: TVar B.ByteString,
    ircConnectionActionQueue :: TQueue IRCConnectionAction,
    ircConnectionEventQueue :: TChan IRCConnectionEvent }

-- | IRC connection state
data IRCConnectionState = IRCConnectionNotStarted
                        | IRCConnectionStarted
                        | IRCConnectionFindingAddr
                        | IRCConnectionNoAddrFound Error
                        | IRCConnectionLookupCanceled
                        | IRCConnectionFindingName
                        | IRCConnectionNoNameFound Error
                        | IRCConnectionReverseLookupCanceled
                        | IRCConnectionConnecting
                        | IRCConnectionConnected
                        | IRCConnectionConnectingFailed Error
                        | IRCConnectionConnectingCanceled
                        | IRCConnectionDisconnected
                        | IRCConnectionDisconnectError Error
                        | IRCConnectionDisconnectedByPeer
                        | IRCConnectionRecvError Error
                        | IRCConnectionSendError Error
                        deriving (Eq, Show)

-- | IRC connection action type
data IRCConnectionAction = ConnectIRC NS.HostName NS.PortNumber (Response ())
                         | DisconnectIRC (Response ())
                         | SendIRCMessage IRCMessage (Response ())
                         | StopIRCConnection (Response ())

-- | IRC connection event type
data IRCConnectionEvent = IRCFoundAddr NS.AddrInfo
                        | IRCNoAddrFound Error
                        | IRCLookupCanceled
                        | IRCFoundName NS.HostName
                        | IRCNoNameFound Error
                        | IRCReverseLookupCanceled
                        | IRCConnectingFailed Error
                        | IRCConnected
                        | IRCConnectingCanceled
                        | IRCDisconnected
                        | IRCDisconnectError Error
                        | IRCDisconnectedByPeer
                        | IRCSendError Error
                        | IRCRecvError Error
                        | IRCRecvMessage IRCMessage
                        | IRCConnectionStopped
                     deriving (Eq, Show)

-- | IRC connection event subscription type
newtype IRCConnectionEventSub = IRCConnectionEventSub (TChan IRCConnectionEvent)

-- | IRC message type
data IRCMessage = IRCMessage
  { ircMessagePrefix :: Maybe B.ByteString,
    ircMessageCommand :: B.ByteString,
    ircMessageParams :: S.Seq B.ByteString,
    ircMessageCoda :: Maybe B.ByteString }
  deriving (Eq, Show)

-- | IRC window type
data Window = Window
  { windowWindow :: TMVar Gtk.Window,
    windowNotebook :: TMVar Gtk.Notebook,
    windowTitle :: TMVar T.Text,
    windowTabs :: TVar (S.Seq Tab),
    windowNextTabIndex :: TVar Integer,
    windowState :: TVar WindowState,
    windowActionQueue :: TQueue WindowAction,
    windowEventQueue :: TChan WindowEvent }

-- | IRC window state type
data WindowState = WindowNotStarted
                 | WindowNotShown
                 | WindowShown
                 deriving (Eq)

-- | IRC window action type
data WindowAction = OpenWindow T.Text (Response ())
                  | CloseWindow (Response ())
                  | OpenTab T.Text T.Text (Response Tab)
                  | CloseTab Tab (Response ())
                  | SetWindowTitle T.Text (Response ())
                  | StopWindow (Response ())
                  | SetTabTitleText Tab T.Text (Response ())
                  | SetTabTitleStyle Tab T.Text (Response ())
                  | SetTabTitleTextAndStyle Tab T.Text T.Text (Response ())
                  | AddTabText Tab T.Text (Response ())
                  | SetEntry Tab T.Text (Response ())
                  | SetTopicVisible Tab Bool (Response ())
                  | SetTopic Tab T.Text (Response ())
                  | SetSideVisible Tab Bool (Response ())
                  | AddTabUser Tab B.ByteString UserType (Response TabUser)
                  | RemoveTabUser TabUser (Response ())
                  | FindTabUser Tab B.ByteString (Response (Maybe TabUser))

-- | IRC window event type
data WindowEvent = WindowClosed
                 | WindowFocused
                 deriving (Eq, Show)

-- | IRC window event subscription
newtype WindowEventSub = WindowEventSub (TChan WindowEvent)

-- | IRC tab type
data Tab = Tab
  { tabIndex :: Integer,
    tabWindow :: Window,
    tabTextView :: Gtk.TextView,
    tabTextBuffer :: Gtk.TextBuffer,
    tabEntry :: Gtk.Entry,
    tabTopicEntry :: Gtk.Entry,
    tabSideListBox :: Gtk.ListBox,
    tabSideBox :: Gtk.Box,
    tabBodyBox :: Gtk.Box,
    tabLabel :: Gtk.Label,
    tabTabBox :: Gtk.Box,
    tabNextUserIndex :: TVar Integer,
    tabUsers :: TVar (S.Seq TabUser),
    tabTitleText :: TVar T.Text,
    tabTitleStyle :: TVar T.Text,
    tabState :: TVar TabState,
    tabEventQueue :: TChan TabEvent }

-- | Show instance for tabs.
instance Show Tab where
  show tab = "Tab " ++ show (tabIndex tab)

-- | Eq instance for tabs.
instance Eq Tab where
  tab0 == tab1 = tabIndex tab0 == tabIndex tab1

-- | IRC tab state type
data TabState = TabIsOpen
              | TabIsClosed
              deriving (Eq, Show)

-- | IRC tab event type
data TabEvent = TabClosed
              | LineEntered T.Text
              | TopicEntered T.Text
              | TabSelected
              | UpPressed
              | DownPressed
              deriving (Eq, Show)

-- | IRC tab event subscription
newtype TabEventSub = TabEventSub (TChan TabEvent)

-- | IRC tab user type
data TabUser = TabUser
  { tabUserTab :: Tab,
    tabUserIndex :: Integer,
    tabUserType :: UserType,
    tabUserNick :: B.ByteString,
    tabUserLabel :: Gtk.Label,
    tabUserRow :: Gtk.ListBoxRow,
    tabUserState :: TVar TabUserState,
    tabUserEventQueue :: TChan TabUserEvent }

-- | Show instance for tab users.
instance Show TabUser where
  show tabUser = "TabUser " ++ show (tabIndex $ tabUserTab tabUser) ++ " " ++
             show (tabUserIndex tabUser)

-- | Eq instance for tab users.
instance Eq TabUser where
  tabUser0 == tabUser1 =
    (tabIndex $ tabUserTab tabUser0) == (tabIndex $ tabUserTab tabUser1) &&
    tabUserIndex tabUser0 == tabUserIndex tabUser1

-- | Eq instance for tab users.
instance Ord TabUser where
  compare tabUser0 tabUser1 =
    if tabUser0 == tabUser1
    then EQ
    else
      let userTypeOrder = compare (tabUserType tabUser0) (tabUserType tabUser1)
      in if userTypeOrder == EQ
         then compare (tabUserNick tabUser0) (tabUserNick tabUser1)
         else userTypeOrder

-- | IRC tab user state type
data TabUserState = TabUserIsOpen
                  | TabUserIsClosed
                  deriving (Eq, Show)

-- | IRC tab user event type
data TabUserEvent = TabUserClosed
                  deriving (Eq, Show)

-- | IRC tab user event subscription
newtype TabUserEventSub = TabUserEventSub (TChan TabUserEvent)

-- | IRC user type type
data UserType = OwnerUser
              | AdminUser
              | OpUser
              | HalfOpUser
              | VoiceUser
              | NormalUser
              deriving (Eq, Show, Ord)

-- | IRC session type
data Session = Session
  { sessionIndex :: Integer,
    sessionState :: TVar SessionState,
    sessionReconnectOnFailure :: TVar Bool,
    sessionIRCConnection :: IRCConnection,
    sessionIRCConnectionEventSub :: IRCConnectionEventSub,
    sessionHostname :: TVar NS.HostName,
    sessionOrigHostname :: TVar NS.HostName,
    sessionPort :: TVar NS.PortNumber,
    sessionNick :: TVar B.ByteString,
    sessionUsername :: TVar B.ByteString,
    sessionRealName :: TVar B.ByteString,
    sessionMode :: TVar (S.Seq Mode),
    sessionChannels :: TVar (S.Seq Channel),
    sessionUsers :: TVar (S.Seq User),
    sessionReconnectingLock :: TMVar (),
    sessionReconnecting :: TVar (Maybe (Async ())),
    sessionPinging :: TVar (Maybe (Async ())),
    sessionPongCount :: TVar Integer }

-- | IRC session state
data SessionState = SessionInactive
                  | SessionConnecting
                  | SessionPreparing
                  | SessionReady
                  | SessionDestroyed
                  deriving (Eq, Show)

-- | Log type.
data Log = Log
  { logHandle :: TVar (Maybe Handle),
    logText :: TVar (S.Seq T.Text) }

-- | History type.
data History = History
  { historyHandle :: TVar (Maybe Handle),
    historyLines :: TVar (S.Seq T.Text),
    historyPosition :: TVar (Maybe Int) }

-- | IRC channel type
data Channel = Channel
  { channelIndex :: Integer,
    channelSession :: Session,
    channelState :: TVar ChannelState,
    channelName :: B.ByteString,
    channelUsers :: TVar (S.Seq User),
    channelTopic :: TVar (Maybe B.ByteString),
    channelMode :: TVar (S.Seq Mode),
    channelLog :: Log }

-- | IRC channel state type
data ChannelState = InChannel
                  | NotInChannel
                  | AwaitingReconnect
                  deriving (Eq, Show)

-- | IRC user type
data User = User
  { userIndex :: Integer,
    userSession :: Session,
    userNick :: TVar B.ByteString,
    userType :: TVar (S.Seq (Channel, S.Seq UserType)),
    userLog :: Log }

-- | IRC mode type
newtype Mode = Mode Word8
               deriving (Eq, Show)

-- | Reconnect with or without delay.
data Delay = WithDelay
           | WithoutDelay
           deriving (Eq, Show)

-- | Client tab subtype type
data ClientTabSubtype = FreeTab
                      | SessionTab Session
                      | ChannelTab Channel
                      | UserTab User

-- | Client window type
data ClientWindow = ClientWindow
  { clientWindowIndex :: Integer,
    clientWindowFocusIndex :: TVar Integer,
    clientWindowWindow :: Window,
    clientWindowEventSub :: WindowEventSub }

-- | Notification level
data Notification = NoNotification
                  | UserChanged
                  | Messaged
                  | Noticed
                  | Mentioned
                  deriving (Eq, Show, Ord)

-- | Client tab type
data ClientTab = ClientTab
  { clientTabIndex :: Integer,
    clientTabSelectIndex :: TVar Integer,
    clientTabTab :: Tab,
    clientTabEventSub :: TabEventSub,
    clientTabSubtype :: TVar ClientTabSubtype,
    clientTabWindow :: ClientWindow,
    clientTabNotification :: TVar Notification,
    clientTabHistory :: History }

-- | Client type
data Client = Client
  { clientRunning :: TVar Bool,
    clientNextIndex :: TVar Integer,
    clientNextTabSelectIndex :: TVar Integer,
    clientNextWindowFocusIndex :: TVar Integer,
    clientSessions :: TVar (S.Seq Session),
    clientChannels :: TVar (S.Seq Channel),
    clientUsers :: TVar (S.Seq User),
    clientWindows :: TVar (S.Seq ClientWindow),
    clientTabs :: TVar (S.Seq ClientTab),
    clientSettings :: TVar Settings }

-- | Client tagged event type
data ClientTaggedEvent = TaggedSessionEvent Session IRCConnectionEvent
                       | TaggedClientWindowEvent ClientWindow WindowEvent
                       | TaggedClientTabEvent ClientTab TabEvent
                       | TaggedClientQuitEvent

-- | Settings type
data Settings = Settings { settingsReconnectDelay :: Double,
                           settingsPongWaitDelay :: Double }

-- | Style
data Style = Bold
           | Italic
           | Underline
           | Reverse
           deriving (Eq, Show)

-- | Style and color
data StyleAndColor =
  StyleAndColor { currentStyle :: S.Seq Style,
                  currentForeground :: Int,
                  currentBackground :: Int }
  deriving (Eq, Show)
