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
   IRCMessage(..))
   
where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Network.Socket as NS
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TMVar (TMVar)

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
    windowState :: TVar WindowState,
    windowActions:: TQueue WindowAction,
    windowEvents :: TChan WindowEvent }

-- | IRC window state type
data WindowState = WindowNotStarted
                 | WindowNotShown
                 | WindowShown

-- | IRC window action type
data WindowAction = OpenWindow T.Text (Response ())
                  | CloseWindow (Response ())
                  | OpenTab T.Text (Response Tab)
                  | CloseTab Tab (Response ())
                  | SetWindowTitle T.Text (Response ())
                  | StopWindow (Response ())
                  | SetTabTitle Tab T.Text (Response ())
                  | AddTabText Tab T.Text (Response ())

-- | IRC window event type
data WindowEvent = WindowClosed
                 | UserOpenedTab Tab

-- | IRC window event subscription
newtype WindowEventSub = WindowEventSub (TChan WindowEvent)

-- | IRC tab type
data Tab = Tab
  { tabWindow :: Window,
    tabTextView :: Gtk.TextView,
    tabTextBuffer :: Gtk.TextBuffer,
    tabEntry :: Gtk.Entry,
    tabBodyBox :: Gtk.Box,
    tabLabel :: Gtk.Label,
    tabTabBox :: Gtk.Box,
    tabState :: TVar TabState,
    tabEvents :: TChan TabEvent }

-- | IRC tab state type
data TabState = TabIsOpen
              | TabIsClosed

-- | IRC tab action type
data TabAction = 

-- | IRC tab event type
data TabEvent = TabClosed
              | LineEntered T.Text

-- | IRC tab event subscription
newtype TabEventSub = TabEventSub (TChan TabEvent)
