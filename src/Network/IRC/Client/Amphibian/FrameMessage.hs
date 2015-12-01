{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Client.Amphibian.FrameMessage

       (setTitle,
        lookupAddressMessage,
        lookupAddressFailedMessage,
        reverseLookupFailedMessage,
        connectingMessage,
        connectedMessage,
        connectErrorMessage,
        disconnectMessage,
        disconnectErrorMessage,
        passwordMismatchMessage,
        bannedFromServerMessage,
        bannedFromServerCommentMessage,
        welcomeMessage,
        welcomeCommentMessage,
        attemptingNickMessage,
        malformedNickMessage,
        joinedMessage,
        partedMessage,
        partedCommentMessage,
        noTopicMessage,
        topicMessage,
        topicWhoTimeMessage,
        namesMessage,
        namesDisplayMessage,
        recvJoinMessage,
        recvPartMessage,
        recvPartCommentMessage,
        recvNickMessage,
        recvTopicMessage,
        recvMessageMessage,
        recvActionMessage,
        recvNoticeMessage,
        motdMessage,
        recvSelfNickMessage,
        selfMessageMessage,
        selfActionMessage,
        selfNoticeMessage,
        unknownCommandMessage,
        badCommandSyntaxMessage,
        unboundFrameMessage,
        notInChannelMessage,
        notAChannelMessage,
        errorMessage)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import Network.IRC.Client.Amphibian.Format
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Control.Monad (mapM,
                      mapM_)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (isSpace)
import Text.Printf
import Data.Time.Clock (UTCTime,
                        getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (TimeLocale)

-- | Decode text sent to a frame.
decode :: Frame -> B.ByteString -> AM T.Text
decode frame bytes = do
  intf <- getInterface
  liftIO . atomically $ decodeFrame intf frame bytes

-- | Set title of frame.
setTitle :: Interface -> Frame -> Maybe T.Text -> Maybe T.Text -> STM ()
setTitle intf frame (Just serverName) (Just nickOrChannel) = do
  let textMap = HM.insert "server" (formatText serverName) HM.empty
  let textMap' = HM.insert "nickOrChannel" (formatText nickOrChannel') textMap
  formatText <- I.lookupText intf "Amphibian: %server:s / %nickOrChannel:s"
  let formattedText = format formatText textMap'
  F.setTitle frame formattedText
setTitle intf frame (Just serverName) Nothing = do
  let textMap = HM.insert "server" (formatText serverName) HM.empty
  formatText <- I.lookupText intf "Amphibian: %server:s"
  let formattedText = format formatText textMap
  F.setTitle frame formattedText
setTitle intf frame Nothing _ = do
  text <- I.lookupText intf "Amphibian: no server"
  F.setTitle frame text

-- | Send a lookup hostname message to a specific frame.
lookupHostnameMessage :: Frame -> HostName -> AM ()
lookupHostnameMessage frame hostName = do
  let textMap = HM.insert "hostName" (formatText $ T.pack hostName) HM.empty
  formatText <- lookupText "Looking up %hostName:s..."
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [TxstForeColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoLookupHostname]

-- | Send a lookup address failed message to a specific frame.
lookupAddressFailedMessage :: Frame -> Error -> AM ()
lookupAddressFailedMessage frame (Error errorLines) = do
  text <- lookupText "Address lookup failed"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, ": ", line] }
      restLines =
        case errorLines of
          _ : rest -> map (makeRestLine time text) rest
          [] -> []
  liftIO . atomically $ do
    F.outputLine frame headLine
    mapM_ (F.outputLine frame) restLines
    F.notify frame [FrnoLookupAddressFailed]
  where makeRestLine time messageText line =
    FrameLine { frliTime = time,
                frliSource = ST.addStyle [TxstForeColor 13] "*",
                frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) " ") line }
     
-- | Send a reverse lookup failed message to a specific frame.
reverseLookupFailedMessage :: Frame -> Error -> AM ()
reverseLookupFailedMessage frame (Error errorLines) = do
  text <- lookupText "Reverse hostname lookup failed"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, ": ", line] }
      restLines =
        case errorLines of
          _ : rest -> map (makeRestLine time text) rest
          [] -> []
  liftIO . atomically $ do
    F.outputLine frame headLine
    mapM_ (F.outputLine frame) restLines
    F.notify frame [FrnoReverseLookupFailed]
  where makeRestLine time messageText line =
    FrameLine { frliTime = time,
                frliSource = ST.addStyle [TxstForeColor 13] "*",
                frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) " ") line }

-- | Send a connecting message to a specific frame.
connectingMessage :: Frame -> HostName -> Port -> AM ()
connectingMessage frame hostName port = do
  let textMap = HM.insert "hostName" (formatText $ T.pack hostName) HM.empty
  let textMap' = HM.insert "port" (formatIntegral port) textMap
  formatText <- lookupText "Connecting to %hostName:s port %port:d..."
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [TxstForeColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoConnecting]

-- | Send a connected message to a specific frame.
connectedMessage :: Frame -> HostName -> Port -> AM ()
connectedMessage frame hostName port = do
  let textMap = HM.insert "hostName" (formatText $ T.pack hostName) HM.empty
  let textMap' = HM.insert "port" (formatIntegral port) textMap
  formatText <- lookupText "Connected to %hostName:s port %port:d"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [TxstForeColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoConnected]

-- | Send a connect failed message to a specific frame.
connectFailedMessage :: Frame -> Error -> AM ()
connectFailedMessage frame (Error errorLines) = do
  text <- lookupText "Connect failed"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, ": ", line] }
      restLines =
        case errorLines of
          _ : rest -> map (makeRestLine time text) rest
          [] -> []
  liftIO . atomically $ do
    F.outputLine frame headLine
    mapM_ (F.outputLine frame) restLines
    F.notify frame [FrnoConnectFailed]
  where makeRestLine time messageText line =
    FrameLine { frliTime = time,
                frliSource = ST.addStyle [TxstForeColor 13] "*",
                frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) " ") line }

-- | Send a disconnect message to a specific frame.
disconnectMessage :: Frame -> AM ()
disconnectMessage frame = do
  text <- lookupText "Disconnected"
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [] text }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send a disconnect error message to a specific frame.
disconnectErrorMessage :: Frame -> Error -> AM ()
disconnectErrorMessage frame (Error errorLines) = do
  text <- lookupText "Disconnected"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, ": ", line] }
      restLines =
        case errorLines of
          _ : rest -> map (makeRestLine time text) rest
          [] -> []
  liftIO . atomically $ do
    F.outputLine frame headLine
    mapM_ (F.outputLine frame) restLines
    F.notify frame [FrnoDisconnected]
  where makeRestLine time messageText line =
    FrameLine { frliTime = time,
                frliSource = ST.addStyle [TxstForeColor 13] "*",
                frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) " ") line }

-- | Send a password mismatch message to a specific frame.
passwordMismatchMessage :: Frame -> Password -> AM ()
passwordMismatchMessage frame password = do
  password' <- decode frame password
  let textMap = HM.insert "password" (formatText password') HM.empty
  formatText <- lookupText "Password mismatch"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoConnected]

-- | Send a banned from server message to a specific frame.
bannedFromServerMessage :: Frame -> AM ()
bannedFromServerMessage frame = do
  text <- lookupText "Banned from server"
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [] text }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoBannedFromServer]

-- | Send a banned from server comment message to a specific frame.
bannedFromServerCommentMessage :: Frame -> MessageComment -> AM ()
bannedFromServerCommentMessage frame comment = do
  styledText <- ST.decode <$> decode frame comment
  time <- liftIO getCurrentTime
let       line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = styledText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoBannedFromServer]

-- | Send a welcome message to a specific frame.
welcomeMessage :: Frame -> Nick -> AM ()
welcomeMessage frame nick = do
  nick' <- decode frame nick
  let textMap = HM.insert "nick" (formatText nick') HM.empty
  formatText <- lookupText "Welcome to IRC %nick:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send a welcome comment message to a specific frame.
welcomeCommentMessage :: Frame -> MessageComment -> AM ()
welcomeCommentMessage frame comment = do
  styledText <- ST.decode <$> decode frame comment
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = styledText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send an attempting nick message to a specific frame.
attemptingNickMessage :: Frame -> Nick -> AM ()
attemptingNickMessage frame nick = do
  nick' <- decode frame nick
  let textMap = HM.insert "nick" (formatText nick') HM.empty
  formatText <- lookupText "Attempting to use nick %nick:s..."
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstForeColor 13] "*",
                         frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                         frliBody = ST.addStyle [TxstForeColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send a malformed nick message to a specific frame.
malformedNickMessage :: Frame -> Nick -> AM ()
malformedNickMessage frame nick = do
  nick' <- decode frame nick
  let textMap = HM.insert "nick" (formatText nick') HM.empty
  formatText <- lookupText "Malformed nick %nick:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoDisconnected]

-- | Send a joined message to a specific frame.
joinedMessage :: Frame -> ChannelName -> AM ()
joinedMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert "channel" (formatText name') HM.empty
  formatText <- lookupText "Now talking on %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime time,
                                     frliSource = ST.addStyle [TxstForeColor 3] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 3] "*",
                                     frliBody = ST.addStyle [TxstForeColor 3] formattedText }
    F.notify frame [FrnoJoined]

-- | Send a parted message to a specific frame.
partedMessage :: Frame -> ChannelName -> AM ()
partedMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert "channel" (formatText name') HM.empty
  formatText <- lookupText "You have left channel %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliBody = ST.addStyle [TxstForeColor 7] formattedText }
    F.notify frame [FrnoParted]

-- | Send a parted message with a comment to a specific frame.
partedCommentMessage :: Frame -> ChannelName -> MessageComment -> AM ()
partedCommentMessage frame name comment = do
  name' <- decode frame name
  comment' <- ST.decode <$> decode frame comment
  let textMap = HM.insert "channel" (formatText name') HM.empty
  formatText <- lookupText "You have left channel %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliBody = ST.concat [ST.addStyle [TxstForeColor 7] formattedText,
                                                           ST.addStyle [TxstForeColor 7] " (",
                                                           ST.mergeStyle [TxstForeColor 7] commment',
                                                           ST.addStyle [TxstForeColor 7] ")"] }
                                                           
    F.notify frame [FrnoParted]

-- | Send a no topic message to a specific frame.
noTopicMessage :: Frame -> ChannelName -> AM ()
noTopicMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert "channel" (formatText name') HM.empty
  formatText <- lookupText "No topic is set for %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoNoTopic]  

-- | Send a topic message to a specific frame.
topicMessage :: Frame -> ChannelName -> ChannelTopic -> AM ()
topicMessage frame name topic = do
  name' <- decode frame name
  topic' <- ST.decode <$> decode frame topic
  let textMap = HM.insert "channel" (formatText name') HM.empty
  formatText <- lookupText "Topic for %channel:s is"
  let formattedText = format (T.append formatText ": ") textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.concat [ST.addStyle [TxstForeColor 13] formattedText,
                                                           ST.mergeStyle [TxstForeColor 13] topic'] }
    F.notify frame [FrnoTopic]

-- | Send a topic user/time message to a specific frame.
topicWhoTimeMessage :: Frame -> ChannelName -> FullName -> UTCTIme -> AM ()
topicWhoTimeMessage frame name user time = do
  timeLocale <- confTimeLocale <$> getConfig
  name' <- decode frame name
  user' <- decode frame user
  timeText <- T.pack $ formatTime timeLocale "%c" time
  let textMap = HM.insert "channel" (formatText name') HM.empty
  let textMap' = HM.insert "user" (formatText user') textMap
  let textMap = HM.insert "time" (formatText timeText) textMap'
  formatText <- lookupText "Topic for %channel:s set by %user:s at %time:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.addStyle [TxstForeColor 13] formattedText }
    F.notify frame [FrnoTopicWhoTime]

-- | Send a names message to a specific frame.
namesMessage :: Frame -> [(Nick, UserStatus)] -> AM ()
namesMessage frame statusNicks = do
  statusNicks' <- mapM convertNick statusNicks
  liftIO . atomically $ F.setUsers frame statusNicks'
  where convertNick (nick, status) = do
          nick' <- decode frame nick
          return (nick', status)

-- | Send a names message to a specific frame with an actual displayed message.
namesDisplayMessage :: Frame -> ChannelName -> [(Nick, UserStatus)] -> AM ()
namesDisplayMessage frame name statusNicks = do
  name' <- decode frame name
  statusNicks' <- mapM convertNick statusNicks
  let textMap = HM.insert "channel" (formatText name') HM.empty
  formatText <- lookupText "Users on %channel:s"
  let formattedText = format (T.append formatText ": ") textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.setUsers frame statusNicks'
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.addStyle [TxstForeColor 10] formattedText }
    F.notify frame [FrnoNames]
  where convertNick (nick, status) = do
          nick' <- decode frame nick
          return (nick', status)

-- | Send a received join message to a specific frame.
recvJoinMessage :: Frame -> ChannelName -> Nick -> FullName -> AM ()
recvJoinMessage frame name nick user = do
  name' <- decode frame name
  nick' <- decode frame nick
  user' <- decode frame user
  let textMap = HM.insert "channel" (formatText name') HM.empty
  let textMap' = HM.insert "nick" (formatText nick') textMap
  let textMap = HM.insert "user" (formatText user') textMap'
  formatText <- lookupText "%nick:s (%user:s) has joined %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 3] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 3] "*",
                                     frliBody = ST.addStyle [TxstForeColor 3] formattedText }
    F.notify frame [FrnoRecvJoin]

-- | Send a received part message to a specific frame.
recvPartMessage :: Frame -> ChannelName -> Nick -> FullName -> AM ()
recvPartMessage frame name nick user = do
  name' <- decode frame name
  nick' <- decode frame nick
  user' <- decode frame user
  let textMap = HM.insert "channel" (formatText name') HM.empty
  let textMap' = HM.insert "nick" (formatText nick') textMap
  let textMap = HM.insert "user" (formatText user') textMap'
  formatText <- lookupText "%nick:s (%user:s) has left %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliBody = ST.addStyle [TxstForeColor 7] formattedText }
    F.notify frame [FrnoRecvPart]

-- | Send a received part message with a comment to a specific frame.
recvPartCommentMessage :: Frame -> ChannelName -> Nick -> FullName -> MessageComment -> AM ()
recvPartCommentMessage frame nick user comment = do
  name' <- decode frame name
  nick' <- decode frame nick
  user' <- decode frame user
  comment' <- ST.decode <$> decode frame comment
  let textMap = HM.insert "channel" (formatText name') HM.empty
  let textMap' = HM.insert "nick" (formatText nick') textMap
  let textMap = HM.insert "user" (formatText user') textMap'
  formatText <- lookupText "%nick:s (%user:s) has left %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliBody = ST.concat [ST.addStyle [TxstForeColor 7] formattedText,
                                                           ST.addStyle [TxstForeColor 7] " (",
                                                           ST.mergeStyle [TxstForeColor 7] commment',
                                                           ST.addStyle [TxstForeColor 7] ")"] }
                                                           
    F.notify frame [FrnoRecvPart]

-- | Send a received quit message to a specific frame.
recvQuitMessage :: Frame -> Nick -> FullName -> AM ()
recvQuitMessage frame nick user = do
  nick' <- decode frame nick
  user' <- decode frame user
  let textMap = HM.insert "nick" (formatText nick') HM.empty
  let textMap' = HM.insert "user" (formatText user') textMap
  formatText <- lookupText "%nick:s (%user:s) has quit"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliBody = ST.addStyle [TxstForeColor 7] formattedText }
    F.notify frame [FrnoRecvPart]

-- | Send a received quit message with a comment to a specific frame.
recvQuitCommentMessage :: Frame -> Nick -> FullName -> MessageComment -> AM ()
recvQuitCommentMessage frame nick user comment = do
  nick' <- decode frame nick
  user' <- decode frame user
  comment' <- ST.decode <$> decode frame comment
  let textMap = HM.insert "nick" (formatText nick') HM.empty
  let textMap' = HM.insert "user" (formatText user') textMap
  formatText <- lookupText "%nick:s (%user:s) has quit"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 7] "*",
                                     frliBody = ST.concat [ST.addStyle [TxstForeColor 7] formattedText,
                                                           ST.addStyle [TxstForeColor 7] " (",
                                                           ST.mergeStyle [TxstForeColor 7] commment',
                                                           ST.addStyle [TxstForeColor 7] ")"] }
                                                           
    F.notify frame [FrnoRecvPart]


-- | Send a received nick message to a specific frame.
recvNickMessage :: Frame -> Nick -> Nick-> AM ()
recvNickMessage frame oldNick newNick = do
  oldNick' <- decode frame oldNick
  newNick' <- decode frame newNick
  let textMap = HM.insert "oldNick" (formatText oldNick') HM.empty
  let textMap' = HM.insert "newNick" (formatText newNick') textMap
  formatText <- lookupText "%oldNick:s is now known as %newNick:s"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoRecvNick]

-- | Send a received topic message to a specific frame.
recvTopicMessage :: Frame -> Nick -> ChannelTopic -> AM ()
recvTopicMessage frame nick topic = do
  nick' <- decode frame nick
  topic' <- ST.decode <$> decode frame topic
  let textMap = HM.insert "nick" (formatText nick') HM.empty
  formatText <- lookupText "%nick:s has changed the topic to"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.concat [ST.addStyle [] formattedText,
                                                           ST.addStyle [] ": ",
                                                           topic'] }
                                                           
    F.notify frame [FrnoRecvTopic]

-- | Send a received message message to a specific frame.
recvMessageMessage :: Frame -> Nick -> MessageComment -> FrameMessageType -> AM ()
recvMessageMessage frame nick comment private = do
  manager <- liftIO . atomically $ F.getConnectionManager frame
  myNick <- liftIO . atomically $ CM.getNick manager
  myNick' <- decode frame myNick
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  if (length $ T.splitOn myNick' (ST.removeStyle comment')) > 1
  then
    liftIO . atomically $ do
      F.outputLine frame $ FrameLine { frliTime = time,
                                       frliSource = ST.addStyle [TxstBold, TxstForeColor 5] nick',
                                       frliAltSource =
                                         ST.concat [ST.addStyle [TxstForeColor 5] "<",
                                                    ST.addStyle [TxstBold, TxstForeColor 5] nick',
                                                    ST.addStyle [TxstForeColor 5] ">"],
                                       frliBody = ST.mergeStyle [TxstForeColor 5] comment' }
      case private of
        FrmtPrivate -> F.notify frame [FrnoRecvPrivateMessage, FrnoRecvMention]
        FrmtChannel F.notify frame [FrnoRecvChannelMessage, FrnoRecvMention]
  else
    liftIO . atomically $ do
      F.outputLine frame $ FrameLine { frliTime = time,
                                       frliSource = ST.addStyle [TxstForeColor 12] nick',
                                       frliAltSource =
                                         ST.concat [ST.addStyle [TxstForeColor 13] "<",
                                                    ST.addStyle [TxstBold, TxstForeColor 12] nick',
                                                    ST.addStyle [TxstForeColor 13] ">"],
                                       frliBody = comment' }
      case private of
        FrmtPrivate -> F.notify frame [FrnoRecvPrivateMessage]
        FrmtChannel -> F.notify frame [FrnoRecvChannelMessage]

-- | Send a received CTCP action message to a specific frame
recvActionMessage :: Frame -> Nick -> MessageComment -> FrameMessageType -> AM ()
recvActionMessage frame nick comment private = do
  manager <- liftIO . atomically $ F.getConnectionManager frame
  myNick <- liftIO . atomically $ CM.getNick manager
  myNick' <- decode frame myNick
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 12] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 12] "*",
                                     frliBody = ST.concat [ST.addStyle [TxstForeColor 12] nick',
                                                           ST.addStyle [] T.singleton ' ',
                                                           comment'] }
    case private of
      FrmtPrivate -> F.notify frame [FrnoRecvPrivateMessage]
      FrmtChannel -> F.notify frame [FrnoRecvChannelMessage]

-- | Send a received notice message to a frame.
recvNoticeMessage :: Frame -> Nick -> MessageComment -> FrameMessageType -> FrameTarget -> AM ()
recvNoticeMessage frame nick comment private target = do
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  let line =
    if nick /= T.empty
    then FrameLine { frliTime = time,
                     frliSource = ST.concat [ST.addStyle [TxstForeColor 12] "-",
                                             ST.addStyle [TxstForeColor 13] nick',
                                             ST.addStyle [TxstForeColor 12] "-"],
                     frliAltSource = ST.concat [ST.addStyle [TxstForeColor 12] "-",
                                                ST.addStyle [TxstForeColor 13] nick',
                                                ST.addStyle [TxstForeColor 12] "-"],
                     frliBody = comment' }
    else FrameLine { frliTime = time,
                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                     frliBody = comment' }
  liftIO . atomically $ do
    case target of
      FrtaLastFocused -> do
        F.outputLineLastFocused frame line
        case private of
          FrmtPrivate -> F.notifyLastFocused frame [FrnoPrivateNotice]
          FrmtChannel -> F.notifyLastFocused frame [FrnoChannelNotice]
      FrtaSpecific -> do
        F.outputLine frame line
        case private of
          FrmtPrivate -> F.notify frame [FrnoPrivateNotice]
          FrmtChannel -> F.notify frame [FrnoChannelNotice]

-- | Send a MOTD message to a specific frame.
motdMessage :: Frame -> [MessageComment] -> AM ()
motdMessage frame lines = do
  lines' <- mapM (\line -> ST.decode <$> decode frame line) lines
  time <- liftIO getCurrentTime
  lightBackground <- confLightBackground <$> getConfig
  let color = if lightBackground then 1 else 0
  liftIO . atomically $ mapM_ (F.outputLine frame . formatLine time color) lines
  where formatLine time color line =
          FrameLine { frliTime = time,
                      frliSource = ST.addStyle [TxstForeColor color] "*",
                      frliAltSource = ST.addStyle [TxstForeColor color] "*",
                      frliBody = line }

-- | Send a received self nick message to a specific frame.
recvSelfNickMessage :: Frame -> Nick -> Nick -> AM ()
recvSelfNickMessage frame oldNick newNick = do
  oldNick' <- decode frame oldNick
  newNick' <- decode frame newNick
  let textMap = HM.insert "oldNick" (formatText oldNick') HM.empty
  let textMap' = HM.insert "newNick" (formatText newNick') textMap
  formatText <- lookupText "You are now known as %newNick:s"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoRecvNick]


-- | Send a self message message to a specific frame.
selfMessageMessage :: Frame -> Nick -> MessageComment -> AM ()
selfMessageMessage frame nick comment = do
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  lightBackground <- confLightBackground <$> getConfig
  let color = if lightBackground then 1 else 0
  liftIO . atomically $
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor color] nick',
                                     frliAltSource = ST.concat [ST.addStyle [TxstForeColor color] "<",
                                                                ST.addStyle [TxstForeColor color] nick',
                                                                St.addStyle [TxstForeColor color] ">"],
                                     frliBody = ST.setBaseForeColor color comment' }

-- | Send a self action message to a specific frame.
selfActionMessage :: Frame -> Nick -> MessageComment -> AM ()
selfActionMessage frame nick comment = do
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  liftIO . atomically $
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstForeColor 12] "*",
                                     frliAltSource = ST.addStyle [TxstForeColor 12] "*",
                                     frliBody = ST.concat [ST.addStyle [TxstForeColor 12] nick',
                                                           ST.addStyle [] T.singleton ' ',
                                                           comment'] }

-- | Send a self notice message to a frame.
selfNoticeMessage :: Frame -> Nick -> MessageComment -> AM ()
selfNoticeMessage frame nick comment = do
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.concat [ST.addStyle [TxstForeColor 9] "-",
                                                             ST.addStyle [TxstForeColor 9] nick',
                                                             ST.addStyle [TxstForeColor 9] "-"],
                                     frliAltSource = ST.concat [ST.addStyle [TxstForeColor 9] "-",
                                                                ST.addStyle [TxstForeColor 9] nick',
                                                                ST.addStyle [TxstForeColor 9] "-"],
                                     frliBody = comment' }

-- | Send an unkown command message to a frame.
unknownCommandMessage :: Frame -> T.Text -> AM ()
unknownCommandMessage frame command = do
  let textMap = HM.insert "command" (formatText command) HM.empty
  formatText <- lookupText "Unknown command \"%command:s\""
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [Txst 13] "*",
                                     frliAltSource = ST.addStyle [Txst 13] "*",
                                     frliBody = ST.addStyle [] formattedText }

-- | Send a bad command syntax message to a frame.
badCommandSyntaxMessage :: Frame -> T.Text -> AM ()
badCommandSyntaxMessage frame syntax = do
  let textMap = HM.insert "syntax" (formatText syntax) HM.empty
  formatText <- lookupText "Command syntax: \"%syntax:s\""
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [Txst 13] "*",
                                     frliAltSource = ST.addStyle [Txst 13] "*",
                                     frliBody = ST.addStyle [] formattedText }

-- | Send an unbound frame message to a frame.
unboundFrameMessage :: Frame -> AM ()
unboundFrameMessage frame syntax = do
  text <- lookupText "Frame is not bound to a connection, channel, or user"
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [Txst 13] "*",
                                     frliAltSource = ST.addStyle [Txst 13] "*",
                                     frliBody = ST.addStyle [] text }

-- | Send a not in channel message to a frame.
notInChannelMessage :: Frame -> ChannelName -> AM ()
notInChannelMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert "name" (formatText name') HM.empty
  formatText <- lookupText "Not in channel %name:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [Txst 13] "*",
                                     frliAltSource = ST.addStyle [Txst 13] "*",
                                     frliBody = ST.addStyle [] formattedText }

-- | Send a not achannel message to a frame.
notAChannelMessage :: Frame -> ChannelName -> AM ()
notAChannelMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert "name" (formatText name') HM.empty
  formatText <- lookupText "Not a channel: %name:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [Txst 13] "*",
                                     frliAltSource = ST.addStyle [Txst 13] "*",
                                     frliBody = ST.addStyle [] formattedText }

-- | Send an arbitrary error message to a frame.
errorMessage :: Frame -> T.Text -> Error -> AM ()
errorMessage frame text (Error errorLines) = do
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstForeColor 13] "*",
                       frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, ": ", line] }
      restLines =
        case errorLines of
          _ : rest -> map (makeRestLine time text) rest
          [] -> []
  liftIO . atomically $ do
    F.outputLine frame headLine
    mapM_ (F.outputLine frame) restLines
    F.notify frame [FrnoError]
  where makeRestLine time messageText line =
    FrameLine { frliTime = time,
                frliSource = ST.addStyle [TxstForeColor 13] "*",
                frliAltSource = ST.addStyle [TxstForeColor 13] "*",
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) " ") line }
