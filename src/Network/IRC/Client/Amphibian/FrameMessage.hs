module Network.IRC.Client.Amphibian.FrameMessage

       (lookupAddressMessage,
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
        recvNoticeMessage,
        motdMessage,
        selfMessageMessage,
        selfNoticeMessage,
        unknownCommandMessage,
        errorMessage)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
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
  manager <- liftIO . atomically $ F.getConnectionManager frame
  case manager of
    Just manager -> do
      config <- getConnectionConfig manager
      case config of
        Just config -> return $ (encoDecoder $ cocoEncoding config) bytes
        Nothing -> lookupText $ T.pack "NO CONNECTION CONFIG SET"
    Nothing -> lookupText $ T.pack "NO CONNECTION MANAGER SET"
      
-- | Send a lookup hostname message to a specific frame.
lookupHostnameMessage :: Frame -> HostName -> AM ()
lookupHostnameMessage frame hostName = do
  let textMap = HM.insert (T.pack "hostName") (formatString $ T.pack hostName) HM.empty
  formatText <- lookupText $ T.pack "Looking up %hostName:s..."
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [TxstColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoLookupHostname]

-- | Send a lookup address failed message to a specific frame.
lookupAddressFailedMessage :: Frame -> Error -> AM ()
lookupAddressFailedMessage frame (Error errorLines) = do
  text <- lookupText $ T.pack "Address lookup failed"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, T.pack ": ", line] }
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
                frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) (T.singleton ' ')) line }
     
-- | Send a reverse lookup failed message to a specific frame.
reverseLookupFailedMessage :: Frame -> Error -> AM ()
reverseLookupFailedMessage frame (Error errorLines) = do
  text <- lookupText $ T.pack "Reverse hostname lookup failed"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, T.pack ": ", line] }
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
                frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) (T.singleton ' ')) line }

-- | Send a connecting message to a specific frame.
connectingMessage :: Frame -> HostName -> Port -> AM ()
connectingMessage frame hostName port = do
  let textMap = HM.insert (T.pack "hostName") (formatString $ T.pack hostName) HM.empty
  let textMap' = HM.insert (T.pack "port") (formatIntegral port) textMap
  formatText <- lookupText $ T.pack "Connecting to %hostName:s port %port:d..."
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [TxstColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoConnecting]

-- | Send a connected message to a specific frame.
connectedMessage :: Frame -> HostName -> Port -> AM ()
connectedMessage frame hostName port = do
  let textMap = HM.insert (T.pack "hostName") (formatString $ T.pack hostName) HM.empty
  let textMap' = HM.insert (T.pack "port") (formatIntegral port) textMap
  formatText <- lookupText $ T.pack "Connected to %hostName:s port %port:d"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [TxstColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoConnected]

-- | Send a connect failed message to a specific frame.
connectFailedMessage :: Frame -> Error -> AM ()
connectFailedMessage frame (Error errorLines) = do
  text <- lookupText $ T.pack "Connect failed"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, T.pack ": ", line] }
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
                frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) (T.singleton ' ')) line }

-- | Send a disconnect message to a specific frame.
disconnectMessage :: Frame -> AM ()
disconnectMessage frame = do
  text <- lookupText $ T.pack "Disconnected"
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [] text }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send a disconnect error message to a specific frame.
disconnectErrorMessage :: Frame -> Error -> AM ()
disconnectErrorMessage frame (Error errorLines) = do
  text <- lookupText $ T.pack "Disconnected"
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, T.pack ": ", line] }
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
                frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) (T.singleton ' ')) line }

-- | Send a password mismatch message to a specific frame.
passwordMismatchMessage :: Frame -> Password -> AM ()
passwordMismatchMessage frame password = do
  password' <- decode frame password
  let textMap = HM.insert (T.pack "password") (formatString password') HM.empty
  formatText <- lookupText $ T.pack "Password mismatch"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoConnected]

-- | Send a banned from server message to a specific frame.
bannedFromServerMessage :: Frame -> AM ()
bannedFroMServerMessage frame = do
  text <- lookupText $ T.pack "Banned from server"
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [] text }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoBannedFromServer]

-- | Send a banned from server comment message to a specific frame.
bannedFromServerCommentMessage :: Frame -> MessageComment -> AM ()
bannedFroMServerCommentMessage frame comment = do
  styledText <- ST.decode <$> decode frame comment
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = styledText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoBannedFromServer]

-- | Send a welcome message to a specific frame.
welcomeMessage :: Frame -> Nick -> AM ()
welcomeMessage frame nick = do
  nick' <- decode frame nick
  let textMap = HM.insert (T.pack "nick") (formatString nick') HM.empty
  formatText <- lookupText $ T.pack "Welcome to IRC %nick:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send a welcome comment message to a specific frame.
welcomeCommentMessage :: Frame -> MessageComment -> AM ()
welcomeCommentMessage frame comment = do
  styledText <- ST.decode <$> decode frame comment
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = styledText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send an attempting nick message to a specific frame.
attemptingNickMessage :: Frame -> Nick -> AM ()
attemptingNickMessage frame nick = do
  nick' <- decode frame nick
  let textMap = HM.insert (T.pack "nick") (formatString nick') HM.empty
  formatText <- lookupText $ T.pack "Attempting to use nick %nick:s..."
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  let hostName' = T.pack hostName
      line = FrameLine { frliTime = time,
                         frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                         frliBody = ST.addStyle [TxstColor 13] formattedText }
  liftIO . atomically $ do
    F.outputLine frame line
    F.notify frame [FrnoDisconnected]

-- | Send a malformed nick message to a specific frame.
malformedNickMessage :: Frame -> Nick -> AM ()
malformedNickMessage frame nick = do
  nick' <- decode frame nick
  let textMap = HM.insert (T.pack "nick") (formatString nick') HM.empty
  formatText <- lookupText $ T.pack "Malformed nick %nick:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoDisconnected]

-- | Send a joined message to a specific frame.
joinedMessage :: Frame -> ChannelName -> AM ()
joinedMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  formatText <- lookupText $ T.pack "Now talking on %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime time,
                                     frliSource = ST.addStyle [TxstColor 3] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 3] formattedText }
    F.notify frame [FrnoJoined]

-- | Send a parted message to a specific frame.
partedMessage :: Frame -> ChannelName -> AM ()
partedMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  formatText <- lookupText $ T.pack "You have left channel %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 7] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 7] formattedText }
    F.notify frame [FrnoParted]

-- | Send a parted message with a comment to a specific frame.
partedCommentMessage :: Frame -> ChannelName -> MessageComment -> AM ()
partedCommentMessage frame name comment = do
  name' <- decode frame name
  comment' <- ST.decode <$> decode frame comment
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  formatText <- lookupText $ T.pack "You have left channel %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 7] $ T.singleton '*',
                                     frliBody = ST.concat [ST.addStyle [TxstColor 7] formattedText,
                                                           ST.addStyle [TxstColor 7] $ T.pack " (",
                                                           ST.mergeStyle [TxstColor 7] commment',
                                                           ST.addStyle [TxstColor 7] $ T.singleton ')'] }
                                                           
    F.notify frame [FrnoParted]

-- | Send a no topic message to a specific frame.
noTopicMessage :: Frame -> ChannelName -> AM ()
noTopicMessage frame name = do
  name' <- decode frame name
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  formatText <- lookupText $ T.pack "No topic is set for %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoNoTopic]  

-- | Send a topic message to a specific frame.
topicMessage :: Frame -> ChannelName -> ChannelTopic -> AM ()
topicMessage frame name topic = do
  name' <- decode frame name
  topic' <- ST.decode <$> decode frame topic
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  formatText <- lookupText $ T.pack "Topic for %channel:s is"
  let formattedText = format (T.append formatText $ T.pack ": ") textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.concat [ST.addStyle [TxstColor 13] formattedText,
                                                           ST.mergeStyle [TxstColor 13] topic'] }
    F.notify frame [FrnoTopic]

-- | Send a topic user/time message to a specific frame.
topicWhoTimeMessage :: Frame -> ChannelName -> FullName -> UTCTIme -> AM ()
topicWhoTimeMessage frame name user time = do
  timeLocale <- confTimeLocale <$> getConfig
  name' <- decode frame name
  user' <- decode frame user
  timeText <- T.pack $ format timeLocale "%c" time
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  let textMap' = HM.insert (T.pack "user") (formatString user') textMap
  let textMap = HM.insert (T.pack "time") (formatString timeText) textMap'
  formatText <- lookupText $ T.pack "Topic for %channel:s set by %user:s at %time:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 13] formattedText }
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
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  formatText <- lookupText $ T.pack "Users on %channel:s"
  let formattedText = format (T.append formatText $ T.pack ": ") textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.setUsers frame statusNicks'
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 10] formattedText }
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
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  let textMap' = HM.insert (T.pack "nick") (formatString nick') textMap
  let textMap = HM.insert (T.pack "user") (formatString user') textMap'
  formatText <- lookupText $ T.pack "%nick:s (%user:s) has joined %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 3] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 3] formattedText }
    F.notify frame [FrnoRecvJoin]

-- | Send a received part message to a specific frame.
recvPartMessage :: Frame -> ChannelName -> Nick -> FullName -> AM ()
recvPartMessage frame name nick user = do
  name' <- decode frame name
  nick' <- decode frame nick
  user' <- decode frame user
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  let textMap' = HM.insert (T.pack "nick") (formatString nick') textMap
  let textMap = HM.insert (T.pack "user") (formatString user') textMap'
  formatText <- lookupText $ T.pack "%nick:s (%user:s) has left %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 7] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 7] formattedText }
    F.notify frame [FrnoRecvPart]

-- | Send a received part message with a comment to a specific frame.
recvPartCommentMessage :: Frame -> ChannelName -> Nick -> FullName -> MessageComment -> AM ()
recvPartCommentMessage frame nick user comment = do
  name' <- decode frame name
  nick' <- decode frame nick
  user' <- decode frame user
  comment' <- ST.decode <$> decode frame comment
  let textMap = HM.insert (T.pack "channel") (formatString name') HM.empty
  let textMap' = HM.insert (T.pack "nick") (formatString nick') textMap
  let textMap = HM.insert (T.pack "user") (formatString user') textMap'
  formatText <- lookupText $ T.pack "%nick:s (%user:s) has left %channel:s"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 7] $ T.singleton '*',
                                     frliBody = ST.concat [ST.addStyle [TxstColor 7] formattedText,
                                                           ST.addStyle [TxstColor 7] $ T.pack " (",
                                                           ST.mergeStyle [TxstColor 7] commment',
                                                           ST.addStyle [TxstColor 7] $ T.singleton ')'] }
                                                           
    F.notify frame [FrnoRecvPart]

-- | Send a received quit message to a specific frame.
recvQuitMessage :: Frame -> Nick -> FullName -> AM ()
recvQuitMessage frame nick user = do
  nick' <- decode frame nick
  user' <- decode frame user
  let textMap = HM.insert (T.pack "nick") (formatString nick') HM.empty
  let textMap' = HM.insert (T.pack "user") (formatString user') textMap
  formatText <- lookupText $ T.pack "%nick:s (%user:s) has quit"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 7] $ T.singleton '*',
                                     frliBody = ST.addStyle [TxstColor 7] formattedText }
    F.notify frame [FrnoRecvPart]

-- | Send a received quit message with a comment to a specific frame.
recvQuitCommentMessage :: Frame -> Nick -> FullName -> MessageComment -> AM ()
recvQuitCommentMessage frame nick user comment = do
  nick' <- decode frame nick
  user' <- decode frame user
  comment' <- ST.decode <$> decode frame comment
  let textMap = HM.insert (T.pack "nick") (formatString nick') HM.empty
  let textMap' = HM.insert (T.pack "user") (formatString user') textMap
  formatText <- lookupText $ T.pack "%nick:s (%user:s) has quit"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 7] $ T.singleton '*',
                                     frliBody = ST.concat [ST.addStyle [TxstColor 7] formattedText,
                                                           ST.addStyle [TxstColor 7] $ T.pack " (",
                                                           ST.mergeStyle [TxstColor 7] commment',
                                                           ST.addStyle [TxstColor 7] $ T.singleton ')'] }
                                                           
    F.notify frame [FrnoRecvPart]


-- | Send a received nick message to a specific frame.
recvNickMessage :: Frame -> Nick -> Nick-> AM ()
recvNickMessage frame oldNick newNick = do
  oldNick' <- decode frame oldNick
  newNick' <- decode frame newNick
  let textMap = HM.insert (T.pack "oldNick") (formatString oldNick') HM.empty
  let textMap' = HM.insert (T.pack "newNick") (formatString newNick') textMap
  formatText <- lookupText $ T.pack "%oldNick:s is now known as %newNick:s"
  let formattedText = format formatText textMap'
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.addStyle [] formattedText }
    F.notify frame [FrnoRecvNick]

-- | Send a received topic message to a specific frame.
recvTopicMessage :: Frame -> Nick -> ChannelTopic -> AM ()
recvTopicMessage frame nick topic = do
  nick' <- decode frame nick
  topic' <- ST.decode <$> decode frame topic
  let textMap = HM.insert (T.pack "nick") (formatString nick') HM.empty
  formatText <- lookupText $ T.pack "%nick:s has changed the topic to"
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                                     frliBody = ST.concat [ST.addStyle [] formattedText,
                                                           ST.addStyle [] $ T.pack ": ",
                                                           topic'] }
                                                           
    F.notify frame [FrnoRecvTopic]

-- | Check whether a message is an action.
getAction :: StyledText -> Just StyledText
getAction (StyledText (StyledTextElement [] firstText : rest)) =
  let (firstPart, firstTextRest) = T.breakOn isSpace firstText in
  if firstPart == T.pack "ACTION"
  then Just . StyledText $ StyledTextElement [] firstTextRest : rest
  else Nothing
getAction (StyledText []) = Nothing

-- | Send a received message message to a specific frame.
recvMessageMessage :: Frame -> Nick -> MessageComment -> FrameMessageType -> AM ()
recvMessageMessage frame nick comment private = do
  manager <- liftIO . atomically $ F.getConnectionManager frame
  myNick <- liftIO . atomically $ CM.getNick manager
  myNick' <- decode frame myNick
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  case getAction comment' of
    Nothing ->
      if (length $ T.splitOn myNick' (ST.removeStyle comment')) > 1
      then
        liftIO . atomically $ do
          F.outputLine frame $ FrameLine { frliTime = time,
                                           frliSource = ST.addStyle [TxstBold, TxstColor 5] nick',
                                           frliBody = ST.mergeStyle [TxstColor 5] comment' }
          case private of
            FrmtPrivate -> F.notify frame [FrnoRecvPrivateMessage, FrnoRecvMention]
            FrmtChannel F.notify frame [FrnoRecvChannelMessage, FrnoRecvMention]
      else
        liftIO . atomically $ do
          F.outputLine frame $ FrameLine { frliTime = time,
                                           frliSource = ST.addStyle [TxstColor 12] nick',
                                           frliBody = comment' }
          case private of
            FrmtPrivate -> F.notify frame [FrnoRecvPrivateMessage]
            FrmtChannel -> F.notify frame [FrnoRecvChannelMessage]
    Just action ->
      liftIO . atomically $ do
        F.outputLine frame $ FrameLine { frliTime = time,
                                         frliSource = ST.addStyle [TxstColor 12] $ T.singleton '*',
                                         frliBody = ST.concat [ST.addStyle [TxstColor 12] nick',
                                                               ST.addStyle [] T.singleton ' ',
                                                               action] }
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
                     frliSource = ST.concat [ST.addStyle [TxstColor 12] $ T.singleton '-',
                                             ST.addStyle [TxstColor 13] nick',
                                             ST.addStyle [TxstColor 12] $ T.singleton '-'],
                     frliBody = comment' }
    else FrameLine { frliTime = time,
                     frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
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
                      frliSource = ST.addStyle [TxstColor color] $ T.singleton '*',
                      frliBody = line }
    

-- | Send a self message message to a specific frame.
selfMessageMessage :: Frame -> Nick -> MessageComment -> AM ()
selfMessageMessage frame nick comment = do
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  case getAction comment' of
    Nothing -> do
      lightBackground <- confLightBackground <$> getConfig
      let color = if lightBackground then 1 else 0
      liftIO . atomically $
        F.outputLine frame $ FrameLine { frliTime = time,
                                         frliSource = ST.addStyle [TxstColor color] nick',
                                         frliBody = ST.setBaseColor color comment' }
    Just action ->
      liftIO . atomically $
        F.outputLine frame $ FrameLine { frliTime = time,
                                         frliSource = ST.addStyle [TxstColor 12] $ T.singleton '*',
                                         frliBody = ST.concat [ST.addStyle [TxstColor 12] nick',
                                                               ST.addStyle [] T.singleton ' ',
                                                               action] }


-- | Send a self notice message to a frame.
selfNoticeMessage :: Frame -> Nick -> MessageComment -> AM ()
selfNoticeMessage frame nick comment = do
  time <- liftIO getCurrentTime
  nick' <- decode frame nick
  comment' <- ST.decode <$> decode frame comment
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.concat [ST.addStyle [TxstColor 9] $ T.singleton '-',
                                                             ST.addStyle [TxstColor 9] nick',
                                                             ST.addStyle [TxstColor 9] $ T.singleton '-'],
                                     frliBody = comment' }

-- | Send an unkown command message to a frame.
unknownCommandMessage :: Frame -> T.Text -> AM ()
unknownCommandMessage frame command = do
  let textMap = HM.insert (T.pack "command") (formatString command) HM.empty
  formatText <- lookupText $ T.pack "Unknown command \"%command:s\""
  let formattedText = format formatText textMap
  time <- liftIO getCurrentTime
  liftIO . atomically $ do
    F.outputLine frame $ FrameLine { frliTime = time,
                                     frliSource = ST.addStyle [Txst 13] $ T.singleton '*',
                                     frliBody = ST.addStyle [] formattedText }

-- | Send an arbitrary error message to a frame.
errorMessage :: Frame -> T.Text -> Error -> AM ()
errorMessage frame text (Error errorLines) = do
  time <- liftIO getCurrentTime
  let headLine =
        case errorLines of
         [] ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] text }
         line : _ ->
           FrameLine { frliTime = time,
                       frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                       frliBody = ST.addStyle [] $ T.intercalate T.empty [text, T.pack ": ", line] }
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
                frliSource = ST.addStyle [TxstColor 13] $ T.singleton '*',
                frliBody = ST.addStyle [] $ T.append (T.replicate (T.length messageText + 2) (T.singleton ' ')) line }
