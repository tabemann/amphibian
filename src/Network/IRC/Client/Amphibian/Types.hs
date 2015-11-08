{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.IRC.Client.Amphibian.Types

       (AM(..),
        Interface(..),
        Error(..),
        Encoding(..),
        Config(..),
        ConnectionConfig(..),
        ServerSetup(..),
        ConnectionInfo(..),
        Plugin(..),
        PluginAction(..),
        PluginInputEvent(..),
        PluginOutputEvent(..),
        PluginRunResponse(..),
        PluginStopResponse(..),
        PluginInputSubscription(..),
        PluginOutputSubscription(..),
        PluginServer(..),
        PluginServerAction(..),
        PluginStartResponse(..),
        PluginServerStopResponse(..),
        Connection(..),
        ConnectionAction(..),
        ConnectionConnectResponse(..),
        ConnectionSendResponse(..),
        ConnectionCloseResponse(..),
        ConnectionEvent(..),
        HostName,
        Port,
        IRCMessage(..),
        MessagePrefix,
        MessageCommand,
        MessageParameter,
        MessageComment,
        ConnectionManager(..),
        ConnectionManagerSetup(..),
        Name,
        UserName,
        Nick,
        Password,
        UserMode,
        ConnectionManagerSubscription(..),
        ConnectionmanagerAction(..)
        ConnectionManagerConnectResponse(..),
        ConnectionManagerReconnectResponse(..),
        ConnectionManagerDisconnectResponse(..),
        ConnectionManagerSendResponse(..),
        ConnectionManagerEvent(..),
        ConnectionManagerStopResponse(..),
        ConnectionManagerServer(..),
        ConnectionManagerServerAction(..),
        ConnectionManagerStartResponse(..),
        ConnectionManagerServerStopResponse(..),
        OperResponse(..),
        OperEvent(..),
        NickResponse(..),
        NickEvent(..),
        AddRemove(..),
        ModeResponse(..),
        ModeEvent(..),
        QuitResponse(..)
        QuitEvent(..),
        SquitResponse(..),
        SquitEvent(..),
        Channel(..),
        ChannelName,
        ChannelKey,
        ChannelTopic,
        ChannelType,
        UserStatus,
        ChannelSubscription(..),
        ChannelAction(..),
        ChannelEvent(..),
        ChannelStopResponse(..),
        ChannelJoinResponse(..),
        ChannelPartResponse(..),
        ChannelMessageResponse(..),
        ChannelNoticeResponse(..),
        ChannelSetTopicResponse(..),
        ChannelServer(..),
        ChannelServerAction(..),
        ChannelStartResponse(..),
        ChannelServerStopResponse(..),
        User(..),
        UserSubscription(..),
        UserAction(..),
        UserEvent(..),
        UserMessageResponse(..),
        UserNoticeResponse(..),
        UserStopResponse(..),
        UserServer(..),
        UserServerAction(..),
        UserStartResponse(..),
        UserServerStopResponse(..),
        StyledText(..),
        StyledTextElement(..),
        TextStyle(..),
        TextColor,
        Frame(..),
        FrameOutputSubscription(..),
        FrameInputSubscription(..),
        FrameOutputEvent(..),
        FrameInputEvent(..),
        FrameLine(..),
        FrameNotifications(..),
        FormatMap,
        ConnectionDisplay(..),
        ConnectionDisplayAction(..),
        ConnectionDisplayStopResponse(..))
       
       where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Concurrent.STM (TVar,
                               TChan,
                               TMVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.Async (Async)
import System.IO.Handle (Handle)
import Data.ByteString (ByteString)
import Network.TLS (Context)
import Network.Socket (SockAddr)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import System.Locale (TimeLocale)

-- | Amphibian monad.
newtype AM a = AM (ReaderT Interface IO a)
             deriving (Monad, MonadIO)

-- | Amphibian interface.
data Interface =
  Interface { intfTextMap :: TVar (HashMap Language (HashMap Text Text)),
              intfAvailableLanguages :: TVar [Language],
              intfAvailableEncodings :: TVar [Encoding],
              intfConfig :: TVar Config,
              intfConnectionManagers :: TVar [ConnectionManager],
              intfConnectionConfigs :: TVar [(ConnectionManager, TVar ConnectionConfig)],
              intfChannels :: TVar [Channel],
              intfUsers :: TVar [User],
              intfFrames :: TVar [Frame],
              intfPlugins :: TVar [Plugin],
              intfInputDispatcher :: TVar (Maybe InputDispatcher),
              intfPluginServer :: TVar (Maybe PluginServer),
              intfConnectionManagerServer :: TVar (Maybe ConnectionManagerServer),
              intfChannelServer :: TVar (Maybe ChannelServer),
              intfUserServer :: TVar (Maybe UserServer),
              intfEvents :: TChan InterfaceEvent }
  deriving Eq

-- | Interface subscription.
newtype InterfaceSubscription = InterfaceSubscription (TChan InterfaceEvent)

-- | Interface event.
data InterfaceEvent = IntfConnectionManagerRegistered ConnectionManager
                    | IntfChannelRegistered Channel
                    | IntfUserRegistered User
                    | IntfFrameRegistered Frame
                    | IntfPluginRegistered Plugin
                    | IntfInputDispatcherRegistered InputDispatcher
                    | IntfPluginServerRegistered PluginServer
                    | IntfConnectionManagerServerRegistered ConnectionManagerServer
                    | IntfChannelServerRegistered ChannelServer
                    | IntfUserServerRegistered UserServer
                    | IntfConnectionManagerUnregistered ConnectionManager
                    | IntfChannelUnregistered Channel
                    | IntfUserUnregistered User
                    | IntfFrameUnregistered Frame
                    | IntfPluginUnregistered Plugin
                    | IntfInputDispatcherUnregistered InputDispatcher
                    | IntfPluginServerUnregistered PluginServer
                    | IntfConnectionManagerServerUnregistered ConnectionManagerServer
                    | IntfChannelServerUnregistered ChannelServer
                    | IntfUserServerUnregistered UsersServer
                    | IntfConfigSet Config
                    | IntfConnectionConfigSet ConnectionManager ConnectionConfig

-- | Language.
type Language = Text

-- | Encoding
data Encoding =
  Encoding { encoName :: Text,
             encoEncoder :: Text -> ByteString,
             encoDecoder :: ByteString -> Text }

-- | Error message.
newtype Error = Error [Text]
              deriving Eq

-- | Amphibian configuration.
data Config =
  Config { confServerList :: [ServerSetup],
           confDefaultScriptEntry :: Text,
           confScriptCompileOptions :: [Text],
           confScriptEvalOptions :: [Text],
           confLanguage :: Language,
           confTimeLocale :: TimeLocale,
           confLightBackground :: Bool,
           confPluginCompileOptions :: [Text],
           confCtcpVersion :: Text,
           confCtcpSource :: Text,
           confCtcpClientInfo :: Text }
  deriving Eq

-- | Connection configuration.
data ConnectionConfig =
  ConnectionConfig { cocoEncoding :: Encoding,
                     cocoCtcpUserInfo :: Text }

-- | Amphibian server setup.
data ServerSetup =
  ServerSetup { srstName :: String,
                srstHost :: String,
                srstPort :: String,
                srstUsername :: String,
                srstNicks :: [String],
                srstPassword :: Maybe String,
                srstDefaultChannels :: [String] }
  deriving Eq

-- | Amphibian connection information.
data ConnectionInfo =
  ConnectionInfo { cninName :: String,
                   cninOriginalHost :: String,
                   cninCurrentHost :: Maybe String,
                   cninPort :: String,
                   cninUsername :: String,
                   cninCurrentNick :: String,
                   cninAllNicks :: [String],
                   cninPassword :: Maybe String,
                   cninDefaultChannels :: [String] }
  deriving Eq

-- | Plugin.
data Plugin =
  Plugin { plugPath :: Text,
           plugEntryPoint :: Maybe Text,
           plugPrecompiled :: Bool,
           plugInterface :: Interface,
           plugActions :: TQueue PluginAction,
           plugInputEvents :: TChan PluginInputEvent,
           plugOutputEvents :: TChan PluginOutputEvent,
           plugRunning :: TVar Bool,
           plugActive :: TVar Bool,
           plugQuitting :: TVar Bool }
  deriving Eq

-- | Plugin action.
data PluginAction = PlugRun PluginRunResponse
                  | PlugStop PluginStopResponse

-- | Plugin input event.
data PluginInputEvent = PievQuit

-- | Plugin output event.
data PluginOutputEvent = PoevStart
                       | PoevExit

-- | Plugin run response.
newtype PluginRunResponse = PluginRunResponse (Either Error ())

-- | Plugin stop response.
newtype PluginStopResponse = PluginStopResponse (Either Error ())

-- | Plugin input subscription.
newtype PluginInputSubscription = PluginInputSubscription (TChan PluginInputEvent)

-- | Plugin output subscription.
newtype PluginOutputSubscription = PluginOutputSubscription (TChan PluginOutputEvent)

-- | Plugin server type.
data PluginServer =
  PluginServer { plseRunning :: TVar Bool,
                 plseActions :: TQueue PluginServerAction }

-- | Plugin server action.
data PluginServerAction = PlsaStartPlugin Plugin PluginStartResponse
                        | PlsaStop PluginServerStopResponse

-- | Plugin start response.
newtype PluginStartResponse = PluginStartResponse (TMVar (Either Error ()))

-- | Plugin server stop response.
newtype PluginServerStopResponse = PluginServerStopResponse (TMVar (Either Error ()))

-- | Connection.
data Connection =
  Connection { connActions :: TQueue ConnectionAction,
               connEvents :: TChan ConnectionEvent,
               connConnected :: TVar Bool }
  deriving Eq

-- | Connection state.
data ConnectionState =
  ConnectionState { connRecvData :: TVar ByteString }

-- | Connection subscription.
data ConnectionSubscription =
  ConnectionSubscription (TChan ConnectionEvent)

-- | Connection action.
data ConnectionAction = ConnSend IRCMessage ConnectionSendResponse
                      | ConnClose ConnectionCloseResponse
                      deriving Eq

-- | Connection connect response.
newtype ConnectionConnectResponse =
  ConnectionConnectResponse (TMVar (Either Error ()))

-- | Connection send response.
newtype ConnectionSendResponse =
  ConnectionSendResponse (TMVar (Either Error ()))

-- | Connection close response.
newtype ConnectionCloseResponse =
  ConnectionCloseResponse (TMVar (Either Error ()))

-- | Connection event.
data ConnectionEvent = ConnLookupAddress HostName
                     | ConnFoundAddress SockAddr
                     | ConnLookupAddressFailed Error
                     | ConnFoundHostname HostName
                     | ConnReverseLookupFailed Error
                     | ConnConnecting HostName Port
                     | ConnConnected HostName Port
                     | ConnConnectFailed Error
                     | ConnDisconnected (Either Error ())
                     | ConnMessage IRCMessage
                     | ConnMalformed ByteString
                       deriving Eq

-- | Host name type.
type HostName = String

-- | Port type.
type Port = Int

-- | IRC message type.
data IRCMessage =
  IRCMessage { ircmPrefix :: Maybe MessagePrefix,
               ircmCommand :: MessageCommand,
               ircmParameters :: [MessageParameter],
               ircmComment :: Maybe MessageComment }
  deriving (Eq, Ord)

-- | IRC message prefix.
type MessagePrefix = ByteString

-- | IRC message command.
type MessageCommand = ByteString

-- | IRC message parameter.
type MessageParameter = ByteString

-- | IRC message comment.
type MessageComment = ByteString

-- | Connection manager.
data ConnectionManager =
  ConnectionManager { comaActions :: TQueue ConnectionManagerAction,
                      comaEvents :: TChan ConnectionManagerEvent,
                      comaStop :: TMVar (ConnectManagerStopResponse),
                      comaSetup :: TVar (Maybe ConnectionManagerSetup),
                      comaConnection :: TVar (Maybe Connection),
                      comaRegistered :: TVar Bool,
                      comaHost :: Maybe HostName,
                      comaNick :: Maybe Nick,
                      comaSubscription :: TVar [ConnectionSubscription],
                      comaMotd :: TVar [MessageComment],
                      comaActive :: TVar Bool }
  deriving Eq

-- | Connection manager setup
data ConnectionManagerSetup =
  ConnectionManagerSetup { comaName :: Name,
                           comaOriginalHost :: HostName,
                           comaPort :: Port,
                           comaUserName :: UserName,
                           comaAllNicks :: [Nick],
                           comaPassword :: Maybe Password,
                           comaMode :: [UserMode] }
  deriving Eq

-- | Name
type Name = ByteString

-- | Username
type UserName = ByteString

-- | Nick
type Nick = ByteString

-- | Password
type Password = ByteString

-- | User mode
type UserMode = ByteString

-- | Full nick user hostname string
type FullName = ByteString

-- | Connection manager subscription.
newtype ConnectionManagerSubscription =
  ConnectionManagerSubscription (TChan ConnectionManagerEvent)

-- | Connection manager action.
data ConnectionManagerAction = ComaConnectNew ConnectionManagerSetup
                               ConnectionManagerConnectResponse
                             | ComaReconnect
                               ConnectionManagerReconnectResponse
                             | ComaDisconnect
                               ConnectionManagerDisconnectResponse
                             | ComaSend IRCMessage
                               ConnectionManagerSendResponse
                             deriving Eq

-- | Connection manager connect response.
newtype ConnectionManagerConnectResponse =
  ConnectionManagerConnectResponse (TMVar (Either Error ()))

-- | Connection manager reconnect response.
newtype ConnectionManagerReconnectResponse =
  ConnectionManagerReconnectResponse (TMVar (Either Error ()))

-- | Connection manager disconnect response.
newtype ConnectionManagerDisconnectResponse =
  ConnectionManagerDisconnectResponse (TMVar (Either Error ()))

-- | Connection manager send response.
newtype ConnectionManagerSendResponse =
  ConnectionManagerSendResponse (TMVar (Either Error ()))

-- | Connection manager event
data ConnectionManagerEvent = ComaLookupAddress HostName
                            | ComaFoundAddress SockAddr
                            | ComaLookupAddressFailed Error
                            | ComaFoundHostname HostName
                            | ComaReverseLookupFailed Error
                            | ComaConnecting HostName Port
                            | ComaConnected HostName Port
                            | ComaConnectFailed Error
                            | ComaDisconnected (Either Error ())
                            | ComaMessage IRCMessage
                            | ComaMalformed ByteString
                            | ComaPasswordMismatch Password
                            | ComaBannedFromServer (Maybe MessageComment)
                            | ComaMalformedNick Nick
                            | ComaUnavailResource
                            | ComaRestricted
                            | ComaAttemptingNick Nick
                            | ComaWelcome (Maybe MessageComment)
                            | ComaRecvNotice Nick MessageComment
                            | ComaRegistrationFailed
                            | ComaMotd [MessageComment]
                            | ComaRecvCtcpRequest Nick ChannelNameOrNick MessageComment
                            | ComaRecvCtcpReply Nick ChannelNameOrNick MessageComment
                            | ComaSelfMessage Nick ChannelNameOrNick MessageComment
                            | ComaSelfNotice Nick ChannelNameOrNick MessageComment
                            | ComaSelfCtcpRequest Nick ChannelNameOrNick MessageComment
                            | ComaSelfCtcpReply Nick ChannelNameOrNick MessageComment
                            deriving Eq

-- | Connection manager stop response.
newtype ConnectionManagerStopResponse =
  ConnectionManagerStopResponse (TMVar ())

-- | Connection manager server.
data ConnectionManagerServer =
  ConnectionManagerServer { cmseRunning :: TVar Bool,
                            cmseActions :: TQueue ConnectionManagerServerAction }

-- | Connection manager server action.
data ConnectionManagerServerAction = CmsaStartConnectionManager ConnectionManager ConnectionManagerStartResponse
                                   | CmsaStop ConnectionManagerServerStopResponse

-- | Connection manager start response.
newtype ConnectionManagerStartResponse = ConnectionManagerStartResponse (TMVar (Either Error ()))

-- | ConnectionManager server stop response.
newtype ConnectionManagerServerStopResponse = ConnectionManagerServerStopResponse (TMVar (Either Error ()))

-- | OPER response.
newtype OperResponse = OperResponse (TMVar OperEvent)

-- | OPER event.
data OperEvent = OperNoOperHost
               | OperYoureOper
               | OperPasswdMismatch
               | OperNeedMoreParams
               | OperOther IRCMessage
               | OperDisconnected
               | OperNotRegistered
               | OperError Error
               deriving Eq

-- | NICK response.
newtype NickResponse = NickResponse (TMVar NickEvent)

-- | NICK event.
data NickEvent = NickSuccess
               | NickNicknameInUse
               | NickErroneusNickname
               | NickNickCollision
               | NickUnavailResource
               | NickNoNicknameGiven
               | NickOther IRCMessage
               | NickDisconnected
               | NickNotRegistered
               | NickError Error
               deriving Eq

-- | Add or remove.
data AddRemove = Add | Remove deriving Eq

-- | MODE response.
newtype ModeResponse = ModeResponse (TMVar ModeEvent)

-- | MODE event.
data ModeEvent = ModeSuccess
               | ModeUmodeUnknownFlag
               | ModeUsersDontMatch
               | ModeNeedMoreParams
               | ModeOther IRCMessage
               | ModeDisconnected
               | ModeNotRegistered
               | ModeError Error
               deriving Eq

-- | QUIT response.
newtype QuitResponse = QuitResponse (TMVar QuitEvent)

-- | QUIT event.
data QuitEvent = QuitSuccess (Maybe MessageComment)
               | QuitOther IRCMessage
               | QuitDisconnected
               | QuitNotRegistered
               | QuitError Error
               deriving Eqhttps://en.wikipedia.org/w/index.php?search=cyanoglobin&title=Special%3ASearch&fulltext=Search

-- | SQUIT response.
newtype SquitResponse = SquitResponse (TMVar SquitEvent)

-- | SQUIT event.
data SquitEvent = SquitSuccess
                | SquitNoPrivileges
                | SquitNoSuchServer
                | SquitNeedMoreParams
                | SquitOther IRCMessage
                | SquitDisconnected
                | SquitNotRegistered
                | SquitError Error
                deriving Eq

-- | Amphibian channel.
data Channel =
  Channel { chanInterface :: Interface,
            chanActions :: TQueue ChannelAction,
            chanEvents :: TChan ChannelEvent,
            chanName :: TVar ChannelName,
            chanConnectionManager :: ConnectionManager,
            chanKey :: TVar (Maybe ChannelKey),
            chanTopic :: TVar (Maybe ChannelTopic),
            chanTopicUser :: TVar (Maybe FullUser),
            chanTopicTime :: TVar UTCTime,
            chanType :: TVar ChannelType,
            chanNames :: TVar [(Nick, UserStatus)],
            chanNamesAccum :: TVar [(Nick, UserStatus)],
            chanAutoJoin :: TVar Bool,
            chanJoined :: TVar Bool,
            chanJoinResponse :: TVar (Maybe ChannelJoinResponse),
            chanActive :: TVar Bool }
  deriving Eq

-- | Channel name.
type ChannelName = ByteString

-- | Channel key.
type ChannelKey = ByteString

-- | Channel topic.
type ChannelTopic = ByteString

-- | Channel type.
data ChannelType = ChanPublic | ChanPrivate | ChanSecret deriving Eq

-- | User status.
data UserStatus = UserNormal | UserVoice | UserHalfOp | UserOp deriving Eq

-- | Channel name or nick.
data ChannelNameOrNick = CnonChannelName ChannelName
                       | CnonNick Nick

-- | Channel subscription.
newtype ChannelSubscription = ChannelSubscription (TChan ChannelEvent)

-- | Channel action.
data ChannelAction = ChanStop ChannelStopResponse
                   | ChanJoin ChannelJoinResponse
                   | ChanPart (Maybe MessageComment) ChannelPartResponse
                   | ChanMessage MessageComment ChannelMessageResponse
                   | ChanNotice MessageComment ChannelNoticeResponse
                   | ChanSetTopic MessageComment ChannelSetTopicResponse
                   deriving Eq

-- | Channel event.
data ChannelEvent = (ChanDisconnected error)
                  | ChanJoined
                  | ChanParted (Maybe MessageComment)
                  | ChanNoTopic
                  | ChanTopic ChannelTopic
                  | ChanTopicWhoTime FullName UTCTime
                  | ChanType ChannelType
                  | ChanNames [(Nick, UserStatus)]
                  | ChanRecvJoin Nick FullName
                  | ChanRecvPart Nick FullName (Maybe MessageComment)
                  | ChanRecvMessage Nick MessageComment
                  | ChanRecvNotice Nick MessageComment
                  | ChanRecvNick Nick Nick
                  | ChanRecvTopic Nick ChannelTopic
                  | ChanRecvQuit Nick FullName (Maybe MessageComment)
                  | ChanRecvCtcpRequest Nick MessageComment
                  | ChanRecvCtcpReply Nick MessageComment
                  | ChanSelfMessage Nick MessageComment
                  | ChanSelfNotice Nick MessageComment
                  | ChanSelfCtcpRequest Nick MessageComment
                  | ChanSelfCtcpReply Nick MessageComment
                  deriving Eq

-- | Channel stop response.
newtype ChannelStopResponse = ChannelStopResponse (TMVar (Either Error ()))

-- | Channel join response.
newtype ChannelJoinResponse = ChannelJoinResponse (TMVar (Either Error ()))

-- | Channel part response.
newtype ChannelPartResponse = ChannelPartResponse (TMVar (Either Error ()))

-- | Channel message response.
newtype ChannelMessageResponse = ChannelMessageResponse (TMVar (Either Error ()))

-- | Channel notice response.
newtype ChannelNoticeResponse = ChannelNoticeResponse (TMVar (Either Error ()))

-- | Channel set topic response.
newtype ChannelSetTopicResponse = ChannelSetTopicResponse (TMVar (Either Error ()))

-- | Channel server type.
data ChannelServer =
  ChannelServer { chseRunning :: TVar Bool,
                  chseActions :: TQueue ChannelServerAction }

-- | Channel server action.
data ChannelServerAction = ChsaStartChannel Channel ChannelStartResponse
                         | ChsaStop ChannelServerStopResponse

-- | Channel start response.
newtype ChannelStartResponse = ChannelStartResponse (TMVar (Either Error ()))

-- | Channel server stop response.
newtype ChannelServerStopResponse = ChannelServerStopResponse (TMVar (Either Error ()))

-- | User.
data User =
  User { userInterface :: Interface,
         userConnectionManager :: ConnectionManager,
         userSubscription :: ConnectionManagerSubscription,
         userNick :: TVar Nick,
         userActions :: TQueue UserAction,
         userInjectedEvents :: TQueue ConnectionManagerEvent,
         userEvents :: TChan UserEvent,
         userDelayEvents :: TVar Bool,
         userActive :: TVar Bool }
  deriving Eq

-- | User subscription.
newtype UserSubscription = UserSubscription (TMVar UserEvent)

-- | User action.
data UserAction = UserMessage MessageComment UserMessageResponse
                | UserNotice MessageComment UserNoticeResponse
                | UserStop UserStopResponse
                deriving Eq

-- | User event.
data UserEvent = UserDisconnected (Either Error ())
               | UserRecvMessage Nick MessageComment
               | UserRecvNotice Nick MessageComment
               | UserRecvNick Nick Nick
               | UserRecvQuit Nick FullName (Maybe MessageComment)
               | UserRecvCtcpRequest Nick MessageComment
               | UserRecvCtcpReply Nick MessageComment
               | UserSelfMessage Nick MessageComment
               | UserSelfNotice Nick MessageComment
               | UserSelfCtcpRequest Nick MessageComment
               | UserSelfCtcpReply Nick MessageComment
               | UserNotPresent
               deriving Eq

-- | Response to user message.
newtype UserMessageResponse = UserMessageResponse (TMVar (Either Error ()))

-- | Response to user notice.
newtype UserNoticeResponse = UserNoticeResponse (TMVar (Either Error ()))

-- | Response to user stop.
newtype UserStopResponse = UserStopResponse (TMVar (Either Error ()))

-- | User server type.
data UserServer =
  UserServer { usseRunning :: TVar Bool,
               usseActions :: TQueue UserServerAction }

-- | User server action.
data UserServerAction = UssaStartUser User UserStartResponse
                      | UssaStop UserServerStopResponse

-- | User start response.
newtype UserStartResponse = UserStartResponse (TMVar (Either Error ()))

-- | User server stop response.
newtype UserServerStopResponse = UserServerStopResponse (TMVar (Either Error ()))

-- | Styled text.
newtype StyledText = StyledText [StyledTextElement]
                     deriving Eq

-- | Styled text element.
data StyledTextElement = StyledTextElement [TextStyle] Text
                         deriving Eq

-- | Text style
data TextStyle = TxstBold | TxstUnderline | TxstColor TextColor
                 deriving Eq

-- | Text color
type TextColor = Int

-- | Frame.
data Frame = Frame { framInputEvents :: TChan FrameInputEvent,
                     framOutputEvents :: TChan FrameOutputEvent,
                     framMapping :: TVar FrameMapping,
                     framTopic :: TVar (Maybe Text),
                     framUsers :: TVar (Maybe [(Text, UserStatus)]),
                     framNick :: TVar Text,
                     framName :: TVar Text,
                     framTitle :: TVar Text,
                     framParent :: TVar (Maybe Frame),
                     framChildren :: TVar [Frame],
                     framFocus :: TVar Bool,
                     framLastFocus :: TVar (Maybe Frame)
                     framNotifications :: TVar [FrameNotifications] }
             deriving Eq

-- | Frame output subscription.
newtype FrameOutputSubscription = FrameOutputSubscription (TChan FrameOutputEvent)

-- | Frame input subscription.
newtype FrameInputSubscription = FrameInputSubscription (TChan FrameInputEvent)

-- | Frame mapping.
data FrameMapping = FrmaConnectionManager ConnectionManager
                  | FrmaChannel Channel
                  | FrmaUser User
                  | FrmaNone
                  deriving Eq

-- | Frame output event.
data FrameOutputEvent = FoevTopic (Maybe Text)
                      | FoevUsers (Maybe [(Text, UserStatus)])
                      | FoevNick Text
                      | FoevName Text
                      | FoevTitle Text
                      | FoevLine FrameLine
                      | FoevNotifications [FrameNotifications]
                      | FoevMapping FrameMapping
                      deriving Eq

-- | Frame input event.
data FrameInputEvent = FievTopic Text
                     | FievLine StyledText
                     | FievFocus Bool
                     | FievMapping FrameMapping
                     deriving Eq

-- | Frame line.
data FrameLine =
  FrameLine { frliTime :: UTCTime,
              frliSource :: StyledText,
              frliBody :: StyledText }
  deriving Eq

-- | Frame notifications.
data FrameNotifications = FrnoLookupHostname
                        | FrnoFoundAddress
                        | FrnoLookupAddressFailed
                        | FrnoFoundHostname
                        | FrnoReverseLookupFailed
                        | FrnoConnecting
                        | FrnoConnected
                        | FrnoConnectFailed
                        | FrnoDisconnected
                        | FrnoPasswordMismatch
                        | FrnoBannedFromServer
                        | FrnoWelcome
                        | FrnoAttemptingNick
                        | FrnoMalformedNick
                        | FrnoNickInUse
                        | FrnoJoined
                        | FrnoParted
                        | FrnoNick
                        | FrnoNoTopic
                        | FrnoTopic
                        | FrnoTopicWhoTime
                        | FrnoNames
                        | FrnoRecvJoin
                        | FrnoRecvPart
                        | FrnoRecvNick
                        | FrnoRecvTopic
                        | FrnoRecvChannelMessage
                        | FrnoRecvChannelNotice
                        | FrnoRecvPrivateMessage
                        | FrnoRecvPrivateNotice
                        | FrnoRecvServer
                        | FrnoRecvMention
                        | FrnoError
                        deriving Eq

-- | Format map.
type FormatMap = HashMap Text (Text -> (Text, Text))

-- | Connection display.
data ConnectionDisplay =
  ConnectionDisplay { codiRunning :: TVar Bool,
                      codiActions :: TQueue ConnectionDisplayAction,
                      codiInterface :: Interface,
                      codiInterfaceSubscription :: InterfaceSubscription,
                      codiFrames :: TVar [ConnectionDisplayFrameMapping] }
  deriving Eq

-- | Connection display frame mapping
data ConnectionDisplayFrameMapping =
  ConnectionDisplayFrameMapping { codfFrame :: Frame,
                                  codfConnectionManager :: ConnectionManager,
                                  codfSubscription :: ConnectionManagerSubscription }

-- | Connection display actions.
data ConnectionDisplayAction = CodaStop ConnectionDisplayStopResponse

-- | Connection display stop response.
data ConnectionisplayStopResponse = ConnectionDisplayStopResponse (TMVar (Either Error ()))

-- | Channel display.
data ChannelDisplay =
  ChannelDisplay { chdiRunning :: TVar Bool,
                   chdiActions :: TQueue ChannelDisplayAction,
                   chdiInterface :: Interface,
                   chdiInterfaceSubscription :: InterfaceSubscription,
                   chdiFrames :: TVar [ChannelDisplayFrameMapping] }
  deriving Eq

-- | Channel display frame mapping
data ChannelDisplayFrameMapping =
  ChannelDisplayFrameMapping { chdfFrame :: Frame,
                               chdfChannel :: Channel,
                               chdfSubscription :: ChannelSubscription }

-- | Channel display actions.
data ChannelDisplayAction = CodaStop ChannelDisplayStopResponse

-- | Channel display stop response.
data ChannelisplayStopResponse = ChannelDisplayStopResponse (TMVar (Either Error ()))

-- | User display.
data UserDisplay =
  UserDisplay { usdiRunning :: TVar Bool,
                usdiActions :: TQueue UserDisplayAction,
                usdiInterface :: Interface,
                usdiInterfaceSubscription :: InterfaceSubscription,
                usdiFrames :: TVar [UserDisplayFrameMapping] }
  deriving Eq

-- | User display frame mapping
data UserDisplayFrameMapping =
  UserDisplayFrameMapping { usdfFrame :: Frame,
                            usdfUser :: User,
                            usdfSubscription :: UserSubscription }

-- | User display actions.
data UserDisplayAction = CodaStop UserDisplayStopResponse

-- | User display stop response.
data UserDisplayStopResponse = UserDisplayStopResponse (TMVar (Either Error ()))

-- | Input dispatcher.
data InputDispatcher =
  InputDispatcher { indiRunning :: TVar Bool,
                    indiActions :: TQueue InputDispatcherAction
                    indiInterface :: Interface,
                    indiInterfaceSubscription :: InterfaceSubscription,
                    indiFrames :: TVar [InputDispatcherFrameMapping],
                    indiMessageHandlers :: TVar [InputDispatcherMessageHandler],
                    indiCommandHandlers :: TVar (HashMap Text [InputDisptacherCommandHandler]) }
  deriving Eq

-- | Input dispatcher actions
data InputDispatcherAction = IndaStop InputDispatcherStopResponse

-- | Input dispatcher stop response.
data InputDispatcherStopResponse = InputDispatcherStopResponse (TMVar (Either Error ()))

-- | Input dispatcher frame mapping
data InputDispatcherFrameMapping =
  InputDispatcherFrameMapping { idfmFrame :: Frame,
                                idfmSubscription :: FrameInputSubscription }
  deriving Eq

-- | Input dispatcher message handler.
data InputDispatcherMessageHandler =
  InputDispatcherMessageHandler { idmhDispatcher :: InputDispatcher,
                                  idmhHandler :: TVar (Frame -> StyledText -> AM Bool) }
  deriving Eq

-- | Input disptacher command handler.
data InputDispatcherCommandHandler =
  InputDispatcherCommandHandler { idchDispatcher :: InputDispatcher,
                                  idchCommand :: Text,
                                  idchHandler :: TVar (Frame -> Text -> StyledText -> AM Bool) }
  deriving Eq

-- | Whether to send a private or channel messsage.
data FrameMessageType = FrmtPrivate | FrmtChannel

-- | Whether to send a message to a specific frame or the most recently focused subframe.
data FrameTarget = FrtaSpecific | FrtaLastFocused

-- | CTCP handler.
data CtcpHandler =
  CtcpHandler { cthaInterface :: Interface,
                cthaRunning :: TVar Bool,
                cthaActions :: TQueue CtcpHandlerAction,
                cthaSubscription :: InterfaceSubscription,
                cthaConnectionManagers :: TVar [CtcpHandlerMapping] }

-- | CTCP handler connection manager mapping.
data CtcpHandlerMapping =
  CtcpHandlerMapping { cthmConnectionManager :: ConnectionManager,
                       cthmSubscription :: ConnectionManagerSubscription }

-- | CTCP handler action.
data CtcpHandlerAction = CthaStop CtcpHandlerStopResponse

-- | CTCP handler stop response.
newtype CtcpHandlerStopResponse = CtcpHandlerStopResponse (TMVar (Either Error ()))

-- | CTCP command
type CtcpCommand = ByteString

-- | CTCP argument
type CtcpArgument = ByteString
