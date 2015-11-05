module Network.IRC.Client.Amphibian.Channel

       (Channel,
        ChannelName,
        ChannelKey,
        ChannelTopic,
        UserStatus,
        ChannelEvent,
        ChannelJoinResponse,
        ChannelPartResponse,
        ChannelMessageResponse,
        ChannelNoticeResponse,
        new,
        getConnectionManager,
        getName,
        getTopic,
        getTopicUser,
        getTopicTime,
        getType,
        getNames,
        getAutoJoin,
        getJoined,
        start,
        stop,
        waitStop,
        subscribe,
        peek,
        recv,
        join,
        waitJoin,
        part,
        waitPart,
        message,
        waitMessage,
        notice,
        waitNotice,
        setTopic,
        waitSetTopic)
       where

import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Commands
import Network.IRC.Client.Amphibian.Utility
import Data.Functor ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad as M
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar,
                               TChan,
                               atomically,
                               orElse,
                               newTVar,
                               writeTVar,
                               readTVar,
                               newEmptyTMVar,
                               putTMVar,
                               readTMVar,
                               newBroadcastTChan,
                               dupTChan,
                               writeTChan,
                               peekTChan,
                               readTChan)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Time.Clock (UTCTime,
                        getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime,
                              posixSecondsToUTCTime)

-- | Create channel.
new :: ConnectionManager -> ChannelName -> Maybe ChannelKey -> STM Channel
new manager name key = do
  async <- newTVar Nothing
  actions <- newTQueue
  events <- newBroadcastTChan
  key <- newTVar key
  topic <- newTVar Nothing
  topicUser <- newTVar Nothing
  topicTime <- newTVar Nothing
  channelType <- newTVar ChanPublic
  names <- newTVar []
  namesAccum <- newTVar []
  autoJoin <- newTVar False
  joined <- newTVar False
  joinResponse <- newTVar Nothing
  return $ Channel { chanAsync = async,
                     chanActions = actions,
                     chanEvents = events,
                     chanName = name,
                     chanConnectionManager = manager,
                     chanKey = key,
                     chanTopic = topic,
                     chanTopicUser = topicUser,
                     chanTopicTime = topicTime,
                     chanType = channelType,
                     chanNames = names,
                     chanNamesAccum = namesAccum,
                     chanAutoJoin = autoJoin,
                     chanJoined = joined,
                     chanJoinResponse = joinResponse }

-- | Get connection manager
getConnectionManager :: Channel -> ConnectionManager
getConnectionManager channel = chanConnectionManager channel

-- | Get channel name.
getName :: Channel -> ChannelName
getName channel = chanName channel

-- | Get channel topic.
getTopic :: Channel -> STM (Maybe ChannelTopic)
getTopic channel = readTVar $ chanTopic channel

-- | Get channel topic user.
getTopicUser :: Channel -> STM (Maybe FullUser)
getTopicUser channel = readTVar $ chanTopicUser channel

-- | Get channel topic time.
getTopicTime :: Channel -> STM UTCTime
getTopicTime channel = readTVar $ chanTopicTime channel

-- | Get channel type.
getType :: Channel -> STM ChannelType
getType channel = readTVar $ chanType channel

-- | Get channel names.
getNames :: Channel -> STM [(Nick, UserStatus)]
getNames channel = readTVar $ chanNames channel

-- | Get whether the channel will be auto-joined.
getAutoJoin :: Channel -> STM Bool
getAutoJoin channel = readTVar $ chanAutoJoin channel

-- | Get whether the channel is joined.
getJoined :: Channel -> STM Bool
getJoined channel = readTVar $ chanJoined channel

-- | Start a channel.
start :: Channel -> AM ()
start channel = do
  registerChannel channel
  currentAsync <- liftIO . atomically $ do
    currentAsync <- readTVar $ chanAsync channel
    case currentAsync of
      Nothing -> writeTVar (chanAsync channel) (Just undefined)
      _ -> return ()
    return currentAsync
  case currentAsync of
    Nothing -> do
      intf <- getInterface
      newAsync <- async $ runAM (runChannel channel) intf
      liftIO . atomically $ writeTVar (chanAsync channel) (Just newAsync)
    _ -> return ()

-- | Stop a channel.
stop :: Channel -> STM ChannelStopResponse
stop channel = do
  response <- ChannelStopResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanStop response)
  return response

-- | Wait for a response to stopping a channel.
waitStop :: ChannelStopResponse -> STM (Either Error ())
waitStop (ChannelStopResponse response) = readTMVar response

-- | Subscribe to events for a channel.
subscribe :: Channel -> STM ChannelSubscription
subscribe channel = ChannelSubscription <$> dupTChan $ chanEvents channel

-- | Peek an event from a channel subscription.
peek :: ChannelSubscription -> STM ChannelEvent
peek (ChannelSubscription events) = peekTChan events

-- | Receive an event from a channel subscription
recv :: ChannelSubscripton -> STM ChannelEvent
recv (ChannelSubscription events) = readTChan events

-- | Join a channel.
join :: Channel -> STM ChannelJoinResponse
join channel = do
  response <- ChannelJoinResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanJoin response)
  return response

-- | Wait for a response to joining a channel.
waitJoin :: ChannelJoinResponse -> STM (Either Error ())
waitJoin (ChannelJoinResponse response) = readTMVar response

-- | Part from a channel.
part :: Channel -> Maybe MessageComment -> STM ChannelPartResponse
part channel comment = do
  response <- ChannelPartResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanPart comment response)
  return response

-- | Wait for a response to parting from a channel.
waitPart :: ChannelPartResponse -> STM (Either Error ())
waitPart (ChannelPartResponse response) = readTMVar response

-- | Send a message to a channel.
message :: Channel -> MessageComment -> STM ChannelMessageResponse
message channel comment = do
  response <- ChannelMessageResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanMessage comment response)
  return response

-- | Wait for a response to sending a message to a channel.
waitMessage :: ChannelMessageResponse -> STM (Either Error ())
waitMessage (ChannelMessageResponse response) = readTMVar response

-- | Send a notice to a channel.
notice :: Channel -> MessageComment -> STM ChannelNoticeResponse
notice channel comment = do
  response <- ChannelNoticeResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanNotice comment response)
  return response

-- | Wait for a response to sending a notice to a channel.
waitNotice :: ChannelNoticeResponse -> STM (Either Error ())
waitNotice (ChannelNoticeResponse response) = readTMVar response

-- | Set the topic of a channel.
setTopic :: Channel -> MessageComment -> STM ChannelSetTopicResponse
setTopic channel comment = do
  response <- ChannelSetTopicResponse <$> newEmptyTMVar
  writeTQueue (chanActions channel) (ChanSetTopic comment response)
  return response

-- | Wait for a response to setting the topic of a channel.
waitSetTopic :: ChannelSetTopicResponse -> STM (Either Error ())
waitSetTopic (ChannelSetTopicResponse response) = readTMVar response

-- | Run a channel.
runChannel :: Channel -> AM ()
runChannel channel = do
  continue <- M.join . liftIO . atomically $ handleActions channel `orElse` handleEvents channel
  if continue
  then runChannel channel
  else return ()

-- | Handle actions.
handleActions :: Channel -> STM (AM Bool)
handleActions channel = do
  action <- readTQueue $ chanActions channel
  case action of
    ChanStop response -> doStop channel response
    ChanJoin response -> doJoin channel response
    ChanPart comment response -> doPart channel comment response
    ChanMessage comment response -> doMessage channel comment response
    ChanNotice comment response -> doNotice channel comment response
    ChanSetTopic comment response -> doSetTopic channel comment response

-- | Carry out stop.
doStop :: Channel -> ChannelStopResponse -> STM (AM Bool)
doStop channel (ChannelStopResponse response) = do
  putTMVar response $ Right ()
  return $ return False

-- | Carry out join.
doJoin :: Channel -> ChannelJoinResponse -> STM (AM Bool)
doJoin channel (ChannelJoinResponse response) = do
  autoJoin <- readTVar $ chanAutoJoin channel
  if not autJoin
  then do
    writeTVar (chanAutoJoin channel) True
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_JOIN,
                                                                           ircmParameters = [chanName channel],
                                                                           ircmComment = Nothing }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <- lookupText "already joined channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out part.
doPart :: Channel -> Maybe MessageComment -> ChannelPartResponse -> STM (AM Bool)
doPart channel comment (ChannelPartResponse response) = do
  autoJoin <- readTVar $ chanAutoJoin channel
  if autoJoin
  then do
    writeTVar (chanAutoJoin channel) False
    writeTVar (chanJoined channel) False
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_PART,
                                                                           ircmParameters = [chanName channel],
                                                                           ircmComment = comment }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <- lookupText "not joined channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out message.
doMessage :: Channel -> MessageComment -> ChannelMessageResponse -> STM (AM Bool)
doMessage channel comment (ChannelMessageResponse response) = do
  joined <- readTVar $ chanJoined channel
  if joined
  then do
    nick <- CM.getNick $ chanConnectionManager channel
    writeTChan (chanEvents channel) (ChanSelfMessage nick comment)
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_PRIVMSG,
                                                                           ircmParameters = [chanName channel].
                                                                           ircmComment = Just comment }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <-lookupText "not in channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out notice.
doNotice :: Channel -> MessageComment -> ChannelNoticeResponse -> STM (AM Bool)
doNotice channel comment (ChannelNoticeResponse response) = do
  joined <- readTVar $ chanJoined channel
  if joined
  then do
    nick <- CM.getNick $ chanConnectionManager channel
    writeTChan (chanEvents channel) (ChanSelfNotice nick comment)
    sendResponse <- CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                           ircmCommand = cmd_NOTICE,
                                                                           ircmParameters = [chanName channel].
                                                                           ircmComment = Just comment }
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <-lookupText "not in channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Carry out set topic.
doSetTopic :: Channel -> MessageComment -> ChannelSetTopicResponse -> STM (AM Bool)
doSetTopic channel comment (ChannelSetTopicResponse response) = do
  joined <- readTVar $ chanJoined channel
  if joined
  then do
    sendResponse < CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                                          ircmCommand = cmd_TOPIC,
                                                                          ircmParameters = [chanName channel],
                                                                          ircmComment = Just comment }
    writeTVar (chanTopic channel) (Just comment)
    let relay = liftIO . atomically $ do errorResponse <- waitSend sendResponse
                                         putTMVar response errorResponse
    return $ do intf <- getInterface
                liftIO . async $ runAM relay intf
                return True
  else return $ do errorText <- lookupText "not in channel"
                   liftIO . atomically . putTMVar response . Left $ Error [errorText]
                   return True

-- | Handle events.
handleEvents :: Channel -> STM (AM Bool)
handleEvents channel = do
  autoJoin <- readTVar $ chanAutoJoin channel
  if autoJoin
  then do
    event <- CM.recv $ chanConnectionManager channel
    case event of
      ComaMessage message@(IRCMessage { ircmCommand = command })
        | command == cmd_JOIN -> handleJoin channel message
        | command == cmd_PART -> handlePart channel message
        | command == cmd_NICK -> handleNick channel message
        | command == cmd_PRIVMSG -> handlePrivmsg channel message
        | command == cmd_NOTICE -> handleNotice channel message
        | command == cmd_TOPIC -> handleTopic channel messsage
        | command == rpl_NAMREPLY -> handleRplNamreply channel message
        | command == rpl_ENDOFNAMES -> handleRplEndofnames channel message
        | command == rpl_NOTOPIC -> handleRplNotopic channel message
        | command == rpl_TOPIC -> handleRplTopic channel message
        | command == rpl_TOPICWHOTIME -> handleRplTopicWhoTime channel message
        | command == cmd_QUIT -> handleQuit channel message
      ComaDisconnected error -> writeTChan (chanEvents channel) (ChanDisconnected error)
  else return $ return True

-- | Handle JOIN message.
handleJoin :: Channel -> IRCMessage -> STM (AM Bool)
handleJoin channel message = do
  currentNick <- CM.getNick $ chanConnectionManager channel
  case (extractNick $ ircmPrefix message, ircmComment message) of
    (Just nick, Just channelName)
      | nick == currentNick && channelName == chanName channel -> do
        response <- readTVar $ chanJoinResponse channel
        case response of
          Just response -> do
            putTMVar response (Right ())
          Nothing -> return ()
        writeTVar (chanJoined channel) True
        writeTChan (chanEvents channel) ChanJoined
      | channelName == chanName channel ->
        writeTChan (chanEvents channel) (ChanRecvJoin nick (ircPrefix message))
        CM.send (chanConnectionManager channel) $ IRCMessage { ircmPrefix = Nothing,
                                                               ircmCommand = cmd_NAMES,
                                                               ircmParameters = [chanName channel],
                                                               ircmComment = Nothing }
    _ -> return ()
  return $ return True

-- | Handle PART message.
handlePart :: Channel -> IRCMessage -> STM (AM Bool)
handlePart channel message = do
  currentNick <- CM.getNick $ chanConnectionManager channel
  case extractNick $ ircmPrefix message of
    Just nick
      | nick == currentNick ->
        writeTChan (chanEvents channel) (ChanParted $ ircmComment comment)
      | otherwise ->
        writeTChan (chanEvents channel) (ChanRecvPart nick (ircmPrefix prefix) (ircmComment comment))
        nicks <- filter (\(knownNick, _) -> knownNick /= nick) <$> readTVar $ chanNames channel
        writeTVar (chanNames channel) nicks
    _ -> return ()
  return $ return True

-- | Handle NICK message.
handleNick :: Channel -> IRCMessage -> STM (AM Bool)
handleNick channel message = do
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just oldNick, [newNick]) -> do
      inChannel <- isNickInChannel channel oldNick
      if inChannel
      then do
        oldNicks <- readTVar $ chanNames channel
        let newNicks = map (\(nick, status) -> (if nick == oldNick then newNick else nick, status)) oldNicks
        writeTVar (chanNames channel) newNicks
        writeTChan (chanEvents channel) (ChanRecvNick oldNick newNick)
      else return ()
    _ -> return ()
  return $ return True

-- | Handle PRIVMSG message.
handlePrivmsg :: Channel -> IRCMessage -> STM (AM Bool)
handlePrivmsg channel message = do
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | channelName == chanName channel -> writeTChan (chanEvents channel) (ChanRecvMessage nick (ircmComment message))
    _ -> return ()
  return $ return True

-- | Handle NOTICE message.
handleNotice :: Channel -> IRCMessage -> STM (AM Bool)
handleNotice channel message = do
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | channelName == chanName channel -> writeTChan (chanEvents channel) (ChanRecvNotice nick (ircmComment message))
    _ -> return ()
  return $ return True

-- | Handle TOPIC message.
handleTopic :: Channel -> IRCMessage -> STM (AM Bool)
handleTopic channel message = do
  case (extractNick $ ircmPrefix message, ircmParameters message) of
    (Just nick, [channelName])
      | channelName == chanName channel -> do
        return $ do
          time <- liftIO getCurrentTime
          liftIO . atomically $ do
            writeTVar (chanTopic channel) (Just $ ircmComment message)
            writeTVar (chanTopicUser channel) (Just $ ircmPrefix message)
            writeTVar (chanTopicTime channel) (Just time)
            writeTChan (chanEvents channel) (ChanRecvTopic nick $ ircmComment message)
            return True
    _ -> return $ return True

-- | Handle rpl_NAMREPLY message.
handleRplNamrply :: Channel -> IRCMessage -> STM (AM Bool)
handleRplNamrply channel message = do
  let parameters = ircmParameters message
  case fromEnd 1 parameters of
    Just name | name == chanName channel -> do
      case fromEnd 2 parameters of
        Just channelType
          | channelType == BUTF8.fromString "=" -> writeTVar (chanType channel) ChanPublic
          | channelType == BUTF8.fromString "*" -> writeTVar (chanType channel) ChanPrivate
          | channelType == BUTF8.fromString "@" -> writeTVar (chanType channel) ChanSecret
        _ -> return ()
      case ircmComment message of
        Just comment -> do
          let names = splitNames comment
          oldNames <- readTVar $ chanNames channel
          writeTVar (chanNames channel) (names ++ oldNames)
        Nothing -> return ()
    _ -> return ()
  return $ return True
 
-- | Split names in a rpl_NAMREPLY message
splitNames :: MessageComment -> [(Nick, UserStatus)]
splitNames comment = splitNames' comment []
  where splitNames' comment names =
          let (name, rest) = BUTF8.break (== ' ') comment in
          let names = if BUTF8.length name > 0
                      then case BUTF8.uncons name of
                             Just ('@', name) -> (name, UserOp) : names
                             Just ('%', name) -> (name, UserHalfOp) : names
                             Just ('+', name) -> (name, UserVoice) : names
                             _ -> (name, UserNormal) : names
                      else names
              rest = BUTF8.drop 1 rest in
          if BUTF8.length rest > 0
          then splitNames' rest names
          else reverse names

-- | Handle rpl_ENDOFNAMES message.
handleRplEndofnames :: Channel -> IRCMessage -> STM (AM Bool)
handleRplEndofnames channel message = do
  case fromEnd 1 $ ircmParameters message of
    Just name | name == chanName channel -> do
      names <- readTVar $ chanNamesAccum channel
      writeTVar (chanNames channel) names
      writeTVar (chanNamesAccum channel) []
      channelType <- readTVar $ chanType channel
      writeTChan (chanEvents channel) (ChanType channelType)
      writeTChan (chanEvents channel) (ChanNames names)
    _ -> return ()
  return $ return True

-- | Handle rpl_NOTOPIC message.
handleRplNotopic :: Channel -> IRCMessage -> STM (AM Bool)
handleRplNotopic channel message = do
  case fromEnd 1 $ ircmParameters message of
    Just name | name == chanName channel -> do
      writeTVar (chanTopic channel) Nothing
      writeTChan (chanEvents channel) ChanNoTopic
    _ -> return ()
  return $ return True

-- | Handle rpl_TOPIC message.
handleRplTopic :: Channel -> IRCMessage -> STM (AM Bool)
handleRplTopic channel message = do
  case (fromEnd 1 $ ircmParameters message, ircmComment message) of
    (Just name, Just comment) | name == chanName channel -> do
      writeTVar (chanTopic channel) comment
      writeTChan (chanEvents channel) (ChanTopic comment)
    _ -> return ()
  return $ return True

-- | Handle rpl_TOPICWHOTIME message.
handleRplTopicWhoTime :: Channel -> IRCMessage -> STM (AM Bool)
handleRplTopicWhoTime channel message = do
  case ircmParameters message of
    [name, user, timeString] | name == chanName channel -> do
      case readMaybe timeString :: Maybe Integer of
        Just time -> do
          let time' = posixSecondsToUTCTime $ realToFrac time
          writeTVar (chanTopicUser channel) user
          writeTVar (chanTopicTime channel) time'
          writeTChan (chanEvents channel) (ChanTopicWhoTime user time')
        _ -> return ()
    _ -> return ()
  return $ return True

-- | Handle QUIT message.
handleQuit :: Channel -> IRCMessage -> STM (AM Bool)
handleQuit channel message = do
  currentNick <- CM.getNick $ chanConnectionManager channel
  case extractNick $ ircmPrefix message of
    Just nick
      | nick /= currentNick ->
        writeTChan (chanEvents channel) (ChanRecvQuit nick (ircmPrefix prefix) (ircmComment comment))
        nicks <- filter (\(knownNick, _) -> knownNick /= nick) <$> readTVar $ chanNames channel
        writeTVar (chanNames channel) nicks
    _ -> return ()
  return $ return True

-- | Get the item in a position from the end of a list
fromEnd :: Int -> [a] -> Maybe a
fromEnd position list
  | length list < position = Nothing
  | otherwise = case drop ((length list) - position) list of
                  item : _ -> Just item
                  _ -> Nothing

-- | Get whether a nick is in a channel.
isNickInChannel :: Channel -> Nick -> STM Bool
isNickInChannel channel nick = elem nick <$> map fst <$> readTVar $ chanNames channel
