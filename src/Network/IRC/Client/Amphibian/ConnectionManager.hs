module Network.IRC.Client.Amphibian.ConnectionManager

       (ConnectionManager,
        new,
        start,
        stop,
        connect,
        reconnect,
        disconnect,
        send,
        waitConnect,
        waitRecvonned,
        waitDisconnect,
        waitSend,
        subscribe,
        recv)

       where

import qualified Network.IRC.Client.Amphibian.Connection as C
import qualified Network.IRC.Client.Amphibian.User as U
import qualified Network.IRC.Client.Amphibian.Interface as I
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Commands
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Monad
import Data.Functor ((<$>))
import Control.Monad (mapM,
                      mapM_,
                      join)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               TMVar,
                               atomically,
                               orElse,
                               newTVar,
                               writeTVar,
                               readTVar,
                               newBroadcastTChan,
                               writeTChan,
                               dupTChan,
                               newEmptyTMVar,
                               putTMVar
                               readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8

-- | Create a connection manager.
new :: STM ConnectionManager
new actionGroup = do
  thread <- newTVar Nothing
  actions <- newTQueue
  events <- newBroadcastTChan
  stop <- newEmptyTMVar
  setup <- newTVar Nothing
  connection <- newTVar Nothing
  registered <- newTVar False
  host <- newTVar Nothing
  nick <- newTVar Nothing
  subscription <- newTVar Nothing
  motd <- newTVar []
  return ConnectionManager { comaAsync = thread,
                             comaActions = actions,
                             comaEvents = events,
                             comaStop = stop,
                             comaSetup = setup,
                             comaConnection = connection,
                             comaRegistered = registered,
                             comaHost = host,
                             comaNick = nick,
                             comaSubscription = subscription,
                             comdaMotd = motd }

-- | Get whether a connection manager is connected.
isConnected :: ConnectionManager -> STM Bool
isConnected manager = (/= Nothing) <$> readTVar $ comaConnection manager

-- | Get whether a connection manager is registered.
isRegistered :: ConnectionManager -> STM Bool
isRegistered manager = readTVar $ comaRegistered manager

-- | Get nick.
getNick :: ConnectionManager -> STM (Maybe Nick)
getNick manager = readTVar $ comaNick manager

-- | Set nick.
setNick :: ConnectionManager -> Nick -> STM ()
setNick manager nick = writeTVar (comaNick manager) (Just nick)

-- | Start connection manager.
start :: ConnectionManager -> AM ()
start manager = do
  registerConnectionManager manager
  intf <- getInterface
  thread <- readTVar (managerAsync manager)
  case thread of
    Nothing -> do
      thread <- liftIO . async $ runAM (connectionManager manager) intf
      liftIO . atomically $ writeTVar (managerAsync manager) thread
    Just _ -> return ()

-- | Stop connection manager.
stop :: ConnectionManager -> STM (ConnectionManagerStopResponse)
stop manager = do
  stop <- ConnectionManagerStopResponse <$> newEmptyTMVar
  putTMVar (comaStop manager) stop
  return stop

-- | Connect to a server.
connect :: ConnectionManager -> ServerSetup ->
           STM ConnectionManagerConnectResponse
connect manager setup = do
  response <- ConnectionManagerConnectResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaConnectNew setup response)
  return response

-- | Reconnect to a server.
reconnect :: ConnectionManager -> STM ConnectionManagerReconnectResponse
reconnect manager = do
  response <- ConnectionManagerReconnectResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaReconnect response)
  return response

-- | Disconnect from a server.
disconnect :: ConnectionManager -> STM ConnectionManagerDisconnectResponse
disconnect manager = do
  response <- ConnectionManagerDisconnectResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaDisconnect response)
  return response

-- | Send a message to a server.
send :: ConnectionManager -> IRCMessage ->
        STM ConnectionManagerSendResponse
send manager message = do
  response <- ConnectionManagerSendResponse <$> newEmptyTMVar
  writeTQueue (comaActions manager) (ComaSend message response)
  return response

-- | Wait for a connect response.
waitConnect :: ConnectionManagerConnectResponse -> STM (Either Error ())
waitConnect (ConnectionManagerConnectResponse response) =
  readTMVar response

-- | Wait for a reconnect response.
waitReconnect :: ConnectionManagerReconnectResponse -> STM (Either Error ())
waitReconnect (ConnectionManagerReconnectResponse response) =
  readTMVar response

-- | Wait for a disconnect response.
waitDisconnect :: ConnectionManagerDisconnectResponse -> STM (Either Error ())
waitDisconnect (ConnectionManagerDisconnectResponse response) =
  readTMVar response

-- | Wait for a send message response.
waitSend :: ConnectionManagerMessageResponse -> STM (Either Error ())
waitsend (ConnectionManagerMessageResponse response) =
  readTMVar response

-- | Subscribe to connection manager events.
subscribe :: ConnectionManager -> STM ConnectionManagerSubscription
subscribe manager =
  ConnectionManagerSubscription <$> dupTChan $ comaEvents manager

-- | Receive an event from a connection manager.
recv :: ConnectionManagerSubscription -> STM ConnectionManagerEvent
recv (ConnectionManagerSubscription chan) = readTChan chan

-- | Connection manager thread body.
connectionManager :: ConnectionManager -> AM ()
connectionManager manager =
  continue <- join . atomically $
    handleAction manager intf `orElse`
    handleEvent manager intf `orElse`
    handleStop manager intf
  if continue
    then connectionManager manager
    else return ()

-- | Handle action for connection manager.
handleAction :: ConnectionManager -> STM (AM Bool)
handleAction manager = do
  action <- readTQueue $ comaActions manager
  case action of
    ComaConnectNew setup (ConnectionManagerConnectResponse response) -> do
      writeTVar (comaSetup manager) setup
      return $ do
        result <- doConnect manager
        liftIO . atomically $ putTMVar response result
    ComaReconnect (ConnectManagerReconnectResponse response) ->
      return $ do
        result <- doReconnect manager
        liftIO . atomically $ putTMVar response result
    ComaDisconnect (ConnectionManagerDisconnectResponse response) ->
      return $ do
        result <- doDisconnect manager
        liftIO . atomically $ putTMVar response result
    ComaSend message (ConnectionManagerSendResponse response) ->
      return $ do
        result <- doSend manager message
        liftIO . atomically $ putTMVar response result
    
-- | Handle event for connection manager.
handleEvent :: ConnectionManager -> STM (AM Bool)
handleEvent manager = do
  subscription <- readTVar $ comaSubscription manager
  connection <- readTVar $ comaConnection manager
  case subscription of
    (subscription : rest) -> do
      event <- C.recv subscription
      case event of
        ConnLookupAddress _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnFoundAddress _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnLookupAddressFailed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          writeTVar (comaConnection manager) Nothing
          writeTVar (comaSubscription manager) rest
          return $ return True
        ConnFoundHostname hostName -> do
          writeTChan (comaHost manager) (Just hostName) 
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnReverseLookupFailed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnConnecting _ _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True
        ConnConnected _ _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ do
            intf <- getInterface
            async $ runAM (register manager) intf
            return True
        ConnConnectFailed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          writeTVar (comaConnection manager) Nothing
          writeTVar (comaSubscription manager) rest
          return $ return True
        ConnDisconnected _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          writeTVar (comaConnection manager) Nothing
          writeTVar (comaRegistered manager) False
          writeTVar (comaHost manager) Nothing
          writeTVar (comaSubscription manager) rest
          return $ return True
        ConnMessage message -> do
          writeTChan (comaEvents manager) (convertEvent event)
          handleMessage manager message
        ConnMalformed _ -> do
          writeTChan (comaEvents manager) (convertEvent event)
          return $ return True 
    None -> return $ return True

-- | Handle message.
handleMessage :: ConnectionManager -> IRCMessage -> STM (AM Bool)
handleMessage manager message@(IRCCommand { ircmPrefix = prefix, ircmCommand = command }) = do
  currentNick <- getNick manager
  if command == cmd_NICK
  then do case (extractNick prefix, ircmParameters message) of
            (Just oldNick, [newNick]) | oldNick == currentNick ->
              setNick manager newNick
            _ -> return ()
          return $ return True
  else if command == cmd_PRIVMSG
  then case (extractNick prefix, ircmParameters message) of
    (Just fromNick, [nick]) | nick == currentNick -> do
      return $ do
        intf <- getInterface
        join . liftIO . atomically $ do
          users <- I.getUsers intf
          nicks <- mapM U.getNick users
          if fromNick `notElem` nicks
          then do
            user <- U.new
            U.inject user $ ComaMessage message
            return $ do
              U.start user
              return True
          else return $ return True
    _ -> return $ return True
  else if command == cmd_NOTICE
  then case (extractNick prefix, ircmParameters message, ircmComment message) of
    (Just fromNick, [nick], Just comment) | nick == currentNick -> do
      return $ do
        intf <- getInterface
        liftIO . atomically $ do
          users <- I.getUsers intf
          nicks <- mapM U.getNick users
          if fromNick `notElem` nicks
          then writeTChan (comaEvents manage) (ComaRecvNotice fromNick comment)
          return True
    _ -> return $ return True
  else return $ return True

-- | Register with a server.
register :: ConnectionManager -> AM ()
register manager = do
  connection <- fromJust <$> liftIO . atomically . readTVar $ comaConnection manager
  continue <- 
  case continue of
    True -> do
      continue <- registerNick manager
      case continue of
        True -> registerUser manager
    False -> return ()

-- | Password registration delay (in microseconds).
passwordRegistrationDelay :: Int
passwordRegistrationDelay = 1000000
  
-- | Register a password with the server.
registerPassword :: ConnectionManager -> AM Bool
registerPassword manager =
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection -> do
      setup <- liftIO . atomically . readTVar $ comaSetup manager
      case comaPassword setup of
        Just password -> do
          subscription <- liftIO . atomically $ C.subscribe connection
          liftIO . atomically . C.send connection $
            IRCMessage { ircmPrefix = Nothing,
                         ircmCommand = cmd_PASS,
                         ircmParameters = [password],
                         ircmComment = Nothing }
          liftIO $ threadDelay passwordDelay
          event <- liftIO . atomically $ C.peek subscription
          case event of
            Just (ConnMessage (IRCCommand { ircmCommand = command })) ->
              | command == err_PASSWDMISMATCH -> do
                liftIO . atomically $ writeTChan (comaEvents manager) (ComaPasswordMismatch password)
                doDisconnect manager
                return False
              | command == err_YOUREBANNEDCREEP -> do
                liftIO . atomically $ writeTChan (comaEvents manager) ComaBannedFromServer
                doDisconnect manager
                return False
            _ -> return True
        Nothing -> return True
    Nothing -> return False

-- | Nick registration delay (in microseconds).
nickRegistrationDelay :: Int
nickRegistrationDelay = 1000000

-- | Register a nick with the server.
registerNick :: ConnectionManager -> AM Bool
registerNick manager = do
  storedNicks <- comaAllNicks <$> liftIO . atomically . readTVar $ comaSetup manager
  nick <- liftIO . atomically . readTVar $ comaNick manager
  let nicks = unique $ case nick of
    Just nick -> nick : storedNicks
    Nothing -> storedNicks
  registerCurrentNick manager nicks

-- | Register a particular nick with the server.
registerCurrentNick :: ConnectionManager -> [Nick] -> AM Bool
registerCurrentNick manager nicks@(nick : _) = do
  liftIO . atomically $ writeTChan (comaEvents manager) (ComaAttemptingNick nick)
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection -> do
      subscription <- liftIO . atomically $ C.subscribe connection
      liftIO . atomically . C.send connection $
        IRCMessage { ircmPrefix = Nothing,
                     ircmCommand = cmd_NICK,
                     ircmParameters = [nick],
                     ircmComment = Nothing }
      liftIO $ threadDelay nickRegistrationDelay
      event <- C.peek subscription
      case event of
        Just (ConnMessage (IRCCommand { ircmCommand = command, ircmComment = comment })) ->
          | err_NICKNAMEINUSE -> registerCurrentNick manager (nextNick nicks)
          | err_ERRONEUSNICKNAME ->
              case nicks of
                [nick] -> do
                  liftIO . atomically $ writeTChan (comaEvents manager) (ComaMalformedNick nick)
                (_ : next) -> registerCurrentNick manager next
          | err_NICKCOLLISION -> registerCurrentNick manager (nextNick nicks)
          | err_UNAVAILRESOURCE -> registerCurrentNick manager (nextNick nicks)
          | err_RESTRICTED -> do
              setNick manager nick
              return True
          | err_YOUREBANNEDCREEP -> do
              liftIO . atomically $ writeTChan (comaEvents manager) (ComaBannedFromServer comment)
              doDisconnect manager
              return False
        _ -> do
          liftIO . atomically $ setNick manager nick
          return True
    _ -> return False

-- | Get the next nick.
nextNick :: [Nick] -> [Nick]
nextNick [nick] = [B.append nick (BC.singleton '_')]
nextNick (_ : next) = next

-- | User registration delay (in microseconds).
userRegistrationDelay :: Int
userRegistrationDelay = 1000000

-- | Register a user with the server.
registerUser :: ConnectionManager -> AM ()
registerUser manager = do
  setup <- liftIO . atomically . readTVar $ comaSetup manager
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection ->
      subscription <- liftIO . atomically $ C.subscribe connection
      liftIO . atomically . C.send connection $
        IRCMessage { ircmPrefix = Nothing,
                     ircCommand = cmd_USER,
                     ircParameters = [comaUserName setup,
                                      convertUserMode $ comaMode setup,
                                      BC.singleton '*'],
                     ircComment = comaName setup }
      liftIO $ threadDelay userRegistrationDelay
      event <- liftIO . atomically $ C.recv subscription
      case event of
        ConnMessage message@(IRCMessage { ircmCommand = command }) ->
          | command == err_YOUREBANNEDCREEP -> do
             liftIO . atomically $ writeChan (comaEvents manager) ComaBannedFromServer
             doDisconnect
          | command == rpl_WELCOME ->
             liftIO . atomically $ do
               writeTVar (comaRegistered manager) True
               writeTChan (comaEvents manager) (ComaWelcome $ ircmComment message)
          | command == rpl_MOTDSTART ->
            case ircmComment message of
              Just comment -> liftIO . atomically $ writeTVar (comaMotd manager) [comment]
              Nothing -> liftIO . atomically $ writeTVar (comaMotd manager) []
          | command == rpl_MOTD ->
            case ircmComment message of
              Just comment -> liftIO . atomically $ do
                motd <- readTVar $ comaMotd manager
                writeTVar (comaMotd manager) (comment : motd)
              Nothing -> return ()
          | command == rpl_ENDOFMOTD ->
            case ircmComment message of
              Just comment -> liftIO . atomically $ do
                motd <- readTVar $ comaMotd manager
                writeTVar (comaMotd manager) []
                writeTChan (comaEvents manager) (ComaMotd . reverse $ comment : motd)
              Nothing -> liftIO . atomically $ do
                motd <- readTVar $ comaMotd manager
                writeTVar (comaMotd manager) []
                writeTChan (comaEvents manager) (ComaMotd $ reverse motd)
        _ -> liftIO . atomically $ writeTChan (comaEvents manager) ComaRegistrationFailed
    Nothing -> return ()

-- | Convert a user mode to a single message parameter.
convertUserMode :: [UserMode] -> MessageParameter
convertUserMode mode =
  let wallopsMode = if elem (BC.singleton 'w') mode then 4 else 0
      invisibleMode = if elem (BC.singleton 'i') mode then 8 else 0 in
  BC.pack . show $ wallopsMode + invisibleMode

-- | Actually connect to a connection.
doConnect :: ConnectionManager -> AM (Either Error ())
doConnect manager = do
  setup <- liftIO . atomically . readTVar $ comaSetup manager
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Nothing -> do
      connection <- liftIO . atomically $ do
        connection <- C.new
        writeTVar (comaConnection manager) (Just connection)
        subscription <- C.subscribe connection
        oldSubscription <- readTVar $ comaSubscription manager
        writeTVar (comaSubscription manager) (oldSubscription ++ [subscription])
        return connection
      response <- liftIO $ C.connect connection (comaOriginalHost setup) (comaPort setup)
      liftIO . atomically $ C.waitConnect response
    Just _ -> do
      errorText <- lookupText $ T.pack "already connected to server"
      return . Left $ Error [errorText]

-- | Actually reconnect to a connection.
doReconnect :: ConnectionManager -> AM (Either Error ())
doReconnect manager = do
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  result <- case connection of
    Just connection -> doDisconnect manager
    Nothing -> return $ Right ()
  case result of ->
    Right () -> doConnect manager
    Left error -> return $ Left error

-- | Actually disconnect from a connection.
doDisconnect :: ConnectionManager -> AM (Either Error ())
doDisconnect manager = do
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection ->
      response <- liftIO . atomically $ C.disconnect connection
      result <- liftIO . atomically $ do C.waitDisconnect response
      liftIO . atomically $ writeTVar (comaConnection manager) Nothing
      return result
    Nothing -> do
      errorText <- lookupText $ T.pack "not connected to server"
      return . Left $ Error [errorText]

-- | Actually send a message to a connection.
doSend :: ConnectionManager -> IRCMessage -> AM (Either Error ())
doSend manager message = do
  connection <- liftIO . atomically . readTVar $ comaConnection manager
  case connection of
    Just connection ->
      response <- liftIO . atomically $ C.send connection
      liftIO . atomically $ do C.waitSend response
    Nothing -> do
      errorText <- lookupText $ T.pack "not connected to server"
      return . Left $ Error [errorText]

-- | Convert events.
convertEvent :: ConnectionEvent -> ConnectionManagerEvent
convertEvent (ConnLookupAddress hostname) = ComaLookupAddress hostname
convertEvent (ConnFoundAddress addr) = ComaFoundAddress addr
convertEvent (ConnLookupAddressFailed error) = ComaLookupAddressFailed error
convertEvent (ConnFoundHostname hostname) = ComaFoundHostname hostname
convertEvent (ConnReverseLookupFailed error) = ComaReverseLookupFailed error
convertEvent (ConnConnecting hostname port) = ComaConnecting hostname port
convertEvent (ConnConnected hostname port) = ComaConnected hostname port
convertEvent (ConnConnectFailed error) = ComaConnectFailed error
convertEvent (ConnDisconnected error) = ComaDisconnected error
convertEvent (ConnMessage message) = ComaMessage message
convertEvent (ConnMalformed bytes) = ComaMalformed bytes

-- | Handle close for connection manager.
handleClose :: ConnectionManager -> ConnectionManagerState -> STM (AM (ConnectionManagerState, Bool))
handleClose manager state = do
  ConnectionManagerStopResponse response <- readTMVar $ comaStop manager
  handle <- readTVar $ comaHandle manager
  putTMVar response ()
  return $ do
    case comaHandle state of
      Just handle -> closeConnectionHandle handle
      Nothing -> return ()
    return (comaHandle { comaHandle = Nothing }, True)
