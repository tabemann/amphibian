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

{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}

module Network.IRC.Client.Amphibian.Client

  (runClient)

where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.IRCConnection
import Network.IRC.Client.Amphibian.UI
import Network.IRC.Client.Amphibian.ServerReplies
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Text.Encoding (encodeUtf8,
                           decodeUtf8With,
                           lenientDecode)
import Control.Functor ((<$>),
                        fmap)
import Data.Sequence ((|>),
                      (:<))
import Data.Foldable (foldl',
                      toList)
import System.IO (stderr)
import Data.Text.IO (hPutStr)
import Text.Printf (printf)
import Control.Monad ((=<<),
                      join,
                      mapM,
                      forM,
                      forM_))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async,
                                 async
                                 cancel)
import Control.Concurrent.STM (STM,
                               atomically,
                               orElse,
                               retry,
                               TVar,
                               writeTVar,
                               readTVar)

-- | Run the client.
runClient :: IO ()
runClient = do
  response <- initWindowing
  result <- atomically $ getResponse response
  case result of
    Right () -> do
      running <- atomically $ newTVar True
      nextIndex <- atomically $ newTVar 0
      nextTabSelectIndex <- atomically $ newTVar 0
      nextWIndowFocusIndex <- atomically $ newTVar 0
      sessions <- atomically $ newTVar S.empty
      channels <- atomically $ newTVar S.empty
      users <- atomically $ newTVar S.empty
      windows <- atomically $ newTVar S.empty
      tabs <- atomically $ newTVar S.empty
      let settings =
            Settings { settingsReconnectDelay = 10.0 }
          client =
            Client { clientRunning = running,
                     clientNextIndex = nextIndex,
                     clientNextTabSelectIndex = nextTabSelectIndex,
                     clientNextWindowSelectIndex = nextWindowSelectIndex,
                     clientSessions = sessions,
                     clientChannels = channels,
                     clientUsers = users,
                     clientWindows = windows,
                     clientTabs = tabs,
                     clientSettings = settings }
      result <- openClientWindow client "Amphibian IRC" "<Not Connected>"
      case result of
        Right _ -> handleClientEvents client
        Left (Error errorText) -> hPutStr stderr errorText
    Left (Error errorText) -> hPutStr stderr errorText

-- | Get next client index.
getNextClientIndex :: Client -> STM Integer
getNextClientIndex client = do
  index <- readTVar $ clientNextIndex client
  writeTVar (clientNextIndex client) $ index + 1
  return index

-- | Get next client tab select index.
getNextClientTabSelectIndex :: Client -> STM Integer
getNextClientTabSelectIndex client = do
  index <- readTVar $ clientNextTabSelectIndex client
  writeTVar (clientNextTabSelectIndex client) $ index + 1
  return index

-- | Get next client window focus index.
getNextClientWindowFocusIndex :: Client -> STM Integer
getNextClientWindowFocusIndex client = do
  index <- readTVar $ clientNextWindowFocusIndex client
  writeTVar (clientNextWindowFocusIndex client) $ index + 1
  return index

-- | Open a client window.
openClientWindow :: Client -> T.Text -> T.Text -> IO (Either Error ClientWindow)
openClientWindow client windowTitle tabTitle = do
  window <- atomically $ newWindow
  windowEventSub <- atomically $ subscribeWindow window
  result <- startWindow window
  case result of
    Right () -> do
      response <- atomically $ openWindow window windowTitle
      result <- atomically $ getResponse response
      case result of
        Right () -> do
          index <- atomically $ getNextClientIndex client
          focusIndex <- atomically $ newTVar (-1)
          let clientWindow =
                ClientWindow { clientWindowIndex = index,
                               clientWindowFocusIndex = focusIndex,
                               clientWindowWindow = window,
                               clientWindowEventSub = windowEventSub }
          atomically $ do
            windows <- readTVar $ clientWindows client
            writeTVar (clientWindows client) $ windows |> clientWindow
          clientTab <- openClientTab client clientWindow tabTitle
          case clientTab of
            Right _ -> return $ Right clientWindow
            Left failure -> do
              (stopWindow window) >> return ()
              return $ Left failure
        Left failure -> do
          (stopWindow window) >> return ()
          return $ Left failure
    Left failure -> return $ Left failure

-- | Open a client tab.
openClientTab :: Client -> ClientWindow -> T.Text -> IO (Either Error ClientTab)
openClientTab client clientWindow tabTitle = do
  response <- atomically $ openTab window tabTitle
  result <- atomically $ getResponse response
  case result of
    Right tab -> do
      index' <- atomically $ getNextClientIndex client
      selectIndex <- atomically $ newTVar (-1)
      tabEventSub <- atomically $ subscribeTab tab
      subtype <- atomically $ newTVar FreeTab
      let clientTab =
            ClientTab { clientTabIndex = index,
                        clientTabSelectIndex = selectIndex,
                        clientTabTab = tab,
                        clientTabEventSub = tabEventSub,
                        clientTabSubtype = subtypes,
                        clientTabWindow = clientWindow }
      atomically $ do
        tabs <- readTVar $ clientTabs client
        writeTVar (ciientTabs client) $ tabs |> clientTabs
      return $ Right clientTab
    Left failure -> do
      return $ Left failure

-- | Handle client events.
handleClientEvents :: Client -> IO ()
handleClientEvents client = do
  event <- atomically . getClientEvent $ client
  case event of
    TaggedClientQuitEvent -> return ()
    TaggedSessionEvent session event -> do
      handleSessionEvent client session event
      handleClientEvents client
    TaggedClientWindowEvent clientWindow event -> do
      handleClientWindowEvent client clientWindow event
      handleClientEvents client
    TaggedClientTabEvent clientTab event -> do
      handleClientTabEvent client clientTab event
      handleClientEvents client
    
-- | Get an event for a client.
getClientEvent :: Client -> STM ClientTaggedEvent
getClientEvent client = do
  running <- readTVar $ clientRunning client
  ircConnectionEventSubs <- extractIRCConnectionEventSubs =<<
                            (readTVar $ clientSessions client)
  windowEventSubs <- extractWindowEventSubs =<<
                     (readTVar $ clientWindows client)
  tabsEventSubs <- extractTabEventSubs =<<
                   (readTVar $ clientTabs client)
  if running
    then do
      getTaggedSessionEvent ircConnectionEventSubs `orElse`
      getTaggedClientWindowEvent windowEventSubs `orElse`
      getTaggedClientTabEvent tabEventSubs
    else return TaggedClientQuitEvent
  where extractIRConnectionEventSubs =
          fmap (\session -> (session, sessionIRCConnectionEventSub session))
        extractWindowEventSubs =
          fmap (\clientWindow -> (clientWindow,
                                  clientWindowEventSub clientWindow))
        extractTabEventSubs =
          fmap (\clientTab -> (clientTab, clientTabEventSub clientTab))
        getTaggedSessionEvent =
          foldl` (\action (session, ircConnectionEventSub) ->
                    action `orElse` (TaggedSessionEvent session <$>
                                     recvIRCConnection ircConnectionEventSub))
          retry
        getTaggedClientWindowEvent =
          foldl' (\action (clientWindow, windowEventSub) ->
                    action `orElse` (TaggedClientWindowEvent clientWindow <$>
                                     recvWindow windowEventSub)) retry
        getTaggedClientTabEvent =
          foldl' (\action (clientTab, tabEventSub) ->
                     action `orElse` (TaggedClientTabEvent clientTab <$>
                                      recvTab tabEventSub)) retry

-- | Find client tabs for session.
findSessionTabsForSession :: Client -> Session -> STM (S.Seq ClientTab)
findSessionTabsForSession client session = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return $ S.filter (matchTab session) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab session (tab, SessionTab currentSession) =
          sessionIndex session == sessionIndex currentSession
        matchTab _ _ = False

-- | Find client tabs for channel.
findChannelTabsForChannel :: Client -> Channel -> STM (S.Seq ClientTab)
findChannelTabsForChannel client channel = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return $ S.filter (matchTab channel) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab channel (tab, ChannelTab currentChannel) =
          channelIndex channel == channelIndex currentChannel
        matchTab _ _ = False

-- | Find client tabs for user.
findUserTabsForUser :: Client -> User -> STM (S.Seq ClientTab)
findUserTabsForUser client user = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return $ S.filter (matchTab user) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab user (tab, UserTab currentUser) =
          userIndex user == userIndex currentUser
        matchTab _ _ = False

-- | Find all client tabs for user.
findAllTabsForUser :: Client -> User -> STM (S.Seq ClientTab)
findAllTabsForUser client user = do
  return $ foldM (matchTab user) S.empty =<< (readTVar $ clientTabs client)
  where matchTab user foundTabs tab = do
          subtype <- readTVar $ clientTabSubtype tab
          case subtype of
            UserTab user'
              | userIndex user == userIndex user' -> return $ foundTabs |> tab
              | otherwise -> return foundTabs
            ChannelTab channel -> do
              inChannel <- isUserInChannel channel user
              if inChannel
                then return $ foundTabs |> tab
                else return foundTabs
            _ -> return foundTabs

-- | Find most recent tab for session.
findMostRecentTabForSession :: Client -> Session -> STM (Maybe ClientTab)
findMostRecentTabForSession client session = do
  findMostRecentTabForSession' session (-1) Nothing =<<
    (readTVar $ clientTabs client)
  where findMostRecentTabForSession' session index currentTab tabs =
          case S.viewl tabs of
            tab :< rest -> do
              subtype <- readTVar $ clientTabSubtype tab
              if
                case subtype of
                  SessionTab session' ->
                    sessionIndex session == sessionIndex session'
                  ChannelTab channel ->
                    sessionIndex session ==
                    (sessionIndex $ channelSession channel)
                  UserTab user ->
                    sessionIndes session ==
                    (sessionIndex $ userSession user)
                  _ -> False
                then do
                  index' <- atomically . readTVar $ clientTabSelectIndex tab
                  if index' > index
                    then findMostRecentTabForSession' session index'
                         (Just tab) rest
                    else findMostRecentTabForSession' session index
                         currentTab rest
                else findMostRecentTabForSession' session index currentTab
                     rest
            S.EmptyL -> return currentTab

-- | Find the most recent tab.
findMostRecentTab :: Client -> STM (Maybe ClientTab)
findMostRecentTab client =
  findMostRecentTab' (-1) Nothing =<< (readTVar $ clientTabs client)
  where findMostRecentab' index currentTab tabs =
          case S.viewl tabs of
            tab :< rest -> do
              index' <- atomically . readTVar $ clientTabSelectIndex tab
              if index' > index
                then findMostRecentTab' index' (Just tab) rest
                else findMostRecentTab' index currentTab rest
            S.EmptyL -> return currentTab

-- | Find the most recent window.
findMostRecentWindow :: Client -> STM (Maybe ClientWindow)
findMostRecentWindow client =
  findMostRecentWindow' (-1) Nothing =<< (readTVar $ clientWindows client)
  where findMostRecentWindow' index currentWindow windows =
          case S.viewl windows of
            window :< rest -> do
              index' <- atomically . readTVar $ clientWindowFocusIndex window
              if index' > index
                then findMostRecentWindow' index' (Just window) rest
                else findMostRecentWindow' index currentWindow rest
            S.EmptyL -> return currentWindow

-- | Display session message.
displaySessionMessage :: Client -> Session -> T.Text -> IO ()
displaySessionMessage client session message = do
  responses <- atomically $ do
    clientTabs <- findSessionTabsForSession client session
    clientTabs <-
      if S.length clientTabs > 0
      then return clientTabs
      else do
        clientTab <- getCurrentTabForSession client session
        case clientTab of
          Just clientTab -> return $ S.singleton clientTab
          Nothing -> return S.empty
    forM clientTabs $ \clientTab ->
      addTabText (clientTabTab clientTab) message
  async $ do
    forM_ responses $ \response -> do
      result <- atomically $ getResponse response
      case result of
        Right () -> return ()
        Left (Error errorText) -> hPutStr stderr errorText

-- | Display channel message.
displayChannelMessage :: Clieant -> Channel -> T.Text -> IO ()
displayChannelMessage client channel message = do
  responses <- atomically $ do
    clientTabs <- findChannelTabsForChannel client channel
    forM clientTabs $ \clientTab ->
      addTabText (clientTabTab clientTab) message
  async $ do
    forM_ responses $ \response -> do
      result <- atomically $ getResponse response
      case result of
        Right () -> return ()
        Left (Error errorText) -> hPutStr stderr errorText

-- | Display user message.
displayUserMessage :: Client -> User -> T.Text -> IO ()
displayUserMessage client user message = do
  responses <- atomically $ do
    clientTabs <- findUserTabsForUser client user
    clientTabs <-
      if S.length clientTabs > 0
      then return clientTabs
      else do
        clientTab <- getCurrentTabForSession client $ userSession user
        case clientTab of
          Just clientTab -> return $ S.singleton clientTab
          Nothing -> return S.empty
    forM clientTabs $ \clientTab ->
      addTabText (clientTabTab clientTab) message
  async $ do
    forM_ responses $ \response -> do
      result <- atomically $ getResponse response
      case result of
        Right () -> return ()
        Left (Error errorText) -> hPutStr stderr errorText

-- | Display message in all tabs for user.
displayUserMessageAll :: Client -> User -> T.Text -> IO ()
displayUserMessageAll client user message = do
  responses <- atomically $ do
    tabs <- findAllUserTabsForUser client user
    forM tabs $ \clientTab ->
      addTabText (clientTabTab clientTab) message
  async $ do
    forM_ responses $ \response -> do
      result <- atomically $ getResponse response
      case result of
        Right () -> return ()
        Left (Error errorText) -> hPutStr stderr errorText

-- | Display message on most recent tab for session.
displaySessionMessageOnMostRecentTab :: Client -> Session -> T.Text -> IO ()
displaySessionMessageOnMostRecentTab client session message = do
  response <- atomically $ do
    clientTab <- findMostRecentTabForSession client session
    case clientTab of
      Just clientTab -> Just <$> addTabText (clientTabTab clientTab) message
      Nothing -> return Nothing
  case response of
    Just response -> do
      async $ do
        result <- atomically $ getResponse response
        case result of
          Right () -> return ()
          Left (Error errorText) -> hPutStr stderr errorText
    Nothing -> return ()

-- | Display message on tab.
displayMessage :: Client -> ClientTab -> T.Text -> IO ()
displayMessage client tab message = do
  response <- atomically $ addTabText (clientTabTab clientTab) message
  async $ do
    result <- atomically $ getResponse response
    case result of
      Right () -> return ()
      Left (Error errorText) -> hPutStr stderr errorText

-- | Display message on tabs.
displayMessageOnTabs :: Client -> S.Seq ClientTab -> T.Text -> IO ()
displayMessageOnTabs client tabs message = do
  responses <- atomically $
    forM tabs $ \clientTab -> addTabText (clientTabTab clientTab) message
  async $ do
    forM_ responses $ \response -> do
      result <- atomically $ getResponse response
      case result of
        Right () -> return ()
        Left (Error errorText) -> hPutStr stderr errorText

-- | Handle a session event.
handleSessionEvent :: Client -> Session -> IRCConnectionEvent -> IO ()
handleSessionEvent client session event =
  case event of
    IRCFoundAddr address -> do
      displaySessionMessage client session
        (printf "* Found address: %s" $ show address) (return ())
    IRCNoAddrFound (Error errorText) -> do
      displaySessionMessage client session
        (printf "* Unable to find address: %s" errorText)
      tryReconnectSession client session
    IRCLookupCanceled -> do
      displaySessionMessage client session
        "* Address lookup canceled"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCFoundName hostname -> do
      displaySessionMessage client session
        (printf "* Found hostname: %s" hostname) (return ())
    IRCNoNameFound (Error errorText) -> do
      displaySessionMessage client session
        (printf "* No name found: %s" errorText) (return ())
    IRCReverseLookupCanceled -> do
      displaySessionMessage client session
        "* Reverse lookup canceled"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCConnectingFailed (Error errorText) -> do
      displaySessionMessage client session
        (printf "* Connecting failed: %s" errorText)
      tryReconnectSession client session
    IRCConnected -> do
      let connection = sessionIRCConnection session
      hostname <- getIRCConnectionHostname connection
      port <- getIRCConnectionPort connection
      displaySessionMessage client session
        (printf "* Connected to %s:%d" hostname port)
      atomically $ do
        let capReqMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "CAP",
                           ircMessageParams = S.singleton $ encodeUtf8 "REQ",
                           ircMessageCoda = Just $ encodeUtf8 "multi-prefix" }
        sendIRCMessage (sessionIRCConnection session) capReqMessage
        let capEndMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "CAP",
                           ircMessageParams = S.singleton $ encodeUtf8 "END",
                           ircMessageCoda = Nothing }
        sendIRCMessage (sessionIRCConnection session) capEndMessage
        nick <- readTVar $ sessionNick session
        let nickMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "NICK",
                           ircMessageParams = S.singleton nick,
                           ircMessageCoda = Nothing }
        sendIRCMessage (sessionIRCConnection session) nickMessage
        username <- readTVar $ sessionUsername session
        realName <- readTvar $ sessionRealName session
        let userMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "USER",
                           ircMessageParams = [username,
                                               encodeUtf8 "0",
                                               encodeUtf8 "*"],
                           ircMessageCoda = Just realName }
        sendIRCMessage (sessionIRCConnection session) userMessage
        writeTVar (sessionState session) SessionPreparing
        writeTVar (sessionReconnectOnFailure session) True
    IRCConnectingCanceled -> do
      displaySessionMessage client session "* Connecting canceled"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnected -> do
      displaySessionMessage client session "* Disconnected"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnectError (Error errorText) -> do
      displaySessionMessage client session
        (printf "* Error disconnecting: %s" errorText)
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnectedByPeer -> do
      displaySessionMessage client session "* Disconnected by peer"
      tryReconnectSession client session
    IRCSendError (Error errorText) -> do
      displaySessionMessage client session
        (printf "* Error sending: %s" errorText)
      tryReconnectSession client session
    IRCRecvError (Error errorText) -> do
      displaySessionMessage client session
        (printf "* Error receiving: %s" errorText)
      tryReconnectSession client session
    IRCRecvMessage message
      | ircMessageCommand message == rpl_WELCOME ->
        handleWelcome client session message
      | ircMessageCommand message == err_NICKNAMEINUSE ->
        handleNicknameInUse client session message
      | ircMessageCommand message == encodeUtf8 "PING" ->
        handlePingMessage client session message
      | ircMessageCommand message == encodeUtf8 "TOPIC" ->
        handleTopicMessage client session message
      | ircMessageCommand message == encodeUtf8 "JOIN" ->
        handleJoinMessage client session message
      | ircMessageCommand message == encodeUtf8 "PART" ->
        handlePartMessage client session message
      | ircMessageCommand message == encodeUtf8 "KICK" ->
        handleKickMessage client session message
      | ircMessageCommand message == encodeUtf8 "QUIT" ->
        handleQuitMessage client session message
      | ircMessageCommand message == encodeUtf8 "ERROR" ->
        handleErrorMessage client session message
      | ircMessageCommand message == encodeUtf8 "NICK" ->
        handleNickMessage client session message
      | ircMessageCommand message == encodeUtf8 "MODE" ->
        handleModeMessage client session message
      | ircMessageCommand message == encodeUtf8 "PRIVMSG" ->
        handlePrivmsgMessage client session message
      | ircMessageCommand message == encodeUtf8 "NOTICE" ->
        handleNoticeMessage client session message
      | otherwise ->
        displaySessionMessage client session $ show message

-- | Handle welcome message.
handleWelcome :: Client -> Session -> IRCMessage -> IO ()
handleWelcome client session message = do
  displaySessionMessage client session $ show message
  atomically $ writeTVar (sessionState session) SessionReady

-- | Handle nickname in use message.
handleNicknameInUse :: Client -> Session -> IRCMessage -> IO ()
handleNicknameInUse client session message = do
  displaySessionMessage client session $ show message
  atomically $ do
    state <- readTVar $ sessionState session
    case state of
      SessionPreparing -> do
        nick <- readTVar $ sessionNick session
        newNick <- B.append nick $ encodeUtf8 "_"
        let nickMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "NICK",
                           ircMessageParams = S.singleton newNick,
                           ircMessageCoda = Nothing }
        sendIRCMessage (sessionIRCConnection session) nickMessage
        writeTVar (sessionNick session) newNick
      _ -> return ()

-- | Handle PING message.
handlePingMessage :: Client -> Session -> IRCMessage -> IO ()
handlePingMessage client session message = do
  case ircMessageCoda message of
    Just coda ->
      sendIRCMessageToSession session $
      IRCMessage { ircMessagePrefix = Nothing,
                   ircMessageCommand = encodeUtf8 "PONG",
                   ircMessageParams = S.singleton coda,
                   ircMessageCoda = Nothing }
    Nothing -> return ()

-- | Handle TOPIC message.
handleTopicMessage :: Client -> Session -> IRCMessage -> IO ()
handleTopicMessage client session message = do
  case (S.lookup 0 $ ircMessageParams message, ircMessageCoda message) of
    (Just name, Just text) ->
      let text = ourDecodeUtf8 text
      in do channel <- atomically $ findChannelByName session name
            case channel of
              Just channel -> do
                case ircMessagePrefix message of
                  Just prefix ->
                    let nick = ourDecodeUtf8 $ extractNick prefix
                    in displayChannelMessage client channel $
                       printf "* %s has changed the topic to: %s" nick text
                  Nothing -> return ()
                atomically $ do
                  channelTabs <- findChannelTabsForChannel client channel
                  forM (clientTabTab <$> channelTabs) $ \tab ->
                    setTopic tab text
              Nothing -> return ()
    _ -> return ()

-- | Handle JOIN message.
handleJoinMessage :: Client -> Session -> IRCMessage -> IO ()
handleJoinMessage client session message =
  case ircMessagePrefix message of
    Just prefix ->
      let nick = extractNick prefix
      in case S.lookup 0 $ ircMessageParams message of
        Just name -> do
          ourNick <- atomically . readTVar $ sessionNick session
          if nick /= ourNick
            then handleNormalJoin client session nick name prefix
            else handleOurJoin client session name
        Nothing -> return ()
    Nothing -> return ()
  where handleNormalJoin client session nick name prefix = do
          channel <- atomically $ findChannelByName session name
          case channel of
            Just channel -> do
              user <- atomically $ findOrCreateUserByNick client session nick
              inChannel <- atomically $ isUserInChannel channel user
              if not inChannel
                then do
                  displayChannelMessage client channel "* %s (%s) has joined"
                    (ourDecodeUtf8 nick) (ourDecodeUtf8 perfix)
                  atomically $ do
                    type' <- readTVar $ userType user
                    let index = channelIndex channel
                        type'' =
                          S.filter (\(channel', _) ->
                                      index /= channelIndex channel') type'
                    writeTVar (userType user) $ type'' |> (channel, S.empty)
                    users <- readTVar $ channelUsers channel
                    writeTVar (channelUsers channel) $ users |> user
                else return ()
            Nothing -> return ()
        handleOurJoin client session name = do
          channel <- atomically $ findOrCreateChannelByName client session name
          ourNick <- atomically . readTVar $ sessionNick session
          displayChannelMessage client channel "* Now talking on %s"
            (ourDecodeUtf8 name)
          user <- findOrCreateUserByNick client session ourNick
          atomically $ do
            users <- readTVar $ channelUsers channel
            writeTVar (channelUsers channel) $ users |> user

-- | Handle PART message.
handlePartMessage :: Client -> Session -> IRCMessage -> IO ()
handlePartMessage client session message =
  case ircMessagePrefix message of
    Just prefix ->
      let nick = extractNick prefix
      in case S.lookup 0 $ ircMessageParams message of
           Just name -> do
             channel <- atomically $ findChannelByName session name
             case channel of
               Just channel -> do
                 user <- atomically $ findUserByNick session nick
                 case user of
                   Just user -> do
                     ourNick <- atomically . readTVar $ sessionNick session
                     if nick /= ourNick
                       then do
                         displayChannelMessage client channel $
                           printf "* %s (%s) has left" (ourDecodeUtf8 nick)
                           (ourDecodeUtf8 prefix)
                         atomically $ do
                           removeUserFromChannel client channel user
                       else do
                         displayChannelMessage client channel "* You have left"
                         atomically $ do
                           removeUserFromChannel client channel user
                           writeTVar (channelState channel) NotInChannel
                   Nothing -> return ()
               Nothing -> return ()
           Nothing -> return ()
    Nothing -> return ()

-- | Handle KICK message.
handleKickMessage :: Client -> Session -> IRCMessage -> IO ()
handleKickMessage client session message =
  case ircMessagePrefix message of
    Just prefix ->
      let kickingNick = extractNick prefix
      in case (S.lookup 0 $ ircMessageParams message,
               S.lookup 1 $ ircMessageParams message) of
           (Just name, Just kickedNick) -> do
             channel <- atomically $ findChannelByName session name
             case channel of
               Just channel -> do
                 user <- atomically $ findUserByNick session kickedNick
                 case user of
                   Just user -> do
                     ourNick <- atomically . readTVar $ sessionNick session
                     if kickedNick /= ourNick
                       then handleNormalKick client channel user kickingNick
                            kickedNick (ircMessageCoda message)
                       else handleOurKick client channel user kickingNick
                            (ircMessageCoda message)
                   Nothing -> return ()
               Nothing -> return ()
           Nothing -> return ()
    Nothing -> return ()
  where handleNormalKick client channel user kickingNick kickedNick coda = do
          case coda of
            Just coda -> do
              displayChannelMessage client channel $
                printf "* %s has kicked %s from %s (%s)"
                (ourDecodeUtf8 kickingNick)
                (ourDecodeUtf8 kickedNick)
                (ourDecodeUtf8 $ channelName channel)
                (ourDecodeUtf8 coda)
            Nothing -> do
              displayChannelMessage client channel $
                printf "* %s has kicked %s from %s"
                (ourDecodeUtf8 kickingNick)
                (ourDecodeUtf8 kickedNick)
                (ourDecodeUtf8 $ channelName channel)
          atomically $ removeUserFromChannel client channel user
        handleOurKick client channel user kickingNick coda = do
          case ircMessageCoda message of
            Just coda -> do
              displayChannelMessage client channel $
                printf "* You have been kicked from %s by %s (%s)"
                (ourDecodeUtf8 $ channelName channel)
                (ourDecodeUtf8 kickingNick)
                (ourDecodeUtf8 coda)
            Nothing -> do
              displayChannelMessage client channel $
                printf "* You have been kicked from %s by %s"
                (ourDecodeUtf8 $ channelName channel)
                (ourDecodeUtf8 kickingNick)
          atomically $ do
            removeUserFromChannel client channel user
            writeTVar (channelState channel) NotInChannel

-- | Handle QUIT message.
handleQuitMessage :: Client -> Session -> IRCMessage -> IO ()
handleQuitMessage client session message =
  case ircMessagePrefix message of
    Just prefix -> do
      let nick = extractNick prefix
      user <- atomically $ findUserByNick session nick
      case user of
        Just user -> do
          let ourNick = atomically . readTVar $ sessionNick session
          if nick /= ourNick
            then do
              case ircMessageCoda message of
                Just coda -> do
                  displayUserMessageAll client user $
                    printf "* %s has quit (%s)" (ourDecodeUtf8 nick)
                    (ourDecodeUtf8 coda)
                Nothing -> do
                  displayUserMessageAll client user $
                    printf "* %s has quit" (ourDecodeUtf8 nick)
            else return ()
          atomically $ removeUserFromAllChannels client session user
        Nothing -> return ()
    Nothing -> return ()

-- | Handle ERROR message.
handleErrorMessage :: Client -> Session -> IRCMessage -> IO ()
handleErrorMessage client session message = do
  (disconnectIRC $ sessionIRCConnection session) >> return ()
  writeTVar (sessionState session) SessionInactive

-- | Handle NICK message.
handleNickMessage :: Client -> Session -> IRCMessage -> IO ()
handleNickMessage client session message =
  case (ircMessagePrefix message, ircMessageCoda message) of
    (Just prefix, Just coda) -> do
      let nick = extractNick prefix
          (newNick, _) = B.breakSubstring (encodeUtf8 " ") coda
      join . atomically $ do
        user <- findUserByNick session nick
        case user of
          Just user -> do
            writeTVar (userNick user) newNick
          Nothing -> return ()
        ourNick <- readTVar $ sessionNick session
        if nick == ourNick
          then do
            writeTVar (sessionNick session) newNick
            return $ do
              displaySessionMessageOnMostRecentTab client session $
                printf "* You are now known as %s" (ourDecodeUtf8 newNick)
          else do
            return $ do
              displaySessionMessageOnMostRecentTab client session $
                printf "* %s is now known as %s" (ourDecodeUtf8 nick)
                (ourDecodeUtf8 newNick)
    _ -> return ()

-- | Handle MODE message.
handleModeMessage :: Client -> Session -> IRCMessage -> IO ()
handleModeMessage client session message =
  case S.lookup 0 $ ircMessageParams message of
    Just nickOrChannel -> do
      ourNick <- atomically . readTVar $ sessionNick session
      if nickOrChannel == ourNick
        then handleUserMode client session message nickOrChannel
        else handleChannelMode client session message nickOrChannel
    Nothing -> return ()
  where handleUserMode client session message nickOrChannel = do
          let param =
                case S.lookup 1 $ ircMessageParams message of
                  Just param -> param
                  Nothing ->
                    case ircMessageCoda message of
                       Just coda ->
                         fst $ B.breakSubstring (encodeUtf8 " ") coda
                       Nothing -> B.empty          
          case B.uncons param of
            Just (byte, _)
              | byte == byteOfChar '+' || byte == byteOfChar '-' ->
                  case ircMessagePrefix message of
                    Just prefix -> do
                      displaySessionMessageOnMostRecentTab client session $
                        printf "* %s sets mode %s on %s"
                        (ourDecodeUtf8 prefix) (ourDecodeUtf8 param)
                        (ourDecodeUtf8 nickOrChannel)
                      atomically $ do
                        mode <- readTVar $ sessionMode session
                        writeTVar (sessionMode session) parseUserMode param mode
                    Nothing -> return ()
            _ -> return ()
        parseUserMode param mode =
          case B.uncons param of
            Just (byte, rest)
              | byte == byteOfChar '+' -> paramUserModeAdd rest mode
              | byte == byteOfChar '-' -> paramuserModeRemove rest mode
              | otherwise -> parseUserModeAdd param mode
            Nothing -> mode
        parseUserModeAdd param mode =
          case B.uncons param of
            Just (byte, rest)
              | byte /= byteOfChar '+' && byte /= byteOfChar '-' ->
                case S.elemIndexL (Mode byte) mode of
                  Nothing -> parseUserModeAdd rest $ mode |> Mode byte
                  _ -> parseUserModeAdd rest mode
              | otherwise -> parseUserMode param mode
            Nothing -> mode
        parseUserModeRemove param mode =
          case B.uncons param of
            Just (byte, rest)
              | byte /= byteOfChar '+' && byte /= byteOfChar '-' ->
                parseUserModeRemove rest $
                S.filter (\(Mode byte') -> byte' /= byte) mode
              | otherwise -> parseUserMode param mode
            Nothing -> mode
        handleChannelMode client session message nickOrChannel = do
          case S.lookup 1 $ ircMessageParams message of
            Just param -> do
              case B.uncons param of
                Just (byte, _)
                  | byte == byteOfChar '+' || byte == byteOfChar '-' ->
                    case ircMessagePrefix message of
                      Just prefix -> do
                        let prefix' = extractNick prefix
                        channel <- findChannelByName session nickOrChannel
                        case channel of
                          Just channel -> do
                            mode <- atomically . readTVar $ channelMode channel
                            mode <- parseChannelMode client channel param prefix'
                              mode . S.drop 2 $ ircMessageParams message
                            atomically $ writeTVar (channelMode channel) mode
                          Nothing -> return ()
                      Nothing -> return ()
                _ -> return ()
            Nothing -> return ()
        parseChannelMode client channel param prefix mode extraParams = do
          case B.uncons param of
            Just (byte, rest)
              | byte == byteOfChar '+' ->
                parseChannelModeAdd client channel rest prefix mode extraParams
              | byte == byteOfChar '-' ->
                parseChannelModeRemove client channel rest prefix mode
                extraParams
              | otherwise ->
                parseChannelModeAdd client channel param prefix mode extraParams
            Nothing -> return mode
        parseChannelModeAdd client channel param prefix mode extraParams = do
          case B.uncons param of
            Just (byte, rest)
              | byte /= byteOfChar '+' && byte /= byteOfChar '-' ->
                let mode =
                      case S.elemIndexL byte channelModesToExclude of
                        Nothing ->
                          case S.elemIndexL (Mode byte) mode of
                            Nothing -> mode |> Mode byte
                            _ -> mode
                        _ -> mode
                in
                  if byte == byteOfChar 'b'
                  then handleChannelModeAddBan client channel rest prefix mode
                       extraParams
                  else if byte == byteOfChar 'o'
                  then handleChannelModeAddUserType client channel rest prefix
                       mode extraParams "op" OpUser
                  else if byte == byteOfChar 'O'
                  then handleChannelModeAddUserType client channel rest prefix
                       mode extraParams "owner" OwnerUser
                  else if byte == byteOfChar 'h'
                  then handleChannelModeAddUserType client channel rest prefix
                       mode extraParams "halfop" HalfOpUser
                  else if byte == byteOfChar 'v'
                  then handleChannelModeAddUserType client channel rest prefix
                       mode extraParams "voice" VoiceUser
                  else if byte == byteOfChar 'I'
                  then handleChannelModeAddInviteMask client channel rest prefix
                       mode extraParams
                  else if byte == byteOfChar 'k'
                  then handleChannelModeAddKeyword client channel rest prefix
                       mode extraParams
                  else if byte == byteOfChar 'l'
                  then handleChannelModeAddLimit client channel rest prefix mode
                       extraParams
                  else do
                    displayChannelMessage client channel $
                      printf "* %s sets mode +%s on %s" prefix
                      (ourDecodeUtf8 $ B.singleton byte)
                      (ourDecodeUtf8 $ channelName channel)
                    parseChannelModeAdd client channel rest prefix mode
                      extraParams
              | otherwise -> parseChannelMode client channel param prefix mode
                             extraParams
            Nothing -> return mode
        handleChannelModeAddBan client channel param prefix mode extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel $
                printf "* %s sets ban on %s" (ourDecodeUtf8 prefix)
                (ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddUserType client channel param prefix mode
          extraParams text userType =
          case S.viewl extraParams of
            extraParam :< rest -> do
              user <- findUserByNick (channelSession channel) extraParam
              case user of
                Just user -> do
                  displayChannelMessage client channel $
                    printf "* %s sets %s on %s" (ourDecodeUtf8 prefix)
                    text (ourDecodeUtf8 extraParam)
                  atomically $ addUserType user channel userType
                Nothing -> return ()
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddInviteMask client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel $
                printf "* %s adds %s to the invite mask"
                (ourDecodeUtf8 prefix) (ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddKeyword client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel $
                printf "* %s sets keyword to %s"
                (ourDecodeUtf8 prefix) (ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddLimit client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel $
                printf "* %s sets user limit to %s"
                (ourDecodeUtf8 prefix) (ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        parseChannelModeRemove client channel param prefix mode extraParams = do
          case B.uncons param of
            Just (byte, rest)
              | byte /= byteOfChar '+' && byte /= byteOfChar '-' ->
                let mode =
                      case S.elemIndexL byte channelModesToExclude of
                        Nothing -> S.filter (/= (Mode byte)) mode
                        _ -> mode
                in
                  if byte == byteOfChar 'b'
                  then handleChannelModeRemoveBan client channel rest prefix mode
                       extraParams
                  else if byte == byteOfChar 'o'
                  then handleChannelModeRemoveUserType client channel rest prefix
                       mode extraParams "op" OpUser
                  else if byte == byteOfChar 'O'
                  then handleChannelModeRemoveUserType client channel rest prefix
                       mode extraParams "owner" OwnerUser
                  else if byte == byteOfChar 'h'
                  then handleChannelModeRemoveUserType client channel rest prefix
                       mode extraParams "halfop" HalfOpUser
                  else if byte == byteOfChar 'v'
                  then handleChannelModeRemoveUserType client channel rest prefix
                       mode extraParams "voice" VoiceUser
                  else if byte == byteOfChar 'I'
                  then handleChannelModeRemoveInviteMask client channel rest
                       prefix mode extraParams
                  else if byte == byteOfChar 'k'
                  then handleChannelModeRemoveKeyword client channel rest prefix
                       mode extraParams
                  else if byte == byteOfChar 'l'
                  then handleChannelModeRemoveLimit client channel rest prefix
                       mode extraParams
                  else do
                    displayChannelMessage client channel $
                      printf "* %s sets mode -%s on %s" prefix
                      (ourDecodeUtf8 $ B.singleton byte)
                      (ourDecodeUtf8 $ channelName channel)
                    parseChannelModeRemove client channel rest prefix mode
                      extraParams
              | otherwise -> parseChannelMode client channel param prefix mode
                             extraParams
            Nothing -> return mode
        handleChannelModeRemoveBan client channel param prefix mode extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel $
                printf "* %s removes ban from %s" (ourDecodeUtf8 prefix)
                (ourDecodeUtf8 extraParam)
              parseChannelModeRemove client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeRemove client channel param prefix mode extraParams
        handleChannelModeRemoveUserType client channel param prefix mode
          extraParams text userType =
          case S.viewl extraParams of
            extraParam :< rest -> do
              user <- findUserByNick (channelSession channel) extraParam
              case user of
                Just user -> do
                  displayChannelMessage client channel $
                    printf "* %s removes %s from %s" (ourDecodeUtf8 prefix)
                    text (ourDecodeUtf8 extraParam)
                  atomically $ removeUserType user channel userType
                Nothing -> return ()
              parseChannelModeRemove client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeRemove client channel param prefix mode extraParams
        handleChannelModeRemoveInviteMask client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              user <- findUserByNick (channelSession channel) extraParam
              case user of
                Just user -> do
                  displayChannelMessage client channel $
                    printf "* %s removes %s from the invite mask"
                    (ourDecodeUtf8 prefix) (ourDecodeUtf8 extraParam)
                Nothing -> return ()
              parseChannelModeRemove client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeRemove client channel param prefix mode extraParams
        handleChannelModeRemoveKeyword client channel param prefix mode
          extraParams = do
          displayChannelMessage client channel $
            printf "* %s unsets keyword"
            (ourDecodeUtf8 prefix) (ourDecodeUtf8 extraParam)
          parseChannelModeRemove client channel param prefix mode extraParams
        handleChannelModeRemoveLimit client channel param prefix mode
          extraParams = do
          displayChannelMessage client channel $
            printf "* %s unsets user limit"
            (ourDecodeUtf8 prefix) (ourDecodeUtf8 extraParam)
          parseChannelModeRemove client channel param prefix mode extraParams

-- | Channel modes with parameters.
channelModesToExclude :: S.Seq Word8
channelModesToExclude = [byteOfChar 'b', byteOfChar 'o', byteOfChar 'O',
                         byteOfChar 'I', byteOfChar 'h', byteOfChar 'v']

-- | Handle PRIVMSG message.
handlePrivmsgMessage :: Client -> Session -> IRCMessage -> IO ()
handlePrivmsgMessage client session message = do
  case (ircMessagePrefix message,
        S.lookup 0 $ ircMessageParams message,
        ircMessageCoda message) of
    (Just prefix, Just target, Just text) -> do
      let source = extractNick prefix
      ourNick <- atomically . readTVar $ sessionNick session
      if target == ourNick
        then do
          user <- atomically $ findOrCreateUserByNick client session source
          tabs <- findOrCreateUserTabsForUser client user
          displayMessageOnTabs client tabs $
            printf "<$s> %s" (ourDecodeUtf8 source) (ourDecodeUtf8 text)
        else do
          channel <- atomically $ findChannelByName session target
          case channel of
            Just channel -> do
              displayChannelMessage client channel $
                printf "<%s> %s" (ourDecodeUtf8 source) (ourDecodeUtf8 text)
            Nothing -> return ()
    _ -> return ()

-- | Handle NOTICE message.
handleNoticeMessage :: Client -> Session -> IRCMessage -> IO ()
handleNoticeMessage client session message = do
  case (ircMessagePrefix message,
        S.lookup 0 $ ircMessageParams message,
        ircMessageCoda message) of
    (Just prefix, Just target, Just text) -> do
      let source = extractNick prefix
      ourNick <- atomically . readTVar $ sessionNick session
      if target == ourNick
        then do
          displaySessionMessageOnMostRecentTab client session $
            printf "-%s- %s" (ourDecodeUtf8 source) (ourDecodeUtf8 text)
        else do
          channel <- atomically $ findChannelByName session target
          case channel of
            Just channel -> do
              displayChannelMessage client channel $
                printf "-%s- %s" (ourDecodeUtf8 source) (ourDecodeUtf8 text)
            Nothing -> return ()
    _ -> return ()

-- | Find or create user tabs for user.
findOrCreateUserTabsForUser :: Client -> User -> IO (S.Seq ClientTab)
findOrCreateUserTabsForUser client user = do
  nick <- atomically . readTVar $ userNick user
  tabs <- atomically $ findUserTabsForUser client user
  if not $ S.null tabs
    then return tabs
    else do
      window <- findMostRecentWindow client
      case window of
        Just window -> do
          result <- openClientTab client window (ourDecodeUtf8 nick)
          case result of
            Right tab -> do
              atomically . writeTVar (clientTabSubtype tab) $ UserTab user
              return $ S.singleton clientTab
            Left (Error errorText) -> do
              hPutStr stderr errorText
              return S.empty
        Nothing -> return S.empty

-- | Add a user type to a user for a channel.
addUserType :: User -> Channel -> UserType -> STM ()
addUserType user channel aType = do
  types <- readTVar $ userType user
  channelIndex <- S.findIndexL
    (\(channel', _) -> channelIndex channel == channelIndex channel') types
  case channelIndex of
    Just channelIndex ->
      case S.lookup channelIndex types of
        Just (_, channelTypes) ->
          case S.elemIndexL aType channelTypes of
            Nothing ->
              writeTVar (userType user) $
                update channelIndex (channel, channelTypes |> aType) types
            _ -> return ()
        Nothing -> error "impossible"
    Nothing ->
      writeTVar (userType user) $ types |> (channel, S.singleton aType)

-- | Remove a user type from a user for a channel.
removeUserType :: User -> Channel -> UserType -> STM ()
removeUserType user channel aType = do
  types <- readTVar $ userType user
  channelIndex <- S.findIndexL
    (\(channel', _) -> channelIndex channel == channelIndex channel') types
  case channelIndex of
    Just channelIndex ->
      case S.lookup channelIndex types of
        Just (_, channelTypes) -> do
          writeTVar (userType user) $
            update channelIndex (channel, S.filter (/= aType) channelTypes) types
        Nothing -> return ()
    Nothing -> return ()

-- | Our UTF-8 decoder.
ourDecodeUtf8 :: B.ByteString -> T.Text
ourDecodeUtf8 = decodeUtf8With lenientDecode

-- | Extract nick from full nick, username, and host.
extractNick :: B.ByteString -> B.ByteString
extractNick = fst . B.breakSubstring (encodeUtf8 "!")

-- | Remove a user from a channel.
removeUserFromChannel :: Client -> Channel -> User -> IO ()
removeUserFromChannel client channel user = do
  atomically $ do
    users <- readTVar $ channelUsers channel
    writeTVar (channelUsers channel) $
      filter (\user' -> userIndex user /= userIndex user') users
  cleanupUserIfNoTabsOrChannels client user

-- | Remove a user from all channels for a session.
removeUserFromAllChannels :: Client -> Session -> User -> IO ()
removeUserFromAllChannels client session user = do
  channels <- atomically . readTVar $ sessionChannels session
  forM channels $ \channel -> removeUserFromChannel client channel userr

-- | Find channel by name.
findChannelByName :: Session -> B.ByteString -> STM (Maybe Channel)
findChannelByName session name = do
  channels <- readTVar $ sessionChannels session
  return $ findChannelByName' channels name
  where findChannelByName' channels name =
          case S.viewl channels of
            channel :< rest ->
              if channelName channel == channel
              then Just channel
              else findChannelByName' rest name
            S.EmptyL -> Nothing

-- | Find user by nick
findUserByNick :: Session -> B.ByteString -> STM (Maybe User)
findUserByNick session nick = do
  users <- readTVar $ sessionUsers session
  return $ findUserByNick' users nick
  where findUserByNick' users nick =
          case S.viewl users of
            user :< rest -> do
              nick' <- readTVar $ userNick user
              if nick == nick'
                then Just user
                else findUserByNick' rest nick
            S.EmptyL -> Nothing

-- | Send a message to an session.
sendIRCMessageToSession :: Session -> IRCMessage -> IO ()
sendIRCMessageToSession session message = do
  _ <- atomically . sendIRCMessage $ sessionIRCConnection session

-- | Try to reconnect a session.
tryReconnectSession :: Client -> Session -> IO ()
tryReconnectSession client session = do
  reconnectOnFailure <- atomically . readTVar $
                        sessionReconnectOnFailure session
  if reconnectOnFailure
    then do
      atomically $ writeTVar (sessionState session) SessionConnecting
      delay <- settingsReconnectDelay <$>
        (atomically . readTVar $ clientSettings client)
      reconnectingAsync <- async $ do
        threadDelay $ floor (delay * 1000000.0)
        atomically $ do
          let connection = sessionIRCConnection session
          state <- getIRCConnectionState state
          if state /= IRCConnectionNotStarted
            then do
              active <- isIRCConnectionActive connection
              if not active
                then do
                   hostname <- readTVar $ sessionHostname session
                   port <- readTVar $ sessionPort session
                   _ <- connectIRC connection hostname port
                else return ()
            else return ()
        atomically $ writeTVar (sessionReconnecting session) Nothing
      atomically $ writeTVar (sessionReconnecting session)
        (Just reconnectingAsync)
    else atomically $ writeTVar (sessionState session) SessionInactive

-- | Handle client window event.
handleClientWindowEvent :: Client -> ClientWindow -> WindowEvent -> IO ()
handleClientWindowEvent client clientWindow event = do
  case event of
    WindowClosed -> handleWindowClosed client clientWindow
    UserPressedKey modifiers key
      | modifiers == [KeyControl] && key == "n" -> handleOpenClientWindow client
      | modifiers == [KeyControl] && key == "t" ->
        handleOpenClientTab client clientWindow
      | otherwise -> return ()
    WindowFocused -> handleWindowFocused client clientWindow

-- | Handle window closed.
handleWindowClosed :: Client -> ClientWindow -> WindowEvent -> IO ()
handleWindowClosed client clientWindow = do
  subtypes <- atomically $ do
    let index = clientWindowIndex clientWindow
    windows <- readTVar clientWindows client
    writeTVar (clientWindows client) $
      filter (\clientWindow' -> clientWindowIndex clientWindow' /= index) windows
    tabs <- readTVar clientTabs client
    let (tabs, deletedTabs) =
          S.partition (\clientTab' ->
                         (clientWindowIndex $ clientTabWindow clientTab') /=
                         index)
    writeTVar (clientTabs client) tabs
    forM deletedTabs $ \tab -> readTVar $ clientTabSubtype tab
  forM subtypes $ \case
    SessionTab session -> cleanupSessionIfNoTabs client session
    ChannelTab channel -> cleanupChannelIfNoTabs client channel
    UserTab user -> cleanupUserIfNoTabsOrChannels client user
    _ -> return ()

-- | Handle opening a client window.
handleOpenClientWindow :: Client -> IO ()
handleOpenClientWindow client = do
  result <- openClientWindow client "Amphibian IRC" "<Not Connected>"
  case result of
    Right () -> return ()
    Left (Error errorText) -> hPutStr stderr errorText

-- | Handle opening a client tab.
handleOpenClientTab :: Client -> ClientWindow -> IO ()
handleOpenClientTab client clientWindow =
  (openClientTab client clientWindow "<Not Connected>") >> return ()

-- | Handle window focused.
handleWindowFocused :: Client -> ClientWindow -> IO ()
handleWindowFocused client clientWindow = do
  atomically $ do
    index <- getClientNextWindowFocusIndex client
    writeTVar (clientWindowFocusIndex clientWindow) index

-- | Handle client tab event.
handleClientTabEvent :: Client -> ClientTab -> TabEvent -> IO ()
handleClientTabEvent client clientTab event = do
  case event of
    TabClosed -> handleTabClosed client clientTab
    LineEntered text -> handleLineEntered client clientTab text
    TopicEntered text -> handleTopicEntered client clientTab text
    TabSelected -> handleTabSelected client clientTab

-- | Handle a tab closed event.
handleTabClosed :: Client -> ClientTab -> IO ()
handleTabClosed client clientTab = do
  subtype <- atomically $ do
    tabs <- readTVar clientTabs client
    let index = clientTabIndex clientTab
    writeTVar (clientTabs client) $
      filter (\clientTab' -> clientTabIndex clientTab' /= index) tabs
    readTVar $ clientTabSubtype clientTab
  case subtype of
    SessionTab session -> cleanupSessionIfNoTabs client session
    ChannelTab channel -> cleanupChannelIfNoTabs client channel
    UserTab user -> cleanupUserIfNoTabsOrChannels client user
    _ -> return ()

-- | Handle a line entered event.
handleLineEntered :: Client -> ClientTab -> T.Text -> IO ()
handleLineEntered client clientTab text = do
  case T.uncons text of
    Just ('/', rest) ->
      case T.uncons rest of
        Just ('/', _) -> handleNormalLine client clientTab rest
        Just _ -> handleCommand client clientTab rest
        Nothing -> return ()
    Just _ -> handleNormalLine client clientTab text
    Nothing -> return ()

-- | Handle a normal line that has been entered.
handleNormalLine :: Client -> ClientTab -> T.Text -> IO ()
handleNormalLine client clientTab text = do
  subtype <- atomically . readTVar $ clientTabSubtype clientTab
  case subtype of
    FreeTab -> displayMessage client clientTab
               "* Not in channel or private messaging"
    SessionTab _ -> displayMessage client clientTab
                    "* Not in channel or privage messaging"
    ChannelTab channel -> do
      let session = channelSession session
      ourNick <- atomically . readTVar $ sessionNick session
      state <- atomically . readTVar $ channelState channel
      case state of
        InChannel -> do
          displayChannelMessage client channel $
            printf "<%s> %s" (ourDecodeUtf8 ourNick) text
          let message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "PRIVMSG",
                                     ircMessageParams =
                                       S.singleton $ channelName channel,
                                     ircMessageCoda = Just $ encodeUtf8 text }
          response <-
            atomically $ sendIRCMessage (sessionIRCConnection session) message
          async $ do
            result <- atomically $ getResponse response
            case result of
              Right () -> return ()
              Left (Error errorText) -> hPutStr stderr errorText
        NotInChannel -> displayMessage client clientTab "* Not in channel"
    UserTab user -> do
      let session = userSession session
      ourNick <- atomically . readTVar $ sessionNick session
      nick <- atomically .  readTVar $ userNick user
      displayUserMessage client user $
        printf "<%s> %s" (ourDecodeUtf8 ourNick) text
      let message = IRCMessage { ircMessagePrefix = Nothing,
                                 ircMessageCommand = encodeUtf8 "PRIVMSG",
                                 ircMessageParams = S.singleton nick
                                 ircMessageCoda = Just $ encodeUtf8 text }
      response <-
        atomically $ sendIRCMessage (sessionIRCConnection session) message
      async $ do
        result <- atomically $ getResponse response
        case result of
          Right () -> return ()
          Left (Error errorText) -> hPutStr stderr errorText

-- | Handle command.
handleCommand :: Client -> ClientTab -> T.Text -> IO ()
handleCommand client clientTab command = do
  case parseCommandFIeld command of
    Just (command, rest)
      | command == "new" -> handleNewCommand client clientTab rest
      | command == "close" -> handleCloseCommand client clientTab rest
      | command == "server" -> handleServerCommand client clientTab rest
      | command == "quit" -> handleQuitCommand client clientTab rest
      | command == "join" -> handleJoinCommand client clientTab rest
      | command == "part" -> handlePartCommand client clientTab rest
      | command == "topic" -> handleTopicCommand client clientTab rest
      | command == "mode" -> handleModeCommand client clientTab rest
      | command == "kick" -> handleKickCommand client clientTab rest
      | otherwise -> handleUnrecognizedCommand client clientTab command
    Nothing -> return ()

-- | Handle unrecognized command.
handleUnrecognizedCommand :: Client -> ClientTab -> T.Text -> IO ()
handleUnrecognizedCommand client clientTab command = do
  displayMessage client clientTab $ printf "* Urecognized command: %s" command

-- | Parse a command field.
parseCommandField :: T.Text -> Maybe (T.Text, T.Text)
parseCommandField text =
  let (part0, part1) = T.breakOn " " $ T.stripStart text
  in if S.null part0
     then Nothing
     else Just (part0, T.stripStart part1)

-- | Handle a topic entered event.
handleTopicEntered :: Client -> ClientTab -> T.Text -> IO ()
handleTopicEntered client clientTab text = do
  atomically $ do
    subtype <- readTVar $ clientTabSubtype clientTab
    case subtype of
      ChannelTab channel -> do
        let session = channelSession channel
        channelState' <- readTVar $ channelState channel
        sessionState' <- readTVar $ sessionState session
        if channelState' == InChannel && sessionState' == SessionReady
          then do
            let topicMessage =
                  IRCMessage { ircMessagePrefix = Nothing,
                               ircMessageCommand = encodeUtf8 "TOPIC",
                               ircMessageParams =
                                 S.singleton $ channelName channel,
                               ircMessageCoda = Just $ encodeUtf8 text }
            (sendIRCMessage (sessionIRCConnection session) topicMessage) >>
              return ()
      _ -> return ()

-- | Handle a tab selected event.
handleTabSelected :: Client -> ClientTab -> IO ()
handleTabSelected client clientTab = do
  atomically $
    writeTVar (clientTab clientTabSelectIndex) =<<
    getNextClientTabSelectIndex client

-- | Close a session if no tabs are open for it.
cleanupSessionIfNoTabs :: Client -> Session -> IO ()
cleanupSessionIfNoTabs client session = do
  found <- atomically $ isTabOpenForSession client session
  if not found
    then do
      atomically $ do
        state <- readTVar $ sessionState session
        if state == SessionReady || state == SessionPreparing
          then
            let quitMessage =
                  IRCMessage { ircMessagePrefix = Nothing,
                               ircMessageCommand = encodeUtf8 "QUIT",
                               ircMessageParams = S.empty,
                               ircMessageCoda = Nothing }
            in sendIRCMessage (sessionIRCConnection session) quitmessage
          else return ()
        (stopIRCConnection $ sessionIRCConnection session) >> return ()
        writeTVar (sessionState session) SessionDestroyed
        sessions <- readTVar $ clientSessions client
        writeTVar (clientSessions client) $
          S.filter (\session' ->
                      sessionIndex session /= sessionIndex session')
          sessions
    else return ()

-- | Get whether there is a tab open for a session.
isTabOpenForSession :: Client -> Session -> STM Bool
isTabOpenForSession client session = do
  tabs <- readTVar $ clientTabs client
  foldM (\found tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            case subtype of
              SessionTab session' ->
                (sessionIndex session == sessionIndex session') || found
              ChannelTab channel ->
                (sessionIndex session ==
                 (sessionIndex $ channelSession channel)) || found
              UserTab user ->
                (sessionIndex session ==
                  (sessionIndex $ userSession user)) || found
              FreeTab -> found)
    False tabs

-- | Close a channel if no tabs are open for it.
cleanupChannelIfNoTabs :: Client -> Channel -> IO ()
cleanupChannelIfNoTabs client channel = do
  found <- atomically $ isTabOpenForChannel client channel
  if not found
    then do
      atomically $ do
        sessionState <- readTVar . sessionState $ channelSession channel
        case sessionState of
          SessionReady ->
            let partMessage =
                  IRCMessage { ircMessagePrefix = Nothing,
                               ircMessageCommand = encodeUtf8 "PART",
                               ircMessageParams =
                                 S.singleton $ channelName channel,
                               ircMessageCoda = Nothing }
                connection = sessionIRCConnection $ channelSession channel
            in (sendIRCMessage connection partMessage) >> return ()
        writeTVar (channelState channel) NotInChannel
        channels <- readTVar $ clientChannels client
        writeTVar (clientChannels client) $
          S.filter (\channel' ->
                      channelIndex channel /= channelIndex channel')
          channels
    else return ()

-- | Get whether there is a tab open for a channel.
isTabOpenForChannel :: Client -> Channel -> STM Bool
isTabOpenForChannel client channel = do
  tabs <- readTVar $ clientTabs client
  foldM (\found tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            case subtype of
              ChannelTab channel' ->
                (channelIndex channel == channelIndex channel') || found
              _ -> found)
    False tabs

-- | Close a user if no tabs are open for it.
cleanupUserIfNoTabsOrChannels :: Client -> User -> IO ()
cleanupUserIfNoTabsOrChannels client user = do
  tabOpenForUser <- atomically $ isTabOpenForUser client user
  userInChannel <- atomically $ isUserInAnyChannel client user
  if not (tabOpenForUser || userInChannel)
    then do
      atomically $ do
        let index = userIndex user
        users <- readTVar $ clientUsers client
        writeTVar (clientUsers client) $
          S.filter (\user' -> index /= userIndex user') users
        let session = userSession user
        users <- readTVar $ sessionUsers client
        writeTVar (sessionUsers session) $
          S.filter (\user' -> index /= userIndex user') users
    else return ()

-- | Get whether there is a tab open for a user.
isTabOpenForUser :: Client -> User -> STM Bool
isTabOpenForUser client user = do
  tabs <- readTVar $ clientTabs client
  foldM (\found tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            case subtype of
              UserTab user' -> (userIndex user == userIndex user') || found
              _ -> found)
    False tabs

-- | Get whether a user is in any channel.
isUserInAnyChannel :: Client -> User -> STM Bool
isUserInAnyChannel client user = do
  channels <- readTVar $ clientChannels client
  foldM (\found channel -> do
            inChannel <- isUserInChannel channel user
            return $ inChannel || found)
  False channels

-- | Get whether a user is in a channel.
isUserInChannel :: Channel -> User -> STM Bool
isUserInChannel channel user = do
  users <- readTVar $ channelUsers channel
  return $ foldl' (\found user' ->
                     (userIndex user == userIndex user') || found)
    False users
            

-- | Find current tab for session.
getCurrentTabForSession :: Client -> Session -> STM (Maybe ClientTab)
getCurrentTabForSession client session = do
  tabs <- readTVar $ clientTabs client
  foldM (\foundTab tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            let forSession =
                  case subtype of
                    SessionTab session' ->
                      sessionIndex session == sessionIndex session'
                    ChannelTab channel ->
                      sessionIndex session ==
                      (sessionIndex $ channelSession channel)
                    UserTab user ->
                      sessionIndex session ==
                      (sessionIndex $ userSession user)
            if forSession
              then case foundTab of
                     Just foundTab -> do
                       foundTabSelectIndex <-
                         readTVar $ clientTabSelectIndex foundTab
                       tabSelectIndex <-
                         readTVar $ tabSelectIndex tab
                       if tabSelectIndex > foundTabSelectIndex
                         then return $ Just tab
                         else return $ Just foundTab
                     Nothing -> return $ Just tab
              else return foundTab)
    Nothing tabs

-- | Find user for session, and if one does not exist, create it.
findOrCreateUserByNick :: Client -> Session -> B.ByteString -> STM User
findOrCreateUserByNick client session nick = do
  user <- findUserByNick session nick
  case user of
    Just user -> return user
    Nothing -> do
      index <- getNextClientIndex client
      nick' <- newTVar nick
      type' <- newTVar S.empty
      let user = User { userIndex = index,
                        userSession = session,
                        userNick = nick',
                        userType = type' }
      users <- readTVar $ clientUsers client
      writeTVar (clientUsers client) $ users |> user
      users <- readTVar $ sessionUsers session
      writeTVar (sessionUsers session) $ users |> user
      return user

-- | Find channel for session, and if one does not exist, create it.
findOrCreateChannelByName :: Client -> Session -> B.ByteString -> STM Channel
findOrCreateChannelByName client session name = do
  channel <- findChannelByName session name
  case channel of
    Just channel -> return channel
    Nothing -> do
      index <- getNextClientIndex client
      state <- newTVar InChannel
      users <- newTVar S.empty
      mode <- newTVar S.empty
      let channel = Channel { channelIndex = index,
                              channelSession = session,
                              channelState = state,
                              channelName = name,
                              channelUsers = users,
                              channelMode = mode }
      channels <- readTVar $ clientChannels client
      writeTVar (clientChannels client) $ channels |> channel
      channels <- readTVar $ sessionChannels client
      writeTVar (sessioonChannels session) $ channels |> channel
      return channel
