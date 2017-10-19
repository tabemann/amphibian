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
                           decodeUtf8')
import Control.Functor ((<$>),
                        fmap)
import Data.Sequence ((|>),
                      (:<))
import Data.Foldable (foldl')
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
      nextIndex <- atomically $ newTVar 0
      nextTabSelectIndex <- atomically $ newTVar 0
      sessions <- atomically $ newTVar S.empty
      channels <- atomically $ newTVar S.empty
      users <- atomically $ newTVar S.empty
      windows <- atomically $ newTVar S.empty
      tabs <- atomically $ newTVar S.empty
      let settings =
            Settings { settingsReconnectDelay = 10.0 }
          client =
            Client { clientNextIndex = nextIndex,
                     clientNextTabSelectIndex = nextTabSelectIndex,
                     clientSessions = sessions,
                     clientChannels = channels,
                     clientUsers = users,
                     clientWindows = windows,
                     clientTabs = tabs,
                     clientSettings = settings }
      result <- openClientWindow client "Amphibian IRC" "<Not Connected>"
      case result of
        Right -> handleClientEvents client
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
  index <- readTVar $ clientNextTabSelectIndex client
  writeTVar (clientNextTabSelectIndex client) $ index + 1
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
          atomically $ do
          response <- atomically $ openTab window tabTitle
          result <- atomically $ getResponse response
          case result of
            Right tab -> do
              index <- atomically $ getNextClientIndex client
              let clientWindow =
                    ClientWindow { clientWindowIndex = index,
                                   clientWindowWindow = window,
                                   clientWindowEventSub = windowEventSub }
              index' <- atomically $ getNextClientIndex client
              tabEventSub <- atomically $ subscribeTab tab
              subtype <- atomically $ newTVar FreeTab
              let clientTab =
                    ClientTab { clientTabIndex = index,
                                clientTabTab = tab,
                                clientTabEventSub = tabEventSub,
                                clientTabSubtype = subtypes,
                                clientTabWindow = clientWindow }
              atomically $ do
                windows <- readTVar $ clientWindows client
                writeTVar (clientWindows client) $ windows |> clientWindow
                tabs <- readTVar $ clientTabs client
                writeTVar (ciientTabs client) $ tabs |> clientTabs
              return clientWindow
            Left failure -> do
              (stopWindow window) >> return ()
                return $ Left failure
        Left failure -> do
          (stopWindow window) >> return ()
          return $ Left failure
    Left failure -> return $ Left failure

-- | Handle client events.
handleClientEvents :: Client -> IO ()
handleClientEvents client = do
  event <- atomically . getClientEvent $ client
  case event of
    TaggedSessionEvent session event ->
      handleSessionEvent client session event
    TaggedClientWindowEvent clientWindow event ->
      handleClientWindowEvent client clientWindow event
    TaggedClientTabEvent clientTab event ->
      handleClientTabEvent client clientTab event
    
-- | Get an event for a client.
getClientEvent :: Client -> STM ClientTaggedEvent
getClientEvent client = do
  ircConnectionEventSubs <- extractIRCConnectionEventSubs =<<
                            (readTVar $ clientSessions client)
  windowEventSubs <- extractWindowEventSubs =<<
                     (readTVar $ clientWindows client)
  tabsEventSubs <- extractTabEventSubs =<<
                   (readTVar $ clientTabs client)
  getTaggedSessionEvent ircConnectionEventSubs `orElse`
  getTaggedClientWindowEvent windowEventSubs `orElse`
  getTaggedClientTabEvent tabEventSubs
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

-- | Find client tab for user.
findUserTabsForUser :: Client -> User -> STM (S.Seq ClientTab)
findUserTabsForUser client user = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return $ S.filter (matchTab user) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab user (tab, UserTab currentUser) =
          userIndex user == userIndex currentUser

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
displayChannelMessage :: Client -> Channel -> T.Text -> IO ()
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

-- | Handle a session event.
handleSessionEvent :: Client -> Session -> IRCConnectionEvent -> IO ()
handleSessionEvent client session event =
  case event of
    IRCFoundAddr address -> do
      displaySessionMessage client session
        (printf "**** Found address: %s\n" $ show address) (return ())
    IRCNoAddrFound (Error errorText) -> do
      displaySessionMessage client session
        (printf "**** Unable to find address: %s\n" errorText)
      tryReconnectSession client session
    IRCLookupCanceled -> do
      displaySessionMessage client session
        "**** Address lookup canceled\n"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCFoundName hostname -> do
      displaySessionMessage client session
        (printf "**** Found hostname: %s\n" hostname) (return ())
    IRCNoNameFound (Error errorText) -> do
      displaySessionMessage client session
        (printf "**** No name found: %s\n" errorText) (return ())
    IRCReverseLookupCanceled -> do
      displaySessionMessage client session
        "**** Reverse lookup canceled\n"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCConnectingFailed (Error errorText) -> do
      displaySessionMessage client session
        (printf "**** Connecting failed: %s\n" errorText)
      tryReconnectSession client session
    IRCConnected -> do
      let connection = sessionIRCConnection session
      hostname <- getIRCConnectionHostname connection
      port <- getIRCConnectionPort connection
      displaySessionMessage client session
        (printf "**** Connected to %s:%d\n" hostname port)
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
      displaySessionMessage client session "**** Connecting canceled\n"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnected -> do
      displaySessionMessage client session "**** Disconnected\n"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnectError (Error errorText) -> do
      displaySessionMessage client session
        (printf "**** Error disconnecting: %s\n" errorText)
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnectedByPeer -> do
      displaySessionMessage client session "**** Disconnected by peer\n"
      tryReconnectSession client session
    IRCSendError (Error errorText) -> do
      displaySessionMessage client session
        (printf "**** Error sending: %s\n" errorText)
      tryReconnectSession client session
    IRCRecvError (Error errorText) -> do
      displaySessionMessage client session
        (printf "**** Error receiving: %s\n" errorText)
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
      | otherwise ->
        displaySessionMessage client session $ show message
  handleClientEvents client

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
      case decodeUtf8' text of
        Right text ->
          channel <- atomically $ findChannelByName session name
          case channel of
            Just channel -> do
              atomically $ do
                channelTabs <- findChannelTabsForChannel client channel
                forM (clientTabTab <$> channelTabs) $ \tab ->
                  setTopic tab text
            Nothing -> return ()
        Left _ -> return ()
    _ -> return ()

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

-- | Handle client tab event.
handleClientTabEvent :: Client -> ClientTab -> TabEvent -> IO ()
handleClientTabEvent client clientTab event = do
  case event of
    TabClosed -> handleTabClosed client clientTab
    LineEntered text -> handleLineEntered client clientTab text
    TopicEntered text -> handleTopicEntered client clientTab text
    TabSelected -> handleTabSelected client clientTab

-- | Handle a tab closed event.
handleTabClosedEvent :: Client -> ClientTab -> IO ()
handleTabClosedEvent client clientTab = do
  subtype <- atomically $ do
    tabs <- readTVar clientTabs client
    let index = clientTabIndex clientTab
    writeTVar (clientTabs client) $
      filter (\clientTab' -> clientTabIndex clientTab' /= index) tabs
    readTVar $ clientTabSubtype clientTab
  case subtype of
    SessionTab session -> cleanupSessionIfNoTabs client session
    ChannelTab channel -> cleanupChannelIfNoTabs client channel
    UserTab user -> cleanupUserIfNoTabsOfChannels client user
  handleClientEvents client

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
  handleClientEvents client

-- | Handle a tab selected event.
handleTabSelected :: Client -> ClientTab -> IO ()
handleTabSelected client clientTab = do
  atomically $
    writeTVar (clientTab clientTabSelectIndex) =<<
    getNextClientTabSelectIndex client
  handleClientEvents client

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
  userInChannel <- atomically $ isUserInChannel client user
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

-- | Get whether a user is in a channel.
isUserInChannel :: Client -> User -> STM Bool
isUserInChannel client user = do
  channels <- readTVar $ clientChannels client
  foldM (\found channel -> do
            users <- readTVar $ channelUsers
            let inChannel =
                  foldl' (\found user' ->
                             (userIndex user == userIndex user') || found)
                  False users
            return $ inChannel || found)
  False channels

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
