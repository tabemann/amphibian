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
import Network.IRC.Client.Amphibian.Log
import Network.IRC.Client.Amphibian.History
import Network.IRC.Client.Amphibian.IgnoreList
import Network.IRC.Client.Amphibian.ServerReplies
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Network.Socket as NS
import Data.Text.Encoding (encodeUtf8)
import Data.Functor ((<$>),
                      fmap)
import Data.Monoid ((<>))
import Data.Sequence ((|>),
                      (<|),
                      (><),
                      ViewL(..))
import Data.Foldable (foldl',
                      toList)
import System.IO (openFile,
                  hClose,
                  hFlush,
                  IOMode(..),
                  Handle)
import Data.Text.IO (hPutStr,
                     readFile)
import Text.Printf (printf)
import Control.Monad ((=<<),
                      join,
                      mapM,
                      forM,
                      forM_,
                      foldM)
import Control.Exception (catch,
                          IOException,
                          SomeException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Concurrent.STM (STM,
                               atomically,
                               orElse,
                               retry,
                               TVar,
                               newTVar,
                               writeTVar,
                               readTVar)
import Control.Concurrent.STM.TMVar (TMVar,
                                     newTMVar,
                                     putTMVar,
                                     takeTMVar)
import Text.Read (readMaybe)
import Data.Word (Word8)
import Data.Time.Clock (UTCTime,
                        getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone,
                            utcToLocalTime,
                            LocalTime(..),
                            TimeOfDay(..))
import System.Clock (getTime,
                     toNanoSecs,
                     fromNanoSecs,
                     diffTimeSpec,
                     TimeSpec(..),
                     Clock(Monotonic))
import Data.Char (isSpace)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)
import Prelude hiding (readFile)

-- | Run the client.
runClient :: IO ()
runClient = do
  withWindowing $ do
    running <- atomically $ newTVar True
    nextIndex <- atomically $ newTVar 0
    nextTabSelectIndex <- atomically $ newTVar 0
    nextWindowFocusIndex <- atomically $ newTVar 0
    sessions <- atomically $ newTVar S.empty
    channels <- atomically $ newTVar S.empty
    users <- atomically $ newTVar S.empty
    windows <- atomically $ newTVar S.empty
    tabs <- atomically $ newTVar S.empty
    settings <- atomically . newTVar $
                Settings { settingsReconnectDelay = 20.0,
                           settingsPongWaitDelay = 60.0,
                           settingsInitialMaxLines = 1000,
                           mentionForegroundColor = 7,
                           mentionBackgroundColor = 99 }
    ignoreList <- atomically $ newIgnoreList
    let client =
          Client { clientRunning = running,
                   clientNextIndex = nextIndex,
                   clientNextTabSelectIndex = nextTabSelectIndex,
                   clientNextWindowFocusIndex = nextWindowFocusIndex,
                   clientSessions = sessions,
                   clientChannels = channels,
                   clientUsers = users,
                   clientWindows = windows,
                   clientTabs = tabs,
                   clientSettings = settings,
                   clientIgnoreList = ignoreList }
    result <- startIgnoreList ignoreList
    case result of
      Right _ -> do
        response <- atomically $ loadIgnoreList ignoreList
        result <- atomically $ getResponse response
        case result of
          Right () -> return ()
          Left (Error errorText) -> displayError errorText
        result <- openClientWindow client "Amphibian IRC" "<Not Connected>"
        case result of
          Right _ -> do
            handleClientEvents client
            channels <- atomically . readTVar $ clientChannels client
            forM_ channels $ \channel -> do
              response <- atomically . stopLog $ channelLog channel
              syncHandleResponse response
            users <- atomically . readTVar $ clientUsers client
            forM_ users $ \user -> do
              response <- atomically . stopLog $ userLog user
              syncHandleResponse response
            windows <- atomically . readTVar $ clientWindows client
            forM_ windows $ \window -> do
              response <- atomically . stopWindow $ clientWindowWindow window
              result <- atomically $ getResponse response
              case result of
                Right () -> return ()
                Left (Error errorText) -> displayError errorText
            response <- atomically . stopIgnoreList $ clientIgnoreList client
            syncHandleResponse response
          Left (Error errorText) -> displayError errorText
      Left (Error errorText) -> displayError errorText

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
            Right clientTab -> do
              atomically $ do
                selectIndex <- getNextClientTabSelectIndex client
                writeTVar (clientTabSelectIndex clientTab) selectIndex
              return $ Right clientWindow
            Left failure -> do
              (atomically $ stopWindow window) >> return ()
              return $ Left failure
        Left failure -> do
          (atomically $ stopWindow window) >> return ()
          return $ Left failure
    Left failure -> return $ Left failure

-- | Open a client tab.
openClientTab :: Client -> ClientWindow -> T.Text -> IO (Either Error ClientTab)
openClientTab client clientWindow title = do
  response <- atomically $ openTab (clientWindowWindow clientWindow) title
    NoNotification
  result <- atomically $ getResponse response
  case result of
    Right tab -> do
      index <- atomically $ getNextClientIndex client
      selectIndex <- atomically $ newTVar (-1)
      tabEventSub <- atomically $ subscribeTab tab
      subtype <- atomically $ newTVar FreeTab
      notification <- atomically $ newTVar NoNotification
      history <- atomically $ newHistory
      let clientTab =
            ClientTab { clientTabIndex = index,
                        clientTabSelectIndex = selectIndex,
                        clientTabTab = tab,
                        clientTabEventSub = tabEventSub,
                        clientTabSubtype = subtype,
                        clientTabWindow = clientWindow,
                        clientTabNotification = notification,
                        clientTabHistory = history }
      startHistory history
      atomically $ do
        tabs <- readTVar $ clientTabs client
        writeTVar (clientTabs client) $ tabs |> clientTab
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
  ircConnectionEventSubs <- extractIRCConnectionEventSubs <$>
                            (readTVar $ clientSessions client)
  windowEventSubs <- extractWindowEventSubs <$>
                     (readTVar $ clientWindows client)
  tabEventSubs <- extractTabEventSubs <$>
                   (readTVar $ clientTabs client)
  if running
    then do
      getTaggedSessionEvent ircConnectionEventSubs `orElse`
        getTaggedClientWindowEvent windowEventSubs `orElse`
        getTaggedClientTabEvent tabEventSubs
    else return TaggedClientQuitEvent
  where extractIRCConnectionEventSubs =
          fmap (\session -> (session, sessionIRCConnectionEventSub session))
        extractWindowEventSubs =
          fmap (\clientWindow -> (clientWindow,
                                  clientWindowEventSub clientWindow))
        extractTabEventSubs =
          fmap (\clientTab -> (clientTab, clientTabEventSub clientTab))
        getTaggedSessionEvent =
          foldl' (\action (session, ircConnectionEventSub) ->
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
  return . fmap (\(clientTab, _) -> clientTab) $
    S.filter (matchTab session) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab session (tab, SessionTab currentSession) =
          sessionIndex session == sessionIndex currentSession
        matchTab _ _ = False

-- | Find all client tabs for session.
findAllTabsForSession :: Client -> Session -> STM (S.Seq ClientTab)
findAllTabsForSession client session = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return . fmap (\(clientTab, _) -> clientTab) $
    S.filter (matchTab session) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab session (tab, SessionTab currentSession) =
          sessionIndex session == sessionIndex currentSession
        matchTab session (tab, ChannelTab channel) =
          let currentSession = channelSession channel
          in sessionIndex session == sessionIndex currentSession
        matchTab session (tab, UserTab user) =
          let currentSession = userSession user
          in sessionIndex session == sessionIndex currentSession
        matchTab _ _ = False

-- | Find client tabs for channel.
findChannelTabsForChannel :: Client -> Channel -> STM (S.Seq ClientTab)
findChannelTabsForChannel client channel = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return . fmap (\(clientTab, _) -> clientTab) $
    S.filter (matchTab channel) clientTabs
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
  return . fmap (\(clientTab, _) -> clientTab) $
    S.filter (matchTab user) clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab user (tab, UserTab currentUser) =
          userIndex user == userIndex currentUser
        matchTab _ _ = False

-- | Find user tabs for session.
findUserTabsForSession :: Client -> Session -> STM (S.Seq ClientTab)
findUserTabsForSession client session = do
  clientTabs <- mapM getSubtype =<< (readTVar $ clientTabs client)
  return . fmap (\(clientTab, _) -> clientTab) $
    S.filter matchTab clientTabs
  where getSubtype tab = do
          subtype <- readTVar $ clientTabSubtype tab
          return (tab, subtype)
        matchTab (tab, UserTab currentUser) =
          sessionIndex session == sessionIndex (userSession currentUser)
        matchTab _ = False

-- | Find all client tabs for user.
findAllTabsForUser :: Client -> User -> STM (S.Seq ClientTab)
findAllTabsForUser client user = do
  foldM (matchTab user) S.empty =<< (readTVar $ clientTabs client)
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
                    sessionIndex session ==
                    (sessionIndex $ userSession user)
                  _ -> False
                then do
                  index' <- readTVar $ clientTabSelectIndex tab
                  if index' >= index
                    then findMostRecentTabForSession' session index' (Just tab)
                         rest
                    else findMostRecentTabForSession' session index  currentTab
                         rest
                else findMostRecentTabForSession' session index currentTab
                     rest
            S.EmptyL -> return currentTab

-- | Find the most recent tab.
findMostRecentTab :: Client -> STM (Maybe ClientTab)
findMostRecentTab client = do
  findMostRecentTab' (-1) Nothing =<< (readTVar $ clientTabs client)
  where findMostRecentTab' index currentTab tabs =
          case S.viewl tabs of
            tab :< rest -> do
              index' <- readTVar $ clientTabSelectIndex tab
              if index' >= index
                then findMostRecentTab' index' (Just tab) rest
                else findMostRecentTab' index currentTab rest
            S.EmptyL -> return currentTab

-- | Find the most recent window.
findMostRecentWindow :: Client -> STM (Maybe ClientWindow)
findMostRecentWindow client = do
  findMostRecentWindow' (-1) Nothing =<< (readTVar $ clientWindows client)
  where findMostRecentWindow' index currentWindow windows =
          case S.viewl windows of
            window :< rest -> do
              index' <- readTVar $ clientWindowFocusIndex window
              if index' >= index
                then findMostRecentWindow' index' (Just window) rest
                else findMostRecentWindow' index currentWindow rest
            S.EmptyL -> return currentWindow

-- | Find the most recent tab for a window.
findMostRecentTabForWindow :: Client -> ClientWindow -> STM (Maybe ClientTab)
findMostRecentTabForWindow client window = do
  findMostRecentTabForWindow' (-1) Nothing =<< (readTVar $ clientTabs client)
  where findMostRecentTabForWindow' index currentTab tabs =
          case S.viewl tabs of
            tab :< rest -> do
              let windowIndex = clientWindowIndex window
                  windowIndex' = clientWindowIndex $ clientTabWindow tab
              if windowIndex == windowIndex'
                then do
                  index' <- readTVar $ clientTabSelectIndex tab
                  if index' >= index
                    then findMostRecentTabForWindow' index' (Just tab) rest
                    else findMostRecentTabForWindow' index currentTab rest
                else findMostRecentTabForWindow' index currentTab rest
            S.EmptyL -> return currentTab

-- | Is tab most recent tab for window.
isTabMostRecentTabForWindow :: Client -> ClientTab -> STM Bool
isTabMostRecentTabForWindow client clientTab = do
  clientTab' <- findMostRecentTabForWindow client $ clientTabWindow clientTab
  case clientTab' of
    Just clientTab' ->
      return $ clientTabIndex clientTab == clientTabIndex clientTab'
    Nothing -> return False

-- | Format a message.
formatMessage :: Client -> Maybe Session -> T.Text -> IO T.Text
formatMessage client session message = do
  currentTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let localTime = utcToLocalTime timeZone currentTime
      timeOfDay = localTimeOfDay localTime
  return . T.pack $ printf "[%02d:%02d:%02d] %s\n" (todHour timeOfDay)
    (todMin timeOfDay) ((floor $ todSec timeOfDay) :: Int) message

-- | Log text for a tab if the tab has a log.
writeToLog :: ClientTab -> T.Text -> IO ()
writeToLog tab text = do
  subtype <- atomically . readTVar $ clientTabSubtype tab
  let log = case subtype of
              ChannelTab channel -> Just $ channelLog channel
              UserTab user -> Just $ userLog user
              _ -> Nothing
  case log of
    Just log -> do
      response <- atomically $ writeLog log text
      asyncHandleResponse response
    Nothing -> return ()

-- | Update style of tab titles based on channel message text.
updateTabTitleForChannelMessage :: Client -> ClientTab -> T.Text -> IO ()
updateTabTitleForChannelMessage client clientTab text = do
  session <- atomically $ getSessionForTab clientTab
  case session of
    Just session -> do
      nick <- atomically . readTVar $ sessionNick session
      if T.isInfixOf (ourDecodeUtf8 nick) text
        then setNotification client clientTab Mentioned
        else setNotification client clientTab ChannelMessaged
    Nothing -> return ()    

-- | Update style of tab titles based on user message text.
updateTabTitleForUserMessage :: Client -> ClientTab -> T.Text -> IO ()
updateTabTitleForUserMessage client clientTab text = do
  session <- atomically $ getSessionForTab clientTab
  case session of
    Just session -> do
      nick <- atomically . readTVar $ sessionNick session
      if T.isInfixOf (ourDecodeUtf8 nick) text
        then setNotification client clientTab Mentioned
        else setNotification client clientTab UserMessaged
    Nothing -> return ()

-- | Set notification for tab
setNotification :: Client -> ClientTab -> Notification -> IO ()
setNotification client clientTab notification = do
  mostRecent <- atomically $ isTabMostRecentTabForWindow client clientTab
  if not mostRecent
    then do
      response <- atomically $ do
        oldNotification <- readTVar $ clientTabNotification clientTab
        let newNotification = max oldNotification notification
        writeTVar (clientTabNotification clientTab) newNotification
        setTabNotification (clientTabTab clientTab) newNotification
      asyncHandleResponse response
    else return ()

-- | Reset notification for tab
resetNotification :: ClientTab -> IO ()
resetNotification clientTab = do
  response <- atomically $ do
    writeTVar (clientTabNotification clientTab) NoNotification
    setTabNotification (clientTabTab clientTab) NoNotification
  asyncHandleResponse response

-- | Display session message.
displaySessionMessage :: Client -> Session -> T.Text -> IO ()
displaySessionMessage client session message = do
  message <- formatMessage client (Just session) message
  responses <- do
    clientTabs <- atomically $ do
      clientTabs <- findSessionTabsForSession client session
      if S.length clientTabs > 0
        then return clientTabs
        else do
          clientTab <- getCurrentTabForSession client session
          case clientTab of
            Just clientTab -> return $ S.singleton clientTab
            Nothing -> return S.empty
    forM clientTabs $ \clientTab -> do
      writeToLog clientTab message
      atomically $ addTabText (clientTabTab clientTab) message
  forM_ responses $ \response -> asyncHandleResponse response

-- | Display channel message.
displayChannelMessage :: Client -> Channel -> T.Text -> IO ()
displayChannelMessage client channel message = do
  message <- formatMessage client (Just $ channelSession channel) message
  responses <- do
    clientTabs <- atomically $ findChannelTabsForChannel client channel
    forM clientTabs $ \clientTab -> do
      writeToLog clientTab message
      atomically $ addTabText (clientTabTab clientTab) message
  forM_ responses $ \response -> asyncHandleResponse response

-- | Display user message.
displayUserMessage :: Client -> User -> T.Text -> IO ()
displayUserMessage client user message = do
  message <- formatMessage client (Just $ userSession user) message
  responses <- do
    clientTabs <- atomically $ do
      clientTabs <- findUserTabsForUser client user
      if S.length clientTabs > 0
        then return clientTabs
        else do
          clientTab <- getCurrentTabForSession client $ userSession user
          case clientTab of
            Just clientTab -> return $ S.singleton clientTab
            Nothing -> return S.empty
    forM clientTabs $ \clientTab -> do
      writeToLog clientTab message
      atomically $ addTabText (clientTabTab clientTab) message
  forM_ responses $ \response -> asyncHandleResponse response

-- | Display message on all tabs for user.
displayUserMessageAll :: Client -> User -> T.Text -> IO ()
displayUserMessageAll client user message = do
  message <- formatMessage client (Just $ userSession user) message
  responses <- do
    tabs <- atomically $ findAllTabsForUser client user
    forM tabs $ \clientTab -> do
      writeToLog clientTab message
      atomically $ addTabText (clientTabTab clientTab) message
  forM_ responses $ \response -> asyncHandleResponse response

-- | Display messsage on all tabs for session.
displaySessionMessageAll :: Client -> Session -> T.Text -> IO ()
displaySessionMessageAll client session message = do
  message <- formatMessage client (Just session) message
  responses <- do
    tabs <- atomically $ findAllTabsForSession client session
    forM tabs $ \clientTab -> do
      writeToLog clientTab message
      atomically $ addTabText (clientTabTab clientTab) message
  forM_ responses $ \response -> asyncHandleResponse response

-- | Display message on most recent tab for session.
displaySessionMessageOnMostRecentTab :: Client -> Session -> T.Text -> IO ()
displaySessionMessageOnMostRecentTab client session message = do
  message <- formatMessage client (Just session) message
  response <- do
    clientTab <- atomically $ findMostRecentTabForSession client session
    case clientTab of
      Just clientTab -> do
        writeToLog clientTab message
        Just <$> (atomically $ addTabText (clientTabTab clientTab) message)
      Nothing -> return Nothing
  case response of
    Just response -> asyncHandleResponse response
    Nothing -> return ()

-- | Display message on tab.
displayMessage :: Client -> ClientTab -> T.Text -> IO ()
displayMessage client clientTab message = do
  session <- atomically $ getSessionForTab clientTab
  message <- formatMessage client session message
  writeToLog clientTab message
  response <- atomically $
    addTabText (clientTabTab clientTab) message
  asyncHandleResponse response

-- | Display message on tabs.
displayMessageOnTabs :: Client -> S.Seq ClientTab -> T.Text -> IO ()
displayMessageOnTabs client tabs message = do
  forM_ tabs $ \clientTab -> displayMessage client clientTab message

-- | Handle a session event.
handleSessionEvent :: Client -> Session -> IRCConnectionEvent -> IO ()
handleSessionEvent client session event = do
  case event of
    IRCFoundAddr address -> do
      displaySessionMessage client session
        (T.pack . printf "* Found address: %s" . stripText . T.pack $
         show address)
    IRCNoAddrFound (Error errorText) -> do
      displaySessionMessage client session
        (T.pack $ printf "* Unable to find address: %s" $ stripText errorText)
      tryReconnectSession client session
    IRCLookupCanceled -> do
      displaySessionMessage client session
        "* Address lookup canceled"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCFoundName hostname -> do
      displaySessionMessage client session
        (T.pack $ printf "* Found hostname: %s" . stripText $ T.pack hostname)
    IRCNoNameFound (Error errorText) -> do
      displaySessionMessage client session
        (T.pack $ printf "* No name found: %s" $ stripText errorText)
      tryReconnectSession client session
    IRCReverseLookupCanceled -> do
      displaySessionMessage client session
        "* Reverse lookup canceled"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCConnectingFailed (Error errorText) -> do
      displaySessionMessage client session
        (T.pack $ printf "* Connecting failed: %s" $ stripText errorText)
      tryReconnectSession client session
    IRCConnected -> do
      let connection = sessionIRCConnection session
      hostnameResponse <- atomically $ getIRCConnectionHostname connection
      portResponse <- atomically $ getIRCConnectionPort connection
      (hostname, port) <- atomically $ do
        hostname <- getResponse hostnameResponse
        let hostname' = case hostname of
                          Right hostname -> hostname
                          Left (Error errorText) -> Nothing
        port <- getResponse portResponse
        let port' = case port of
                      Right port -> port
                      Left (Error errorText) -> Nothing
        return (hostname', port')
      case (hostname, port) of
        (Just hostname, Just port) -> do
          displaySessionMessage client session
            (T.pack $ printf "* Connected to %s:%d"
             (stripText $ T.pack hostname)
             (fromIntegral port :: Int))
          atomically $ do
            writeTVar (sessionHostname session) hostname
            writeTVar (sessionReconnectOnFailure session) True
        _ -> return ()
      (response0, response1, response2, response3) <- atomically $ do
        let capReqMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "CAP",
                           ircMessageParams = S.singleton $ encodeUtf8 "REQ",
                           ircMessageCoda = Just $ encodeUtf8 "multi-prefix" }
        response0 <- sendIRCMessage (sessionIRCConnection session) capReqMessage
        let capEndMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "CAP",
                           ircMessageParams = S.singleton $ encodeUtf8 "END",
                           ircMessageCoda = Nothing }
        response1 <- sendIRCMessage (sessionIRCConnection session) capEndMessage
        nick <- readTVar $ sessionNick session
        let nickMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "NICK",
                           ircMessageParams = S.singleton nick,
                           ircMessageCoda = Nothing }
        response2 <- sendIRCMessage (sessionIRCConnection session) nickMessage
        username <- readTVar $ sessionUsername session
        realName <- readTVar $ sessionRealName session
        let userMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "USER",
                           ircMessageParams = [username,
                                               encodeUtf8 "0",
                                               encodeUtf8 "*"],
                           ircMessageCoda = Just realName }
        response3 <- sendIRCMessage (sessionIRCConnection session) userMessage
        writeTVar (sessionState session) SessionPreparing
        writeTVar (sessionReconnectOnFailure session) True
        return (response0, response1, response2, response3)
      asyncHandleResponse response0
      asyncHandleResponse response1
      asyncHandleResponse response2
      asyncHandleResponse response3
    IRCConnectingCanceled -> do
      displaySessionMessageAll client session "* Connecting canceled"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnected -> do
      displaySessionMessageAll client session "* Disconnected"
      atomically $ writeTVar (sessionState session) SessionInactive
    IRCDisconnectError (Error errorText) -> do
      displaySessionMessageAll client session
        (T.pack $ printf "* Error disconnecting: %s" $ stripText errorText)
      tryReconnectSession client session
    IRCDisconnectedByPeer -> do
      displaySessionMessageAll client session "* Disconnected by peer"
      tryReconnectSession client session
    IRCSendError (Error errorText) -> do
      displaySessionMessageAll client session
        (T.pack $ printf "* Error sending: %s" $ stripText errorText)
      tryReconnectSession client session
    IRCRecvError (Error errorText) -> do
      displaySessionMessageAll client session
        (T.pack $ printf "* Error receiving: %s" $ stripText errorText)
      tryReconnectSession client session
    IRCRecvMessage message
      | ircMessageCommand message == rpl_WELCOME ->
        handleWelcome client session message
      | ircMessageCommand message == err_NICKNAMEINUSE ->
        handleNicknameInUse client session message
      | ircMessageCommand message == rpl_NAMREPLY ->
        handleNamreply client session message
      | ircMessageCommand message == rpl_ENDOFNAMES -> return ()
      | ircMessageCommand message == rpl_TOPIC ->
        handleTopicReply client session message
      | ircMessageCommand message == encodeUtf8 "PING" ->
        handlePingMessage client session message
      | ircMessageCommand message == encodeUtf8 "PONG" ->
        handlePongMessage client session message
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
        handleOtherMessage client session message

-- | Handle welcome message.
handleWelcome :: Client -> Session -> IRCMessage -> IO ()
handleWelcome client session message = do
  atomically $ writeTVar (sessionState session) SessionReady
  startPinging client session
  channels <- atomically . readTVar $ sessionChannels session
  forM_ channels $ \channel -> do
    state <- atomically . readTVar $ channelState channel
    if state == InChannel || state == AwaitingReconnect
      then do
        let message = IRCMessage { ircMessagePrefix = Nothing,
                                   ircMessageCommand = encodeUtf8 "JOIN",
                                   ircMessageParams =
                                     S.singleton $ channelName channel,
                                   ircMessageCoda = Nothing }
        sendIRCMessageToSession session message
      else return ()

-- | Handle nickname in use message.
handleNicknameInUse :: Client -> Session -> IRCMessage -> IO ()
handleNicknameInUse client session message = do
  response <- atomically $ do
    state <- readTVar $ sessionState session
    case state of
      SessionPreparing -> do
        nick <- readTVar $ sessionNick session
        let newNick = B.append nick $ encodeUtf8 "_"
        writeTVar (sessionNick session) newNick
        let nickMessage =
              IRCMessage { ircMessagePrefix = Nothing,
                           ircMessageCommand = encodeUtf8 "NICK",
                           ircMessageParams = S.singleton newNick,
                           ircMessageCoda = Nothing }
        response <- sendIRCMessage (sessionIRCConnection session) nickMessage
        return $ Just response
      _ -> return Nothing
  case response of
    Just response -> asyncHandleResponse response
    Nothing -> return ()
  updateNickForAllSessionTabs client session

-- | Handle NAMREPLY message.
handleNamreply :: Client -> Session -> IRCMessage -> IO ()
handleNamreply client session message = do
  case S.lookup 2 $ ircMessageParams message of
    Just name -> do
      channel <- atomically $ findChannelByName session name
      case channel of
        Just channel ->
          case ircMessageCoda message of
            Just coda -> handleUsers channel coda
            Nothing -> return ()
        Nothing -> return ()
    Nothing -> return ()
  where handleUsers channel coda = do
          let (nickWithPrefix, rest) = splitOnSpaces coda
          if nickWithPrefix /= B.empty
            then do let (nick, userType') = getNickAndUserType nickWithPrefix
                    user <- atomically $ do
                      user <- findOrCreateUserByNick client session nick
                      setUserTypeForChannel user channel userType'
                      users <- readTVar $ channelUsers channel
                      writeTVar (channelUsers channel) $ users |> user
                      return user
                    updateChannelTabsForUser client channel user
                    case rest of
                      Just rest -> handleUsers channel rest
                      Nothing -> return ()
            else return ()
        getNickAndUserType nickWithPrefix = do
          let (nickWithPrefix', userType') =
                case B.uncons nickWithPrefix of
                  Just (char, rest)
                    | char == byteOfChar '~' -> (rest, S.singleton OwnerUser)
                  _ -> (nickWithPrefix, S.empty)
              (nickWithPrefix'', userType'') =
                case B.uncons nickWithPrefix' of
                  Just (char, rest)
                    | char == byteOfChar '&' -> (rest, userType' |> AdminUser)
                  _ -> (nickWithPrefix', userType')
              (nickWithPrefix''', userType''') =
                case B.uncons nickWithPrefix'' of
                  Just (char, rest)
                    | char == byteOfChar '@' -> (rest, userType'' |> OpUser)
                  _ -> (nickWithPrefix'', userType'')
              (nickWithPrefix'''', userType'''') =
                case B.uncons nickWithPrefix''' of
                  Just (char, rest)
                    | char == byteOfChar '%' ->
                        (rest, userType''' |> HalfOpUser)
                  _ -> (nickWithPrefix''', userType''')
            in case B.uncons nickWithPrefix'''' of
                 Just (char, rest)
                   | char == byteOfChar '+' ->
                     (rest, userType'''' |> VoiceUser)
                 _ -> (nickWithPrefix'''', userType'''')

-- | Handle topic reply message.
handleTopicReply :: Client -> Session -> IRCMessage -> IO ()
handleTopicReply client session message = do
  case (ircMessageCoda message, S.lookup 1 $ ircMessageParams message) of
    (Just topic, Just name) -> do
      channel <- atomically $ findChannelByName session name
      case channel of
        Just channel -> do
          atomically $ writeTVar (channelTopic channel) $ Just topic
          tabs <- atomically $ findChannelTabsForChannel client channel
          forM_ tabs $ \tab ->
            atomically $ setTopic (clientTabTab tab) $ ourDecodeUtf8 topic
        Nothing -> return ()
    _ -> return ()

-- | Handle PING message.
handlePingMessage :: Client -> Session -> IRCMessage -> IO ()
handlePingMessage client session message = do
  case ircMessageCoda message of
    Just coda -> do
      sendIRCMessageToSession session $
        IRCMessage { ircMessagePrefix = Nothing,
                     ircMessageCommand = encodeUtf8 "PONG",
                     ircMessageParams = S.singleton coda,
                     ircMessageCoda = Nothing }
    Nothing -> return ()

-- | Handle PONG message.
handlePongMessage :: Client -> Session -> IRCMessage -> IO ()
handlePongMessage client session message = do
  atomically $ do
    count <- readTVar $ sessionPongCount session
    writeTVar (sessionPongCount session) $ count + 1

-- | Handle TOPIC message.
handleTopicMessage :: Client -> Session -> IRCMessage -> IO ()
handleTopicMessage client session message = do
  case (S.lookup 0 $ ircMessageParams message, ircMessageCoda message) of
    (Just name, Just text) ->
      let text' = ourDecodeUtf8 text
      in do channel <- atomically $ findChannelByName session name
            case channel of
              Just channel -> do
                case ircMessagePrefix message of
                  Just prefix ->
                    let nick = ourDecodeUtf8 $ extractNick prefix
                    in displayChannelMessage client channel . T.pack $
                       printf "* %s has changed the topic to: %s" nick
                       (ourDecodeUtf8 text)
                  Nothing -> return ()
                atomically $ do
                  writeTVar (channelTopic channel) $ Just text
                  channelTabs <- findChannelTabsForChannel client channel
                  forM_ (clientTabTab <$> channelTabs) $ \tab ->
                    setTopic tab text'
              Nothing -> return ()
    _ -> return ()

-- | Handle JOIN message.
handleJoinMessage :: Client -> Session -> IRCMessage -> IO ()
handleJoinMessage client session message = do
  case ircMessagePrefix message of
    Just prefix ->
      let nick = extractNick prefix
      in case ircMessageCoda message of
        Just name -> do
          ourNick <- atomically . readTVar $ sessionNick session
          if nick /= ourNick
            then handleNormalJoin client session nick name prefix
            else handleOurJoin client session name
        Nothing ->
          case S.lookup 0 $ ircMessageParams message of
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
                  response <- atomically $ filterWithIgnoreList
                              (clientIgnoreList client) prefix [UserEventStatus]
                  result <- atomically $ getResponse response
                  case result of
                    Right True -> do
                      displayChannelMessage client channel . T.pack $
                        printf "* %s (%s) has joined"
                        (stripText $ ourDecodeUtf8 nick)
                        (stripText $ ourDecodeUtf8 prefix)
                    Right False -> return ()
                    Left (Error errorText) -> displayError errorText
                  atomically $ do
                    type' <- readTVar $ userType user
                    let index = channelIndex channel
                        type'' =
                          S.filter (\(channel', _) ->
                                      index /= channelIndex channel') type'
                    writeTVar (userType user) $ type'' |> (channel, S.empty)
                    users <- readTVar $ channelUsers channel
                    writeTVar (channelUsers channel) $ users |> user
                  tabs <- atomically $ findChannelTabsForChannel client channel
                  forM_ tabs $ \tab -> setNotification client tab UserChanged
                else return ()
              updateChannelTabsForUser client channel user
            Nothing -> return ()
        handleOurJoin client session name = do
          channel <- atomically $ findOrCreateChannelByName client session name
          removeAllUsersFromChannel client channel
          tabs <- findOrCreateChannelTabsForChannel client channel
          forM_ tabs $ \tab -> do
            response <- atomically . addCompletion (clientTabTab tab) $
                        ourDecodeUtf8 name
            asyncHandleResponse response
            response <- atomically $ setTopicVisible (clientTabTab tab) True
            result <- atomically $ getResponse response
            case result of
              Right () -> do
                topic <- atomically . readTVar $ channelTopic channel
                case topic of
                  Just topic -> do
                    response <- atomically $ setTopic (clientTabTab tab)
                                (ourDecodeUtf8 topic)
                    asyncHandleResponse response
                  Nothing -> return ()
              Left (Error errorText) -> displayError errorText
            response <- atomically $ setSideVisible (clientTabTab tab) True
            asyncHandleResponse response
          displayChannelMessage client channel . T.pack $
            printf "* Now talking on %s" (stripText $ ourDecodeUtf8 name)
          user <- atomically $ do
            ourNick <- readTVar $ sessionNick session
            user <- findOrCreateUserByNick client session ourNick
            users <- readTVar $ channelUsers channel
            writeTVar (channelState channel) InChannel
            writeTVar (channelUsers channel) $ users |> user
            return user
          updateChannelTabsForUser client channel user
          updateNickForAllSessionTabs client session

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
                         response <- atomically $ filterWithIgnoreList
                                     (clientIgnoreList client) prefix
                                     [UserEventStatus]
                         result <- atomically $ getResponse response
                         case result of
                           Right True -> do
                             displayChannelMessage client channel . T.pack $
                               printf "* %s (%s) has left"
                               (stripText $ ourDecodeUtf8 nick)
                               (stripText $ ourDecodeUtf8 prefix)
                           Right False -> return ()
                           Left (Error errorText) -> displayError errorText
                         removeUserFromChannel client channel user
                         tabs <- atomically $ findChannelTabsForChannel client
                                 channel
                         forM_ tabs $ \tab ->
                           setNotification client tab UserChanged
                       else do
                         displayChannelMessage client channel "* You have left"
                         removeAllUsersFromChannel client channel
                         atomically $
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
           _ -> return ()
    Nothing -> return ()
  where handleNormalKick client channel user kickingNick kickedNick coda = do
          case coda of
            Just coda -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s has kicked %s from %s (%s)"
                (stripText $ ourDecodeUtf8 kickingNick)
                (stripText $ ourDecodeUtf8 kickedNick)
                (stripText . ourDecodeUtf8 $ channelName channel)
                (stripText $ ourDecodeUtf8 coda)
            Nothing -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s has kicked %s from %s"
                (stripText $ ourDecodeUtf8 kickingNick)
                (stripText $ ourDecodeUtf8 kickedNick)
                (stripText . ourDecodeUtf8 $ channelName channel)
          removeUserFromChannel client channel user
        handleOurKick client channel user kickingNick coda = do
          case ircMessageCoda message of
            Just coda -> do
              displayChannelMessage client channel . T.pack $
                printf "* You have been kicked from %s by %s (%s)"
                (stripText . ourDecodeUtf8 $ channelName channel)
                (stripText $ ourDecodeUtf8 kickingNick)
                (stripText $ ourDecodeUtf8 coda)
            Nothing -> do
              displayChannelMessage client channel . T.pack $
                printf "* You have been kicked from %s by %s"
                (stripText . ourDecodeUtf8 $ channelName channel)
                (stripText $ ourDecodeUtf8 kickingNick)
          removeUserFromChannel client channel user
          removeAllUsersFromChannelTabs client channel
          atomically $ writeTVar (channelState channel) NotInChannel

-- | Handle QUIT message.
handleQuitMessage :: Client -> Session -> IRCMessage -> IO ()
handleQuitMessage client session message = do
  case ircMessagePrefix message of
    Just prefix -> do
      let nick = extractNick prefix
      user <- atomically $ findUserByNick session nick
      case user of
        Just user -> do
          tabs <- atomically $ findAllTabsForUser client user
          forM_ tabs $ \tab -> setNotification client tab UserChanged
          ourNick <- atomically . readTVar $ sessionNick session
          if nick /= ourNick
            then do
              response <- atomically $ filterWithIgnoreList
                          (clientIgnoreList client) prefix [UserEventStatus]
              result <- atomically $ getResponse response
              case result of
                Right True -> do
                  case ircMessageCoda message of
                    Just coda -> do
                      displayUserMessageAll client user . T.pack $
                        printf "* %s has quit (%s)"
                        (stripText $ ourDecodeUtf8 nick)
                        (stripText $ ourDecodeUtf8 coda)
                    Nothing -> do
                      displayUserMessageAll client user . T.pack $
                        printf "* %s has quit"
                        (stripText $ ourDecodeUtf8 nick)
                Right False -> return ()
                Left (Error errorText) -> displayError errorText
            else return ()
          removeUserFromAllChannels client session user
        Nothing -> return ()
    Nothing -> return ()

-- | Handle ERROR message.
handleErrorMessage :: Client -> Session -> IRCMessage -> IO ()
handleErrorMessage client session message = do
  leaveAllChannelsInSession client session
  response <- atomically $ do
    response <- disconnectIRC $ sessionIRCConnection session
    writeTVar (sessionState session) SessionInactive
    return response
  asyncHandleResponse response

-- | Handle NICK message.
handleNickMessage :: Client -> Session -> IRCMessage -> IO ()
handleNickMessage client session message =
  case (ircMessagePrefix message) of
    Just prefix -> do
      let nick = extractNick prefix
          newNick =
            case S.lookup 0 $ ircMessageParams message of
              Just newNick -> newNick
              Nothing ->
                case ircMessageCoda message of
                  Just coda -> fst $ B.breakSubstring (encodeUtf8 " ") coda
                  Nothing -> ""
      if newNick /= ""
        then do
          user <- atomically $ findUserByNick session nick
          case user of
            Just user -> do
              tabs <- atomically $ findAllTabsForUser client user
              forM_ tabs $ \tab -> setNotification client tab UserChanged
              removeUserFromAllChannelTabs client user
            Nothing -> return ()
          join . atomically $ do
            case user of
              Just user -> writeTVar (userNick user) newNick
              Nothing -> return ()
            ourNick <- readTVar $ sessionNick session
            if nick == ourNick
              then do
                writeTVar (sessionNick session) newNick
                return $ do
                  case user of
                    Just user -> do
                      updateTabTitleForAllUserTabs client user
                      updateAllChannelTabsForUser client user
                      updateNickForAllSessionTabs client session
                      tabs <- atomically $ findUserTabsForSession client session
                      forM_ tabs $ \tab -> do
                        response <- atomically $
                                    removeCompletion (clientTabTab tab)
                                    (ourDecodeUtf8 nick)
                        asyncHandleResponse response
                        response <- atomically $
                                    addCompletion (clientTabTab tab)
                                    (ourDecodeUtf8 newNick)
                        asyncHandleResponse response
                      displayUserMessageAll client user . T.pack $
                        printf "* You are now known as %s"
                        (stripText $ ourDecodeUtf8 newNick)
                    Nothing -> return ()
              else do
                return $ do
                  case user of
                    Just user -> do
                      updateTabTitleForAllUserTabs client user
                      updateAllChannelTabsForUser client user
                      tabs <- atomically $ findUserTabsForUser client user
                      forM_ tabs $ \tab -> do
                        response <- atomically $
                                    removeCompletion (clientTabTab tab)
                                    (ourDecodeUtf8 nick)
                        asyncHandleResponse response
                        response <- atomically $
                                    addCompletion (clientTabTab tab)
                                    (ourDecodeUtf8 newNick)
                        asyncHandleResponse response
                      response <- atomically $ filterWithIgnoreList
                                  (clientIgnoreList client) prefix
                                  [UserEventStatus]
                      result <- atomically $ getResponse response
                      case result of
                        Right True -> do
                          displayUserMessageAll client user . T.pack $
                            printf "* %s is now known as %s"
                            (stripText $ ourDecodeUtf8 nick)
                            (stripText $ ourDecodeUtf8 newNick)
                        Right False -> return ()
                        Left (Error errorText) -> displayError errorText
                    Nothing -> return ()
        else return ()
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
  where handleUserMode :: Client -> Session -> IRCMessage -> B.ByteString ->
                          IO ()
        handleUserMode client session message nickOrChannel = do
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
                      displaySessionMessageOnMostRecentTab client session .
                        T.pack $ printf "* %s sets mode %s on %s"
                        (stripText $ ourDecodeUtf8 prefix)
                        (stripText $ ourDecodeUtf8 param)
                        (stripText $ ourDecodeUtf8 nickOrChannel)
                      atomically $ do
                        mode <- readTVar $ sessionMode session
                        writeTVar (sessionMode session) $
                          parseUserMode param mode
                    Nothing -> return ()
            _ -> return ()
        parseUserMode :: B.ByteString -> S.Seq Mode -> S.Seq Mode
        parseUserMode param mode =
          case B.uncons param of
            Just (byte, rest)
              | byte == byteOfChar '+' -> parseUserModeAdd rest mode
              | byte == byteOfChar '-' -> parseUserModeRemove rest mode
              | otherwise -> parseUserModeAdd param mode
            Nothing -> mode
        parseUserModeAdd :: B.ByteString -> S.Seq Mode -> S.Seq Mode
        parseUserModeAdd param mode =
          case B.uncons param of
            Just (byte, rest)
              | byte /= byteOfChar '+' && byte /= byteOfChar '-' ->
                case S.elemIndexL (Mode byte) mode of
                  Nothing -> parseUserModeAdd rest $ mode |> Mode byte
                  _ -> parseUserModeAdd rest mode
              | otherwise -> parseUserMode param mode
            Nothing -> mode
        parseUserModeRemove :: B.ByteString -> S.Seq Mode -> S.Seq Mode
        parseUserModeRemove param mode =
          case B.uncons param of
            Just (byte, rest)
              | byte /= byteOfChar '+' && byte /= byteOfChar '-' ->
                parseUserModeRemove rest $
                S.filter (\(Mode byte') -> byte' /= byte) mode
              | otherwise -> parseUserMode param mode
            Nothing -> mode
        handleChannelMode :: Client -> Session -> IRCMessage -> B.ByteString ->
                             IO ()
        handleChannelMode client session message nickOrChannel = do
          case S.lookup 1 $ ircMessageParams message of
            Just param -> do
              case B.uncons param of
                Just (byte, _)
                  | byte == byteOfChar '+' || byte == byteOfChar '-' ->
                    case ircMessagePrefix message of
                      Just prefix -> do
                        let prefix' = extractNick prefix
                        channel <-
                          atomically $ findChannelByName session nickOrChannel
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
        parseChannelMode :: Client -> Channel -> B.ByteString -> B.ByteString ->
                            S.Seq Mode -> S.Seq B.ByteString -> IO (S.Seq Mode)
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
        parseChannelModeAdd :: Client -> Channel -> B.ByteString ->
                               B.ByteString -> S.Seq Mode ->
                               S.Seq B.ByteString -> IO (S.Seq Mode)
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
                    displayChannelMessage client channel . T.pack $
                      printf "* %s sets mode +%s on %s"
                      (stripText $ ourDecodeUtf8 prefix)
                      (stripText . ourDecodeUtf8 $ B.singleton byte)
                      (stripText . ourDecodeUtf8 $ channelName channel)
                    parseChannelModeAdd client channel rest prefix mode
                      extraParams
              | otherwise -> parseChannelMode client channel param prefix mode
                             extraParams
            Nothing -> return mode
        handleChannelModeAddBan :: Client -> Channel -> B.ByteString ->
                                   B.ByteString -> S.Seq Mode ->
                                   S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeAddBan client channel param prefix mode extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s sets ban on %s"
                (stripText $ ourDecodeUtf8 prefix)
                (stripText $ ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddUserType :: Client -> Channel -> B.ByteString ->
                                        B.ByteString -> S.Seq Mode ->
                                        S.Seq B.ByteString -> T.Text ->
                                        UserType -> IO (S.Seq Mode)
        handleChannelModeAddUserType client channel param prefix mode
          extraParams text userType =
          case S.viewl extraParams of
            extraParam :< rest -> do
              user <-
                atomically $ findUserByNick (channelSession channel) extraParam
              case user of
                Just user -> do
                  displayChannelMessage client channel . T.pack $
                    printf "* %s sets %s on %s"
                    (stripText $ ourDecodeUtf8 prefix)
                    (stripText text)
                    (stripText $ ourDecodeUtf8 extraParam)
                  atomically $ addUserType user channel userType
                  updateChannelTabsForUser client channel user
                Nothing -> return ()
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddInviteMask :: Client -> Channel -> B.ByteString ->
                                          B.ByteString -> S.Seq Mode ->
                                          S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeAddInviteMask client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s adds %s to the invite mask"
                (stripText $ ourDecodeUtf8 prefix)
                (stripText $ ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddKeyword :: Client -> Channel -> B.ByteString ->
                                       B.ByteString -> S.Seq Mode ->
                                       S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeAddKeyword client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s sets keyword to %s"
                (stripText $ ourDecodeUtf8 prefix)
                (stripText $ ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        handleChannelModeAddLimit :: Client -> Channel -> B.ByteString ->
                                     B.ByteString -> S.Seq Mode ->
                                     S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeAddLimit client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s sets user limit to %s"
                (stripText $ ourDecodeUtf8 prefix)
                (stripText $ ourDecodeUtf8 extraParam)
              parseChannelModeAdd client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeAdd client channel param prefix mode extraParams
        parseChannelModeRemove :: Client -> Channel -> B.ByteString ->
                                  B.ByteString -> S.Seq Mode ->
                                  S.Seq B.ByteString -> IO (S.Seq Mode)
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
                  then handleChannelModeRemoveBan client channel rest prefix
                       mode extraParams
                  else if byte == byteOfChar 'o'
                  then handleChannelModeRemoveUserType client channel rest
                       prefix mode extraParams "op" OpUser
                  else if byte == byteOfChar 'O'
                  then handleChannelModeRemoveUserType client channel rest
                       prefix mode extraParams "owner" OwnerUser
                  else if byte == byteOfChar 'h'
                  then handleChannelModeRemoveUserType client channel rest
                       prefix mode extraParams "halfop" HalfOpUser
                  else if byte == byteOfChar 'v'
                  then handleChannelModeRemoveUserType client channel rest
                       prefix mode extraParams "voice" VoiceUser
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
                    displayChannelMessage client channel . T.pack $
                      printf "* %s sets mode -%s on %s"
                      (stripText $ ourDecodeUtf8 prefix)
                      (stripText . ourDecodeUtf8 $ B.singleton byte)
                      (stripText . ourDecodeUtf8 $ channelName channel)
                    parseChannelModeRemove client channel rest prefix mode
                      extraParams
              | otherwise -> parseChannelMode client channel param prefix mode
                             extraParams
            Nothing -> return mode
        handleChannelModeRemoveBan :: Client -> Channel -> B.ByteString ->
                                      B.ByteString -> S.Seq Mode ->
                                      S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeRemoveBan client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              displayChannelMessage client channel . T.pack $
                printf "* %s removes ban from %s"
                (stripText $ ourDecodeUtf8 prefix)
                (stripText $ ourDecodeUtf8 extraParam)
              parseChannelModeRemove client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeRemove client channel param prefix mode
              extraParams
        handleChannelModeRemoveUserType :: Client -> Channel -> B.ByteString ->
                                           B.ByteString -> S.Seq Mode ->
                                           S.Seq B.ByteString -> T.Text ->
                                           UserType -> IO (S.Seq Mode)
        handleChannelModeRemoveUserType client channel param prefix mode
          extraParams text userType =
          case S.viewl extraParams of
            extraParam :< rest -> do
              user <-
                atomically $ findUserByNick (channelSession channel) extraParam
              case user of
                Just user -> do
                  displayChannelMessage client channel . T.pack $
                    printf "* %s removes %s from %s"
                    (stripText $ ourDecodeUtf8 prefix)
                    (stripText text)
                    (stripText $ ourDecodeUtf8 extraParam)
                  atomically $ removeUserType user channel userType
                  updateChannelTabsForUser client channel user
                Nothing -> return ()
              parseChannelModeRemove client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeRemove client channel param prefix mode
              extraParams
        handleChannelModeRemoveInviteMask :: Client -> Channel ->
                                             B.ByteString ->
                                             B.ByteString -> S.Seq Mode ->
                                             S.Seq B.ByteString ->
                                             IO (S.Seq Mode)
        handleChannelModeRemoveInviteMask client channel param prefix mode
          extraParams =
          case S.viewl extraParams of
            extraParam :< rest -> do
              user <-
                atomically $ findUserByNick (channelSession channel)
                extraParam
              case user of
                Just user -> do
                  displayChannelMessage client channel . T.pack $
                    printf "* %s removes %s from the invite mask"
                    (stripText $ ourDecodeUtf8 prefix)
                    (stripText $ ourDecodeUtf8 extraParam)
                Nothing -> return ()
              parseChannelModeRemove client channel param prefix mode rest
            S.EmptyL ->
              parseChannelModeRemove client channel param prefix mode extraParams
        handleChannelModeRemoveKeyword :: Client -> Channel -> B.ByteString ->
                                          B.ByteString -> S.Seq Mode ->
                                          S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeRemoveKeyword client channel param prefix mode
          extraParams = do
          displayChannelMessage client channel . T.pack $
            printf "* %s unsets keyword" (stripText $ ourDecodeUtf8 prefix)
          parseChannelModeRemove client channel param prefix mode extraParams
        handleChannelModeRemoveLimit :: Client -> Channel -> B.ByteString ->
                                        B.ByteString -> S.Seq Mode ->
                                        S.Seq B.ByteString -> IO (S.Seq Mode)
        handleChannelModeRemoveLimit client channel param prefix mode
          extraParams = do
          displayChannelMessage client channel . T.pack $
            printf "* %s unsets user limit" (stripText $ ourDecodeUtf8 prefix)
          parseChannelModeRemove client channel param prefix mode extraParams

-- | Channel modes with parameters.
channelModesToExclude :: S.Seq Word8
channelModesToExclude = [byteOfChar 'b', byteOfChar 'o', byteOfChar 'O',
                         byteOfChar 'I', byteOfChar 'h', byteOfChar 'v']

-- | Handle PRIVMSG message.
handlePrivmsgMessage :: Client -> Session -> IRCMessage -> IO ()
handlePrivmsgMessage client session message = do
  settings <- atomically . readTVar $ clientSettings client
  case (ircMessagePrefix message,
        S.lookup 0 $ ircMessageParams message,
        ircMessageCoda message) of
    (Just prefix, Just target, Just text) -> do
      let source = extractNick prefix
      ourNick <- atomically . readTVar $ sessionNick session
      let text' = ourDecodeUtf8 text
          text'' =
            if T.isInfixOf (ourDecodeUtf8 ourNick) text'
            then colorText text'
                 (mentionForegroundColor settings)
                 (mentionBackgroundColor settings)
            else text'
      if target == ourNick
        then do
          case parseCtcp text of
            Nothing -> do
              response <- atomically $ filterWithIgnoreList
                          (clientIgnoreList client) prefix [UserEventPrivate]
              result <- atomically $ getResponse response
              case result of
                Right True -> do
                  user <- atomically $ findOrCreateUserByNick client session
                          source
                  tabs <- findOrCreateUserTabsForUser client user
                  forM_ tabs $ \tab -> do
                    updateTabTitleForUserMessage client tab text'
                    response <- atomically . addCompletion (clientTabTab tab) $
                                ourDecodeUtf8 ourNick
                    asyncHandleResponse response
                    response <- atomically . addCompletion (clientTabTab tab) $
                                ourDecodeUtf8 source
                    asyncHandleResponse response
                  updateNickForAllSessionTabs client session
                  displayMessageOnTabs client tabs . T.pack $
                    printf "<%s> %s" (stripText $ ourDecodeUtf8 source) text''
                Right False -> return ()
                Left (Error errorText) -> displayError errorText
            Just (command, param) -> do
              response <- atomically $ filterWithIgnoreList
                          (clientIgnoreList client) prefix [UserEventCtcp]
              result <- atomically $ getResponse response
              case result of
                Right True -> do
                  user <- atomically $ findOrCreateUserByNick client session
                          source
                  handleUserCtcp client user command param
                Right False -> return ()
                Left (Error errorText) -> displayError errorText
        else do
          channel <- atomically $ findChannelByName session target
          case channel of
            Just channel -> do
              case parseCtcp text of
                Nothing -> do
                  response <- atomically $ filterWithIgnoreList
                              (clientIgnoreList client) prefix
                              [UserEventChannel]
                  result <- atomically $ getResponse response
                  case result of
                    Right True -> do
                      user <- atomically $ findOrCreateUserByNick client
                              (channelSession channel) source
                      userPrefix <-
                        userTypePrefix <$> (atomically $ getSingleUserType user
                                            channel)
                      tabs <- atomically $ findChannelTabsForChannel client channel
                      forM_ tabs $ \tab -> do
                        updateTabTitleForChannelMessage client tab $
                          ourDecodeUtf8 text
                      displayChannelMessage client channel . T.pack $
                        printf "<%s%s> %s" (stripText $ userPrefix)
                        (stripText $ ourDecodeUtf8 source) text''
                    Right False -> return ()
                    Left (Error errorText) -> displayError errorText
                Just (command, param) -> do
                  response <- atomically $ filterWithIgnoreList
                              (clientIgnoreList client) prefix [UserEventCtcp]
                  result <- atomically $ getResponse response
                  case result of
                    Right True -> do
                      user <- atomically $ findOrCreateUserByNick client
                              (channelSession channel) source
                      handleChannelCtcp client channel user command param
                    Right False -> return ()
                    Left (Error errorText) -> displayError errorText
            Nothing -> return ()
    _ -> return ()

-- | Handle NOTICE message.
handleNoticeMessage :: Client -> Session -> IRCMessage -> IO ()
handleNoticeMessage client session message = do
  settings <- atomically . readTVar $ clientSettings client
  case (ircMessagePrefix message,
        S.lookup 0 $ ircMessageParams message,
        ircMessageCoda message) of
    (Just prefix, Just target, Just text) -> do
      let source = extractNick prefix
      ourNick <- atomically . readTVar $ sessionNick session
      let text' = ourDecodeUtf8 text
          text'' =
            if T.isInfixOf (ourDecodeUtf8 ourNick) text'
            then colorText text'
                 (mentionForegroundColor settings)
                 (mentionBackgroundColor settings)
            else text'
      if target == ourNick
        then do
          case parseCtcp text of
            Just (command, rest) -> do
              response <- atomically $ filterWithIgnoreList
                          (clientIgnoreList client) prefix [UserEventCtcp]
              result <- atomically $ getResponse response
              case result of
                Right True -> do
                  user <- atomically $ findOrCreateUserByNick client session
                          source
                  handleCtcpNotice client user command rest
                Right False -> return ()
                Left (Error errorText) -> displayError errorText
            Nothing -> do
              response <- atomically $ filterWithIgnoreList
                          (clientIgnoreList client) prefix [UserEventNotice,
                                                            UserEventPrivate]
              result <- atomically $ getResponse response
              case result of
                Right True -> do
                  tab <-
                    atomically $ findMostRecentTabForSession client session
                  case tab of
                    Just tab ->
                      updateTabTitleForUserMessage client tab text'
                  displaySessionMessageOnMostRecentTab client session . T.pack $
                    printf "-%s- %s" (stripText $ ourDecodeUtf8 source) text''
                Right False -> return ()
                Left (Error errorText) -> displayError errorText
        else do
          response <- atomically $ filterWithIgnoreList
                      (clientIgnoreList client) prefix [UserEventNotice,
                                                        UserEventChannel]
          result <- atomically $ getResponse response
          case result of
            Right True -> do
              channel <- atomically $ findChannelByName session target
              case channel of
                Just channel -> do
                  tabs <- atomically $ findChannelTabsForChannel client channel
                  forM_ tabs $ \tab ->
                    updateTabTitleForChannelMessage client tab $
                    ourDecodeUtf8 text
                  displayChannelMessage client channel . T.pack $
                    printf "-%s- %s" (stripText $ ourDecodeUtf8 source) text''
                Nothing -> return ()
            Right False -> return ()
            Left (Error errorText) -> displayError errorText
    _ -> return ()

-- | Handle some other message.
handleOtherMessage :: Client -> Session -> IRCMessage -> IO ()
handleOtherMessage client session message = do
  ourNick <- atomically . readTVar $ sessionNick session
  let firstParts =
        case S.viewl $ ircMessageParams message of
          nick :< rest
            | nick == ourNick -> rest
          _ -> ircMessageParams message
      coda =
        case ircMessageCoda message of
          Just coda -> S.singleton coda
          Nothing -> S.empty
      text = B.concat . toList . S.intersperse " " $ firstParts >< coda
  displaySessionMessage client session . T.pack . printf ": %s" . stripText $
    ourDecodeUtf8 text

-- | Handle user CTCP message.
handleUserCtcp :: Client -> User -> B.ByteString -> Maybe B.ByteString -> IO ()
handleUserCtcp client user command param =
  if command == encodeUtf8 "ACTION"
  then
    case param of
      Just param -> do
        tabs <- findOrCreateUserTabsForUser client user
        nick <- atomically . readTVar $ userNick user
        displayMessageOnTabs client tabs . T.pack $
          printf "* %s %s" (stripText $ ourDecodeUtf8 nick) (ourDecodeUtf8 param)
      Nothing -> return ()
  else if command == encodeUtf8 "PING"
  then
    case param of
      Just param -> handleCtcpPingMessage client user param
      Nothing -> return ()
  else return ()

-- | Handle channel CTCP message.
handleChannelCtcp :: Client -> Channel -> User -> B.ByteString ->
                     Maybe B.ByteString -> IO ()
handleChannelCtcp client channel user command param =
  if command == encodeUtf8 "ACTION"
  then
    case param of
      Just param -> do
        nick <- atomically . readTVar $ userNick user
        displayChannelMessage client channel . T.pack $
          printf "* %s %s" (stripText $ ourDecodeUtf8 nick) (ourDecodeUtf8 param)
      Nothing -> return ()
  else if command == encodeUtf8 "PING"
  then
    case param of
      Just param -> handleCtcpPingMessage client user param
      Nothing -> return ()
  else return ()

-- | Handle CTCP PING message.
handleCtcpPingMessage :: Client -> User -> B.ByteString -> IO ()
handleCtcpPingMessage client user param = do
  nick <- atomically . readTVar $ userNick user
  sendIRCMessageToUser user $
    formatCtcpReply nick (encodeUtf8 "PING") (Just param)
  displaySessionMessageOnMostRecentTab client (userSession user) . T.pack $
    printf "* Received a CTCP PING %s from %s"
    (stripText $ ourDecodeUtf8 param)
    (stripText $ ourDecodeUtf8 nick)

-- | Handle CTCP notice.
handleCtcpNotice :: Client -> User -> B.ByteString -> Maybe B.ByteString ->
                    IO ()
handleCtcpNotice client user command param =
  if command == encodeUtf8 "PING"
  then
    case param of
      Just param ->
        let origTime = readMaybe . T.unpack $ ourDecodeUtf8 param
        in case origTime of
             Just origTime -> do
               nick <- atomically . readTVar $ userNick user
               currentTime <- getTime Monotonic
               let diff = toNanoSecs . diffTimeSpec currentTime $
                          fromNanoSecs origTime
               displaySessionMessageOnMostRecentTab client (userSession user) .
                 T.pack $ printf "* Ping reply from %s: %.3f second(s)"
                 (stripText $ ourDecodeUtf8 nick)
                 ((fromIntegral diff :: Double) / 1000000000.0)
             Nothing -> return ()
      Nothing -> return ()
  else return ()

-- | Find or create channel tabs for channel.
findOrCreateChannelTabsForChannel :: Client -> Channel -> IO (S.Seq ClientTab)
findOrCreateChannelTabsForChannel client channel = do
  tabs <- atomically $ findChannelTabsForChannel client channel
  if not $ S.null tabs
    then return tabs
    else do
      window <- atomically $ findMostRecentWindow client
      case window of
        Just window -> do
          result <-
            openClientTab client window . ourDecodeUtf8 $ channelName channel
          case result of
            Right tab -> do
              populateTabFromLog (channelSession channel) tab
                (channelName channel) (channelLog channel)
              populateHistory (channelSession channel)
                (Just $ channelName channel) (clientTabHistory tab)
              atomically $ writeTVar (clientTabSubtype tab) $ ChannelTab channel
              return $ S.singleton tab
            Left (Error errorText) -> do
              displayError errorText
              return S.empty
        Nothing -> do
          displayError "NO WINDOW AVAILABLE"
          return S.empty

-- | Find or create user tabs for user.
findOrCreateUserTabsForUser :: Client -> User -> IO (S.Seq ClientTab)
findOrCreateUserTabsForUser client user = do
  nick <- atomically . readTVar $ userNick user
  tabs <- atomically $ findUserTabsForUser client user
  if not $ S.null tabs
    then return tabs
    else do
      window <- atomically $ findMostRecentWindow client
      case window of
        Just window -> do
          result <- openClientTab client window (ourDecodeUtf8 nick)
          case result of
            Right tab -> do
              populateTabFromLog (userSession user) tab nick (userLog user)
              populateHistory (userSession user) (Just nick)
                (clientTabHistory tab)
              atomically . writeTVar (clientTabSubtype tab) $ UserTab user
              return $ S.singleton tab
            Left (Error errorText) -> do
              displayError errorText
              return S.empty
        Nothing -> return S.empty

-- | Add a user type to a user for a channel.
addUserType :: User -> Channel -> UserType -> STM ()
addUserType user channel aType = do
  types <- readTVar $ userType user
  let index = S.findIndexL
              (\(channel', _) -> channelIndex channel == channelIndex channel')
              types
  case index of
    Just index ->
      case S.lookup index types of
        Just (_, channelTypes) ->
          case S.elemIndexL aType channelTypes of
            Nothing ->
              writeTVar (userType user) $
                S.update index (channel, channelTypes |> aType) types
            _ -> return ()
        Nothing -> error "impossible"
    Nothing ->
      writeTVar (userType user) $ types |> (channel, S.singleton aType)

-- | Remove a user type from a user for a channel.
removeUserType :: User -> Channel -> UserType -> STM ()
removeUserType user channel aType = do
  types <- readTVar $ userType user
  let index = S.findIndexL
              (\(channel', _) -> channelIndex channel == channelIndex channel')
              types
  case index of
    Just index ->
      case S.lookup index types of
        Just (_, channelTypes) -> do
          writeTVar (userType user) $
            S.update index (channel, S.filter (/= aType) channelTypes) types
        Nothing -> return ()
    Nothing -> return ()

-- | Extract nick from full nick, username, and host.
extractNick :: B.ByteString -> B.ByteString
extractNick = fst . B.breakSubstring (encodeUtf8 "!")

-- | Remove a user from a channel.
removeUserFromChannel :: Client -> Channel -> User -> IO ()
removeUserFromChannel client channel user = do
  atomically $ do
    users <- readTVar $ channelUsers channel
    writeTVar (channelUsers channel) $
      S.filter (\user' -> userIndex user /= userIndex user') users
  removeUserFromChannelTabs client channel user
  cleanupUserIfNoTabsOrChannels client user

-- | Remove all users from a channel.
removeAllUsersFromChannel :: Client -> Channel -> IO ()
removeAllUsersFromChannel client channel = do
  removeAllUsersFromChannelTabs client channel
  users <- atomically $ do
    users <- readTVar $ channelUsers channel
    writeTVar (channelUsers channel) S.empty
    return users
  forM_ users $ \user -> cleanupUserIfNoTabsOrChannels client user

-- | Remove all users from all channels for session.
leaveAllChannelsInSession :: Client -> Session -> IO ()
leaveAllChannelsInSession client session = do
  channels <- atomically . readTVar $ sessionChannels session
  forM_ channels $ \channel -> do
    atomically $ writeTVar (channelState channel) NotInChannel
    removeAllUsersFromChannel client channel

-- | Remove a user from all channels for a session.
removeUserFromAllChannels :: Client -> Session -> User -> IO ()
removeUserFromAllChannels client session user = do
  channels <- atomically . readTVar $ sessionChannels session
  forM_ channels $ \channel -> removeUserFromChannel client channel user

-- | Find channel by name.
findChannelByName :: Session -> B.ByteString -> STM (Maybe Channel)
findChannelByName session name = do
  channels <- readTVar $ sessionChannels session
  return $ findChannelByName' channels name
  where findChannelByName' channels name =
          case S.viewl channels of
            channel :< rest ->
              if channelName channel == name
              then Just channel
              else findChannelByName' rest name
            S.EmptyL -> Nothing

-- | Find user by nick
findUserByNick :: Session -> B.ByteString -> STM (Maybe User)
findUserByNick session nick = do
  users <- readTVar $ sessionUsers session
  findUserByNick' users nick
  where findUserByNick' users nick =
          case S.viewl users of
            user :< rest -> do
              nick' <- readTVar $ userNick user
              if nick == nick'
                then return $ Just user
                else findUserByNick' rest nick
            S.EmptyL -> return Nothing

-- | Try to reconnect a session.
tryReconnectSession :: Client -> Session -> IO ()
tryReconnectSession client session = do
  markChannelsAsAwaitingReconnect client session
  reconnectOnFailure <- atomically . readTVar $
                        sessionReconnectOnFailure session
  if reconnectOnFailure
    then reconnectSession client session WithDelay
    else do
      stopPinging session
      atomically $ writeTVar (sessionState session) SessionInactive

-- | Start pinging
startPinging :: Client -> Session -> IO ()
startPinging client session = do
  pingingAsync <- atomically . readTVar $ sessionPinging session
  case pingingAsync of
    Nothing -> return ()
    Just pingingAsync -> cancel pingingAsync
  pingingAsync <- Just <$> async doPinging
  atomically $ writeTVar (sessionPinging session) pingingAsync
  where doPinging = do
          state <- atomically . readTVar $ sessionState session
          case state of
            SessionReady -> do
              hostname <- atomically . readTVar $ sessionHostname session
              let message = IRCMessage { ircMessagePrefix = Nothing,
                                         ircMessageCommand = encodeUtf8 "PING",
                                         ircMessageParams =
                                           S.singleton (encodeUtf8 $
                                                        T.pack hostname),
                                         ircMessageCoda = Nothing }
              pongCount <- atomically . readTVar $ sessionPongCount session
              sendIRCMessageToSession session message
              delay <- settingsPongWaitDelay <$>
                       (atomically . readTVar $ clientSettings client)
              threadDelay . floor $ delay * 1000000.0
              (state, newPongCount) <- atomically $ do
                state <- readTVar $ sessionState session
                newPongCount <- readTVar $ sessionPongCount session
                return (state, newPongCount)
              case state of
                SessionReady ->
                  if newPongCount <= pongCount
                  then do
                    displaySessionMessageOnMostRecentTab client session .
                      T.pack $ printf "* Ping timeout: %f seconds" delay
                    (async $ tryReconnectSession client session) >> return ()
                  else doPinging
                _ -> return ()
            _ -> return ()

-- | Stop pinging.
stopPinging :: Session -> IO ()
stopPinging session = do
  pingingAsync <- atomically $ do
    pingingAsync <- readTVar $ sessionPinging session
    writeTVar (sessionPinging session) Nothing
    return pingingAsync
  case pingingAsync of
    Just pingingAsync -> cancel pingingAsync
    Nothing -> return ()

-- | Reconnect a session.
reconnectSession :: Client -> Session -> Delay -> IO ()
reconnectSession client session withDelay = do
  stopPinging session
  reconnectingAsync <- atomically $ do
    takeTMVar (sessionReconnectingLock session) >> return ()
    reconnectingAsync <- readTVar $ sessionReconnecting session
    return reconnectingAsync
  case reconnectingAsync of
    Nothing -> do
      atomically $ writeTVar (sessionState session) SessionConnecting
      delay <- case withDelay of
                 WithDelay -> settingsReconnectDelay <$>
                              (atomically . readTVar $ clientSettings client)
                 WithoutDelay -> return 0
      reconnectingAsync <- async $ do
        threadDelay . floor $ delay * 1000000.0
        let connection = sessionIRCConnection session
        stateResponse <- atomically $ getIRCConnectionState connection
        state <- atomically $ getResponse stateResponse
        case state of
          Right IRCConnectionNotStarted -> return ()
          Right state -> do
            if isIRCConnectionStateActive state
              then do
                response <- atomically $ disconnectIRC connection
                result <- atomically $ getResponse response
                case result of
                  Right () -> return ()
                  Left (Error errorText) -> displayError errorText
              else return ()
            hostname <- atomically . readTVar $ sessionOrigHostname session
            port <- atomically . readTVar $ sessionPort session
            response <- atomically $ connectIRC connection hostname port
            asyncHandleResponse response
          Left (Error errorText) -> displayError errorText
        atomically $ writeTVar (sessionReconnecting session) Nothing
      atomically $ do
        writeTVar (sessionReconnecting session) (Just reconnectingAsync)
        putTMVar (sessionReconnectingLock session) ()
    Just _ -> atomically $ putTMVar (sessionReconnectingLock session) ()

-- | Handle client window event.
handleClientWindowEvent :: Client -> ClientWindow -> WindowEvent -> IO ()
handleClientWindowEvent client clientWindow event = do
  case event of
    WindowClosed -> handleWindowClosed client clientWindow
    WindowFocused -> handleWindowFocused client clientWindow

-- | Handle window closed.
handleWindowClosed :: Client -> ClientWindow -> IO ()
handleWindowClosed = cleanupClosedWindow

-- | Actually handle a closed window.
cleanupClosedWindow :: Client -> ClientWindow -> IO ()
cleanupClosedWindow client clientWindow = do
  deletedTabs <- atomically $ do
    let index = clientWindowIndex clientWindow
    windows <- readTVar $ clientWindows client
    let windows' =
          S.filter (\clientWindow' -> clientWindowIndex clientWindow' /= index)
          windows
    writeTVar (clientWindows client) windows'
    if S.length windows' == 0
      then writeTVar (clientRunning client) False
      else return ()
    tabs <- readTVar $ clientTabs client
    let (_, deletedTabs) =
          S.partition (\clientTab' ->
                         (clientWindowIndex $ clientTabWindow clientTab') /=
                         index) tabs
    return deletedTabs
  forM_ deletedTabs $ \tab -> cleanupClosedTab client tab

-- | Handle window focused.
handleWindowFocused :: Client -> ClientWindow -> IO ()
handleWindowFocused client clientWindow = do
  atomically $ do
    index <- getNextClientWindowFocusIndex client
    writeTVar (clientWindowFocusIndex clientWindow) index

-- | Handle client tab event.
handleClientTabEvent :: Client -> ClientTab -> TabEvent -> IO ()
handleClientTabEvent client clientTab event = do
  case event of
    TabClosed -> handleTabClosed client clientTab
    LineEntered text -> handleLineEntered client clientTab text
    TopicEntered text -> handleTopicEntered client clientTab text
    TabSelected -> handleTabSelected client clientTab
    UpPressed -> handleUpPressed client clientTab
    DownPressed -> handleDownPressed client clientTab

-- | Handle a tab closed event.
handleTabClosed :: Client -> ClientTab -> IO ()
handleTabClosed = cleanupClosedTab

-- | Handle an up pressed event.
handleUpPressed :: Client -> ClientTab -> IO ()
handleUpPressed client clientTab = do
  response <- atomically . getPrevHistory $ clientTabHistory clientTab
  result <- atomically $ getResponse response
  case result of
    Right (Just text) -> do
      response <- atomically $ setEntry (clientTabTab clientTab) text
      asyncHandleResponse response
    Right Nothing -> return ()
    Left (Error errorText) -> displayError errorText

-- | Handle a down pressed event
handleDownPressed :: Client -> ClientTab -> IO ()
handleDownPressed client clientTab = do
  response <- atomically . getNextHistory $ clientTabHistory clientTab
  result <- atomically $ getResponse response
  case result of
    Right (Just text) -> do
      response <- atomically $ setEntry (clientTabTab clientTab) text
      asyncHandleResponse response
    Right Nothing -> return ()
    Left (Error errorText) -> displayError errorText

-- | Actually handle a closed tab.
cleanupClosedTab :: Client -> ClientTab -> IO ()
cleanupClosedTab client clientTab = do
  response <- atomically . stopHistory $ clientTabHistory clientTab
  syncHandleResponse response
  subtype <- atomically $ do
    tabs <- readTVar $ clientTabs client
    let index = clientTabIndex clientTab
    writeTVar (clientTabs client) $
      S.filter (\clientTab' -> clientTabIndex clientTab' /= index) tabs
    readTVar $ clientTabSubtype clientTab
  case subtype of
    SessionTab session -> cleanupSessionIfNoTabs client session
    ChannelTab channel -> cleanupChannelIfNoTabs client channel
    UserTab user -> cleanupUserIfNoTabsOrChannels client user
    _ -> return ()

-- | Handle a line entered event.
handleLineEntered :: Client -> ClientTab -> T.Text -> IO ()
handleLineEntered client clientTab text = do
  forM_ (T.splitOn "\n" text) $ \text -> do
    let history = clientTabHistory clientTab
    response <- atomically $ addHistory history text
    asyncHandleResponse response
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
      let session = channelSession channel
      ourNick <- atomically . readTVar $ sessionNick session
      ourUser <- atomically $ findUserByNick session ourNick
      userPrefix <-
        case ourUser of
          Just ourUser ->
            userTypePrefix <$> (atomically $ getSingleUserType ourUser channel)
          Nothing -> return ""
      state <- atomically . readTVar $ channelState channel
      case state of
        InChannel -> do
          displayChannelMessage client channel . T.pack $
            printf "<%s%s> %s" (stripText $ userPrefix)
            (stripText $ ourDecodeUtf8 ourNick) text
          let message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "PRIVMSG",
                                     ircMessageParams =
                                       S.singleton $ channelName channel,
                                     ircMessageCoda = Just $ encodeUtf8 text }
          sendIRCMessageToSession session message
        NotInChannel -> displayMessage client clientTab "* Not in channel"
        AwaitingReconnect -> displayMessage client clientTab
                             "* Awaiting reconnection..."
    UserTab user -> do
      let session = userSession user
      ourNick <- atomically . readTVar $ sessionNick session
      nick <- atomically .  readTVar $ userNick user
      let displayText = filterMessageText nick text
      displayUserMessage client user . T.pack $
        printf "<%s> %s" (stripText $ ourDecodeUtf8 ourNick) displayText
      let message = IRCMessage { ircMessagePrefix = Nothing,
                                 ircMessageCommand = encodeUtf8 "PRIVMSG",
                                 ircMessageParams = S.singleton nick,
                                 ircMessageCoda = Just $ encodeUtf8 text }
      sendIRCMessageToSession session message

-- | Handle command.
handleCommand :: Client -> ClientTab -> T.Text -> IO ()
handleCommand client clientTab command = do
  case parseCommandField command of
    Just (command, rest) -> do
      let command' = T.toLower command
      if command' == "new"
        then handleNewCommand client clientTab rest
        else if command' == "new-window"
        then handleNewWindowCommand client clientTab rest
        else if command' == "close"
        then handleCloseCommand client clientTab rest
        else if command' == "server"
        then handleServerCommand client clientTab rest
        else if command' == "quit"
        then handleQuitCommand client clientTab rest
        else if command' == "join"
        then handleJoinCommand client clientTab rest
        else if command' == "part"
        then handlePartCommand client clientTab rest
        else if command' == "msg"
        then handleMsgCommand client clientTab rest
        else if command' == "notice"
        then handleNoticeCommand client clientTab rest
        else if command' == "topic"
        then handleTopicCommand client clientTab rest
        else if command' == "mode"
        then handleModeCommand client clientTab rest
        else if command' == "kick"
        then handleKickCommand client clientTab rest
        else if command' == "nick"
        then handleNickCommand client clientTab rest
        else if command' == "me"
        then handleMeCommand client clientTab rest
        else if command' == "ping"
        then handlePingCommand client clientTab rest
        else if command' == "reconnect"
        then handleReconnectCommand client clientTab rest
        else if command' == "whois"
        then handleWhoisCommand client clientTab rest
        else if command' == "whowas"
        then handleWhowasCommand client clientTab rest
        else if command' == "ignore"
        then handleIgnoreCommand client clientTab rest
        else if command' == "unignore"
        then handleUnignoreCommand client clientTab rest
        else handleUnrecognizedCommand client clientTab command
    Nothing -> return ()

-- | Handle the /new command.
handleNewCommand :: Client -> ClientTab -> T.Text -> IO ()
handleNewCommand client clientTab text =
  if text == ""
  then do
    response <- openClientTab client (clientTabWindow clientTab)
                "<Not Connected>"
    case response of
      Right _ -> return ()
      Left (Error errorText) -> displayError errorText
  else displayMessage client clientTab "* Syntax: /new"

-- | Handle the /new-window command.
handleNewWindowCommand :: Client -> ClientTab -> T.Text -> IO ()
handleNewWindowCommand client clientTab text =
  if text == ""
  then do
    response <- openClientWindow client "Amphibian IRC" "<Not Connected>"
    case response of
      Right _ -> return ()
      Left (Error errorText) -> displayError errorText
  else displayMessage client clientTab "* Syntax: /new-window"

-- | Handle the /close command.
handleCloseCommand :: Client -> ClientTab -> T.Text -> IO ()
handleCloseCommand client clientTab text =
  if text == ""
  then do
    let window = clientTabWindow clientTab
        actualWindow = clientWindowWindow $ clientTabWindow clientTab
    response <- atomically . closeTab $ clientTabTab clientTab
    result <- atomically $ getResponse response
    case result of
      Right () -> do
        cleanupClosedTab client clientTab
        count <- atomically $ handleGetNumberOfTabsInWindow client window
        if count == 0
          then do
            response <- atomically $ closeWindow actualWindow
            result <- atomically $ getResponse response
            case result of
              Right () -> return ()
              Left (Error errorText) -> displayError errorText
            response <- atomically $ stopWindow actualWindow
            asyncHandleResponse response
            cleanupClosedWindow client window
          else return ()
      Left (Error errorText) -> displayError errorText
  else displayMessage client clientTab "* Syntax: /close"

-- | Handle the /server command.
handleServerCommand :: Client -> ClientTab -> T.Text -> IO ()
handleServerCommand client clientTab text = do
  let params =
        case parseCommandField text of
          Just (hostname, rest) ->
            let hostname' = T.unpack hostname
            in case parseCommandField rest of
                 Just (port, rest) ->
                   case readMaybe $ T.unpack port of
                     Just port ->
                       case parseCommandField rest of
                         Just (nick, rest) ->
                           let nick' = encodeUtf8 nick
                           in case parseCommandField rest of
                                Just (username, rest) ->
                                  let username' = encodeUtf8 username
                                  in if rest /= ""
                                     then let realName = encodeUtf8 rest
                                          in Just (hostname', port, nick',
                                                   username', realName)
                                     else Just (hostname', port, nick',
                                                username', username')
                                Nothing ->
                                  Just (hostname', port, nick', nick', nick')
                         Nothing -> Nothing
                     Nothing -> Nothing
                 Nothing -> Nothing
          Nothing -> Nothing
  case params of
    Just (hostname, port, nick, username, realName) -> do
      subtype <- atomically . readTVar $ clientTabSubtype clientTab
      case subtype of
        SessionTab session -> do
          state <- atomically . readTVar $ sessionState session
          case state of
            SessionInactive -> do
              currentHostname <-
                atomically . readTVar $ sessionOrigHostname session
              currentPort <- atomically . readTVar $ sessionPort session
              if hostname == currentHostname && port == currentPort
                then reuseSession client session nick username realName
                else do (createSessionWithNewTab client hostname port nick
                         username realName) >> return ()
            SessionReady -> do
              (createSessionWithNewTab client hostname port nick
                username realName) >> return ()
            SessionPreparing -> do
              (createSessionWithNewTab client hostname port nick
                username realName) >> return ()
            SessionConnecting -> do
              (createSessionWithNewTab client hostname port nick
                username realName) >> return ()
            SessionDestroyed -> do
              (createSessionInTab client clientTab hostname port nick username
               realName) >> return ()
        FreeTab -> do
          (createSessionInTab client clientTab hostname port nick username
           realName) >> return ()
        ChannelTab _ -> do
          (createSessionWithNewTab client hostname port nick username
           realName) >> return ()
        UserTab _ -> do
          (createSessionWithNewTab client hostname port nick username
           realName) >> return ()
    Nothing -> displayMessage client clientTab
               "* Syntax: /server hostname port nick [<username> [<real name>]]"

-- | Handle the /quit command.
handleQuitCommand :: Client -> ClientTab -> T.Text -> IO ()
handleQuitCommand client clientTab text = do
  session <- atomically $ getSessionForTab clientTab
  case session of
    Nothing -> displayMessage client clientTab "* Tab has no associated session"
    Just session -> do
      atomically $ writeTVar (sessionReconnectOnFailure session) False
      stopPinging session
      reconnecting <- atomically $ do
        reconnecting <- readTVar $ sessionReconnecting session
        writeTVar (sessionReconnecting session) Nothing
        return reconnecting
      case reconnecting of
        Just reconnecting -> cancel reconnecting
        Nothing -> return ()
      state <- atomically . readTVar $ sessionState session
      case state of
        SessionReady -> sendQuitMessage session
        SessionPreparing -> sendQuitMessage session
        SessionConnecting -> do
          response <- atomically . disconnectIRC $ sessionIRCConnection session
          asyncHandleResponse response
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"
  where sendQuitMessage session = do
          let message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "QUIT",
                                     ircMessageParams = S.empty,
                                     ircMessageCoda = Just $ encodeUtf8 text }
          sendIRCMessageToSession session message
          
-- | Handle the /join command.
handleJoinCommand :: Client -> ClientTab -> T.Text -> IO ()
handleJoinCommand client clientTab text =
  case parseCommandField text of
    Just (name, rest)
      | rest == "" -> do
          handleCommandWithReadySession client clientTab $ \session -> do
            let message =
                  IRCMessage { ircMessagePrefix = Nothing,
                               ircMessageCommand = encodeUtf8 "JOIN",
                               ircMessageParams = S.singleton $ encodeUtf8 name,
                               ircMessageCoda = Nothing }
            sendIRCMessageToSession session message
    _ -> displayMessage client clientTab "* Syntax: /join channel"

-- | Handle the /part command.
handlePartCommand :: Client -> ClientTab -> T.Text -> IO ()
handlePartCommand client clientTab text = do
  handleCommandWithReadyChannel client clientTab $ \channel -> do
    let message =
          IRCMessage { ircMessagePrefix = Nothing,
                       ircMessageCommand = encodeUtf8 "PART",
                       ircMessageParams = S.singleton $ channelName channel,
                       ircMessageCoda = if text /= ""
                                        then Just $ encodeUtf8 text
                                        else Nothing }
    sendIRCMessageToChannel channel message

-- | Handle the /msg command.
handleMsgCommand :: Client -> ClientTab -> T.Text -> IO ()
handleMsgCommand client clientTab text =
  case parseCommandField text of
    Just (nickOrName, rest) -> do
      handleCommandWithReadySession client clientTab $ \session -> do
        let displayText = filterMessageText (encodeUtf8 nickOrName) rest
        displayMessage client clientTab . T.pack $
          printf ">%s< %s" (stripText nickOrName) displayText
        let nickOrName' = encodeUtf8 nickOrName
            rest' = encodeUtf8 rest
            message = IRCMessage { ircMessagePrefix = Nothing,
                                   ircMessageCommand = encodeUtf8 "PRIVMSG",
                                   ircMessageParams = S.singleton nickOrName',
                                   ircMessageCoda = Just rest' }
        sendIRCMessageToSession session message
    _ -> displayMessage client clientTab "* Syntax: /msg target message"

-- | Handle the /notice command.
handleNoticeCommand :: Client -> ClientTab -> T.Text -> IO ()
handleNoticeCommand client clientTab text =
  case parseCommandField text of
    Just (nickOrName, rest) -> do
      handleCommandWithReadySession client clientTab $ \session -> do
        let displayText = filterMessageText (encodeUtf8 nickOrName) rest
        displayMessage client clientTab . T.pack $
          printf "->%s<- %s" (stripText nickOrName) displayText
        let nickOrName' = encodeUtf8 nickOrName
            rest' = encodeUtf8 rest
            message = IRCMessage { ircMessagePrefix = Nothing,
                                   ircMessageCommand = encodeUtf8 "NOTICE",
                                   ircMessageParams = S.singleton nickOrName',
                                   ircMessageCoda = Just rest' }
        sendIRCMessageToSession session message
    _ -> displayMessage client clientTab "* Syntax: /notice target message"

-- | Handle the /topic command.
handleTopicCommand :: Client -> ClientTab -> T.Text -> IO ()
handleTopicCommand client clientTab text = do
  handleCommandWithReadyChannel client clientTab $ \channel -> do
    let message =
          IRCMessage { ircMessagePrefix = Nothing,
                       ircMessageCommand = encodeUtf8 "TOPIC",
                       ircMessageParams = S.singleton $ channelName channel,
                       ircMessageCoda = Just $ encodeUtf8 text }
    sendIRCMessageToChannel channel message

-- | Handle the /mode command.
handleModeCommand :: Client -> ClientTab -> T.Text -> IO ()
handleModeCommand client clientTab text = do
  case parseCommandField text of
    Just (start, rest) -> do
      handleCommandWithReadySession client clientTab $ \session -> do
        ourNick <- atomically . readTVar $ sessionNick session
        if start == ourDecodeUtf8 ourNick
          then handleModeChange client clientTab session ourNick rest
          else
            case T.uncons start of
              Just (char, _)
                | char == '#' -> do
                    let name = encodeUtf8 start
                    handleModeChange client clientTab session name rest
                | otherwise -> do
                    handleCommandWithReadyChannel client clientTab $
                      \channel -> do
                        handleModeChange client clientTab session
                          (channelName channel) text
              Nothing -> error "impossible"
    _ -> displayMessage client clientTab
         "* Syntax: /mode [nick | channel] change [parameters]"
  where handleModeChange client clientTab session target text =
          let fields = parseCommandFields text
              message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "MODE",
                                     ircMessageParams =
                                       target <| fmap encodeUtf8 fields,
                                     ircMessageCoda = Nothing }
          in sendIRCMessageToSession session message

-- | Handle the /kick command.
handleKickCommand :: Client -> ClientTab -> T.Text -> IO ()
handleKickCommand client clientTab text = do
  case parseCommandField text of
    Just (target, rest) -> do
      handleCommandWithReadyChannel client clientTab $ \channel -> do
        let message = IRCMessage { ircMessagePrefix = Nothing,
                                   ircMessageCommand = encodeUtf8 "KICK",
                                   ircMessageParams =
                                     [channelName channel, encodeUtf8 target],
                                   ircMessageCoda =
                                     if T.null rest
                                     then Nothing
                                     else Just $ encodeUtf8 rest }
        sendIRCMessageToChannel channel message
    _ -> displayMessage client clientTab "* Syntax: /kick target [message]"

-- | Handle the /nick command.
handleNickCommand :: Client -> ClientTab -> T.Text -> IO ()
handleNickCommand client clientTab text =
  case parseCommandField text of
    Just (newNick, rest)
      | rest == "" ->
        handleCommandWithReadySession client clientTab $ \session -> do
          let message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "NICK",
                                     ircMessageParams = [encodeUtf8 newNick],
                                     ircMessageCoda = Nothing }
          sendIRCMessageToSession session message
    _ -> displayMessage client clientTab "* Syntax: /nick new-nick"

-- | Handle /me command.
handleMeCommand :: Client -> ClientTab -> T.Text -> IO ()
handleMeCommand client clientTab text = do
  session <- atomically $ getSessionForTab clientTab
  case session of
    Just session -> do
      nick <- atomically . readTVar $ sessionNick session
      handleCommandWithReadyChannelOrUser client clientTab (handleChannel nick)
        (handleUser nick)
    Nothing -> displayMessage client clientTab
               "* Command must be executed in channel or user tab"
  where handleChannel nick channel = do
          displayChannelMessage client channel . T.pack $
            printf "* %s %s" (stripText $ ourDecodeUtf8 nick) text
          let message = formatCtcpRequest (channelName channel)
                        (encodeUtf8 "ACTION") (Just $ encodeUtf8 text)
          sendIRCMessageToChannel channel message
        handleUser nick user = do
          displayUserMessage client user . T.pack $
            printf "* %s %s" (stripText $ ourDecodeUtf8 nick) text
          nick' <- atomically . readTVar $ userNick user
          let message = formatCtcpRequest nick' (encodeUtf8 "ACTION")
                        (Just $ encodeUtf8 text)
          sendIRCMessageToUser user message

-- | Handle /ping command.
handlePingCommand :: Client -> ClientTab -> T.Text -> IO ()
handlePingCommand client clientTab text =
  case parseCommandField text of
    Just (target, rest)
      | rest == "" -> handlePingCommandWithTarget $ encodeUtf8 target
      | otherwise -> displayMessage client clientTab "* Syntax: /ping [target]"
    Nothing -> handlePingCommandWithoutTarget
  where handlePingCommandWithTarget target = do
          handleCommandWithReadySession client clientTab $ \session -> do
            sendIRCMessageToSession session =<< formatPing target
        handlePingCommandWithoutTarget =
          handleCommandWithReadyChannelOrUser client clientTab handleChannel
          handleUser
        handleChannel channel = do
          message <- formatPing (channelName channel)
          sendIRCMessageToChannel channel message
        handleUser user = do
          nick <- atomically . readTVar $ userNick user
          sendIRCMessageToUser user =<< formatPing nick
        formatPing target =
          formatCtcpRequest target (encodeUtf8 "PING") . Just . encodeUtf8 .
          T.pack . printf "%d" . toNanoSecs <$> getTime Monotonic

-- | Handle /reconnect command.
handleReconnectCommand :: Client -> ClientTab -> T.Text -> IO ()
handleReconnectCommand client clientTab text =
  case parseCommandField text of
    Nothing -> do
      session <- atomically $ getSessionForTab clientTab
      case session of
        Just session -> do
          displaySessionMessageAll client session "* Attempting to reconnect..."
          markChannelsAsAwaitingReconnect client session
          atomically $ writeTVar (sessionReconnectOnFailure session) False
          reconnectSession client session WithoutDelay
        Nothing -> displayMessage client clientTab
                   "* Tab has no associated session"
    _ -> displayMessage client clientTab "* Syntax: /reconnect"

-- | Handle /whois command.
handleWhoisCommand :: Client -> ClientTab -> T.Text -> IO ()
handleWhoisCommand client clientTab text =
  case parseCommandField text of
    Just (target, rest)
      | rest == "" ->
        handleCommandWithReadySession client clientTab $ \session ->
          let message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "WHOIS",
                                     ircMessageParams =
                                       S.singleton $ encodeUtf8 target,
                                     ircMessageCoda = Nothing }
          in sendIRCMessageToSession session message
    _ -> displayMessage client clientTab "* Syntax: /whois target"

-- | Handle /whowas command.
handleWhowasCommand :: Client -> ClientTab -> T.Text -> IO ()
handleWhowasCommand client clientTab text =
  case parseCommandField text of
    Just (target, rest)
      | rest == "" ->
        handleCommandWithReadySession client clientTab $ \session ->
          let message = IRCMessage { ircMessagePrefix = Nothing,
                                     ircMessageCommand = encodeUtf8 "WHOWAS",
                                     ircMessageParams =
                                       S.singleton $ encodeUtf8 target,
                                     ircMessageCoda = Nothing }
          in sendIRCMessageToSession session message
    _ -> displayMessage client clientTab "* Syntax: /whowas target"

-- | Handle /ignore command
handleIgnoreCommand :: Client -> ClientTab -> T.Text -> IO ()
handleIgnoreCommand client clientTab text =
  case parseCommandField text of
    Just (mask, rest) ->
      case parseCommandField rest of
        Just (field, rest')
          | T.toLower field == "all" && rest' == "" -> do
              response <- atomically $ updateIgnoreList
                          (clientIgnoreList client) (encodeUtf8 mask)
                          [UserEventAll]
              handleIgnoreResponse response . T.pack $
                printf "* Ignored ALL for %s" mask
          | T.toLower field == "none" && rest' == "" -> do
              response <- atomically $ updateIgnoreList
                          (clientIgnoreList client) (encodeUtf8 mask) []
              handleIgnoreResponse response . T.pack $
                printf "* Unignored %s" mask
          | otherwise ->
            case parseUserEventTypes rest S.empty of
              Just userEventTypes -> do
                response <- atomically $ updateIgnoreList
                            (clientIgnoreList client) (encodeUtf8 mask)
                            userEventTypes
                handleIgnoreResponse response . T.pack $
                  printf "* Ignored %s for %s"
                  (T.intercalate ", " . toList $
                   fmap textOfUserEventType userEventTypes) mask
              Nothing -> displayIgnoreHelp
        Nothing -> displayIgnoreHelp
    Nothing -> do
      response <- atomically . getIgnoreListEntries $ clientIgnoreList client
      result <- atomically $ getResponse response
      case result of
        Right entries -> do
          displayMessage client clientTab "* Ignore list:"
          forM_ entries $ \entry -> displayIgnore entry
        Left (Error errorText) -> displayError errorText
  where handleIgnoreResponse response successMessage = do
          result <- atomically $ getResponse response
          case result of
            Right () ->
              displayMessage client clientTab successMessage
            Left (Error errorText) ->
              displayMessage client clientTab . T.pack $
              printf "* Unable to update ignore list: %s" errorText
        parseUserEventTypes text userEventTypes =
          case parseCommandField text of
            Just (field, rest) ->
              case T.toLower field of
                field
                  | field == "chan" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventChannel)
                  | field == "priv" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventPrivate)
                  | field == "noti" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventNotice)
                  | field == "ctcp" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventCtcp)
                  | field == "dcc" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventDcc)
                  | field == "invi" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventInvite)
                  | field == "stat" ->
                    parseUserEventTypes rest
                    (userEventTypes |> UserEventStatus)
                  | otherwise -> Nothing
            Nothing -> Just userEventTypes
        displayIgnore (mask, userEventTypes) = do
          displayMessage client clientTab $
            T.intercalate " " . toList $
            (T.pack $ printf "*   %s" (ourDecodeUtf8 mask)) <|
            fmap textOfUserEventType userEventTypes
        displayIgnoreHelp = do
          displayMessage client clientTab
            ("* Syntax: /ignore mask ([CHAN] [PRIV] [NOTI] [CTCP] " <>
             "[STAT] | [ALL] | [NONE])")
        textOfUserEventType UserEventChannel = "CHAN"
        textOfUserEventType UserEventPrivate = "PRIV"
        textOfUserEventType UserEventNotice = "NOTI"
        textOfUserEventType UserEventCtcp = "CTCP"
        textOfUserEventType UserEventDcc = "DCC"
        textOfUserEventType UserEventInvite = "INVI"
        textOfUserEventType UserEventStatus = "STAT"
        textOfUserEventType UserEventAll = "ALL"

-- | Handle /unignore command.
handleUnignoreCommand :: Client -> ClientTab -> T.Text -> IO ()
handleUnignoreCommand client clientTab text =
  case parseCommandField text of
    Just (mask, rest)
      | rest == "" -> do
          response <- atomically $ updateIgnoreList
                      (clientIgnoreList client) (encodeUtf8 mask) []
          handleIgnoreResponse response . T.pack $
            printf "* Unignored %s" mask
    _ -> displayMessage client clientTab "* Syntax: /unignore mask"
  where handleIgnoreResponse response successMessage = do
          result <- atomically $ getResponse response
          case result of
            Right () ->
              displayMessage client clientTab successMessage
            Left (Error errorText) ->
              displayMessage client clientTab . T.pack $
              printf "* Unable to update ignore list: %s" errorText

-- | Handle command for tab that requires a ready session.
handleCommandWithReadySession :: Client -> ClientTab -> (Session -> IO ()) ->
                                 IO ()
handleCommandWithReadySession client clientTab func = do
  session <- atomically $ getSessionForTab clientTab
  case session of
    Nothing -> displayMessage client clientTab "* Tab has no associated session"
    Just session -> do
      state <- atomically . readTVar $ sessionState session
      case state of
        SessionReady -> func session
        SessionPreparing ->
          displayMessage client clientTab "* Session is not ready"
        SessionConnecting ->
          displayMessage client clientTab "* Session is not connected"
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"

-- | Handle client for tab that requires a channel tab.
handleCommandWithReadyChannel :: Client -> ClientTab -> (Channel -> IO ()) ->
                                 IO ()
handleCommandWithReadyChannel client clientTab func = do
  subtype <- atomically . readTVar $ clientTabSubtype clientTab
  case subtype of
    ChannelTab channel -> do
      state <- atomically . readTVar . sessionState $ channelSession channel
      case state of
        SessionReady -> do
          state <- atomically . readTVar $ channelState channel
          case state of
            InChannel -> func channel
            NotInChannel -> displayMessage client clientTab "* Not in channel"
            AwaitingReconnect -> displayMessage client clientTab
                                 "* Awaiting reconnection..."
        SessionPreparing ->
          displayMessage client clientTab "* Session is not ready"
        SessionConnecting ->
          displayMessage client clientTab "* Session is not connected"
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"
    SessionTab _ -> displayMessage client clientTab
                    "* Command must be executed in channel tab"
    UserTab _ -> displayMessage client clientTab
                 "* Command must be executed in channel tab"
    FreeTab -> displayMessage client clientTab
               "* Command must be executed in channel tab"

-- | Handle client for tab that requires a channel or user tab.
handleCommandWithReadyChannelOrUser :: Client -> ClientTab ->
                                       (Channel -> IO ()) ->
                                       (User -> IO ()) ->
                                       IO ()
handleCommandWithReadyChannelOrUser client clientTab channelFunc userFunc = do
  subtype <- atomically . readTVar $ clientTabSubtype clientTab
  case subtype of
    ChannelTab channel -> do
      state <- atomically . readTVar . sessionState $ channelSession channel
      case state of
        SessionReady -> do
          state <- atomically . readTVar $ channelState channel
          case state of
            InChannel -> channelFunc channel
            NotInChannel -> displayMessage client clientTab "* Not in channel"
            AwaitingReconnect -> displayMessage client clientTab
                                 "* Awaiting reconnection..."
        SessionPreparing ->
          displayMessage client clientTab "* Session is not ready"
        SessionConnecting ->
          displayMessage client clientTab "* Session is not connected"
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"
    UserTab user -> do
      state <- atomically . readTVar . sessionState $ userSession user
      case state of
        SessionReady -> userFunc user
        SessionPreparing ->
          displayMessage client clientTab "* Session is not ready"
        SessionConnecting ->
          displayMessage client clientTab "* Session is not connected"
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"
    SessionTab _ -> displayMessage client clientTab
                    "* Command must be executed in channel or user tab"
    FreeTab -> displayMessage client clientTab
               "* Command must be executed in channel or user tab"

-- | Handle client for tab that requires a user tab.
handleCommandWithReadyUser :: Client -> ClientTab -> (User -> IO ()) -> IO ()
handleCommandWithReadyUser client clientTab userFunc = do
  subtype <- atomically . readTVar $ clientTabSubtype clientTab
  case subtype of
    ChannelTab channel ->
      displayMessage client clientTab "* Command must be executed in user tab"
    UserTab user -> do
      state <- atomically . readTVar . sessionState $ userSession user
      case state of
        SessionReady -> userFunc user
        SessionPreparing ->
          displayMessage client clientTab "* Session is not ready"
        SessionConnecting ->
          displayMessage client clientTab "* Session is not connected"
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"
    SessionTab _ ->
      displayMessage client clientTab "* Command must be executed in user tab"
    FreeTab ->
      displayMessage client clientTab "* Command must be executed in user tab"

-- | Get session for tab.
getSessionForTab :: ClientTab -> STM (Maybe Session)
getSessionForTab clientTab = do
  subtype <- readTVar $ clientTabSubtype clientTab
  case subtype of
    FreeTab -> return Nothing
    SessionTab session -> return $ Just session
    ChannelTab channel -> return . Just $ channelSession channel
    UserTab user -> return . Just $ userSession user

-- | Reuse a session.
reuseSession :: Client -> Session -> B.ByteString -> B.ByteString ->
                B.ByteString -> IO ()
reuseSession client session nick username realName = do
  atomically $ do
    writeTVar (sessionNick session) nick
    writeTVar (sessionUsername session) username
    writeTVar (sessionRealName session) realName
    hostname <- readTVar $ sessionOrigHostname session
    port <- readTVar $ sessionPort session
    (connectIRC (sessionIRCConnection session) hostname port) >> return ()
    writeTVar (sessionState session) SessionConnecting

-- | Create a new session with a tab.
createSessionWithNewTab :: Client -> NS.HostName -> NS.PortNumber ->
                           B.ByteString -> B.ByteString -> B.ByteString ->
                           IO (Either Error (Session, ClientTab))
createSessionWithNewTab client hostname port nick username realName = do
  window <- atomically $ findMostRecentWindow client
  case window of
    Just window -> do
      newClientTab <- openClientTab client window (T.pack hostname)
      case newClientTab of
        Right newClientTab -> do
          session <- createSessionInTab client newClientTab hostname port nick
                     username realName
          case session of
            Right session -> return $ Right (session, newClientTab)
            Left failure -> return $ Left failure
        Left failure -> return $ Left failure
    Nothing -> return . Left $ Error "no window is open"

-- | Create a new session in a tab.
createSessionInTab :: Client -> ClientTab -> NS.HostName -> NS.PortNumber ->
                      B.ByteString -> B.ByteString -> B.ByteString ->
                      IO (Either Error Session)
createSessionInTab client clientTab hostname port nick username realName = do
  session <- createSession client hostname port nick username realName
  case session of
    Right session -> do
      atomically . writeTVar (clientTabSubtype clientTab) $
        SessionTab session
      (atomically . setTabTitle (clientTabTab clientTab) $ T.pack hostname)
        >> return ()
      populateHistory session Nothing (clientTabHistory clientTab)
      updateNickForAllSessionTabs client session
      return $ Right session
    Left failure -> return $ Left failure

-- | Create a new session.
createSession :: Client -> NS.HostName -> NS.PortNumber -> B.ByteString ->
                 B.ByteString -> B.ByteString -> IO (Either Error Session)
createSession client hostname port nick username realName = do
  (ircConnection, sub) <- atomically $ do
    ircConnection <- newIRCConnection
    sub <- subscribeIRCConnection ircConnection
    return (ircConnection, sub)
  result <- startIRCConnection ircConnection
  case result of
    Right () -> do
      atomically $ do
        index <- getNextClientIndex client
        state <- newTVar SessionConnecting
        reconnectOnFailure <- newTVar False
        hostname' <- newTVar hostname
        origHostname <- newTVar hostname
        port' <- newTVar port
        nick' <- newTVar nick
        username' <- newTVar username
        realName' <- newTVar realName
        mode <- newTVar []
        channels <- newTVar []
        users <- newTVar []
        reconnectingLock <- newTMVar ()
        reconnecting <- newTVar Nothing
        pinging <- newTVar Nothing
        pongCount <- newTVar 0
        let session = Session { sessionIndex = index,
                                sessionState = state,
                                sessionReconnectOnFailure = reconnectOnFailure,
                                sessionIRCConnection = ircConnection,
                                sessionIRCConnectionEventSub = sub,
                                sessionHostname = hostname',
                                sessionOrigHostname = origHostname,
                                sessionPort = port',
                                sessionNick = nick',
                                sessionUsername = username',
                                sessionRealName = realName',
                                sessionMode = mode,
                                sessionChannels = channels,
                                sessionUsers = users,
                                sessionReconnectingLock = reconnectingLock,
                                sessionReconnecting = reconnecting,
                                sessionPinging = pinging,
                                sessionPongCount = pongCount }
        ourUserIndex <- getNextClientIndex client
        ourUserNick <- newTVar nick
        ourUserType <- newTVar []
        settings <- readTVar $ clientSettings client
        ourUserLog <- newLog $ settingsInitialMaxLines settings
        let user = User { userIndex = ourUserIndex,
                          userSession = session,
                          userNick = ourUserNick,
                          userType = ourUserType,
                          userLog = ourUserLog }
        writeTVar users [user]
        sessions <- readTVar $ clientSessions client
        writeTVar (clientSessions client) $ sessions |> session
        (connectIRC ircConnection hostname port) >> return ()
        return $ Right session
    Left failure -> return $ Left failure

-- | Get number of tabs in window.
handleGetNumberOfTabsInWindow :: Client -> ClientWindow -> STM Int
handleGetNumberOfTabsInWindow client clientWindow = do
  tabs <- readTVar $ clientTabs client
  let index = clientWindowIndex clientWindow
  return $ foldl' (\count tab ->
                     if (clientWindowIndex $ clientTabWindow tab) == index
                     then count + 1
                     else count)
    0 tabs

-- | Handle unrecognized command.
handleUnrecognizedCommand :: Client -> ClientTab -> T.Text -> IO ()
handleUnrecognizedCommand client clientTab command = do
  displayMessage client clientTab . T.pack .
    printf "* Unrecognized command: %s" $ stripText command

-- | Parse a command field.
parseCommandField :: T.Text -> Maybe (T.Text, T.Text)
parseCommandField text =
  let (part0, part1) = T.breakOn " " $ T.stripStart text
  in if T.null part0
     then Nothing
     else Just (part0, T.stripStart part1)

-- | Parse multiple command fields.
parseCommandFields :: T.Text -> S.Seq T.Text
parseCommandFields text = parseCommandFields' text S.empty
  where parseCommandFields' text parts =
          let (part0, part1) = T.breakOn " " $ T.stripStart text
          in if T.null part0
             then parts
             else parseCommandFields' part1 $ parts |> part0

-- | Handle a topic entered event.
handleTopicEntered :: Client -> ClientTab -> T.Text -> IO ()
handleTopicEntered client clientTab text = do
  subtype <- atomically . readTVar $ clientTabSubtype clientTab
  case subtype of
    ChannelTab channel -> do
      state <- atomically . readTVar . sessionState $ channelSession channel
      case state of
        SessionReady -> do
          state <- atomically . readTVar $ channelState channel
          case state of
            InChannel -> do
              let topicMessage =
                    IRCMessage { ircMessagePrefix = Nothing,
                                 ircMessageCommand = encodeUtf8 "TOPIC",
                                 ircMessageParams =
                                   S.singleton $ channelName channel,
                                 ircMessageCoda = Just $ encodeUtf8 text }
              sendIRCMessageToChannel channel topicMessage
            NotInChannel -> displayMessage client clientTab "* Not in channel"
            AwaitingReconnect -> displayMessage client clientTab
                                 "* Awaiting reconnection..."
        SessionPreparing ->
          displayMessage client clientTab "* Session is not ready"
        SessionConnecting ->
          displayMessage client clientTab "* Session is not connected"
        SessionInactive ->
          displayMessage client clientTab "* Session is not active"
        SessionDestroyed ->
          displayMessage client clientTab "* Session has been destroyed"
    _ -> return ()

-- | Handle a tab selected event.
handleTabSelected :: Client -> ClientTab -> IO ()
handleTabSelected client clientTab = do
  atomically $ do
    writeTVar (clientTabSelectIndex clientTab) =<<
      getNextClientTabSelectIndex client
  resetNotification clientTab

-- | Close a session if no tabs are open for it.
cleanupSessionIfNoTabs :: Client -> Session -> IO ()
cleanupSessionIfNoTabs client session = do
  found <- atomically $ isTabOpenForSession client session
  if not found
    then do
      response <- atomically $ do
        state <- readTVar $ sessionState session
        response <-
          if state == SessionReady || state == SessionPreparing
          then do
            let message =
                  IRCMessage { ircMessagePrefix = Nothing,
                               ircMessageCommand = encodeUtf8 "QUIT",
                               ircMessageParams = S.empty,
                               ircMessageCoda = Nothing }
            Just <$> sendIRCMessage (sessionIRCConnection session) message
          else return Nothing
        (stopIRCConnection $ sessionIRCConnection session) >> return ()
        writeTVar (sessionState session) SessionDestroyed
        sessions <- readTVar $ clientSessions client
        writeTVar (clientSessions client) $
          S.filter (\session' ->
                      sessionIndex session /= sessionIndex session')
          sessions
        return response
      case response of
        Just response -> asyncHandleResponse response
        Nothing -> return ()
    else return ()

-- | Get whether there is a tab open for a session.
isTabOpenForSession :: Client -> Session -> STM Bool
isTabOpenForSession client session = do
  tabs <- readTVar $ clientTabs client
  foldM (\found tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            return $ case subtype of
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
      response <- atomically $ do
        let session = channelSession channel
        sessionState <- readTVar $ sessionState session
        response <- case sessionState of
          SessionReady -> do
            let message =
                  IRCMessage { ircMessagePrefix = Nothing,
                               ircMessageCommand = encodeUtf8 "PART",
                               ircMessageParams =
                                 S.singleton $ channelName channel,
                               ircMessageCoda = Nothing }
            Just <$> sendIRCMessage (sessionIRCConnection session) message
          _ -> return Nothing
        writeTVar (channelState channel) NotInChannel
        channels <- readTVar $ clientChannels client
        writeTVar (clientChannels client) $
          S.filter (\channel' ->
                      channelIndex channel /= channelIndex channel')
          channels
        channels <- readTVar $ sessionChannels session
        writeTVar (sessionChannels session) $
          S.filter (\channel' ->
                      channelIndex channel /= channelIndex channel')
          channels
        return response
      case response of
        Just response -> asyncHandleResponse response
        Nothing -> return ()
      response <- atomically . stopLog $ channelLog channel
      syncHandleResponse response
      cleanupSessionIfNoTabs client $ channelSession channel
    else return ()
    

-- | Get whether there is a tab open for a channel.
isTabOpenForChannel :: Client -> Channel -> STM Bool
isTabOpenForChannel client channel = do
  tabs <- readTVar $ clientTabs client
  foldM (\found tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            return $ case subtype of
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
        users <- readTVar $ sessionUsers session
        writeTVar (sessionUsers session) $
          S.filter (\user' -> index /= userIndex user') users
      response <- atomically . stopLog $ userLog user
      syncHandleResponse response
      cleanupSessionIfNoTabs client $ userSession user
    else return ()

-- | Get whether there is a tab open for a user.
isTabOpenForUser :: Client -> User -> STM Bool
isTabOpenForUser client user = do
  tabs <- readTVar $ clientTabs client
  foldM (\found tab -> do
            subtype <- readTVar $ clientTabSubtype tab
            return $ case subtype of
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
  ourNick <- readTVar . sessionNick $ channelSession channel
  userNick' <- readTVar $ userNick user
  if ourNick /= userNick'
    then do
      users <- readTVar $ channelUsers channel
      return $ foldl' (\found user' ->
                         (userIndex user == userIndex user') || found)
        False users
    else do
      state <- readTVar $ channelState channel
      return $ state == InChannel || state == AwaitingReconnect

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
                         readTVar $ clientTabSelectIndex tab
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
      hostname <- readTVar $ sessionOrigHostname session
      port <- readTVar $ sessionPort session
      settings <- readTVar $ clientSettings client
      log <- newLog $ settingsInitialMaxLines settings
      let user = User { userIndex = index,
                        userSession = session,
                        userNick = nick',
                        userType = type',
                        userLog = log }
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
      topic <- newTVar Nothing
      mode <- newTVar S.empty
      hostname <- readTVar $ sessionOrigHostname session
      port <- readTVar $ sessionPort session
      settings <- readTVar $ clientSettings client
      log <- newLog $ settingsInitialMaxLines settings
      let channel = Channel { channelIndex = index,
                              channelSession = session,
                              channelState = state,
                              channelName = name,
                              channelUsers = users,
                              channelTopic = topic,
                              channelMode = mode,
                              channelLog = log }
      channels <- readTVar $ clientChannels client
      writeTVar (clientChannels client) $ channels |> channel
      channels <- readTVar $ sessionChannels session
      writeTVar (sessionChannels session) $ channels |> channel
      return channel

-- | Send an IRC message for a session.
sendIRCMessageToSession :: Session -> IRCMessage -> IO ()
sendIRCMessageToSession session message = do
  response <- atomically $ sendIRCMessage (sessionIRCConnection session) message
  asyncHandleResponse response

-- | Send an IRC message for a channel.
sendIRCMessageToChannel :: Channel -> IRCMessage -> IO ()
sendIRCMessageToChannel channel message =
  sendIRCMessageToSession (channelSession channel) message

-- | Send an IRC message for a user.
sendIRCMessageToUser :: User -> IRCMessage -> IO ()
sendIRCMessageToUser user message =
  sendIRCMessageToSession (userSession user) message

-- | Set user type for channel.
setUserTypeForChannel :: User -> Channel -> S.Seq UserType -> STM ()
setUserTypeForChannel user channel userType' = do
  userTypes <- readTVar $ userType user
  let index = channelIndex channel
  case S.findIndexL (\(channel', _) -> channelIndex channel' == index)
       userTypes of
    Just userTypeIndex -> do
      writeTVar (userType user) $
        S.update userTypeIndex (channel, userType') userTypes
    Nothing -> writeTVar (userType user) $ userTypes |> (channel, userType')

-- | Update channel tabs for user.
updateChannelTabsForUser :: Client -> Channel -> User -> IO ()
updateChannelTabsForUser client channel user = do
  tabs <- atomically $ findChannelTabsForChannel client channel
  nick <- atomically . readTVar $ userNick user
  forM_ tabs $ \tab -> do
    response <- atomically . addCompletion (clientTabTab tab) $
                ourDecodeUtf8 nick
    asyncHandleResponse response
    response <- atomically $ findTabUser (clientTabTab tab) nick
    result <- atomically $ getResponse response
    case result of
      Right (Just tabUser) -> do
        response <- atomically $ removeTabUser tabUser
        asyncHandleResponse response
        userType' <- atomically $ getSingleUserType user channel
        response <- atomically $ addTabUser (clientTabTab tab) nick userType'
        asyncHandleResponse response
      Right Nothing -> do
        userType' <- atomically $ getSingleUserType user channel
        response <- atomically $ addTabUser (clientTabTab tab) nick userType'
        asyncHandleResponse response
      Left (Error errorText) -> displayError errorText
  ourNick <- atomically . readTVar . sessionNick $ channelSession channel
  if nick == ourNick
    then updateNickForAllSessionTabs client (channelSession channel)
    else return ()

-- | Update all channel tabs for user.
updateAllChannelTabsForUser :: Client -> User -> IO ()
updateAllChannelTabsForUser client user = do
  channels <- atomically $ findAllChannelsForUser client user
  forM_ channels $ \channel -> updateChannelTabsForUser client channel user

-- | Get a single user type for a user
getSingleUserType :: User -> Channel -> STM UserType
getSingleUserType user channel = do
  condenseUserType . findUserTypeForChannel <$> (readTVar $ userType user)
  where findUserTypeForChannel userTypes =
          case S.viewl userTypes of
            (channel', userType') :< rest
              | channelIndex channel == channelIndex channel' -> userType'
              | otherwise -> findUserTypeForChannel rest
            S.EmptyL -> S.empty
        condenseUserType userType' =
          if S.elemIndexL OwnerUser userType' /= Nothing then OwnerUser
          else if S.elemIndexL AdminUser userType' /= Nothing then AdminUser
          else if S.elemIndexL OpUser userType' /= Nothing then OpUser
          else if S.elemIndexL HalfOpUser userType' /= Nothing then HalfOpUser
          else if S.elemIndexL VoiceUser userType' /= Nothing then VoiceUser
          else NormalUser

-- | Remove user from channel tabs.
removeUserFromChannelTabs :: Client -> Channel -> User -> IO ()
removeUserFromChannelTabs client channel user = do
  tabs <- atomically $ findChannelTabsForChannel client channel
  nick <- atomically . readTVar $ userNick user
  forM_ tabs $ \tab -> do
    response <- atomically . removeCompletion (clientTabTab tab) $
                ourDecodeUtf8 nick
    asyncHandleResponse response
    response <- atomically $ findTabUser (clientTabTab tab) nick
    result <- atomically $ getResponse response
    case result of
      Right (Just tabUser) -> do
        response <- atomically $ removeTabUser tabUser
        asyncHandleResponse response
      Right Nothing -> return ()
      Left (Error errorText) -> displayError errorText

-- | Remove user from all channel tabs.
removeUserFromAllChannelTabs :: Client -> User -> IO ()
removeUserFromAllChannelTabs client user = do
  channels <- atomically $ findAllChannelsForUser client user
  forM_ channels $ \channel -> removeUserFromChannelTabs client channel user

-- | Remove all users from channel tabs.
removeAllUsersFromChannelTabs :: Client -> Channel -> IO ()
removeAllUsersFromChannelTabs client channel = do
  users <- atomically . readTVar $ channelUsers channel
  forM_ users $ \user -> removeUserFromChannelTabs client channel user

-- | Update tab title for all user tabs.
updateTabTitleForAllUserTabs :: Client -> User -> IO ()
updateTabTitleForAllUserTabs client user = do
  nick <- ourDecodeUtf8 <$> (atomically . readTVar $ userNick user)
  tabs <- atomically $ findUserTabsForUser client user
  forM_ tabs $ \tab -> do
    response <- atomically $ setTabTitle (clientTabTab tab) nick
    asyncHandleResponse response

-- | Find all channels user is in.
findAllChannelsForUser :: Client -> User -> STM (S.Seq Channel)
findAllChannelsForUser client user = do
  foldM filterChannelsForUser S.empty =<< (readTVar $ clientChannels client)
  where filterChannelsForUser acc channel = do
          users <- readTVar $ channelUsers channel
          return $ case S.findIndexL matchUser users of
            Just _ -> acc |> channel
            Nothing -> acc
        matchUser user' = userIndex user == userIndex user'

-- | Check whether text is for CTCP message, and if so, parse it.
parseCtcp :: B.ByteString -> Maybe (B.ByteString, Maybe B.ByteString)
parseCtcp message =
  case B.uncons message of
    Just (start, rest)
      | start == 1 ->
        case B.unsnoc rest of
          Just (rest, end)
            | end == 1 ->
              let (command, rest') = B.breakSubstring (encodeUtf8 " ") rest
              in case B.uncons rest' of
                Just (sep, rest)
                  | sep == byteOfChar ' ' -> Just (command, Just rest)
                _ -> Just (command, Nothing)
          _ -> Nothing
    _ -> Nothing

-- | Format a CTCP request message.
formatCtcpRequest :: B.ByteString -> B.ByteString -> Maybe B.ByteString ->
                     IRCMessage
formatCtcpRequest target command (Just param) =
  IRCMessage { ircMessagePrefix = Nothing,
               ircMessageCommand = encodeUtf8 "PRIVMSG",
               ircMessageParams = S.singleton target,
               ircMessageCoda = Just $ B.concat [B.singleton 1,
                                                 command,
                                                 B.singleton $ byteOfChar ' ',
                                                 param,
                                                 B.singleton 1] }
formatCtcpRequest target command Nothing =
  IRCMessage { ircMessagePrefix = Nothing,
               ircMessageCommand = encodeUtf8 "PRIVMSG",
               ircMessageParams = S.singleton target,
               ircMessageCoda = Just $ B.concat [B.singleton 1,
                                                 command,
                                                 B.singleton 1] }

-- | Format a CTCP reply message.
formatCtcpReply :: B.ByteString -> B.ByteString -> Maybe B.ByteString ->
                     IRCMessage
formatCtcpReply target command (Just param) =
  IRCMessage { ircMessagePrefix = Nothing,
               ircMessageCommand = encodeUtf8 "NOTICE",
               ircMessageParams = S.singleton target,
               ircMessageCoda = Just $ B.concat [B.singleton 1,
                                                 command,
                                                 B.singleton $ byteOfChar ' ',
                                                 param,
                                                 B.singleton 1] }
formatCtcpReply target command Nothing =
  IRCMessage { ircMessagePrefix = Nothing,
               ircMessageCommand = encodeUtf8 "NOTICE",
               ircMessageParams = S.singleton target,
               ircMessageCoda = Just $ B.concat [B.singleton 1,
                                                 command,
                                                 B.singleton 1] }

-- | Populate a client tab from a log.
populateTabFromLog :: Session -> ClientTab -> B.ByteString -> Log -> IO ()
populateTabFromLog session clientTab nickOrName log = do
  running <- atomically $ getLogRunning log
  result <-
    if not running
    then startLog log
    else return $ Right ()
  case result of
    Right () -> do
      hostname <- atomically . readTVar $ sessionOrigHostname session
      port <- atomically . readTVar $ sessionPort session
      response <- atomically $ getLogLoaded log
      result <- atomically $ getResponse response
      case result of
        Right False -> do
          response <- atomically $ loadLog log hostname port nickOrName
          asyncHandleResponse response
        Right True -> return ()
        Left (Error errorText) -> displayError errorText
      response <- atomically $ readLog log
      result <- atomically $ getResponse response
      case result of
        Right text -> do
          response <- atomically $ addTabText (clientTabTab clientTab) text
          asyncHandleResponse response
        Left (Error errorText) -> displayError errorText
    Left (Error errorText) -> displayError errorText

-- | Populate history.
populateHistory :: Session -> Maybe B.ByteString -> History -> IO ()
populateHistory session nickOrName history = do
  origHostname <- atomically . readTVar $ sessionOrigHostname session
  port <- atomically . readTVar $ sessionPort session
  response <- atomically $ loadHistory history origHostname port nickOrName
  asyncHandleResponse response

-- | Mark channels as awaiting reconnect.
markChannelsAsAwaitingReconnect :: Client -> Session -> IO ()
markChannelsAsAwaitingReconnect client session = do
  channels <- atomically $ do
    channels <- readTVar $ sessionChannels session
    forM_ channels $ \channel -> do
      state <- readTVar $ channelState channel
      case state of
        InChannel -> writeTVar (channelState channel) AwaitingReconnect
        _ -> return ()
    return channels
  forM_ channels $ \channel -> removeAllUsersFromChannel client channel
    
-- | Get nick or name for client tab.
getTabNickOrName :: ClientTab -> STM (Maybe B.ByteString)
getTabNickOrName clientTab = do
  subtype <- readTVar $ clientTabSubtype clientTab
  case subtype of
    ChannelTab channel -> return . Just $ channelName channel
    UserTab user -> Just <$> readTVar (userNick user)
    SessionTab _ -> return Nothing
    FreeTab -> return Nothing

-- | Update nick for all session tabs.
updateNickForAllSessionTabs :: Client -> Session -> IO ()
updateNickForAllSessionTabs client session = do
  tabs <- atomically $ findAllTabsForSession client session
  nick <- atomically . readTVar $ sessionNick session
  user <- atomically $ findUserByNick session nick
  case user of
    Just user -> do
      forM_ tabs $ \tab -> do
        subtype <- atomically . readTVar $ clientTabSubtype tab
        userType' <-
          case subtype of
            ChannelTab channel ->
              Just <$> (atomically $ getSingleUserType user channel)
            _ -> return Nothing
        response <-
          atomically $ setNick (clientTabTab tab) (Just (nick, userType'))
        asyncHandleResponse response
    Nothing -> return ()
