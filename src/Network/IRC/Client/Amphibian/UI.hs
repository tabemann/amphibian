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

module Network.IRC.Client.Amphibian.UI

  (Response(..),
   Error(..),
   Window,
   WindowState(..),
   WindowEvent(..),
   WindowEventSub,
   Tab,
   TabState(..),
   TabEvent(..),
   TabEventSub,
   TabUser,
   TabUserState(..),
   TabUserEvent(..),
   TabUserEventSub,
   UserType(..),
   withWindowing,
   newWindow,
   startWindow,
   stopWindow,
   openWindow,
   closeWindow,
   setWindowTitle,
   getWindowState,
   subscribeWindow,
   recvWindow,
   tryRecvWindow,
   openTab,
   closeTab,
   setTabTitle,
   setTabNotification,
   setTabTitleAndNotification,
   addTabText,
   setNick,
   setEntry,
   setTopicVisible,
   setTopic,
   setSideVisible,
   getTabState,
   subscribeTab,
   recvTab,
   tryRecvTab,
   addTabUser,
   removeTabUser,
   findTabUser,
   getTabUserState,
   subscribeTabUser,
   recvTabUser,
   tryRecvTabUser,
   stripText)

where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString as B
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import Data.Int (Int32)
import Data.Functor ((<$>))
import Data.Sequence ((|>),
                      (><),
                      ViewL((:<)))
import Data.Foldable (toList)
import Data.List (elemIndex)
import Control.Monad (forM_,
                      join)
import Data.Text.Encoding (encodeUtf8,
                           decodeUtf8)
import Control.Concurrent (forkOS,
                           threadDelay)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Concurrent.STM (STM,
                               atomically,
                               orElse,
                               TVar,
                               newTVar,
                               readTVar,
                               writeTVar)
import Control.Concurrent.STM.TChan (TChan,
                                     newBroadcastTChan,
                                     dupTChan,
                                     writeTChan,
                                     readTChan,
                                     tryReadTChan)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue,
                                      tryReadTQueue,
                                      isEmptyTQueue)
import Control.Concurrent.STM.TMVar (TMVar,
                                     newEmptyTMVar,
                                     putTMVar,
                                     tryTakeTMVar,
                                     tryReadTMVar,
                                     takeTMVar,
                                     readTMVar)
import Text.Printf (printf)
import System.IO.Unsafe (unsafePerformIO)

-- | Default window width.
defaultWindowWidth :: Int32
defaultWindowWidth = 800

-- | Default window height.
defaultWindowHeight :: Int32
defaultWindowHeight = 600

-- | Execute with windowing windowing.
withWindowing :: IO () -> IO ()
withWindowing action = do
  lock0 <- atomically $ newEmptyTMVar
  lock1 <- atomically $ newEmptyTMVar
  Gtk.init Nothing
  forkOS $ do
    atomically $ takeTMVar lock0
    action
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      Gtk.mainQuit
      return False
    atomically $ takeTMVar lock1
    return ()
  GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
    atomically $ putTMVar lock0 ()
    return False
  Gtk.main
  atomically $ putTMVar lock1 ()

-- | Create a new window.
newWindow :: STM Window
newWindow = do
  window <- newEmptyTMVar
  notebook <- newEmptyTMVar
  title <- newEmptyTMVar
  tabs <- newTVar S.empty
  nextTabIndex <- newTVar 0
  state <- newTVar WindowNotStarted
  actionQueue <- newTQueue
  eventQueue <- newBroadcastTChan
  return $ Window { windowWindow = window,
                    windowNotebook = notebook,
                    windowTitle = title,
                    windowTabs = tabs,
                    windowNextTabIndex = nextTabIndex,
                    windowState = state,
                    windowActionQueue = actionQueue,
                    windowEventQueue = eventQueue }
-- | Start a window.
startWindow :: Window -> IO (Either Error ())
startWindow window = do
  alreadyRunning <- atomically $ do
    state <- readTVar $ windowState window
    if state == WindowNotStarted
      then do writeTVar (windowState window) WindowNotShown
              tryTakeTMVar (windowWindow window) >> return ()
              tryTakeTMVar (windowNotebook window) >> return ()
              tryTakeTMVar (windowTitle window) >> return ()
              return False
      else return True
  if not alreadyRunning
    then do async $ runWindow window
            return $ Right ()
    else return . Left $ Error "window already started"

-- | Stop a window.
stopWindow :: Window -> STM (Response ())
stopWindow window = do
  state <- readTVar $ windowState window
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue window) (StopWindow $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Open a window.
openWindow :: Window -> T.Text -> STM (Response ())
openWindow window title = do
  state <- readTVar $ windowState window
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue window)
         (OpenWindow title $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Close a window.
closeWindow :: Window -> STM (Response ())
closeWindow window = do
  state <- readTVar $ windowState window
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue window)
         (CloseWindow $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the window title.
setWindowTitle :: Window -> T.Text -> STM (Response ())
setWindowTitle window title = do
  state <- readTVar $ windowState window
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue window)
         (SetWindowTitle title $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Get window state.
getWindowState :: Window -> STM WindowState
getWindowState = readTVar . windowState

-- | Subscribe to a window.
subscribeWindow :: Window -> STM WindowEventSub
subscribeWindow window = WindowEventSub <$> dupTChan (windowEventQueue window)

-- | Receive an event from a window.
recvWindow :: WindowEventSub -> STM WindowEvent
recvWindow (WindowEventSub sub) = readTChan sub

-- | Try to receive an event from a window.
tryRecvWindow :: WindowEventSub -> STM (Maybe WindowEvent)
tryRecvWindow (WindowEventSub sub) = tryReadTChan sub

-- | Open a tab on a window.
openTab :: Window -> T.Text -> Notification -> STM (Response Tab)
openTab window title notification = do
  state <- readTVar $ windowState window
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue window)
         (OpenTab title notification $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Close a tab on a window.
closeTab :: Tab -> STM (Response ())
closeTab tab = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (CloseTab tab $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the title of a tab.
setTabTitle :: Tab -> T.Text -> STM (Response ())
setTabTitle tab title = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetTabTitle tab title $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the notification of a tab.
setTabNotification :: Tab -> Notification -> STM (Response ())
setTabNotification tab notification = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetTabNotification tab notification $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the title and notification of a tab.
setTabTitleAndNotification :: Tab -> T.Text -> Notification -> STM (Response ())
setTabTitleAndNotification tab title notification = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetTabTitleAndNotification tab title notification $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Add text to a tab.
addTabText :: Tab -> T.Text -> STM (Response ())
addTabText tab text = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (AddTabText tab text $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the nick of a tab.
setNick :: Tab -> Maybe (B.ByteString, Maybe UserType) -> STM (Response ())
setNick tab nickAndUserType = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetNick tab nickAndUserType $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the entered text of a tab.
setEntry :: Tab -> T.Text -> STM (Response ())
setEntry tab text = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetEntry tab text $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set whether the topic of a tab is visible.
setTopicVisible :: Tab -> Bool -> STM (Response ())
setTopicVisible tab visible = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetTopicVisible tab visible $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set the topic of a tab.
setTopic :: Tab -> T.Text -> STM (Response ())
setTopic tab topic = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetTopic tab topic $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Set whether the sidebar of a tab is visible.
setSideVisible :: Tab -> Bool -> STM (Response ())
setSideVisible tab visible = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (SetSideVisible tab visible $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Add a tab user.
addTabUser :: Tab -> B.ByteString -> UserType -> STM (Response TabUser)
addTabUser tab nick userType = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (AddTabUser tab nick userType $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Remove a tab user.
removeTabUser :: TabUser -> STM (Response ())
removeTabUser tabUser = do
  state <- readTVar . windowState . tabWindow $ tabUserTab tabUser
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue . tabWindow $ tabUserTab tabUser)
         (RemoveTabUser tabUser $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Find a tab user.
findTabUser :: Tab -> B.ByteString -> STM (Response (Maybe TabUser))
findTabUser tab nick = do
  state <- readTVar . windowState $ tabWindow tab
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue $ tabWindow tab)
         (FindTabUser tab nick $ Response response)
    else putTMVar response . Left $ Error "window not started"
  return $ Response response

-- | Get tab state.
getTabState :: Tab -> STM TabState
getTabState = readTVar . tabState

-- | Subscribe to a tab.
subscribeTab :: Tab -> STM TabEventSub
subscribeTab tab = TabEventSub <$> dupTChan (tabEventQueue tab)

-- | Receive an event from a tab.
recvTab :: TabEventSub -> STM TabEvent
recvTab (TabEventSub sub) = readTChan sub

-- | Try to receive an event from a tab.
tryRecvTab :: TabEventSub -> STM (Maybe TabEvent)
tryRecvTab (TabEventSub sub) = tryReadTChan sub

-- | Get tab user state.
getTabUserState :: TabUser -> STM TabUserState
getTabUserState = readTVar . tabUserState

-- | Subscribe to a tab user.
subscribeTabUser :: TabUser -> STM TabUserEventSub
subscribeTabUser tabUser =
  TabUserEventSub <$> dupTChan (tabUserEventQueue tabUser)

-- | Receive an event from a tab user.
recvTabUser :: TabUserEventSub -> STM TabUserEvent
recvTabUser (TabUserEventSub sub) = readTChan sub

-- | Try to receive an event from a tab user.
tryRecvTabUser :: TabUserEventSub -> STM (Maybe TabUserEvent)
tryRecvTabUser (TabUserEventSub sub) = tryReadTChan sub

-- | Run a window.
runWindow :: Window -> IO ()
runWindow window = do
  (action, state) <- atomically $ do
    action <- readTQueue $ windowActionQueue window
    state <- readTVar $ windowState window
    return (action, state)
  case action of
    OpenWindow title (Response response) -> do
      if state == WindowNotShown
        then do        
          lock <- atomically $ newEmptyTMVar
--          printf "*** OPENING WINDOW\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            actualWindow <- Gtk.windowNew Gtk.WindowTypeToplevel
            Gtk.windowSetDefaultSize actualWindow defaultWindowWidth
              defaultWindowHeight
            Gtk.setWindowTitle actualWindow title
            notebook <- Gtk.notebookNew
            Gtk.notebookSetScrollable notebook True
            atomically $ do
              putTMVar (windowWindow window) actualWindow
              putTMVar (windowNotebook window) notebook
              putTMVar (windowTitle window) title
              writeTVar (windowState window) WindowShown
            installEventHandlers window
            Gtk.containerAdd actualWindow notebook
            Gtk.widgetShowAll actualWindow
            atomically $ putTMVar lock ()
            return False
          atomically $ do
            takeTMVar lock
            putTMVar response $ Right ()
--          printf "*** DONE OPENING WINDOW\n"
        else atomically . putTMVar response . Left $ Error "window already open"
      runWindow window
    CloseWindow (Response response) -> do
      if state == WindowShown
        then do
          actuallyCloseWindow window
          atomically $ do
            writeTVar (windowState window) WindowNotShown
            putTMVar response $ Right ()
        else atomically . putTMVar response . Left $ Error "window not open"
      runWindow window
    StopWindow (Response response) -> do
      if state == WindowShown
        then actuallyCloseWindow window
        else return ()
      atomically $ do
        writeTVar (windowState window) WindowNotStarted
        putTMVar response $ Right ()
    OpenTab titleText titleStyle (Response response) -> do
      if state == WindowShown
        then do
          lock <- atomically $ newEmptyTMVar
--          printf "*** OPENING TAB\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            tab <- createTab window titleText titleStyle
            case tab of
              Just tab -> atomically . putTMVar response $ Right tab
              Nothing ->
                atomically . putTMVar response . Left $
                Error "unable to create tab"
            atomically $ putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE OPENING TAB\n"
        else atomically . putTMVar response . Left $ Error "window not open"
      runWindow window
    CloseTab tab (Response response) -> do
      if state == WindowShown
        then do
          (tabState', notebook) <- atomically $ do
            tabState' <- readTVar $ tabState tab
            notebook <- tryReadTMVar $ windowNotebook window
            return (tabState', notebook)
          case (tabState', notebook) of
            (TabIsOpen, Just notebook) -> do
              lock <- atomically $ newEmptyTMVar
--              printf "*** CLOSING TAB\n"
              Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                pageNum <- Gtk.notebookPageNum notebook $ tabBodyBox tab
                Gtk.notebookRemovePage notebook pageNum
                Gtk.widgetHide $ tabBodyBox tab
                Gtk.widgetHide $ tabTabBox tab
                atomically $ do
                  users <- readTVar $ tabUsers tab
                  forM_ users $ \user -> do
                    writeTVar (tabUserState user) TabUserIsClosed
                    writeTChan (tabUserEventQueue user) TabUserClosed
                  writeTVar (tabState tab) TabIsClosed
                  writeTChan (tabEventQueue tab) TabClosed
                  putTMVar response $ Right ()
                  putTMVar lock ()
                return False
              (atomically $ takeTMVar lock) >> return ()
--              printf "*** DONE CLOSING TAB\n"
            _ ->
              atomically . putTMVar response . Left $ Error "tab is closed"
        else atomically . putTMVar response . Left $ Error "window not open"
      runWindow window
    SetWindowTitle title (Response response) -> do
      if state == WindowShown
        then do
          actualWindow <- atomically . tryReadTMVar $ windowWindow window
          case actualWindow of
            Just actualWindow -> do
              lock <- atomically $ newEmptyTMVar
--              printf "*** SETTING WINDOW TITLE\n"
              Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                Gtk.setWindowTitle actualWindow title
                atomically $ do
                  (takeTMVar $ windowTitle window) >> return ()
                  putTMVar (windowTitle window) title
                  putTMVar response $ Right ()
                  putTMVar lock ()
                return False
              (atomically $ takeTMVar lock) >> return ()
--              printf "*** DONE SETTING WINDOW TITLE\n"
            Nothing -> atomically . putTMVar response . Left $
                       Error "could not find window"
        else atomically . putTMVar response . Left $ Error "window not open"
      runWindow window
    SetTabTitle tab title (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          atomically $ writeTVar (tabTitle tab) title
          updateTabTitle tab
          atomically . putTMVar response $ Right ()
        TabIsClosed -> atomically .  putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetTabNotification tab notification (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          atomically $ writeTVar (tabNotification tab) notification
          updateTabTitle tab
          atomically . putTMVar response $ Right ()
        TabIsClosed -> atomically .  putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetTabTitleAndNotification tab title notification (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          atomically $ do
            writeTVar (tabTitle tab) title
            writeTVar (tabNotification tab) notification
          updateTabTitle tab
          atomically . putTMVar response $ Right ()
        TabIsClosed -> atomically .  putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    AddTabText tab text (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** ADDING TAB TEXT\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            iter <- Gtk.textBufferGetStartIter $ tabTextBuffer tab
            Gtk.textIterForwardToEnd iter
            let text' = formatText text
            Gtk.textBufferInsertMarkup (tabTextBuffer tab) iter text'
              (fromIntegral . B.length $ encodeUtf8 text')
            Gtk.textViewScrollToMark (tabTextView tab) (tabTextMark tab)
              0.0 True 0.5 0.05
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE ADDING TAB TEXT\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetNick tab (Just (nick, userType)) (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** SETTING NICK\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            let prefix =
                  case userType of
                    Just userType -> userTypePrefix userType
                    Nothing -> ""
            Gtk.labelSetText (tabNickLabel tab) . T.pack $
              printf "%s%s" prefix (ourDecodeUtf8 nick)
            Gtk.widgetShow $ tabNickLabel tab
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE SETTING NICK\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetNick tab Nothing (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** SETTING NICK\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.widgetHide $ tabNickLabel tab
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE SETTING NICK\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetEntry tab text (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** SETTING ENTRY TEXT\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.entrySetText (tabEntry tab) text
            Gtk.editableSetPosition (tabEntry tab) . fromIntegral $
              T.length text
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE SETTING ENTRY TEXT\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetTopic tab text (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** SETTING TOPIC TEXT\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.entrySetText (tabTopicEntry tab) text
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE SETTING TOPIC TEXT\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetTopicVisible tab visible (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** SETTING TOPIC VISIBILITY\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            if visible
              then Gtk.widgetShow $ tabTopicEntry tab
              else Gtk.widgetHide $ tabTopicEntry tab
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE SETTING TOPIC VISIBILITY\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetSideVisible tab visible (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** SETTING SIDE VISIBILITY\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            if visible
              then Gtk.widgetShowAll $ tabSideBox tab
              else Gtk.widgetHide $ tabSideBox tab
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE SETTING SIDE VISIBILITY\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    AddTabUser tab nick userType (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** ADDING TAB USER\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            users <- atomically . readTVar $ tabUsers tab
            case S.findIndexL (\user -> tabUserNick user == nick) users of
              Nothing -> do
                nextUserIndex <- atomically $ do
                  nextUserIndex <- readTVar $ tabNextUserIndex tab
                  writeTVar (tabNextUserIndex tab) $ nextUserIndex + 1
                  return nextUserIndex
                label <- Gtk.labelNew . Just $
                  T.concat [userTypePrefix userType, decodeUtf8 nick]
                row <- Gtk.listBoxRowNew
                Gtk.containerAdd row label
                Gtk.widgetShowAll row
                state <- atomically $ newTVar TabUserIsOpen
                eventQueue <- atomically $ newBroadcastTChan
                let tabUser = TabUser { tabUserTab = tab,
                                        tabUserIndex = nextUserIndex,
                                        tabUserType = userType,
                                        tabUserNick = nick,
                                        tabUserLabel = label,
                                        tabUserRow = row,
                                        tabUserState = state,
                                        tabUserEventQueue = eventQueue }
                let insertIndex = findFirstLarger tabUser users
                atomically $
                  writeTVar (tabUsers tab)
                  (S.insertAt insertIndex tabUser users)
                Gtk.listBoxInsert (tabSideListBox tab) row
                  (fromIntegral insertIndex)
                atomically $ do
                  putTMVar response $ Right tabUser
                  putTMVar lock ()
              _ -> atomically . putTMVar response . Left $
                   Error "nick already in tab"
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE ADDING TAB USER\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    RemoveTabUser tabUser (Response response) -> do
      tabUserState' <- atomically . readTVar $ tabUserState tabUser
      case tabUserState' of
        TabUserIsOpen -> do
          lock <- atomically $ newEmptyTMVar
--          printf "*** REMOVING TAB USER\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            index <- atomically $ do
              users <- readTVar . tabUsers $ tabUserTab tabUser
              let index = S.elemIndexL tabUser users
              case index of
                Just index -> writeTVar (tabUsers $ tabUserTab tabUser) $
                              S.deleteAt index users
                Nothing -> return ()
              return index
            case index of
              Just index -> do
                Gtk.widgetHide $ tabUserRow tabUser
                Gtk.containerRemove (tabSideListBox $ tabUserTab tabUser) $
                  tabUserRow tabUser
              Nothing -> return ()
            atomically $ do
              writeTVar (tabUserState tabUser) TabUserIsClosed
              writeTChan (tabUserEventQueue tabUser) TabUserClosed
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
--          printf "*** DONE REMOVING TAB USER\n"
        TabUserIsClosed -> atomically . putTMVar response . Left $
                       Error "tab user is closed"
      runWindow window
    FindTabUser tab nick (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> atomically $ do
          users <- readTVar $ tabUsers tab
          case S.findIndexL (\user -> tabUserNick user == nick) users of
            Just index ->
              case S.lookup index users of
                Just user -> putTMVar response . Right $ Just user
                Nothing -> error "impossible"
            Nothing -> putTMVar response $ Right Nothing
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window       

-- | Find index of first item larger than item.
findFirstLarger :: Ord a => a -> S.Seq a -> Int
findFirstLarger x xs = findFirstLarger' x (S.viewl xs) 0
  where findFirstLarger' _ S.EmptyL i = i
        findFirstLarger' x (y :< ys) i
          | x < y = i
          | otherwise = findFirstLarger' x (S.viewl ys) (i + 1)

-- | Actually close a window.
actuallyCloseWindow :: Window -> IO ()
actuallyCloseWindow window = do
  (actualWindow, notebook) <- atomically $ do
    actualWindow <- tryReadTMVar $ windowWindow window
    notebook <- tryReadTMVar $ windowNotebook window
    return (actualWindow, notebook)
  case (actualWindow, notebook) of
    (Just actualWindow, Just notebook) -> do
      lock <- atomically $ newEmptyTMVar
      Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        tabs <- atomically . readTVar $ windowTabs window
        forM_ tabs $ \tab -> do
          pageNum <- Gtk.notebookPageNum notebook $ tabBodyBox tab
          Gtk.notebookRemovePage notebook pageNum
          Gtk.widgetHide $ tabBodyBox tab
          Gtk.widgetHide $ tabTabBox tab
        Gtk.widgetHide actualWindow
        atomically $ do
          (takeTMVar $ windowWindow window) >> return ()
          (takeTMVar $ windowNotebook window) >> return ()
          (takeTMVar $ windowTitle window) >> return ()
          forM_ tabs $ \tab -> do
            writeTVar (tabState tab) TabIsClosed
            writeTChan (tabEventQueue tab) TabClosed
          putTMVar lock ()
        return False
      atomically $ takeTMVar lock
    _ -> return ()
           
-- | Install event handlers for a window.
installEventHandlers :: Window -> IO ()
installEventHandlers window = do
  actualWindow <- atomically . tryReadTMVar $ windowWindow window
  notebook <- atomically . tryReadTMVar $ windowNotebook window
  case (actualWindow, notebook) of
    (Just actualWindow, Just notebook) -> do
      Gtk.onWidgetDestroy actualWindow $ do
        atomically $ do
          writeTVar (windowState window) WindowNotShown
          writeTChan (windowEventQueue window) WindowClosed
          tabs <- readTVar $ windowTabs window
          forM_ tabs $ \tab -> do
            users <- readTVar $ tabUsers tab
            forM_ users $ \user -> do
              writeTVar (tabUserState user) TabUserIsClosed
              writeTChan (tabUserEventQueue user) TabUserClosed
            writeTVar (tabState tab) TabIsClosed
            writeTChan (tabEventQueue tab) TabClosed
          writeTVar (windowTabs window) S.empty
      Gtk.onWidgetWindowStateEvent actualWindow $ \e -> do
        state <- Gdk.getEventWindowStateNewWindowState e
        if elemIndex Gdk.WindowStateFocused state /= Nothing
          then do
            page <- fromIntegral <$> Gtk.notebookGetCurrentPage notebook
            tabs <- atomically . readTVar $ windowTabs window
            if page >= 0 && page < S.length tabs
              then
                case S.lookup page tabs of
                  Just tab -> do
                    atomically $ do
                      writeTChan (windowEventQueue window) WindowFocused
                      writeTChan (tabEventQueue tab) TabSelected
                  Nothing -> return ()
              else return ()
          else return ()
        return False
      Gtk.onNotebookSwitchPage notebook $ \widget index -> do
        join . atomically $ do
          tab <- S.lookup (fromIntegral index) <$>
                 (readTVar $ windowTabs window)
          case tab of
            Just tab -> do
              writeTChan (tabEventQueue tab) TabSelected
              return $ do
                GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                  Gtk.widgetGrabFocus $ tabEntry tab
                  return False
                return ()
            Nothing -> return $ return ()
      return ()
    _ -> return ()

-- | Update tab title.
updateTabTitle :: Tab -> IO ()
updateTabTitle tab = do
--  printf "*** SETTING TAB TITLE\n"
  (text, style) <- atomically $ do
    text <- readTVar $ tabTitle tab
    style <- styleOfNotification <$> (readTVar $ tabNotification tab)
    return (text, style)
  Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    let titleMarkup = T.pack $ printf "<span%s>%s</span>" style
                      (escapeText text)
    Gtk.labelSetMarkup (tabLabel tab) titleMarkup
    return False
  return ()
--  printf "*** DONE SETTING TAB TITLE\n"

-- | Attempt to create a tab for a window.
createTab :: Window -> T.Text -> Notification -> IO (Maybe Tab)
createTab window title notification = do
  (state, actualWindow, notebook, nextTabIndex) <- atomically $ do
    state <- readTVar $ windowState window
    actualWindow <- tryReadTMVar $ windowWindow window
    notebook <- tryReadTMVar $ windowNotebook window
    nextTabIndex <- readTVar $ windowNextTabIndex window
    writeTVar (windowNextTabIndex window) $ nextTabIndex + 1
    return (state, actualWindow, notebook, nextTabIndex)
  case (state, actualWindow, notebook) of
    (WindowShown, Just actualWindow, Just notebook) -> do
--      printf "*** STARTING CREATING TAB\n"
      sideBox <- Gtk.boxNew Gtk.OrientationVertical 0
      mainBox <- Gtk.boxNew Gtk.OrientationVertical 0
      bodyBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
      lowBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
      topicEntry <- Gtk.entryNew
      scrolledWindow <- Gtk.scrolledWindowNew
        (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
      Gtk.scrolledWindowSetPolicy scrolledWindow Gtk.PolicyTypeNever
        Gtk.PolicyTypeAlways
      scrolledSideWindow <- Gtk.scrolledWindowNew
        (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
      Gtk.scrolledWindowSetPolicy scrolledSideWindow Gtk.PolicyTypeNever
        Gtk.PolicyTypeAutomatic
      textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
      textView <- Gtk.textViewNewWithBuffer textBuffer
      textMark <- Gtk.textMarkNew Nothing False
      iter <- Gtk.textBufferGetStartIter textBuffer
      Gtk.textIterForwardToEnd iter
      Gtk.textBufferAddMark textBuffer textMark iter
      Gtk.onWidgetSizeAllocate textView $ \rect -> do
        Gtk.textViewScrollToMark textView textMark 0.0 True 0.5 0.5
      Gtk.textViewSetMonospace textView True
      Gtk.textViewSetWrapMode textView Gtk.WrapModeChar
      Gtk.textViewSetEditable textView False
      Gtk.containerAdd scrolledWindow textView
      sideListBox <- Gtk.listBoxNew
      Gtk.containerAdd scrolledSideWindow sideListBox
      nickLabel <- Gtk.labelNew Nothing
      entry <- Gtk.entryNew
      Gtk.boxPackStart lowBox nickLabel False False 10
      Gtk.boxPackStart lowBox entry True True 0
      Gtk.boxPackStart mainBox topicEntry False False 0
      Gtk.boxPackStart mainBox scrolledWindow True True 0
      Gtk.boxPackStart mainBox lowBox False False 0
      Gtk.boxPackStart sideBox scrolledSideWindow True True 0
      Gtk.boxPackStart bodyBox mainBox True True 0
      Gtk.boxPackStart bodyBox sideBox False False 0
      Gtk.widgetSetNoShowAll sideBox True
      Gtk.widgetSetNoShowAll topicEntry True
      Gtk.widgetSetNoShowAll nickLabel True
      Gtk.widgetShowAll bodyBox
      Gtk.widgetHide sideBox
      Gtk.widgetHide topicEntry
      Gtk.widgetHide nickLabel
      Gtk.widgetSetNoShowAll sideBox False
      Gtk.widgetSetNoShowAll topicEntry False
      Gtk.widgetSetNoShowAll nickLabel False
      tabBox <- Gtk.boxNew Gtk.OrientationHorizontal 10
      label <- Gtk.labelNew Nothing
      image <- Gtk.imageNewFromIconName (Just "window-close") 12
      closeButton <- Gtk.toolButtonNew (Just image) (Nothing :: Maybe T.Text)
      Gtk.boxPackStart tabBox label False False 0
      Gtk.boxPackStart tabBox closeButton False False 0
      Gtk.widgetShowAll tabBox
      menuLabel <- Gtk.labelNew (Nothing :: Maybe T.Text)
      nextUserIndex <- atomically $ newTVar 0
      users <- atomically $ newTVar S.empty
      tabTitle' <- atomically $ newTVar title
      tabNotification' <- atomically $ newTVar notification
      state <- atomically $ newTVar TabIsOpen
      eventQueue <- atomically $ newBroadcastTChan
      let tab = Tab { tabIndex = nextTabIndex,
                      tabWindow = window,
                      tabTextView = textView,
                      tabTextBuffer = textBuffer,
                      tabEntry = entry,
                      tabTopicEntry = topicEntry,
                      tabSideListBox = sideListBox,
                      tabScrolledWindow = scrolledWindow,
                      tabSideBox = sideBox,
                      tabBodyBox = bodyBox,
                      tabLabel = label,
                      tabTabBox = tabBox,
                      tabNextUserIndex = nextUserIndex,
                      tabUsers = users,
                      tabTitle = tabTitle',
                      tabNotification = tabNotification',
                      tabNickLabel = nickLabel,
                      tabTextMark = textMark,
                      tabState = state,
                      tabEventQueue = eventQueue }
      updateTabTitle tab
      atomically $ do
        tabs <- readTVar $ windowTabs window
        writeTVar (windowTabs window) $ tabs |> tab
      Gtk.onToolButtonClicked closeButton $ do
        state <- atomically $ readTVar state
        case state of
          TabIsOpen -> do
--            printf "*** STARTING CLOSING TAB\n"
            pageNum <- Gtk.notebookPageNum notebook bodyBox
            Gtk.notebookRemovePage notebook pageNum
            Gtk.widgetHide tabBox
            Gtk.widgetHide bodyBox
            atomically $ do
              users <- readTVar $ tabUsers tab
              forM_ users $ \user -> do
                writeTVar (tabUserState user) TabUserIsClosed
                writeTChan (tabUserEventQueue user) TabUserClosed
              writeTVar (tabState tab) TabIsClosed
              writeTChan (tabEventQueue tab) TabClosed
              tabs <- readTVar $ windowTabs window
              writeTVar (windowTabs window) $ S.filter (/= tab) tabs
--            printf "*** DONE CLOSING TAB\n"
          TabIsClosed -> return ()
      Gtk.onEntryActivate entry $ do
        state <- atomically $ readTVar state
        case state of
          TabIsOpen -> do
            text <- Gtk.entryGetText entry
            atomically . writeTChan (tabEventQueue tab) $ LineEntered text
            Gtk.entrySetText entry ""
          TabIsClosed -> return ()
      Gtk.onEntryActivate topicEntry $ do
        state <- atomically $ readTVar state
        case state of
          TabIsOpen -> do
            text <- Gtk.entryGetText topicEntry
            atomically . writeTChan (tabEventQueue tab) $ TopicEntered text
          TabIsClosed -> return ()
      Gtk.onWidgetKeyPressEvent entry $ \e ->
        Gdk.getEventKeyKeyval e >>= Gdk.keyvalName >>= \case
          Just "Up" -> do
            atomically $ writeTChan (tabEventQueue tab) UpPressed
            return True
          Just "Down" -> do
            atomically $ writeTChan (tabEventQueue tab) DownPressed
            return True
          _ -> return False
      Gtk.notebookAppendPageMenu notebook bodyBox (Just tabBox) (Just menuLabel)
--      printf "*** DONE CREATING TAB\n"
      return $ Just tab
    _ -> return Nothing

-- | Escape text for markup.
escapeText :: T.Text -> T.Text
escapeText text =
  unsafePerformIO $ GLib.markupEscapeText text
  (fromIntegral . B.length $ encodeUtf8 text)

-- | Format text for markup.
formatText :: T.Text -> T.Text
formatText text =
  let parts = T.splitOn "\n" text
      parts' =
        fmap (\part -> formatText' part [] $ StyleAndColor [] 99 99) parts
  in T.intercalate "\n" parts'
  where formatText' text parts styleAndColor =
          case T.uncons text of
            Just (char, rest)
              | char == '\x000002' ->
                handleStyle rest parts styleAndColor Bold
              | char == '\x00001D' ->
                handleStyle rest parts styleAndColor Italic
              | char == '\x00001F' ->
                handleStyle rest parts styleAndColor Underline
              | char == '\x000016' ->
                handleStyle rest parts styleAndColor Reverse
              | char == '\x00000F' ->
                clearStyle rest parts styleAndColor
              | char == '\x000003' ->
                handleColor rest parts styleAndColor
              | otherwise ->
                let (text', rest) = T.span (not . isFormattingChar) text
                in formatText' rest (parts |> escapeText text') styleAndColor
            Nothing -> T.concat . toList $ closeStyle parts styleAndColor
        closeStyle parts styleAndColor =
          if styleAndColor /= StyleAndColor [] 99 99
          then parts |> "</span>"
          else parts
        handleStyle text parts styleAndColor changedStyle =
          let newStyleAndColor = toggleStyle styleAndColor changedStyle
              parts' = closeStyle parts styleAndColor
              parts'' = styleMarkup parts' newStyleAndColor
          in formatText' text parts'' newStyleAndColor
        clearStyle text parts styleAndColor =
          formatText' text (closeStyle parts styleAndColor)
          (StyleAndColor [] 99 99)
        handleColor text parts styleAndColor =
          case parseColorNumber text of
            (Just foreground, rest) ->
              case T.uncons rest of
                Just (',', rest') ->
                  case parseColorNumber rest' of
                    (Just background, rest'') ->
                      let newStyleAndColor =
                            styleAndColor { currentForeground = foreground,
                                            currentBackground = background }
                          parts' = closeStyle parts styleAndColor
                          parts'' = styleMarkup parts' newStyleAndColor
                      in formatText' rest'' parts'' newStyleAndColor
                    (Nothing, _) ->
                      let newStyleAndColor =
                            styleAndColor { currentForeground = foreground,
                                            currentBackground = 99 }
                          parts' = closeStyle parts styleAndColor
                          parts'' = styleMarkup parts' newStyleAndColor
                      in formatText' rest parts'' newStyleAndColor
                _ ->
                  let newStyleAndColor =
                        styleAndColor { currentForeground = foreground,
                                        currentBackground = 99 }
                      parts' = closeStyle parts styleAndColor
                      parts'' = styleMarkup parts' newStyleAndColor
                  in formatText' rest parts'' newStyleAndColor
            (Nothing, rest) ->
              let newStyleAndColor = styleAndColor { currentForeground = 99,
                                                     currentBackground = 99 }
                  parts' = closeStyle parts styleAndColor
                  parts'' = styleMarkup parts' newStyleAndColor
              in formatText' rest parts'' newStyleAndColor
        styleMarkup parts styleAndColor =
          if styleAndColor /= StyleAndColor [] 99 99
          then
            let part0 =
                  case S.elemIndexL Bold $ currentStyle styleAndColor of
                    Just _ -> " weight=\"bold\""
                    Nothing -> ""
                part1 =
                  case S.elemIndexL Italic $ currentStyle styleAndColor of
                    Just _ -> " style=\"italic\""
                    Nothing -> ""
                part2 =
                  case S.elemIndexL Underline $ currentStyle styleAndColor of
                    Just _ -> " underline=\"single\""
                    Nothing -> ""
                part3 =
                  case S.elemIndexL Reverse $ currentStyle styleAndColor of
                    Just _ ->
                      T.pack $ printf " foreground=\"%s\" background=\"%s\""
                      (getColor "#FFFFFF" $ currentBackground styleAndColor)
                      (getColor "#000000" $ currentForeground styleAndColor)
                    Nothing ->
                      T.pack $ printf " foreground=\"%s\" background=\"%s\""
                      (getColor "#000000" $ currentForeground styleAndColor)
                      (getColor "#FFFFFF" $ currentBackground styleAndColor)
            in parts >< ["<span", part0, part1, part2, part3, ">"]
          else parts

-- | Parse a color number.
parseColorNumber :: T.Text -> (Maybe Int, T.Text)
parseColorNumber text =
  case T.uncons text of
    Just (char, rest)
      | char == '0' ->
        case T.uncons rest of
          Just (char, rest')
            | char == '0' -> (Just 0, rest')
            | char == '1' -> (Just 1, rest')
            | char == '2' -> (Just 2, rest')
            | char == '3' -> (Just 3, rest')
            | char == '4' -> (Just 4, rest')
            | char == '5' -> (Just 5, rest')
            | char == '6' -> (Just 6, rest')
            | char == '7' -> (Just 7, rest')
            | char == '8' -> (Just 8, rest')
            | char == '9' -> (Just 9, rest')
          _ -> (Nothing, text)
      | char == '1' ->
        case T.uncons rest of
          Just (char, rest')
            | char == '0' -> (Just 10, rest')
            | char == '1' -> (Just 11, rest')
            | char == '2' -> (Just 12, rest')
            | char == '3' -> (Just 13, rest')
            | char == '4' -> (Just 14, rest')
            | char == '5' -> (Just 15, rest')
          _ -> (Nothing, text)
      | char == '9' ->
        case T.uncons rest of
          Just ('9', rest') -> (Just 99, rest')
          _ -> (Nothing, text)
    _ -> (Nothing, text)

-- | Get whether char is formatting char.
isFormattingChar :: Char -> Bool
isFormattingChar '\x000002' = True
isFormattingChar '\x00001D' = True
isFormattingChar '\x00001F' = True
isFormattingChar '\x000016' = True
isFormattingChar '\x00000F' = True
isFormattingChar '\x000003' = True
isFormattingChar _ = False

-- | Colors with defaults.
getColor :: T.Text -> Int -> T.Text
getColor _ 0 = "#FFFFFF"
getColor _ 1 = "#000000"
getColor _ 2 = "#00007F"
getColor _ 3 = "#009300"
getColor _ 4 = "#FF0000"
getColor _ 5 = "#7F0000"
getColor _ 6 = "#9C009C"
getColor _ 7 = "#FC7F00"
getColor _ 8 = "#FFFF00"
getColor _ 9 = "#00FC00"
getColor _ 10 = "#009393"
getColor _ 11 = "#00FFFF"
getColor _ 12 = "#0000FC"
getColor _ 13 = "#FF00FF"
getColor _ 14 = "#7F7F7F"
getColor _ 15 = "#D2D2D2"
getColor color _ = color

-- | Toggle style.
toggleStyle :: StyleAndColor -> Style -> StyleAndColor
toggleStyle styleAndColor style =
  case S.elemIndexL style (currentStyle styleAndColor) of
    Just _ ->
      styleAndColor { currentStyle =
                        S.filter (/= style) $ currentStyle styleAndColor }
    Nothing ->
      styleAndColor { currentStyle = currentStyle styleAndColor |> style }

-- | Format text for markup.
stripText :: T.Text -> T.Text
stripText text =
  let parts = T.splitOn "\n" text
      parts' = fmap (\part -> stripText' part []) parts
  in T.intercalate "\n" parts'
  where stripText' text parts =
          case T.uncons text of
            Just (char, rest)
              | char == '\x000002' -> stripText' rest parts
              | char == '\x00001D' -> stripText' rest parts
              | char == '\x00001F' -> stripText' rest parts
              | char == '\x000016' -> stripText' rest parts
              | char == '\x00000F' -> stripText' rest parts
              | char == '\x000003' -> handleColor rest parts
              | otherwise ->
                let (text', rest) = T.span (not . isFormattingChar) text
                in stripText' rest (parts |> text')
            Nothing -> T.concat $ toList parts
        handleColor text parts =
          case parseColorNumber text of
            (Just foreground, rest) ->
              case T.uncons rest of
                Just (',', rest') ->
                  case parseColorNumber rest' of
                    (Just background, rest'') -> stripText' rest'' parts
                    (Nothing, _) -> stripText' rest parts
                _ -> stripText' rest parts
            (Nothing, rest) -> stripText' rest parts

-- | Get a style for a notification.
styleOfNotification :: Notification -> T.Text
styleOfNotification NoNotification = ""
styleOfNotification UserChanged = " foreground=\"brown\""
styleOfNotification ChannelMessaged = " foreground=\"blue\""
styleOfNotification UserMessaged = " foreground=\"purple\""
styleOfNotification Mentioned = " foreground=\"orange\""
