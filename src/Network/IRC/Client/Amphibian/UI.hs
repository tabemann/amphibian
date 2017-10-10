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
   initWindowing,
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
   addTabText,
   setTopicVisible,
   setTopic,
   setSideVisible,
   getTabState,
   subscribeTab,
   recvTab,
   tryRecvTab)

where

import Network.IRC.Client.Amphibian.Types
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import Data.Int (Int32)
import Data.Functor ((<$>))
import Data.Sequence ((|>))
import Control.Monad (forM_)
import Control.Concurrent (forkOS)
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

-- | Default window width.
defaultWindowWidth :: Int32
defaultWindowWidth = 800

-- | Default window height.
defaultWindowHeight :: Int32
defaultWindowHeight = 600

-- | Initialize windowing.
initWindowing :: IO (Response ())
initWindowing = do
  response <- atomically $ newEmptyTMVar
  Gtk.init Nothing
  forkOS $ do
    GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      atomically $ putTMVar response $ Right ()
      return False
    Gtk.main
  return $ Response response

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
openTab :: Window -> T.Text -> STM (Response Tab)
openTab window title = do
  state <- readTVar $ windowState window
  response <- newEmptyTMVar
  if state /= WindowNotStarted
    then writeTQueue (windowActionQueue window)
         (OpenTab title $ Response response)
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
          printf "*** OPENING WINDOW\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            actualWindow <- Gtk.windowNew Gtk.WindowTypeToplevel
            Gtk.windowSetDefaultSize actualWindow defaultWindowWidth
              defaultWindowHeight
            Gtk.setWindowTitle actualWindow title
            Gtk.onWidgetDestroy actualWindow $ do
              atomically $ do
                writeTVar (windowState window) WindowNotShown
                writeTChan (windowEventQueue window) WindowClosed
                tabs <- readTVar $ windowTabs window
                forM_ tabs $ \tab -> do
                  writeTVar (tabState tab) TabIsClosed
                  writeTChan (tabEventQueue tab) TabClosed
                writeTVar (windowTabs window) S.empty
            notebook <- Gtk.notebookNew
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
          printf "*** DONE OPENING WINDOW\n"
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
    OpenTab title (Response response) -> do
      if state == WindowShown
        then do
          lock <- atomically $ newEmptyTMVar
          printf "*** OPENING TAB\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            tab <- createTab window title
            case tab of
              Just tab -> atomically . putTMVar response $ Right tab
              Nothing ->
                atomically . putTMVar response . Left $
                Error "unable to create tab"
            atomically $ putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
          printf "*** DONE OPENING TAB\n"
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
              printf "*** CLOSING TAB\n"
              Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                pageNum <- Gtk.notebookPageNum notebook $ tabBodyBox tab
                Gtk.notebookRemovePage notebook pageNum
                Gtk.widgetHide $ tabBodyBox tab
                Gtk.widgetHide $ tabTabBox tab
                atomically $ do
                  writeTVar (tabState tab) TabIsClosed
                  writeTChan (tabEventQueue tab) TabClosed
                  putTMVar response $ Right ()
                  putTMVar lock ()
                return False
              (atomically $ takeTMVar lock) >> return ()
              printf "*** DONE CLOSING TAB\n"
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
              printf "*** SETTING WINDOW TITLE\n"
              Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
                Gtk.setWindowTitle actualWindow title
                atomically $ do
                  (takeTMVar $ windowTitle window) >> return ()
                  putTMVar (windowTitle window) title
                  putTMVar response $ Right ()
                  putTMVar lock ()
                return False
              (atomically $ takeTMVar lock) >> return ()
              printf "*** DONE SETTING WINDOW TITLE\n"
            Nothing -> atomically . putTMVar response . Left $
                       Error "could not find window"
        else atomically . putTMVar response . Left $ Error "window not open"
      runWindow window
    SetTabTitle tab title (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
          printf "*** SETTING TAB TITLE\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.labelSetText (tabLabel tab) title
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
          printf "*** DONE SETTING TAB TITLE\n"
        TabIsClosed -> atomically .  putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    AddTabText tab text (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
          printf "*** ADDING TAB TEXT\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            iter <- Gtk.textBufferGetStartIter $ tabTextBuffer tab
            Gtk.textIterForwardToEnd iter
            Gtk.textBufferInsert (tabTextBuffer tab) iter text
              (fromIntegral $ T.length text)
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
          printf "*** DONE ADDING TAB TEXT\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetTopicVisible tab visible (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
          printf "*** SETTING TOPIC VISIBILITY\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            if visible
              then Gtk.widgetShow $ tabTopicEntry tab
              else Gtk.widgetHide $ tabTopicEntry tab
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
          printf "*** DONE SETTING TOPIC VISIBILITY\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetTopic tab text (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
          printf "*** SETTING TOPIC TEXT\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.entrySetText (tabTopicEntry tab) text
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
          printf "*** DONE SETTING TOPIC TEXT\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window
    SetSideVisible tab visible (Response response) -> do
      tabState' <- atomically . readTVar $ tabState tab
      case tabState' of
        TabIsOpen -> do
          lock <- atomically $ newEmptyTMVar
          printf "*** SETTING SIDE VISIBILITY\n"
          Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            if visible
              then Gtk.widgetShow $ tabSideBox tab
              else Gtk.widgetHide $ tabSideBox tab
            atomically $ do
              putTMVar response $ Right ()
              putTMVar lock ()
            return False
          (atomically $ takeTMVar lock) >> return ()
          printf "*** DONE SETTING SIDE VISIBILITY\n"
        TabIsClosed -> atomically . putTMVar response . Left $
                       Error "tab is closed"
      runWindow window

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
  case actualWindow of
    Just actualWindow -> do
      Gtk.onWidgetKeyPressEvent actualWindow $ \e ->
        Gdk.getEventKeyState e >>= \case
          [Gdk.ModifierTypeControlMask] ->
            Gdk.getEventKeyKeyval e >>= Gdk.keyvalName >>= \case
              Just "n" -> do
                tab <- createTab window ""
                case tab of
                  Just tab -> do
                    atomically $ do
                      writeTChan (windowEventQueue window) $ UserOpenedTab tab
                    printf "*** ENQUEUED USER OPENED TAB EVENT\n"
                  Nothing -> return ()
                return True
              _ -> return False
          _ -> return False
      return ()
    Nothing -> return ()

-- | Attempt to create a tab for a window.
createTab :: Window -> T.Text -> IO (Maybe Tab)
createTab window title = do
  (state, actualWindow, notebook, nextTabIndex) <- atomically $ do
    state <- readTVar $ windowState window
    actualWindow <- tryReadTMVar $ windowWindow window
    notebook <- tryReadTMVar $ windowNotebook window
    nextTabIndex <- readTVar $ windowNextTabIndex window
    writeTVar (windowNextTabIndex window) $ nextTabIndex + 1
    return (state, actualWindow, notebook, nextTabIndex)
  case (state, actualWindow, notebook) of
    (WindowShown, Just actualWindow, Just notebook) -> do
      printf "*** STARTING CREATING TAB\n"
      sideBox <- Gtk.boxNew Gtk.OrientationVertical 10
      mainBox <- Gtk.boxNew Gtk.OrientationVertical 10
      bodyBox <- Gtk.boxNew Gtk.OrientationHorizontal 10
      topicEntry <- Gtk.entryNew
      scrolledWindow <- Gtk.scrolledWindowNew
        (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
      Gtk.scrolledWindowSetPolicy scrolledWindow Gtk.PolicyTypeNever
        Gtk.PolicyTypeAlways
      textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
      textView <- Gtk.textViewNewWithBuffer textBuffer
      Gtk.onWidgetSizeAllocate textView $ \rect -> do
        adjustment <- Gtk.scrolledWindowGetVadjustment scrolledWindow
        upper <- Gtk.adjustmentGetUpper adjustment
        pageSize <- Gtk.adjustmentGetPageSize adjustment
        Gtk.adjustmentSetValue adjustment $ upper - pageSize
      Gtk.textViewSetMonospace textView True
      Gtk.textViewSetWrapMode textView Gtk.WrapModeChar
      Gtk.textViewSetEditable textView False
      Gtk.containerAdd scrolledWindow textView
      entry <- Gtk.entryNew
      Gtk.boxPackStart mainBox topicEntry False False 0
      Gtk.boxPackStart mainBox scrolledWindow True True 0
      Gtk.boxPackStart mainBox entry False False 0
      Gtk.boxPackStart bodyBox mainBox True True 0
      Gtk.boxPackStart bodyBox sideBox False False 0
      Gtk.widgetShowAll bodyBox
      Gtk.widgetHide sideBox
      Gtk.widgetHide topicEntry
      tabBox <- Gtk.boxNew Gtk.OrientationHorizontal 10
      label <- Gtk.labelNew $ Just title
      image <- Gtk.imageNewFromIconName (Just "window-close") 12
      closeButton <- Gtk.toolButtonNew (Just image) (Nothing :: Maybe T.Text)
      Gtk.boxPackStart tabBox label False False 0
      Gtk.boxPackStart tabBox closeButton False False 0
      Gtk.widgetShowAll tabBox
      menuLabel <- Gtk.labelNew (Nothing :: Maybe T.Text)
      state <- atomically $ newTVar TabIsOpen
      eventQueue <- atomically $ newBroadcastTChan
      let tab = Tab { tabIndex = nextTabIndex,
                      tabWindow = window,
                      tabTextView = textView,
                      tabTextBuffer = textBuffer,
                      tabEntry = entry,
                      tabTopicEntry = topicEntry,
                      tabSideBox = sideBox,
                      tabBodyBox = bodyBox,
                      tabLabel = label,
                      tabTabBox = tabBox,
                      tabState = state,
                      tabEventQueue = eventQueue }
      atomically $ do
        tabs <- readTVar $ windowTabs window
        writeTVar (windowTabs window) $ tabs |> tab
      Gtk.onToolButtonClicked closeButton $ do
        state <- atomically $ readTVar state
        case state of
          TabIsOpen -> do
            printf "*** STARTING CLOSING TAB\n"
            pageNum <- Gtk.notebookPageNum notebook bodyBox
            Gtk.notebookRemovePage notebook pageNum
            Gtk.widgetHide tabBox
            Gtk.widgetHide bodyBox
            atomically $ do
              writeTVar (tabState tab) TabIsClosed
              writeTChan (tabEventQueue tab) TabClosed
              tabs <- readTVar $ windowTabs window
              writeTVar (windowTabs window) $ S.filter (/= tab) tabs
            printf "*** DONE CLOSING TAB\n"
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
      Gtk.notebookAppendPageMenu notebook bodyBox (Just tabBox) (Just menuLabel)
      printf "*** DONE CREATING TAB\n"
      return $ Just tab
    _ -> return Nothing
