module Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend

       (VtyFrontend,
        VtyWindow,
        VtyBufferPosition,
        VtyKeyHandler,
        VtyUnmappedKeyHandler,
        getCurrentWindow,
        getHeight,
        getWidth,
        getScrollHeight,
        getInterface,
        getWindowServer,
        getWindows,
        formatLine,
        new,
        start,
        registerKeyHandler,
        unregisterKeyHandler,
        registerUnmappedKeyHandler,
        unregisterUnmappedKeyHandler,
        registerWindowServer,
        unregisterWindowServer,
        registerWindow,
        unregisterWindow,
        redraw)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Utilities
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frontend as F
import qualified Network.IRC.Client.Amphibian.Frame as Fr
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import Control.Monad (join,
                      forM_,
                      mapM)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               atomically,
                               orElse,
                               retry,
                               newTVar,
                               readTVar,
                               writeTVar,
                               readTChan)
import Control.Concurrent.Async (Async,
                                 async)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config as VC
import qualified Graphics.Vty.Input as VI
import qualified Graphics.Vty.Image as VIm
import Graphics.Vty.Image ((<|>),
                           (<->))
import qualified Graphics.Vty.Picture as VP
import qualified Graphics.Vty.Attributes as VA
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence ((<|),
                      (|>),
                      (><))

-- | Get current Vty frontend window.
getCurrentWindow :: VtyFrontend -> STM (Maybe VtyWindow)
getCurrentWindow = readTVar . vtfrCurrentWindow

-- | Get height.
getHeight :: VtyFrontend -> STM Int
getHeight = readTVar . vtfrHeight

-- | Get width.
getWidth :: VtyFrontend -> STM Int
getWidth = readTVar . vtfrWidth

-- | Get buffer height.
getScrollHeight :: VtyFrontend -> VtyWindow -> STM Int
getScrollHeight vtyFrontend vtyWindow = do
  width <- readTVar $ vtfrWidth vtyFrontend
  height <- readTVar $ vtfrHeight vtyFrontend
  topic <- Fr.getTopic $ vtwiFrame vtyWindow
  case topic of
   Just topic ->
     let topicHeight = breakLines width topic in
     if height > 3 + topicHeight
     then return $ height - (3 + topicHeight)
     else return 0
   Nothing ->
     if height > 3
     then return $ height - 3
     else return 0

-- | Get interface.
getInterface :: VtyFrontend -> Interface
getInterface = vtfrInterface

-- | Get window server.
getWindowServer :: VtyFrontend -> STM (Maybe VtyWindowServer)
getWindowServer = readTVar . vtfrWindowServer

-- | Get window count.
getWindows :: VtyFrontend -> STM [VtyWindow]
getWindows = readTVar . vtfrWindows

-- | Format line.
formatLine :: VtyFrontend -> FrameLine -> STM StyledText
formatLine vtyFrontend line = do
  return $ ST.concat [frliAltSource line, ST.addStyle [] $ T.singleton ' ', frliBody line]

-- | Create a new Vty frontend.
new :: Interface -> STM VtyFrontend
new intf = do
  frontend <- newTVar Nothing
  frontendSubscription <- newTVar Nothing
  running <- newTVar False
  vty <- newTVar Nothing
  vtyEvents <- newTVar Nothing
  currentWindow <- newTVar Nothing
  currentWindowIndex <- newTVar Nothing
  windows <- newTVar []
  keyMappings <- newTVar M.empty
  unmappedKeyHandlers <- newTVar []
  return $ VtyFrontend { vtfrInterface = intf,
                         vtfrFrontend = frontend,
                         vtfrFrontendSubscription = frontendSubscription,
                         vtfrRunning = running,
                         vtfrVty = vty,
                         vtfrVtyEvents = vtyEvents,
                         vtfrCurrentWindow = currentWindow,
                         vtfrCurrentWindowIndex = currentWindowIndex,
                         vtfrKeyMappings = keyMappings,
                         vtfrUnmappedKeyHandlers = unmappedKeyHandlers,
                         vtfrWindows = windows }

-- | Start a Vty frontend.
start :: VtyFrontend -> AM ()
start vtyFrontend = do
  join . liftIO . atomically $ do
    frontend <- I.getFrontend $ vtfrInterface vtyFrontend
    case frontend of
      Just frontend -> do
        running <- readTVar $ vtfrRunning vtyFrontend
        if not running
        then do
          writeTVar (vtfrRunning vtyFrontend) True
          writeTVar (vtfrFrontend vtyFrontend) $ Just frontend
          subscription <- F.subscribeOutput frontend
          writeTVar (vtfrFrontendSubscription vtyFrontend) $ Just subscription
          return $ liftIO . async $ runAM (start' vtyFrontend) (vtfrInterface vtyFrontend)
        else return $ return ()
  where start' vtyFrontend = do
          config <- liftIO VC.standardIOConfig
          vty <- liftIO $ V.mkVty config
          liftIO . atomically . writeTVar (vtfrVty vtyFrontend) $ Just vty
          runVtyFrontend vtyFrontend

-- | Register key handler.
registerKeyHandler :: VtyFrontend -> Key -> [Modifier] -> (VtyFrontend -> Key -> [Modifier] -> AM Bool) ->
                      STM VtyKeyHandler
registerKeyHandler vtyFrontend key modifiers handler = do
  keyMappings <- readTVar $ vtfrKeyMappings vtyFrontend
  let keyCombo = VtyKeyCombination { vtkcKey = key,
                                     vtkcModifiers = modifiers }
  let keyHandlers = maybe [] id $ HM.lookup keyCombo keyMappings
  handler' <- newTVar handler
  let handler'' = VtyKeyHandler { vtkhFrontend = vtyFrontend,
                                  vtkhKeyCombo = keyCombo,
                                  vtkhHandler = handler' }
  writeTVar (vtfrKeyMappings vtyFrontend) $ HM.insert keyCombo (handler'' : keyHandlers) keyMappingsMap
  return handler''

-- | Unregister key handler.
unregisterKeyHandler :: VtyKeyHandler -> STM ()
unregisterKeyHandler handler = do
  keyMappings <- readTVar . vtfrKeyMappings $ vtkhFrontend handler
  let keyHandlers = maybe [] id $ HM.lookup (vtkhKeyCombo handler) keyMappings
  writeTVar (vtfrKeyMappings vtyFrontend) $ HM.insert keyCombo (filter (/= handler) keyHandlers) keyMappings

-- | Register unmapped key handler.
registerUnmappedKeyHandler :: VtyFrontend -> (VtyFrontend -> Key -> [Modifier] -> AM Bool) ->
                              STM VtyUnmappedKeyHandler
registerUnmappedKeyHandler vtyFrontend key modifiers handler = do
  keyHandlers <- readTVar $ vtfrUnmappedKeyHandlers vtyFrontend
  handler' <- newTVar handler
  let handler'' = VtyUnmappedKeyHandler { vukhFrontend = vtyFrontend,
                                          vukhHandler = handler' }
  writeTVar (vtfrUnmappedKeyHandlers vtyFrontend) $ handler'' : keyHandlers
  return handler''

-- | Unregister unmapped key handler.
unregisterUnmappedKeyHandler :: VtyUnmappedKeyHandler -> STM ()
unregisterUnmappedKeyHandler handler = do
  keyHandlers <- readTVar . vtfrUnmappedKeyHandlers $ vtkhFrontend handler
  writeTVar (vtfrUnmappedKeyHandlers vtyFrontend) $ filter (/= handler) keyHandlers

-- | Run the Vty frontend.
runVtyFrontend :: VtyFrontend -> AM ()
runVtyFrontend vtyFrontend = do
  continue <- join . liftIO . atomically $ do
    handleFrontend vtyFrontend `orElse` handleVty vtyFrontend

-- | Register window server.
registerWindowServer :: VtyFrontend -> VtyWindowServer -> STM ()
registerWindowServer vtyFrontend vtyWindowServer = do
  currentWindowServer <- readTVar $ vtfrWindowServer vtyFrontend
  case currentWindowServer of
   Nothing -> writeTVar (vtyWindowServer vtyFrontend) $ Just vtyWindowServer
   _ -> return ()

-- | Unregister window server.
unregisterWindowServer :: VtyFrontend -> VtyWindowServer -> STM ()
unregisterWindowServer vtyFrontend vtyWindowServer = do
  currentWindowServer <- readTVar $ vtfrWindowServer vtyFrontend
  case currentWindowServer of
   Just currentWindowServer
     | currentWindowServer == vtyWindowServer -> writeTVar (vtfrWindowServer vtyFrontend) Nothing
   _ -> return ()

-- | Register window.
registerWindow :: VtyFrontend -> VtyWindow -> STM ()
registerWindow vtyFrontend vtyWindow = do
  windows <- readTVar $ vtfrWindows vtyFrontend
  case S.elemIndexL vtyWindow windows of
   Nothing -> do
     currentWindowIndex <- readTVar $ vtfrCurrentWindowIndex vtyFrontend
     let (before, after) = S.splitAt (maybe 0 (+ 1) currentWindowIndex) windows
     writeTVar (vtfrWindows vtyFrontend) $ (before |> vtyWindow) >< after
   Just _ -> return ()

-- | Unregister window.
unregisterWindow :: VtyFrontend -> VtyWindow -> STM ()
unregisterWindow vtyFrontend vtyWindow = do
  windows <- readTVar $ vtfrWindows vtyFrontend
  case S.elemIndexL vtyWindow windows of
   Just index -> do
     let before = S.take index
         after = S.drop (index + 1)
     writeTVar (vtfrWindows vtyFrontend) $ before >< after
     currentWindow <- readTVar $ vtfrCurrentWindow vtyFrontend
     case currentWindow of
      Just currentWindow
        | currentWindow == vtyWindow -> do
            if index > 0
              then do writeTVar (vtfrCurrentWindowIndex vtyFrontend) . Just $ index - 1
                      let newCurrentWindow = S.index windows $ index - 1
                      writeTVar (vtfrCurrentWindow vtyFrontend) $ Just newCurrentWindow
                      F.setFocus (vtwiFrame newCurrentWindow) True
              else if S.length windows > 1
                   then do writeTVar (vtfrCurrentWindowIndex vtyFrontend) $ Just 0
                           let newCurrentWIndow = S.index windows 1
                           writeTVar (vtfrCurrentWindow vtyFrontend) $ Just newCurrentWindow
                           F.setFocus (vtwiFrame newCurrentWindow) True
                   else do writeTVar (vtfrCurrentWindowIndex vtyFrontend) Nothing
                           writeTVar (vtfrCurrentWindow vtyFrontend) Nothing
      _ -> return ()
   Nothing -> return ()

-- | Redraw Vty frontend.
redraw :: VtyFrontend -> AM ()
redraw vtyFrontend = do
  join . liftIO . atomically $ do
    width <- readTVar $ vtfrWidth vtyFrontend
    height <- readTVar $ vtfrHeight vtyFrontend
    currentWindow <- readTVar $ vtfrCurrentWindow vtyFrontend
    currentWindowIndex <- readTVar $ vtfrCurrentWindowIndex vtyFrontend
    inputText <- readTVar $ vtwiInputText currentWindow
    inputCursorPosition <- readTVar $ vtwiInputCursorPosition currentWindow
    inputVisiblePosition <- readTVar $ vtwiInputVisiblePosition currentWindow
    title <- Fr.getTitle $ vtwiFrame currentWindow
    topic <- Fr.getTopic $ vtwiFrame currentWindow
    nick <- Fr.getNick $ vtwiFrame currentWindow
    name <- Fr.getName $ vtwiFrame currentWindow
    bufferLines <- readTVar $ vtwiBufferLines currentWindow
    bufferLines' <- mapM (formatLine vtyFrontend) bufferLines
    bufferPosition <- readTVar $ vtwiBufferPosition currentWindow
    let titleImageBackground = VIm.charFill (convertStyle [TxstForeColor 1, TxstBackColor 12]) width 1 in
    let titleImage =
          VIm.cropRight width . convertStyledTextLine . ST.setBaseForeColor 1 $ ST.setBaseBackColor 12 title in
    let topicImage = convertStyledTextLines . ST.setBaseForeColor 1 $ ST.setBaseBackColor 12 topic in
    let topicImageBackground = VIm.pad 0 1 0 0 $ VIm.charFill (convertStyle [TxstForeColor 1, TxstBackColor 12])
                               width (VIm.imageHeight topicImage) in
    let topicImage' = VIm.pad 0 1 0 0 topicImage in
    let scrollImage =
          case bufferPosition of
           VtbpFixed position ->
             let scrollImage =
                   fixedScrollImage width ((height - 2) - VIm.imageHeight topicImage') bufferLines' position
                   VIm.emptyImage in
             let scrollImage' = VIm.cropBottom ((height - 2) - VIm.imageHeight topicImage') scrollImage in
             VIm.pad 0 (VIm.imageHeight topicImage') 0 0 scrollImage'
           VtbpDynamic ->
             let scrollImage =
                   dynamicScrollImage width ((height - 2) - VIm.imageHeight topicImage') bufferLines' 0
                   VIm.emptyImage in
             let scrollImage' = VIm.cropTop ((height - 2) - VIm.imageHeight topicImage') scrollImage'
             VIm.pad 0 ((height - 2) - VIm.imageHeight scrollImage') 0 0 scrollImage' in
    let descrImageBackground =
          VIm.pad 0 (height - 2) 0 0 $ VIm.charFill (convertStyle [TxstForeColor 1, TxstBackColor 12]) width 1 in
    let inputLineImage =
          let visibleInputText = ST.drop inputVisiblePosition inputText in
          let visibleInputText' =
                case fitWidth width $ ST.removeStyle visibleInputText of
                 Just (_, visibleInputLength) -> ST.take visibleInputLength visibleInputText
                 Nothing -> visibleInputText in
          VIm.pad 0 (height - 1) 0 0 $ convertStyledTextLine visibleInputText' in
  where fixedScrollImage width height bufferLines position image
          | position >= 0 && VIm.imageHeight image < height =
              fixedScrollImage width height bufferLines (position - 1)
              (image <-> convertStyledTextLines width (S.index bufferLines position))
          | otherwise = image
        dynamicScrollImage width height bufferLines position image
          | position < S.length bufferLines && VIm.imageHeight image < height =
              dynamicScrollImage width height bufferLines (position + 1)
              (convertStyledTextLines width (S.index bufferLines position) <-> image)
          | otherwise = image

-- | Handle Vty event.
handleVty :: VtyFrontend -> STM (AM Bool)
handleVty vtyFrontend = do
  vty <- readTVar $ vtfrVty vtyFrontend
  case vty of
    Just vty -> do
      event <- readTChan . VI._eventChannel $ V.inputIFace vty
      case event of
        VI.EvKey key modifiers -> handleInput vtyFrontend $ VtyKeyCombination key modifiers
        VI.EvResize width height ->
          writeTVar (vtfrWidth vtyFrontend) width
          writeTVar (vtfrHeight vtyFrontend) height
          windows <- readTVar $ vtfrWindows vtyFrontend
          forM_ windows $ \window -> do
            inputCursorPosition <- readTVar $ vtwiInputCursorPosition window
            inputVisiblePosition <- readTVar $ vtwiInputVisiblePosition window
            if inputCursorPosition > inputVisiblePosition + width
              then writeTVar (vtwInputVisiblePosition window) $ inputCursorPosition - width
              else return ()
          return $ do
            redraw vtyFrontend
            return True
        _ -> return $ return True
    Nothing -> retry

-- | Handle Vty input event.
handleInput :: VtyFrontend -> VtyKeyCombination -> STM (AM Bool)
handleInput vtyFrontend keyCombo = do
  keyMappings <- readTVar $ vtfrKeyMappings vtyFrontend
  case M.lookup keyCombo keyMappings of
    Just keyMappings -> return $ handleMappedKey keyMappings vtyFrontend keyCombo
    Nothing -> return $ handleUnmappedKey vtyFrontend keyCombo
  where handleMappedKey (handler : rest) vtyFrontend keyCombo = do
          handler' <- liftIO . atomically . readTVar $ vtkhHandler handler
          handled <- handler' vtyFrontend (vtkcKey keyCombo) (vtkcModifiers keyCombo)
          if not handled
          then handleMappedKey rest vtyFrontend keyCombo
          else return True
        handleMappedKey [] vtyFrontend keyCombo = handleUnmappedKey vtyFrontend keyCombo
        handleUnmappedKey vtyFrontend keyCombo = do
          handlers <- liftIO . atomically . readTVar $ vtfrUnmappedKeyHandlers vtyFrontend
          handleUnmappedKey' handlers vtyFrontend keyCombo
        handleUnmappedKey' (handler : rest) vtyFrontend keyCombo = do
          handler' <- liftIO . atomically . readTVar $ vukhHandler handler
          handled <- handler' vtyFrontend (vtkcKey keyCombo) (vtkcModifiers keyCombo)
          if not handled
          then handleUnmappedKey' rest vtyFrontend keyCombo
          else return True
        handleUnmappedKey' [] _ _ = return True

-- | Create an image from styled text broken up into lines.
convertStyledTextLines :: Int -> StyledText -> VIm.Image
convertStyledTextLines width styledText = VIm.vertCat . map convertStyledTextLine $ breakLines width styledText

-- | Create an image from a line of styled text.
convertStyledTextLine :: StyledText -> VIm.Image
convertStyledTextLine (StyledText xs) =
  foldl' (\image (StyledTextElement style text) -> image <|> VIm.text' (convertStyle style) text) VIm.emptyImage xs

-- | Convert a style into a Vty style.
convertStyle :: [TextStyle] -> VA.Attr
convertStyle style = foldl' convertStyle' VA.defAddr style
  where convertStyle' attr (TxstForeColor color) = attr `VA.withForeColor` convertColor color
        convertStyle' attr (TxstBackColor color) = attr `VA.withBackColor` convertColor color
        convertStyle' attr TxstBold = attr `VA.withStyle` VA.bold
        convertStyle' attr TxstUnderline = attr `VA.withStyle` VA.underline

-- | Convert a color into a Vty ANSI color.
convertColor :: Int -> VA.Color
convertColor 0 = VA.brightWhite
convertColor 1 = VA.brightBlack
convertColor 2 = VA.blue
convertColor 3 = VA.green
convertColor 4 = VA.red
convertColor 5 = VA.brightRed
convertColor 6 = VA.magenta
convertColor 7 = VA.yellow
convertColor 8 = VA.brightYellow
convertColor 9 = VA.brightGreen
convertColor 10 = VA.cyan
convertColor 11 = VA.brightCyan
convertColor 12 = VA.brightBlue
convertColor 13 = VA.brightMagenta
convertColor 14 = VA.black
convertColor 15 = VA.white
convertColor _ = VA.white
