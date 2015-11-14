module Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend

       (VtyFrontend,
        VtyWindow,
        VtyBufferPosition,
        VtyKeyHandler,
        VtyUnmappedKeyHandler,
        getCurrentWindow,
        getScrollHeight,
        formatLine,
        new,
        start,
        registerKeyHandler,
        unregisterKeyHandler,
        registerUnmappedKeyHandler,
        unregisterUnmappedKeyHandler,
        redraw)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frontend as F
import Control.Monad (join)
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
import qualified Graphics.Vty.Attributes as VA
import qualified Data.Map.Strict as M

-- | Get current Vty frontend window.
getCurrentWindow :: VtyFrontend -> STM (Maybe VtyWindow)
getCurrentWindow = readTVar . vtfrCurrentWindow

-- | Get buffer height.
getScrollHeight :: VtyFrontend -> STM Int
getScrollHeight vtyFrontend = do
  height <- readTVar $ vtfrHeight vtyFrontend
  if height > 4
    then return $ height - 4
    else return 0

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

-- | Handle Vty event.
handleVty :: VtyFrontend -> STM (AM Bool)
handleVty vtyFrontend = do
  vty <- readTVar $ vtfrVty vtyFrontend
  case vty of
    Just vty -> do
      event <- readTChan . VI._eventChannel $ V.inputIFace vty
      case event of
        VI.EvKey key modifiers -> handleInput vtyFrontend $ VtyKeyCombination key modifiers
        VI.EvResize _ _ -> return $ do
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

-- | Convert a style into a Vty style.
convertStyle :: [TextStyle] -> VA.Attr
convertStyle style = convertStyle' style VA.defAttr
  where convertStyle' (TxstColor color : rest) attr = convertStyle' rest (attr `VA.withForeColor` convertColor color)
        convertStyle' (TxstBold : rest) attr = convertStyle' rest (attr `VA.withStyle` VA.bold)
        convertStyle' (TxstUnderline : rest) attr = convertStyle' rest (attr `VA.withStyle` VA.underline)
        convertStyle' [] attr = attr

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
