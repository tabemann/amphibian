module Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow

       (VtyWindow,
        VtyWindowStartResponse,
        new,
        start,
        waitStart,
        enterChar,
        enterLine,
        moveLeft,
        moveRight,
        prevInput,
        nextInput,
        scrollPrev,
        scrollNext)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend as VF
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar,
                               atomically,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newEmptyTMVar,
                               readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      writeTQueue)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Sequence ((<|))

-- | Create a new window.
new :: VtyFrontend -> Frame -> STM VtyWindow
new vtyFrontend frame = do
  subscription <- F.subscribeOutput frame
  active <- newTVar Bool
  bufferLines <- newTVar S.empty
  bufferPosition <- newTVar VtbpDynamic
  prevInputBufffer <- newTVar S.empty
  prevInputPosition <- newTVar (-1)
  inputText <- newTVar ST.empty
  inputCursorPosition <- newTVar 0
  inputVisiblePosition <- newTVar 0
  return $ VtyWindow { vtwiFrontend = vtyFrontend,
                       vtwiFrame = frame,
                       vtwiSubscription = subscription,
                       vtwiActive = active,
                       vtwiBufferLines = bufferLines,
                       vtwiBufferPosition = bufferPosition,
                       vtwiPrevInputBuffer = prevInputBuffer,
                       vtwiPrevInputPosition = prevInputPosition,
                       vtwiInputText = inputText,
                       vtwiInputCursorPosition = inputCursorPosition,
                       vtwiInputVisiblePosition = inputVisiblePosition }

-- | Start handling frame output events for a window.
start :: VtyWindow -> STM ()
start vtyWIndow = do
  response <- newEmptyTMVar
  let response' = VtyWindowStartResponse response
  windowServer <- VF.getWindowServer $ vtwiFrontend vtyWindow
  case windowServer of
   Just windowServer -> writeTQueue (vtwsActions windowServer) $ VtwsStartWindow vtyWindow response'
   Nothing -> do
     errorText <- I.lookupText (VF.getInterface $ vtwiFrontend vtyWIndow) $ T.pack "Window server is not registered"
     putTMVar response . Left $ Error [errorText]
  return response'

-- | Wait for response to starting handling frame output events for a window.
waitStart :: VtyWindowStartResponse -> STM (Either Error ())
waitStart (VtyWindowStartResponse resonse) = readTMVar response

-- | Enter a character in the input for a window.
enterChar :: VtyWindow -> Char -> STM ()
enterChar vtyWindow char = do
  inputText <- readTVar $ vtwiInputText vtyWindow
  inputCursorPosition <- readTVar $ vtwiInputCursorPosition vtyWindow
  writeTVar (vtwiInputText vtyWindow) $ ST.insertUnstyled inputCursorPosition (T.singleton char) inputText
  moveRight vtyWindow

-- | Enter a line in the input for a window.
enterLine :: VtyWindow -> STM ()
enterLine vtyWindow = do
  prevInputBuffer <- readTVar $ vtwiPrevInputBuffer vtyWindow
  inputText <- readTVar $ vtwiInputText vtyWindow
  writeTVar (vtwiPrevInputBuffer vtyWindow) (inputText <| prevInputBuffer)
  writeTVar (vtwiPrevInputPosition vtyWindow) (-1)
  writeTVar (vtwiInputText vtyWindow) ST.empty
  writeTVar (vtwiInputCursorPosition vtyWindow) 0
  writeTVar (vtwiInputVisiblePosition vtyWindow) 0
  F.inputLine (vtwiFrame vtyWindow) inputText

-- | Move left in the input for a window.
moveLeft :: VtyWindow -> STM ()
moveLeft vtyWindow = do
  inputCursorPosition <- readTVar $ vtwiInputCursorPosition vtyWindow
  if inputCursorPosition > 0
    then do inputVisiblePosition <- readTVar $ vtwiInputVisiblePosition vtyWindow
            writeTVar (vtwiInputCursorPosition vtyWindow) (inputCursorPosition - 1)
            if (inputCursorPosition - 1) < vtwInputVisiblePosition
              then do writeTVar (vtwiInputVisiblePosition vtyWindow) (inputCursorPosition - 1)
              else return ()
    else return ()

-- | Move right in the input for a window.
moveRight :: VtyWindow -> STM ()
moveRight vtyWindow = do
  inputText <- readTVar $ vtwiInputText vtyWindow
  if inputCursorPosition < (ST.length inputText) - 1
    then do inputVisiblePosition <- readTVar $ vtwiInputVisiblePosition vtyWindow
            width <- VF.getWidth $ vtwiFrontend vtyWindow
            writeTVar (vtwiInputCursorPosition vtyWindow) (inputCursorPosition + 1)
            if (inputCursorPosition + 1) > (vtwInputVisiblePosition + width - 1)
              then do writeTVar (vtwiInputVisiblePosition vtyWindow) (inputCursorPosition - (width - 1))
              else return()
    else return ()

-- | Move to the previous input.
prevInput :: VtyWindow -> STM ()
prevInput vtyWindow = do
  prevInputPosition <- readTVar $ vtwiPrevInputPosition vtyWindow
  prevInputBuffer <- readTVar $ vtwiPrevInputBuffer vtyWindow
  if prevInputPosition < S.length prevInputBuffer - 1
    then do
    writeTVar (vtwiPrevInputPosition vtyWindow) $ prevInputPosition + 1
    writeTVar (vtwiInputText vtyWindow) $ S.index prevInputBuffer (prevInputPosition + 1)
    writeTVar (vtwiInputCursorPosition vtyWindow) 0
    writeTVar (vtwiInputVisiblePosition vtyWindow) 0
    else return ()

-- | Move to the next input.
nextInput :: VtyWindow -> STM ()
nextInput vtyWindow = do
  prevInputPosition <- readTVar $ vtwiPrevInputPosition vtyWindow
  prevInputBuffer <- readTVar $ vtwiPrevInputBuffer vtyWindow
  if prevInputPosition > 0
    then do
    writeTVar (vtwiPrevInputPosition vtyWindow) $ prevInputPosition - 1
    writeTVar (vtwiInputText vtyWindow) $ S.index prevInputBuffer (prevInputPosition - 1)
    writeTVar (vtwiInputCursorPosition vtyWindow) 0
    writeTVar (vtwiInputVisiblePosition vtyWindow) 0
    else if prevInputPosition == 0
         then do
           writeTVar (vtwiPrevInputPosition vtyWindow) (-1)
           writeTVar (vtwiInputText vtyWindow) ST.empty
           writeTVar (vtwiInputCursorPosition vtyWindow) 0
           writeTVar (vtwiInputVisiblePosition vtyWindow) 0
         else return ()

-- | Scroll to the previous half-window height.
scrollPrev :: VtyWindow -> STM ()
scrollPrev vtyWindow = do
  bufferLines <- readTVar $ vtwiBufferLines vtyWindow
  bufferPosition <- readTVar $ vtwiBufferPosition vtyWindow
  width <- VF.getWidth $ vtwiFrontend vtyWindow
  scrollHeight <- VF.getScrollHeight (vtwiFrontend vtyWindow) vtyWindow
  newPosition <- case bufferPosition of
    VtbpFixed position -> findScroll vtyWindow bufferLines position 0 (scrollHeight `div` 2) scrollHeight
    VtbpDynamic -> findScroll vtyWindow bufferLines 0 0 ((scrollHeight `div` 2) + scrollHeight) scrollHeight
  newPosition' <- normalizeBufferPosition vtyWindow newPosition
  writeTVar (vtwiBufferPosition vtyWindow) newPosition'
  where findScroll vtyWindow bufferLines bufferPosition totalHeight checkHeight scrollHeight =
          if bufferPosition < S.length bufferLines - 1
          then do
            lineHeight <- length . VF.breakLines width <$> VF.formatLine (vtwiFrontend vtyWindow)
                          (S.index bufferLines bufferPosition)
            if (totalHeight + lineHeight) < checkHeight
              then findScroll vtyWindow bufferLines (bufferPosition + 1) (totalHeight + lineHeight) checkHeight
                   scrollHeight
              else return $ VtbpFixed bufferPosition
          else return $ VtbpFixed bufferPosition

-- | Scroll to the next half-window height.
scrollNext :: VtyWindow -> STM ()
scrollNext vtyWindow = do
  bufferLines <- readTVar $ vtwiBufferLines vtyWindow
  bufferPosition <- readTVar $ vtwiBufferPosition vtyWindow
  width <- VF.getWidth $ vtwiFrontend vtyWindow
  scrollHeight <- VF.getScrollHeight (vtwiFrontend vtyWindow) vtyWindow
  newPosition <- case bufferPosition of
    VtbpFixed position -> findScroll vtyWindow bufferLines position 0 scrollHeight
    VtbpDynamic -> return VtbpDynamic
  newPosition' <- normalizeBufferPosition vtyWindow newPosition
  writeTVar (vtwiBufferPosition vtyWindow) newPosition'
  where findScroll vtyWindow bufferLines bufferPosition totalHeight scrollheight =
          if bufferPosition > 0
          then do
            lineHeight <- length . VF.breakLines width <$> VF.formatLine (vtwiFrontend vtyWindow)
                          (S.index bufferLines bufferPosition)
            if (totalHeight + lineHeight) < scrollHeight `div` 2
              then findScroll vtyWindow bufferLines (bufferPosition - 1) (totalHeight + lineHeight) scrollHeight
              else return $ VtbpFixed bufferPosition
          else return VtbpDynamic

-- | Normalize window buffer position.
normalizeBufferPosition :: VtyWindow -> VtyBufferPosition -> STM VtyBufferPosition
normalizeBufferPosition vtyWindow (VtbpFixed position) = do
  bufferLines <- readTVar $ vtwiBufferLines vtyWindow
  width <- VF.getWidth $ vtwiFrontend vtyWindow
  scrollHeight <- VF.getScrollHeight (vtwiFrontend vtyWindow) vtyWindow
  hasRoom <- checkHeight vtyWIndow bufferLines position 0 scrollHeight
  if hasRoom
    then return $ VtbpFixed position
    else return VtbpDynamic
  where checkHeight vtyWindow bufferLines bufferPosition totalHeight scrollHeight =
          if totalHeight > scrollHeight
          then
            if bufferPosition >= 0
            then do
              lineHeight <- length . VF.breakLines width <$> VF.formatLine (vtwiFrontend vtyWindow)
                            (S.index bufferLines bufferPosition)
              checkHeight vtyWindow bufferLines (bufferPosition - 1) (totalHeight + lineHeight) scrollHeight
            else return False
          else return True
normalizeBufferPosition _ VtbpDynamic = return VtbpDynamic
