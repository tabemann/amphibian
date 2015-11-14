module Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow

       (VtyWindow,
        enterChar,
        moveLeft,
        moveRight,
        prevInput,
        nextInput,
        scrollPrev,
        scrollNext)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend as VF
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import Control.Concurrent.STM (STM,
                               TVar
                               readTVar,
                               writeTVar)
import qualified Data.Text as T
import qualified Data.Sequence as S

-- | Enter a character in the input for a window.
enterChar :: VtyWindow -> Char -> STM ()
enterChar vtyWindow char = do
  inputText <- readTVar $ vtwiInputText vtyWindow
  inputCursorPosition <- readTVar $ vtwiInputCursorPosition vtyWindow
  writeTVar (vtwiInputText vtyWindow) $ ST.insertUnstyled inputCursorPosition (T.singleton char) inputText
  moveRight vtyWindow

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
            width <- readTVar . vtfrWidth $ vtwiFrontend vtyWindow
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
    else return ()

-- | Scroll to the previous half-window height.
scrollPrev :: VtyWindow -> STM ()
scrollPrev vtyWindow = do
  bufferLines <- readTVar $ vtwiBufferLines vtyWindow
  bufferPosition <- readTVar $ vtwiBufferPosition vtyWindow
  scrollHeight <- VF.getScrollHeight $ vtwiFrontend vtyWindow
  newPosition <- case bufferPosition of
    VtbpFixed position -> findScroll vtyWindow bufferLines position 0 (scrollHeight `div` 2) scrollHeight
    VtbpDynamic -> findScroll vtyWindow bufferLines 0 0 ((scrollHeight `div` 2) + scrollHeight) scrollHeight
  newPosition' <- normalizeBufferPosition vtyWindow newPosition
  writeTVar (vtwiBufferPosition vtyWindow) newPosition'
  where findScroll vtyWindow bufferLines bufferPosition totalHeight checkHeight scrollHeight =
          if bufferPosition < S.length bufferLines - 1
          then do
            lineHeight <- VF.formatLine (vtwiFrontend vtyWindow) (S.index bufferLines bufferPosition)
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
  scrollHeight <- VF.getScrollHeight $ vtwiFrontend vtyWindow
  newPosition <- case bufferPosition of
    VtbpFixed position -> findScroll vtyWindow bufferLines position 0 scrollHeight
    VtbpDynamic -> return VtbpDynamic
  newPosition' <- normalizeBufferPosition vtyWindow newPosition
  writeTVar (vtwiBufferPosition vtyWindow) newPosition'
  where findScroll vtyWindow bufferLines bufferPosition totalHeight scrollheight =
          if bufferPosition > 0
          then do
            lineHeight <- VF.formatLine (vtwiFrontend vtyWindow) (S.index bufferLines bufferPosition)
            if (totalHeight + lineHeight) < scrollHeight `div` 2
              then findScroll vtyWindow bufferLines (bufferPosition - 1) (totalHeight + lineHeight) scrollHeight
              else return $ VtbpFixed bufferPosition
          else return VtbpDynamic

-- | Normalize window buffer position.
normalizeBufferPosition :: VtyWindow -> VtyBufferPosition -> STM VtyBufferPosition
normalizeBufferPosition vtyWindow (VtbpFixed position) = do
  bufferLines <- readTVar $ vtwiBufferLines vtyWindow
  scrollHeight <- VF.getScrollHeight $ vtwiFrontend vtyWindow
  hasRoom <- checkHeight vtyWIndow bufferLines position 0 scrollHeight
  if hasRoom
    then return $ VtbpFixed position
    else return VtbpDynamic
  where checkHeight vtyWindow bufferLines bufferPosition totalHeight scrollHeight =
          if totalHeight > scrollHeight
          then
            if bufferPosition >= 0
            then do
              lineHeight <- VF.formatLine (vtwiFrontend vtyWindow) (S.index bufferLines bufferPosition)
              checkHeight vtyWindow bufferLines (bufferPosition - 1) (totalHeight + lineHeight) scrollHeight
            else return False
          else return True
normalizeBufferPosition _ VtbpDynamic = return VtbpDynamic
