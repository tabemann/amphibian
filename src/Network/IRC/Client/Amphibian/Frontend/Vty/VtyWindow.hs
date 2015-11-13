module Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow

       (VtyWindow,
        enterChar,
        moveLeft,
        moveRight,
        prevInput,
        nextInput,
        scrollUp,
        scrollDown)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import Control.Concurrent.STM (STM,
                               TVar
                               readTVar,
                               writeTVar)

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
