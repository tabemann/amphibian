-- Copyright (c) 2015, Travis Bemann
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
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow

       (VtyWindow,
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
import Network.IRC.Client.Amphibian.Frontend.Vty.Utilities
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
            if (inputCursorPosition - 1) < vtwiInputVisiblePosition
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
            writeTVar (vtwiInputCursorPosition vtyWindow) $ inputCursorPosition + 1
            case fitWidth width . T.drop inputVisiblePosition $ ST.removeStyle inputText of
             Just (_, visibleLength)
               | inputCursorPosition + 1 >= vtwiInputVisiblePosition + visibleLenth ->
                   let revInputText = T.drop ((ST.length inputText - (inputCursorPosition + 1)) - 1)
                       (T.reverse (ST.removeStyle inputText)) in
                   case fitWidth width revInputText of
                    Just (_, newVisibleLength) ->
                      writeTVar (vtwiInputVisiblePosition vtyWindow) (inputCursorPosition - (newVisibleLength - 1))
                    Nothing -> return ()
               | otherwise -> return ()
             Nothing -> return ()
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
