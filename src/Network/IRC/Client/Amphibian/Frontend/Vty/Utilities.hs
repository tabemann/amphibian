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

module Network.IRC.Client.Amphibian.Frontend.Vty.Utilities

       (breakLines,
        fitWidth)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import qualified Data.Text as T
import qualified Graphics.Vty.Image as VIm

-- | Break text into lines.
breakLines :: Int -> StyledText -> [StyledText]
breakLines width (StyledText styledText) = breakLines' width styledText 0 [] []
  where breakLines' width (StyledTextElement style part : rest) widthCount
          lineParts@(StyledTextElement lastStyle lastPart : lineRest) otherParts =
            case T.uncons part of
             Just (char, partRest) ->
               let charWidth = VIm.safeWcWidth char in
               if widthCount + charWidth <= width
               then if style == lastStyle
                    then breakLines' width (StyledTextElement style partRest : rest) (widthCount + charWidth)
                         (StyledTextElement lastStyle (T.snoc lastPart char) : lineRest) otherParts
                    else breakLines' width (StyledTextElement style partRest : rest) (widthCount + charWidth)
                         (StyledTextElement style (T.singleton char) : lineParts) otherParts
               else breakLines' width (StyledTextElement style partRest : rest) charWidth
                    [StyledTextElement style (T.singleton char)] (StyledText (reverse lineParts) : otherParts)
             Nothing -> breakLines' width rest widthCount lineParts otherParts
        breakLines' _ [] _ lineParts@(_ : _) otherParts = reverse $ Styledtext (reverse lineParts) : otherParts
        breakLines' _ [] _ [] otherParts = reverse otherParts

-- | Get width and length in characters of text that fit within a given width, or Nothing if characters do not
-- reach total width.
fitWidth :: Int -> T.Text -> Maybe (Int, Int)
fitWidth totalWidth text = fitWidth' totalWidth text 0 0
  where fitWidth' totalWidth text prevWidth prevLength =
          case T.uncons text of
           Just (char, rest) ->
             let charWidth = VIm.safeWcWidth char in
             if prevWidth + charWidth <= totalWidth
             then fitWidth' totalWidth rest (prevWidth + charWidth) (prevLength + 1)
             else Just (prevWidth, prevLength)
           Nothing
             | prevWidth == totalWidth -> Just (prevWidth, prevLength)
             | otherwise -> Nothing
