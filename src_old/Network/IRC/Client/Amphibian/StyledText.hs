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

module Network.IRC.Client.Amphibian.StyledText

       (StyledText(..),
        StyledTextElement(..),
        TextStyle(..),
        TextColor,
        empty,
        addStyle,
        removeStyle,
        setStyle,
        mergeStyle,
        setBaseForeColor,
        setBaseBackColor,
        length,
        append,
        appendUnstyled,
        insertUnstyled,
        reverse,
        take,
        takeEnd,
        drop,
        dropEnd,
        takeWhile,
        dropWhile,
        splitAt,
        span,
        break,
        concat,
        intercalate,
        isForeColor,
        isBackColor,
        decode,
        encode)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.Text as T
import Text.Read (readMaybe)
import Text.Printf
import Data.List (find,
                  intercalate)

-- | Empty styled text.
empty :: StyledText
empty = StyledText []

-- | Attach a style to text.
addStyle :: [TextStyle] -> T.Text -> StyledText
addStyle style text = StyledText [StyledTextElement style text]

-- | Remove style from text.
removeStyle :: StyledText -> T.Text
removeStyle (StyledText xs) = T.intercalate T.empty $ map (\(StyledTextElement _ text) -> text) xs

-- | Set style for text.
setStyle :: [TextStyle] -> StyledText -> StyledText
setStyle style styledText = addStyle style . removeStyle

-- | Merge style into existing style for text.
mergeStyle :: [TextStyle] -> StyledText -> StyledText
mergeStyle mergedStyle (StyledText elements) = StyledText $ mergeStyle' elements
  where mergeStyle' (StyledTextElement style text : rest) =
          let style' = if any isForeColor mergedStyle then filter (not . isForeColor) style else style in
          let style = if any isBackColor mergedStyle then filter (not . isBackColor) style' else style' in
          let style' = if TxstBold `elem` mergedStyle then filter (/= TxstBold) style else style in
          let style = if TxstUnderline `elem` mergedStyle then filter (/= TxstUnderline) style' else style' in
          StyledTextElement (mergedStyle ++ style) text : mergeStyle' rest
        mergeStyle' [] = []  

-- | Set base foreground color for style.
setBaseForeColor :: TextColor -> StyledText -> StyledText
setBaseForeColor color (StyledText elements) = StyledText $ setBaseForeColor' color elements
  where setBaseForeColor' color (StyledTextElement style text : rest)
          | any isForeColor style = StyledTextElement style text : setBaseForeColor' color rest
          | otherwise = StyledTextElement (TxstForeColor color : style) text : setBaseForeColor' color rest
        setBaseForeColor' _ [] = []

-- | Set base background color for style.
setBaseBackColor :: TextColor -> StyledText -> StyledText
setBaseBackColor color (StyledText elements) = StyledText $ setBaseBackColor' color elements
  where setBaseBackColor' color (StyledTextElement style text : rest)
          | any isBackColor style = StyledTextElement style text : setBaseBackColor' color rest
          | otherwise = StyledTextElement (TxstBackColor color : style) text : setBaseColor' color rest
        setBaseColor' _  [] = []

-- | Get the length of styled text.
length :: StyledText -> Int
length (StyledText xs) = foldl' (\prev (StyledTextElement _ text) -> prev + T.length text) 0 xs

-- | Append two sections of styled text.
append :: StyledText -> StyledText -> StyledText
append (StyledText xs) (StyledText ys) = StyledText $ xs ++ ys

-- | Append unstyled text after styled text.
appendUnstyled :: StyledText -> T.Text -> StyledText
appendUnstyled (StyledText xs) text = StyledText $ appendUnstyled' xs text
  where appendUnstyled' (x : xs@(_ : _)) text = x : appendUnstyled' xs text
        appendUnstyled' (StyledTextElement style text : []) appendText =
          [StyledTextElement style $ T.append text appendedText]
        appendUnstyled' [] text = [StyledTextElement [] text]

-- | Insert unstyled text into styled text.
insertUnstyled :: Int -> T.Text -> StyledText -> StyledText
insertUnstyled insertIndex insertedText styledText =
  let (beforeStyledText, afterStyledText) = splitAt insertIndex styledText in
  append (appendUnstyled beforeStyledText insertedText) afterStyledText

-- | Reverse styled text.
reverse :: StyledText -> StyledText
reverse (StyledText xs) = reverse' xs []
  where reverse' (StyledTextElement style text : rest) prev =
          reverse' rest (StyledTextElement style (T.reverse text) : rest)
        reverse' [] prev = StyledText prev

-- | Take n characters from styled text.
take :: Int -> StyledText -> StyledText
take count (StyledText xs) = take' count xs []
  where take' count (x@(StyledTextElement style text) : rest) prev
          | count < T.length text =
              StyledText . reverse $ StyledTextElement style (T.take count text) : prev
          | otherwise = take' (count - T.length text) rest (x : prev)
        take' _  [] prev = StyledText $ reverse prev

-- | Take n characters from the end of styled text.
takeEnd :: Int -> StyledText -> StyledText
takeEnd count styledText = reverse . take count $ reverse styledText

-- | Drop n characters from styled text.
drop :: Int -> StyledText -> StyledText
drop count (StyledText xs) = drop' count xs
  where drop' count (StyledTextElement style text : rest)
          | count < T.length text =
            StyledText $ StyledTextElement style (T.drop count text) : rest
          | otherwise = drop' (count - T.length text) rest
        drop' _ [] = StyledText []

-- | Drop n characters from theend of styled text.
dropEnd :: Int -> StyledText -> StyledText
dropEnd count styledText = reverse . drop count $ reverse styledText

-- | Get the prefix fitting a predicate of styled text.
takeWhile :: (Char -> Bool) -> StyledText -> StyledText
takeWhile f (StyledText xs) = takeWhile' f xs []
  where takeWhile' f (this@(StyledTextElement style text) : rest) prev =
          let before = T.takeWhile f text in
          if T.length text /= T.length before
          then StyledText . reverse $ StyledTextElement style before : prev
          else takeWhile' f rest (this : prev)
        takeWhile' _ [] prev = StyledText $ reverse prev

-- | Get the remainder not fitting a predicate  of styled text.
dropWhile :: (Char -> Bool) -> StyledText -> StyledText
dropWhile f (StyledText xs) = dropWhile' f xs
  where dropWhile' f (this@(StyledTextElement style text) : rest) =
          let after = T.dropWhile f text in
          if T.length after > 0
          then StyledText $ StyledTextElement style after : rest
          else span' f rest
        dropWhile' _ [] = StyledText []

-- | Split styled text at a index.
splitAt :: Int -> StyledText -> (StyledText, StyledText)
splitAt index (StyledText xs) = splitAt' index xs []
  where splitAt' index (x@(StyledTextElement style text) : rest) prev
          | index < T.length text =
              let (before, after) = T.splitAt index text in
              (StyledText . reverse $ StyledTextElement style before : prev,
               StyledText $ StyledTextElement style after : rest)
          | otherwise = splitAt' (index - T.length text) rest (x : prev)
        splitAt' _ [] prev = (StyledText $ reverse prev, StyledText [])

-- | Get the prefix fitting a predicate and remainder of styled text.
span :: (Char -> Bool) -> StyledText -> (StyledText, StyledText)
span f (StyledText xs) = span' f xs []
  where span' f (this@(StyledTextElement style text) : rest) prev =
          let (before, after) = T.span f text in
          if T.length after > 0
          then (StyledText . reverse $ StyledTextElement style before : prev,
                StyledText $ StyledTextElement style after : rest)
          else span' f rest (this : prev)
        span' _ [] prev = (StyledText $ reverse prev, StyledText [])

-- | Get the prefix not fitting a predicate and remainder of styled text.
break :: (Char -> Bool) -> StyledText -> (StyledText, StyledText)
break f styledText = span (not . f) styledText

-- | Concatenate a list of styled text.
concat :: [StyledText] -> StyledText
concat xs = StyledText . concat $ map (\(StyledText ys) -> ys) xs

-- | Intercalate a list of styled text.
intercalate :: StyledText -> [StyledText] -> StyledText
intercalate (StyledText x) xs = StyledText . intercalate x $ map (\(StyledText ys) -> ys) xs

-- | Get whether a style element is a foreground color.
isForeColor :: TextStyle -> Bool
isForeColor (TxstForeColor _) = True
isForeColor _ = False

-- | Get whether a style element is a background color.
isBackColor :: TextStyle -> Bool
isBackColor (TxstBackColor _) = True
isBackColor _ = False

-- | Special encoded characters.
specialChars :: [Char]
specialChars = ['\x2', '\x1F', '\xF', '\x3']

-- | Decode styled text.
decode :: T.Text -> StyledText
decode text = decode' text [] []
  where decode' text style parts =
          case T.uncons text of
            Just (char, rest)
              | char == '\x2' ->
                  if TxstBold `notElem` style
                  then decode' rest (TxstBold : style) parts
                  else case parts of
                        (StyledTextElement _ lastPart) : otherParts ->
                          let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                          decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                        _ -> decode' rest style parts
              | char == '\x1F' ->
                  if TxstUnderline `notElem` style
                  then decode' rest (TxstUnderline : style) parts
                  else case parts of
                        (StyledTextElement _ lastPart) : otherParts ->
                          let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                          decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                        _ -> decode' rest style parts
              | char == '\xF' ->
                  case parts of
                   (StyledTextElement [] lastPart) : otherParts ->
                     let (part, rest') = T.break (\char -> char `elem` specialChars) rest in
                     decode' rest' [] (StyledTextElement [] (T.append lastPart part) : otherParts)
                   _ -> decode' rest [] parts
              | char == '\x3' ->
                  case T.uncons rest of
                   Just (char, rest')
                     | char == ',' -> decodeBackColor' rest style parts
                   _ -> decodeForeColor rest style parts
              | otherwise ->
                  let (part, rest) = T.break (`elem` specialChars) text in
                  decode' rest style (StyledTextElement style part : parts)
            Nothing -> StyledText $ reverse parts
        decodeForeColor text style parts =
          let (color, rest) = T.splitAt 2 text in
          case readMaybe color of
           Just color ->
             if color `elem` [0 .. 15]
             then
               if TxstForeColor color `notElem` style
               then decode' rest (TxstForeColor color : filter (not . isForeColor) style) parts
               else case parts of
                     StyledTextElement [] lastPart : otherParts ->
                       let (part, rest') = T.break (`elem` specialChars) rest in
                       decodeBackColor rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                     _ -> decodeBackColor rest style parts
             else decode' text style parts
           Nothing -> decode' rest style parts
        decodeBackColor text style parts =
          case T.cons text of
           Just (char, rest)
             | char ==  ',' -> decodeBackColor' rest style parts
           _ -> decode' text style parts
        decodeBackColor' text style parts =
          let (color, rest) = T.splitAt 2 text in
          case readMaybe color of
           Just color ->
             if color `elem` [0 .. 15]
             then
               if TxstBackColor color `notElem` style
               then decode' rest (TxstBackColor color : filter (not . isBackColor) style) parts
               else case parts of
                     StyledTextElement [] lastPart : otherParts ->
                       let (part, rest') = T.break (`elem` specialChars) rest in
                       decode' rest' style (StyledTextElement style (T.append lastPart part) : otherParts)
                     _ -> decode' rest style parts
             else decode' (T.append (T.singleton ',') text) style parts
           Nothing -> decode' (T.append (T.singleton ',') text) style parts

-- | Encode styled text.
encode :: StyledText -> T.Text
encode (StyledText parts) = encode' parts [] []
  where encode' ((StyledTextElement partStyle part) : rest) style encoded =
          if part /= T.empty
          then
            let (style', encoded') =
                  if ((TxstBold `elem` style) && (TxstBold `notElem` partStyle)) ||
                     ((TxstUnderline `elem` style) && (TxstUnderline `notElem` partStyle)) ||
                     (any isForeColor style && not (any isForeColor partStyle)) ||
                     (any isBackColor style && not (any isBackColor partStyle))
                  then ([], T.singleton '\xF' : encoded)
                  else (style, encoded) in
            let (style, encoded) =
                  if (TxstBold `elem` partStyle) && (TxstBold `notElem` style')
                  then (TxstBold : style', T.singleton '\x2' : encoded')
                  else (style', encoded') in
            let (style', encoded') =
                  if (TxstUnderline `elem` partStyle) && (TxstUnderline `notElem` style)
                  then (TxstUnderline : style, T.singleton '\x1F' : encoded)
                  else (style, encoded) in
            let (style, encoded, hasForeColor) =
                  case find isForeColor partStyle of
                   Just (TxstForeColor partStyleColor) ->
                     if partStyleColor `elem` [0 .. 15]
                     then
                       case find isForeColor style' of
                        Just (TxstForeColor styleColor) ->
                          if partStyleColor /= styleColor
                          then (TxstForeColor partStyleColor : filter (not . isForeColor) style',
                                T.pack (printf "\x3%02d" partStyleColor) : encoded', True)
                          else (style', encoded', False)
                        Nothing -> (TxstForeColor partStyleColor : filter (not . isForeColor) style',
                                    T.pack (printf "\x3%02d" partStyleColor) : encoded', True)
                     else (style', encoded', False)
                   Nothing -> (style', encoded', False) in
            let (style', encoded') =
                  case find isBackColor partStyle of
                   Just (TxstBackColor partStyleColor) ->
                     if partStyleColor `elem` [0 .. 15]
                     then
                       case find isBackColor style of
                        Just (TxstBackColor styleColor) ->
                          if partStyleColor /= styleColor
                          then
                            if hasForeColor
                            then (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                  T.pack (printf ",%02d" partStyleColor) : encoded)
                            else (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                  T.pack (printf "\x3,%02d" partStyleColor) : encoded)
                          else (style, encoded)
                        Nothing ->
                          if hasForeColor
                          then (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                T.pack (printf ",%02d" partStyleColor) : encoded)
                          else (TxstBackColor partStyleColor : filter (not . isBackColor) style,
                                T.pack (printf "\x3,%02d" partStyleColor) : encoded)
                     else (style, encoded)
                   Nothing -> (style, encoded) in
            encode' rest style' (part : encoded')
          else encode' rest style encoded
        encode' [] _ encoded = T.intercalate T.empty $ reverse encoded
