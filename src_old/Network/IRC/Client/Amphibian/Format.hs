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

module Network.IRC.Client.Amphibian.Format

       (format)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.Text as T
import Data.String (IsString)
import Text.Printf
import qualified Data.HashMap.Strict as HM

-- | Carry out formatting.
format :: T.Text -> FormatMap -> T.Text
format formatString map = format' formatString map T.empty
  where format' formatString map acc =
          let (before, after) = T.breakOn "%" formatString in
          case T.uncons after of
            Just ('%', rest) ->
              let acc = T.append acc before in
              case T.uncons rest of
                Just ('%', rest) -> format' rest map (T.snoc acc '%')
                _ -> let (before, after) = T.breakOn ":" rest in
                     case T.uncons after of
                       Just (':', rest) ->
                         case HM.lookup before map of
                           Just formatter -> let (formatted, rest) = formatter rest in
                                             format' rest map (T.append acc formatted)
                           Nothing -> format' rest map (T.append acc mapEntryNotFound)
                       _ -> acc
            _ -> T.append acc before

-- | Map entry not found string.
mapEntryNotFound :: T.Text
mapEntryNotFound = "<MAP ENTRY NOT FOUND>"

-- | Bad formatting string.
badFormatting :: T.Text
badFormatting = "<BAD FORMATTING STRING>"

-- | Format a string.
formatString :: String -> T.Text -> (T.Text, T.Text)
formatString x formatting =
  case parseFormat formatting ParseString of
    Just (formatString, rest) -> (T.pack $ printf formatString x, rest)
    Nothing -> (badFormatting, formatting)

-- | Format text.
formatText :: T.Text -> T.Text -> (T.Text, T.Text)
formatText x formatting =
  case parseFormat formatting ParseString of
    Just (formatString, rest) -> (T.pack . printf formatString $ T.unpack x, rest)
    Nothing -> (badFormatting, formatting)

-- | Format an integer.
formatIntegral :: (Integral a) => a -> T.Text -> (T.Text, T.Text)
formatIntegral x formatting =
  case parseFormat formatting ParseIntegral of
    Just (formatString, rest) -> (T.pack $ printf formatString x, rest)
    Nothing -> (badFormatting, formatting)

-- | Format a floating-point value.
formatRealFloat :: (RealFloat a, Show a) => a -> T.Text -> (T.Text, T.Text)
formatRealFloat x formatting =
  case parseFormat formatting ParseRealFloat of
    Just (formatString, rest) -> (T.pack $ printf formatString x, rest)
    Nothing -> (badFormatting, formatting)

-- | Parsing type
data ParseType = ParseString | ParseIntegral | ParseRealFloat

-- | Parse a numeric formatting string
parseFormat :: T.Text -> ParseType -> Maybe (String, T.Text)
parseFormat formatting parseType =
  let (formatString, rest) = getInitial formatting [] in
  let (formatString, rest) = getFieldWidth rest formatString in
  let (formatString, rest) = getPrecision rest formatString in
  case parseType of
    ParseString -> parseStringSpecifier rest formatString
    ParseIntegral ->
      let (formatString, rest) = getWidthModifier rest formatString in
      parseIntegralSpecifier rest formatString
    ParseRealFloat -> parseRealFloatSpecifier rest formatString
  where 
    getInitial formatting = getInitial formatting []
    getInitial formatting formatString =
      case T.uncons formatting of
        Just (char, rest) | char `elem` ['-', '+', ' ', '0', '#'] -> getInitial rest (char : formatString)
        _ -> (reverse formatString, formatting)
    getFieldWidth formatting formatString = getFieldWidth' formatting (reverse formatString)
    getFieldWidth' formatting formatString =
      case T.uncons formatting of
        Just (char, rest) | char `elem` ['0' .. '9'] -> getFieldWidth' rest (char : formatString)
        _ -> (reverse formatString, formatting)
    getPrecision formatting formatString = getPrecision' formatting (reverse formatString)
    getPrecision' formatting formatString =
      case T.uncons formatting of
        Just ('.', rest) -> getPrecisionValue rest ('.' : formatString)
        _ -> (reverse formatString, formatting)
    getPrecisionValue formatting formatString =
      case T.uncons formatting of
        Just (char, rest) | char `elem` ['0' .. '9'] -> getPrecisionValue rest (char : formatString)
        _ -> (reverse formatString, formatting)
    getWidthModifier formatting formatString = getWidthModifier' formatting (reverse formatString)
    getWidthModifier' formatting formatString =
      case T.uncons formatting of
        Just ('h', rest) ->
          case T.uncons rest of
            Just ('h', rest') -> (reverse ('h' : 'h' : formatString), rest')
            _ -> (reverse ('h' : formatString), rest)
        Just ('l', rest) ->
          case T.uncons rest of
            Just ('l', rest') -> (reverse ('l' : 'l' : formatString), rest')
            _ -> (reverse ('l' : formatString), rest)
        Just ('L', rest) -> (reverse ('L' : formatString), rest)
        _ -> (reverse formatString, formatting)
    parseStringSpecifier formatting formatString = parseStringSpecifier' formatting (reverse formatString)
    parseStringSpecifier' formatting formatString =
      case T.uncons formatting of
        Just (char, rest) | char `elem` ['s', 'v'] -> Just (reverse (char : formatString), rest)
        _ -> Nothing
    parseIntegralSpecifier formatting formatString = parseIntegralSpecifier' formatting (reverse formatString)
    parseIntegralSpecifier' formatting formatString =
      case T.uncons formatting of
        Just (char, rest) | char `elem` ['c', 'd', 'o', 'x', 'X', 'b', 'u', 'v'] ->
          Just (reverse (char : formatString), rest)
        _ -> Nothing
    parseRealFloatSpecifier formatting formatString = parseRealFloatSpecifier' formatting (reverse formatString)
    parseRealFloatSpecifier' formatting formatString =
      case T.uncons formatting of
        Just (char, rest) | char `elem` ['f', 'F', 'g', 'G', 'e', 'E', 'v'] ->
          Just (reverse (char : formatString), rest)
        _ -> Nothing
