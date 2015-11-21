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

module Network.IRC.Client.Amphibian.Ctcp
       
       (checkCtcp,
        parseCtcp,
        formatCtcp,
        escapeCtcp,
        unescapeCtcp)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8
import Data.Char (isSpace)
import Data.Strings (byteToChar)

-- | Is a message or notice a CTCP request or reply?
checkCtcp :: MessageComment -> Maybe MessageComment
checkCtcp comment =
  case BUTF8.uncons $ unescapeCtcp comment of
    Just ('\x1', rest) ->
      let len = BUTF8.length rest of
      if len > 0
      then let (body, end) = BUTF8.splitAt (len - 1) rest in
           if end == '\x1'
           then Just body
           else Nothing
      else Nothing
    Nothing -> Nothing

-- | Parse CTCP request or response.
parseCtcp :: MessageComment -> Maybe (CtcpCommand, Maybe CtcpArgument)
parseCtcp comment =
  let (command, rest) = BUTF8.break isSpace comment in
  if command /= B.empty
  then case BUTF8.uncons rest of
    Just (_, argument) -> Just (command, Just argument)
    Nothing -> Just (command, Nothing)
  else Nothing

-- | Format CTCP request or response.
formatCtcp :: CtcpCommand -> Maybe CtcpArgument -> MessageComment
formatCtcp command (Just argument) = escapeCtcp $ B.concat ["\x1", command, " ", argument, "\x1"]
formatCtcp command nothing = escapeCtcp $ B.concat ["\x1", command, "\x1"]

-- | Escape CTCP data.
escapeCtcp :: B.ByteString -> MessageComment
escapeCtcp data =
  B.concat $ B.foldr' doEscape [] data
  where doEscape byte accum =
    case byteToChar byte of
      '\r' -> "\x10r" : accum
      '\n' -> "\x10n" : accum
      '\x0' -> B.append "\x10" "0" : accum
      '\x10' -> "\x10\x10": accum
      _ -> B.singleton byte : accum

-- | Unescape CTCP data.
unescapeCtcp :: MessageComment -> B.ByteString
unescapeCtcp comment = unescapeCtcp' comment []
  where unescapeCtcp' data accum =
    case B.uncons data of
      Just (byte, rest)
        | byteToChar byte == '\x10' ->
          case B.uncons rest of
            Just (byte, rest)
              | byteToChar byte == 'r' -> unescapeCtcp' rest ("\r" : accum)
              | byteToChar byte == 'n' -> unescapeCtcp' rest ("\n" : accum)
              | byteToChar byte == '0' -> unescapeCtcp' rest ("\x0" : accum)
              | byteToChar byte == '\x10' -> unescapeCtcp' rest ("\x10" : accum)
            _ -> unescapeCtcp' rest ("\x10" : accum)
        | otherwise -> unescapeCtcp' data (B.singleton byte : accum)
      Nothing -> B.concat $ reverse accum
