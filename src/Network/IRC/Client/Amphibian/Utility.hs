-- Copyright (c) 2017, Travis Bemann
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
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Network.IRC.Client.Amphibian.Utility

  (Response,
   Error(..),
   getResponse,
   tryGetResponse,
   byteOfChar,
   splitOnSpaces,
   userTypePrefix,
   ourDecodeUtf8,
   asyncHandleResponse,
   syncHandleResponse,
   displayError,
   filterMessageText,
   matchByteStringGlob)

where

import Network.IRC.Client.Amphibian.Types
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Sequence (ViewL(..),
                      ViewR(..))
import Data.Text.Encoding (encodeUtf8,
                           decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Text.Printf (printf)
import Control.Concurrent.Async (Async,
                                 async)
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Concurrent.STM.TMVar (readTMVar,
                                     tryReadTMVar)
import System.IO (stderr)
import Data.Text.IO (hPutStr)
import Data.Char (isSpace)

-- | Get a response.
getResponse :: Response a -> STM (Either Error a)
getResponse (Response response) = readTMVar response

-- | Try to get a response.
tryGetResponse :: Response a -> STM (Maybe (Either Error a))
tryGetResponse (Response response) = tryReadTMVar response

-- | Get byte of char.
byteOfChar :: Char -> Word8
byteOfChar char = B.head . encodeUtf8 $ T.pack [char]

-- | Split a bytestring on one or more spaces.
splitOnSpaces :: B.ByteString -> (B.ByteString, Maybe B.ByteString)
splitOnSpaces bytes =
  let (part, rest) = B.break (== byteOfChar ' ') bytes
      rest' = B.dropWhile (== byteOfChar ' ') rest
  in if B.length rest' > 0
     then (part, Just rest')
     else (part, Nothing)

-- | Get prefix character for user type.
userTypePrefix :: UserType -> T.Text
userTypePrefix OwnerUser = "~"
userTypePrefix AdminUser = "&"
userTypePrefix OpUser = "@"
userTypePrefix HalfOpUser = "%"
userTypePrefix VoiceUser = "+"
userTypePrefix NormalUser = ""

-- | Our UTF-8 decoder.
ourDecodeUtf8 :: B.ByteString -> T.Text
ourDecodeUtf8 = decodeUtf8With lenientDecode

-- | Asynchronously handle response.
asyncHandleResponse :: Response a -> IO ()
asyncHandleResponse response = do
  async $ do
    result <- atomically $ getResponse response
    case result of
      Right _ -> return ()
      Left (Error errorText) -> displayError errorText
  return ()

-- | Synchronously handle response.
syncHandleResponse :: Response a -> IO ()
syncHandleResponse response = do
  result <- atomically $ getResponse response
  case result of
    Right _ -> return ()
    Left (Error errorText) -> displayError errorText

-- | Display an error.
displayError :: T.Text -> IO ()
displayError = hPutStr stderr . T.pack . printf "%s\n"

-- | Filter message text to eliminate passwords and like.
filterMessageText :: B.ByteString -> T.Text -> T.Text
filterMessageText nickOrName text =
  if nickOrName == encodeUtf8 "NickServ"
  then
    let (part0, part1) = T.span isSpace text
        (part1', part2) = T.span (not . isSpace) part1
        (part2', part3) = T.span isSpace part2
        (part3', part4) = T.span (not . isSpace) part3
        (part4', part5) = T.span isSpace part4
    in if part1' == "identify"
       then T.concat [part0, part1', part2', "****"]
       else if part1' == "release"
            then T.concat [part0, part1', part2', part3', part4', "****"]
            else text
  else text

-- | Match a bytestring against a glob.
matchByteStringGlob :: B.ByteString -> B.ByteString -> Bool
matchByteStringGlob glob matched =
  let parts = S.fromList $ B.split (byteOfChar '*') glob
  in case S.viewl parts of
    first :< parts ->
      case B.stripPrefix first matched of
        Just matched ->
          case S.viewr parts of
            parts :> last ->
              case B.stripSuffix last matched of
                Just matched -> matchParts parts matched
                Nothing -> False
            EmptyR -> B.null matched
        Nothing -> False
    EmptyL -> error "impossible"
  where matchParts parts matched =
          case S.viewl parts of
            first :< parts ->
              let (_, matchedPart) = B.breakSubstring first matched
              in case B.stripPrefix first matchedPart of
                Just matched -> matchParts parts matched
                Nothing -> False
            EmptyL -> True
