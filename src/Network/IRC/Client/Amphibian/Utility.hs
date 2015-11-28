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

module Network.IRC.Client.Amphibian.Utility
       
       (extractNick,
        convertAddRemove,
        isError,
        unique,
        parseChannelNameOrNick,
        decodeFrame)

       where

import Network.IRC.Client.Amphibian.Types
import Text.Read (readMaybe)
import qualified Data.ByteString.UTF8 as BUTF8

-- | Extract nick.
extractNick :: Maybe MessagePrefix -> Maybe Nick
extractNick (Just prefix) = Just . first $ BUTF8.break (== '!') prefix
extractNick Nothing = Nothing

-- | Convert add/remove into a bytestring.
convertAddRemove :: AddRemove -> B.ByteString
convertAddRemove Add = "+"
convertAddRemove Remove = "-"

-- | Check whether command corresponds to an error
isError :: MessageCommand -> Bool
isError command =
  case readMaybe (BUTF8.toString command) :: Maybe Integer of
    Just numeric -> numeric >= 400
    Nothing -> False

-- | Make a list containing all the unique entries of a list, favoring the first unique entries in the list.
unique :: Eq a => [a] -> [a]
unique (x : xs) = x : unique (filter (/= x) xs)
unique [] = []

-- | Parse channel name or nick.
parseChannelNameOrNick :: ByteString -> ChannelNameOrNick
parseChannelNameOrNick channelNameOrNick =
  case BUTF8.uncons channelNameOrNick of
    Just ('#', _) -> CnonChannelName channelNameOrNick
    _ -> CnonNick channelNameOrNick

-- | Decode text for a frame.
decodeFrame :: Interface -> Frame -> B.ByteString -> STM T.Text
decodeFrame intf frame bytes =
  manager <- F.getConnectionManager frame
  case manager of
    Just manager -> do
      config <- I.getConnectionConfig intf manager
      case config of
        Just config -> return $ (encoDecoder $ cocoEncoding config) bytes
        Nothing -> I.lookupText intf "NO CONNECTION CONFIG SET"
    Nothing -> I.lookupText intf "NO CONNECTION MANAGER SET"
