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
        encodeFrame,
        decodeFrame,
        waitM,
        findM,
        uncons,
        findOrCreateConnectionManager,
        findChannel,
        findOrCreateUser,
        findOrCreateConnectionFrame,
        findOrCreateChannelFrame,
        findOrCreateUserFrame)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.Channel as C
import qualified Network.IRC.Client.Amphibian.User as U
import qualified Network.IRC.Client.Amphibian.Frame as F
import Text.Read (readMaybe)
import qualified Data.ByteString.UTF8 as BUTF8
import Data.Functor ((<$>))
import Control.Monad ((=<<))

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

-- | Encode text for a frame.
encodeFrame :: Interface -> Frame -> T.Text -> STM B.ByteString
encodeFrame intf frame text =
  manager <- F.getConnectionManager frame
  case manager of
    Just manager -> do
      config <- I.getConnectionConfig intf manager
      case config of
        Just config -> return $ (encoEncoder $ cocoEncoding config) text
        Nothing -> return B.empty
    Nothing -> return B.empty

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

-- | Wait for an STM monad to return a value meeting a condition
waitM :: Monad m => (a -> Bool) -> m a -> m ()
waitM f m = do
  x <- m
  if not $ f x
    then waitMonad f m
    else return ()

-- | Find a member of a list to meet a criterion that returns a monad.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f (x : xs) = do
  match <- f x
  if not match
    then findM f xs
    else return $ Just x
findM _ [] = return Nothing

-- | Uncons.
uncons :: [a] -> (Maybe a, [a])
uncons (x : xs) = (Just a, xs)
uncons [] = (Nothing, [])

-- | Find or create connection manager.
findOrCreateConnectionManager :: Interface -> ServerName -> STM ConnectionManager
findOrCreateConnectionManager intf serverName = do
  manager <- findM (matchConnectionManager serverName) =<< I.getConnectionManagers intf
  case manager of
   Just manager -> return manager
   Nothing -> do
     manager <- CM.new intf
     CM.start manager
     return manager
  where matchConnectionManager serverName manager = (== serverName) <$> comaServerName <$> CM.getSetup manager

-- | Find channel.
findChannel :: Interface -> ConnectionManager -> ChannelName -> STM (Maybe Channel)
findChannel intf manager name = findM (matchChannel manager name) =<< I.getChannels intf
  where matchChannel manager name channel = do
          manager' <- C.getConnectionManager channel
          name' <- C.getName channel
          return $ manager == manager' && name == name'

-- | Find or create user.
findOrCreateUser :: Interface -> ConnectionManager -> Nick -> STM User
findOrCreateUser intf manager nick = do
  user <- findM (matchUser manager nick) =<< I.getUsers intf
  case user of
   Just user -> return user
   Nothing -> do
     user <- U.new intf manager nick
     U.start user
     return user
  where matchUser manager nick user = do
          manager' <- U.getConnectionManager user
          nick' <- U.getNick user
          return $ manager == manager' && nick == nick'

-- | Find or create connection manager frame.
findOrCreateConnectionFrame :: Interface -> ConnectionManager -> STM Frame
findOrCreateConnectionFrame intf manager = do
  frame <- findM (matchFrame manager) =<< I.getFrames intf
  case frame of
   Just frame -> return frame
   Nothing -> do
     frame <- F.new intf Nothing (FrmaConnectionManager manager)
     F.start frame
     return frame
  where matchFrame manager frame = do
          mapping <- F.getMapping frame
          case mapping of
           FrmaConnectionManager manager' -> return $ manager == manager'
           _ -> return False

-- | Find or create channel frame.
findOrCreateChannelFrame :: Interface -> Channel -> STM Frame
findOrCreateChannelFrame intf channel = do
  frame <- findM (matchFrame channel) =<< I.getFrames intf
  case frame of
   Just frame -> return frame
   Nothing -> do
     manager <- C.getConnectionManager channel
     parentFrame <- findOrCreateConnectionFrame intf manager
     frame <- F.new intf (Just parentFrame) (FrmaChannel channel)
     F.start frame
     return frame
  where matchFrame channel frame = do
          mapping <- F.getMapping frame
          case mapping of
           FrmaChannel channel' -> return $ channel == channel'
           _ -> return False

-- | Find or create user frame.
findOrCreateUserFrame :: Interface -> User -> STM Frame
findOrCreateUserFrame intf user = do
  frame <- findM (matchFrame user) =<< I.getFrames intf
  case frame of
   Just frame -> return frame
   Nothing -> do
     manager <- U.getConnectionManager user
     parentFrame <- findOrCreateConnectionFrame intf manager
     frame <- F.new intf (Just parentFrame) (FrmaUser user)
     F.start frame
     return frame
  where matchFrame user frame = do
          mapping <- F.getMapping frame
          case mapping of
           FrmaUser user' -> return $ user == user'
           _ -> retutnr False
