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

module Network.IRC.Client.Amphibian.CtcpHandler

       (installHandlers)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Commands
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.CtcpDispatcher as CD
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Monad ((=<<))
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock (UTCTime,
                        getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (TimeLocale)

-- | Install handlers.
installHandlers :: Interface -> STM ()
installHandlers intf = do
  dispatcher <- I.getCtcpDispatcher intf
  case dispatcher of
    Just dispatcher -> do
      CD.registerRequestHandler dispatcher ctcp_FINGER handleFinger
      CD.registerRequestHandler dispatcher ctcp_VERSION handleVersion
      CD.registerRequestHandler dispatcher ctcp_SOURCE handleSource
      CD.registerRequestHandler dispatcher ctcp_USERINFO handleUserInfo
      CD.registerRequestHandler dispatcher ctcp_PING handlePing
      CD.registerRequestHandler dispatcher ctcp_TIME handleTime
    Nothing -> return ()

-- | Handle FINGER CTCP request.
handleFinger :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool
handleFinger manager nick command Nothing
  | command == ctcp_FINGER = do
    liftIO . atomically $ do
      name <- comaName <$> CM.getSetup manager
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick],
                                     ircmComment = Just . formatCtcp ctcp_FINGER . Just $ B.append ":" name }
    return True
  | otherwise = return False
handleFinger manger nick command (Just _) = return False

-- | Handle VERSION CTCP request.
handleVersion :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool
handleVersion manager nick command Nothing
  | command == ctcp_VERSION = do
    intf <- getInterface
    liftIO . atomically $ do
      version <- encode intf manager =<< confCtcpVersion <$> I.getConfig intf
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick],
                                     ircmComment = Just . formatCtcp ctcp_VERSION $ Just version }
    return True
  | otherwise = return False
handleVersion manager nick command (Just _) = return False

-- | Handle SOURCE CTCP request.
handleSource :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool
handleSource manager nick command Nothing
  | command == ctcp_SOURCE = do
    intf <- getInterface
    liftIO . atomically $ do
      source <- encode intf manager =<< confCtcpSource <$> I.getConfig intf
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick],
                                     ircmComment = Just . formatCtcp ctcp_SOURCE $ Just source }
    return True
  | otherwise = return False
handleSource manager nick command (Just _) = return False

-- | Handle USERINFO CTCP request.
handleUserInfo :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool
handleUserInfo manager nick command Nothing
  | command == ctcp_USERINFO = do
    intf <- getInterface
    liftIO . atomically $ do
      userInfo <- encode intf manager =<< cocoCtcpUserInfo <$> I.getConnectionConfig intf manager
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick],
                                     ircmComment = Just . formatCtcp ctcp_USERINFO . Just $
                                       B.append (BC.singleton ':') userInfo }
    return True
  | otherwise = return False
handleUserInfo manager nick command (Just _) = return False

-- | Handle PING CTCP request.
handlePing :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool
handlePing manager nick command (Just argument)
  | command == ctcp_PING = do
    liftIO . atomically $
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick],
                                     ircmComment = Just . formatCtcp ctcp_PING $ Just argument }
    return True
  | otherwise = return False
handlePing manager nick command Nothing
  | command == ctcp_PING = do
    liftIO . atomically $
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick],
                                     ircmComment = Just $ formatCtcp ctcp_PING Nothing }
    return True
  | otherwise = return False

-- | Handle TIME CTCP request.
handleTime :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool
handleTime manager nick command Nothing
  | command == ctcp_TIME = do
    intf <- getInterface
    time <- liftIO getCurrentTime
    liftIO . atomically $
      timeLocale <- confTimeLocale <$> I.getConfig intf
      let timeData = encode intf manager . T.pack $ formatTime timeLocale "%c" time
      CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                     ircmCommand = cmd_NOTICE,
                                     ircmParameters = [nick]
                                     ircmComment = Just . formatCtcp ctcp_TIME $ Just timeData }
    return True
  | otherwise = return False
handleTime manager nick command (Just _) = return False

-- | Encode text sent to a connection manager.
encode :: Interface -> ConnectionManager -> T.Text -> STM B.ByteString
encode intf manager text = do
  config <- I.getConnectionConfig intf manager
  case config of
    Just config -> return $ (encoEncoder $ cocoEncoding config) text
    Nothing -> return B.empty
  Nothing -> return B.empty
