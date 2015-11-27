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

module Network.IRC.Client.Amphibian.Command

       (OperResponse,
        OperEvent,
        oper,
        waitOper,
        NickResponse,
        NickEvent,
        nick,
        waitNick,
        ModeResponse,
        ModeEvent,
        mode,
        waitMode,
        QuitResponse,
        QuitEvent,
        quit,
        waitQuit,
        SquitResponse,
        SquitEvent,
        squit,
        waitSquit)
       where

import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Commands
import Network.IRC.Client.Amphibian.Utility
import Data.Functor ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (STM,
                               TMVar,
                               atomically,
                               newEmptyTMVar,
                               putTMVar,
                               readTMVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8

-- | Invoke the OPER command.
oper :: ConnectionManager -> Name -> Password -> AM OperResponse
oper manager name password = do
  response <- liftIO . atomically $ newEmptyTMVar
  registered <- liftIO . atomically $ CM.isRegistered manager
  if registered
  then do subscription <- liftIO . atomically $ CM.subscribe manager
          sendResponse <- liftIO . atomically . CM.send $ IRCCommand { ircmPrefix = Nothing,
                                                                       ircmCommand = cmd_OPER,
                                                                       ircmParameters = [name, password],
                                                                       ircmComment = Nothing }
          intf <- interface
          async $ runAM (handleOperResponse subscription sendResponse response) intf
  else liftIO . atomically $ putTMVar response OperNotRegistered
  return $ OperResponse response

-- | Wait for the response from OPER.
waitOper :: OperResponse -> STM OperEvent
waitOper (OperResponse response) = readTMVar response

-- | Handle the response to the OPER command.
handleOperResponse :: ConnectionManagerSubscription -> ConnectionManagerSendResponse -> TMVar OperEvent -> AM ()
handleOperResponse subscription sendResponse response = do
  sendResponse' <- liftIO . atomically $ waitSend sendResponse
  case sendResponse' of
    Right () -> handleOperResponse' subscription response
    Left error -> liftIO . atomically . putTMVar response $ OperError error
  where handleOperResponse' subscription response = do
          event <- liftIO . atomically $ CM.recv subscription
          case event of
            ComaMessage message@(IRCMessage { ircmCommand = command })
              | command == rpl_YOUROPER -> liftIO . atomically $ putTMVar response OperYourOper
              | command == err_NOOPERHOST -> liftIO . atomically $ putTMVar response OperNoOperHost
              | command == err_PASSWDMISMATCH -> liftIO . atomically $ putTMVar response OperPasswrdMismatch
              | command == err_NEEDMOREPARAMS -> liftIO . atomically $ putTMVar response OperNeedMoreParams
              | isError command -> liftIO . atomically $ putTMVar response (OperOther message)
            ComaDisconnected -> liftIO . atomically $ putTMVar response OperDisconnected
            _ -> handleOperResponse' subscription response

-- | Invoke the NICK command.
nick :: ConnectionManager -> Nick -> AM NickResponse
nick manager nick = do
  response <- liftIO . atomically $ newEmptyTMVar
  registered <- liftIO . atomically $ CM.isRegistered manager
  if registered
  then do subscription <- liftIO . atomically $ CM.subscribe manager
          oldNick <- liftIO . atomically $ CM.getNick manager
          sendResponse <- liftIO . atomically . CM.send $ IRCCommand { ircmPrefix = Nothing,
                                                                       ircmCommand = cmd_NICK,
                                                                       ircmParameters = [nick],
                                                                       ircmComment = Nothing }
          intf <- interface
          async $ runAM (handleNickResponse subscription oldNick sendResponse response) intf
  else liftIO . atomically $ putTMVar response NickNotRegistered
  return $ NickResponse response

-- | Wait for response from NICK.
waitNick :: NickResponse -> STM NickEvent
waitNick (NickResponse response) = readTMVar response
  
-- | Handle the response to the NICK command.
handleNickResponse :: ConnectionManagerSubscription -> Nick -> ConnectionManagerSendResponse -> TMVar NickEvent ->
                      AM ()
handleNickResponse subscription oldNick sendResponse response = do
  sendResponse' <- liftIO . atomically $ waitSend sendResponse
  case sendResponse' of
    Right () -> handleNickResponse' subscription oldNick response
    Left error -> liftIO . atomically . putTMVar response $ NickError error
  where handleNickResponse' subscription oldNick response = do
          event <- liftIO . atomically $ CM.recv subscription
          case event of
            ComaMessage message@(IRCMessage { ircmPrefix = prefix, ircmCommand = command }) ->
              | command == cmd_NICK && extractNick prefix == Just oldNick ->
                liftIO . atomically $ putTMVar response NickSuccess
              | command == err_NICKNAMEINUSE -> liftIO . atomically $ putTMVar response NickNicknameInUse
              | command == err_NICKCOLLISION -> liftIO . atomically $ putTMVar response NickNickCollision
              | command == err_UNAVAILRESOURCE -> liftIO . atomically $ putTMVar response NickUnavailResource
              | command == err_NONICKNAMEGIVEN -> liftIO . atomically $ putTMVar response NickNoNicknameGiven
              | isError command -> liftIO . atomically $ putTMVar response (NickOther message)
            ComaDisconnected -> liftIO . atomically $ putTMVar response NickDisconnected
            _ -> handleNickResponse' subscription oldNick response
            
-- | Invoke the MODE command.
mode :: ConnectionManager -> AddRemove -> UserMode -> AM ModeResponse
mode manager addRemove mode = do
  response <- liftIO . atomically $ newEmptyTMVar
  registered <- liftIO . atomically $ CM.isRegistered manager
  if registered
  then do subscription <- liftIO . atomicallly $ CM.subscribe manager
          nick <- liftIO . atomically $ CM.getNick manager
          sendResponse <- liftIO . atomically . CM.send $
            IRCComand { ircmPrefix = Nothing,
                        ircmCommand = cmd_MODE,
                        ircmParameters = [B.append (convertAddRemove addRemove) mode],
                        ircmComment = Nothing }
          intf <- interface
          async $ runAM (handleModeResponse subscription nick sendResponse response) intf
  else liftIO . atomically $ putTMVar response ModeNotRegistered
  return $ ModeResponse response

-- | Wait for response from MODE.
waitMode :: ModeResponse -> STM ModeEvent
waitMode (ModeResponse response) = readTMVar response

-- | Handle the response to the MODE command.
handleModeResponse :: ConnectionManagerSubscription -> Nick -> ConnectionManagerSendResponse -> TMVar ModeEvent ->
                      AM ()
handleModeResponse subscription nick sendResponse response = do
  sendResponse' <- liftIO . atomically $ waitSend sendResponse
  case sendResponse' of
    Right () -> handleModeResponse' subscription nick response
    Left error -> liftIO . atomically . putTMVar response $ ModeError error
  where handleModeResponse' subscription nick response = do
          event <- liftIO . atomically $ CM.recv subscription
          case event of
            ComaMessage message@(IRCMessage { ircmPrefix = prefix, ircmCommand = command }) ->
              | command == cmd_MODE && extractNick prefix == Just nick ->
                liftIO . atomically $ putTMVar response ModeSuccess
              | command == rpl_UMODEIS -> liftIO . atomically $ putTMVar response ModeSuccess
              | command == err_UMODEUNKNOWNFLAG -> liftIO . atomically $ putTMVar response ModeUmodeUnknownFlag
              | command == err_USERSDONTMATCH -> liftIO . atomically $ putTMVar response ModeUsersDontMatch
              | command == err_NEEDMOREPARAMS -> liftIO . atomically $ putTMVar response ModeNeedMoreParams
              | isError command -> liftIO . atomically $ putTMVar response (ModeOther message)
            ComaDisconnected -> liftIO . atomically $ putTMVar response ModeDisconnected
            _ -> handleModeResponse' subscription nick response

-- | Invoke the QUIT command.
quit :: ConnectionManager -> MessageComment -> AM QuitResponse
quit manager comment = do
  response <- liftIO . atomically $ newEmptyTMVar
  subscription <- liftIO . atomically $ CM.subscribe manager
  sendResponse <- liftIO . atomically . CM.send $ IRCCommand { ircmPrefix = Nothing,
                                                               ircmCommand = cmd_QUIT,
                                                               ircmParameters = [],
                                                               ircmComment = Just comment }
  intf <- interface
  async $ runAM (handleQuitResponse subscription sendResponse response) intf
  return $ QuitResponse response

-- | Wait for response from QUIT.
waitQuit :: QuitResponse -> STM QuitEvent
waitQuit (QuitResponse response) = readTMVar response

-- | Handle the response to the QUIT command.
handleQuitResponse :: ConnectionManagerSubscription -> ConnectionManagerSendResponse -> TMVar QuitEvent -> AM ()
handleQuitRepsonse subscription sendResponse response = do
  sendResponse' <- liftIO . atomically $ waitSend sendResponse
  case sendResponse' of
    Right () -> handleQuitResponse' subscription response
    Left error -> liftIO . atomically . putTMVar response $ QuitError error
  where handleQuitResponse' subscription response = do
          event <- liftIO . atomically $ CM.recv subscription
          case event of
            ComaMessage message@(IRCMessage { ircmCommand = command, ircmComment = comment }) ->
              | command == cmd_ERROR -> liftIO . atomically $ putTMVar response (QuitSuccess comment)
              | isError command -> liftIO . atomically $ putTMVar response (QuitOther message)
            ComaDisconnected -> liftIO . atomically $ putTMVar response QuitDisconnected
            _ -> handleQuitResponse' subscription response

-- | Invoke the SQUIT command.
squit :: ConnectionManager -> HostName -> MessageComment -> AM SquitResponse
squit manager hostName comment = do
  response <- liftIO . atomically $ newEmptyTMVar
  registered <- liftIO . atomically $ CM.isRegistered manager
  if registered
  then do subscription <- liftIO . atomically $ CM.subscribe manager
          sendResponse <- liftIO . atomically . CM.send $ IRCCommand { ircmPrefix = Nothing,
                                                                       ircmCommand = cmd_SQUIT,
                                                                       ircmParameters = [hostName],
                                                                       ircmComment = Just comment }
          intf <- interface
          async $ runAM (handleSquitResponse subscription hostName sendResponse response) intf
  else liftIO . atomically $ putTMVar response SquitNotRegistered
  return $ SquitResponse response

-- | Wait for response from SQUIT.
waitSquit :: SquitResponse -> STM SquitEvent
waitSquit (SquitResponse response) = readTMVar response

-- | Handle the response to the SQUIT command.
handleSquitResponse :: ConnectionManagerSubscription -> HostName -> ConnectionManagerSendResponse ->
                       TMVar SquitEvent -> AM ()
handleSquitResponse subscription hostName sendResponse response = do
  sendResponse' <- liftIO . atomically $ waitSend sendResponse
  case sendResponse' of
    Right () -> handleSquitResponse' subscription hostName response
    Left error -> liftIO . atomically . putTMVar response $ SquitError error
  where handleSquitResponse' subscription hostName response = do
          event <- liftIO . atomically $ CM.recv subscription
          case event of
            ComaMessage message@(IRCMessage { ircmCommand = command, ircmParameters = parameters }) ->
              | command == cmd_SQUIT && parameters == [hostName] ->
                liftIO . atomically $ putTMVar response SquitSuccess
              | command == err_NOPRIVILEGES -> liftIO . atomically $ putTMVar response SquitNoPrivileges
              | command == err_NOSUCHSERVER -> liftIO . atomically $ putTMVar response SquitNoSuchServer
              | command == err_NEEDMOREPARAMS -> liftIO . atomically $ putTMVar respons SquitNeedMoreParams
              | isError command -> liftIO . atomically $ putTMVar response (SquitOther message)
            ComaDisconnected -> liftIO . atomically $ putTMVar response SquitDisconnected
            _ -> handleSquitResponse' subscription hostName response
