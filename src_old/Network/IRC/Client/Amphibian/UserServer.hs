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

module Network.IRC.Client.Amphibian.UserServer

       (UserServer,
        UserServerStopResponse,
        new,
        start,
        stop,
        waitStop)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               TMVar,
                               atomically,
                               orElse,
                               retry,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newBroadcastTChan,
                               peekTChan,
                               readTChan,
                               writeTChan,
                               dupTChan,
                               newEmptyTMVar,
                               putTMVar,
                               readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue,
                                      peekTQueue)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Functor ((<$>))

-- | Create a new user server.
new :: STM UserServer
new = do
  running <- newTVar Bool
  actions <- newTQueue
  return $ UserServer { usseRunning = running,
                        usseActions = actions }

-- | Start a user server
start :: UserServer -> AM ()
start server = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ usseRunning server
    if not running
    then do
      writeTVar (usseRunning server) True
      I.registerUserServer intf server
      return . async $ runAM (runServer server)
    else return $ return ()

-- | Stop user server.
stop :: UserServer -> STM UserServerStopResponse
stop server = do
  response <- UserServerStopResponse <$> newEmptyTMVar
  writeTVar (usseActions server) (UssaStop response)
  return response

-- | Wait for user server to stop.
waitStop :: UserServerStopResponse -> STM (Either Error ())
waitStop (UserServerStopResponse response) = readTMVar response

-- | Run user server.
runServer :: UserServer -> AM ()
runServer server = do
  intf <- getInterface
  continue <- join . liftIO . atomically $ do
    action <- readTQueue $ usseActions server
    case action of
      UssaStartUser user (UserStartResponse response) -> do
        active <- readTVar $ userActive user
        if not active
        then do
          writeTVar (userActive user) True
          I.registerUser intf user
          putTMVar response $ Right ()
          return $ do
            async $ runAM (runUser user) intf
            return True
        else do
          errorText <- I.lookupText intf "User is already started"
          putTMVar response . Left $ Error [errorText]
          return $ return True
      UssaStop -> do
        I.unregisterUserServer intf server
        return $ return False
  if continue
  then runServer server
  else return ()

-- | Run user actions and events.
runUser :: User -> AM ()
runUser user = do
  continue <- join . liftIO . atomically $ handleAction user `orElse` handleEvent user
  if continue
  then runUser user
  else return ()

-- | Handle action.
handleAction :: User -> STM (AM Bool)
handleAction user = do
  action <- readTQueue $ userActions user
  nick <- readTVar $ userNick user
  case action of
    UserMessage comment (UserMessageResponse response) -> do
      sendResponse <- liftIO . atomically $ do
        myNick <- CM.getNick $ userConnectionManager user
        sendResponse <- CM.send (userConnectionManager user) $
          IRCMessage { ircmPrefix = Nothing, ircmCommand = cmd_PRIVMSG, ircmParameters = [nick],
                       ircmComment = Just comment }

        return selfResponse
      return $ do async . atomically $ do responseValue <- CM.waitSend sendResponse
                                          writeTMVar response responseValue
                  return True
    UserNotice comment (UserNoticeResponse response) -> do
      sendResponse <- liftIO . atomically $ do
        myNick <- CM.getNick $ userConnectionManager user
        sendResponse <- CM.send (userConnectionManager user) $
          IRCMessage { ircmPrefix = Nothing, ircmCommand = cmd_NOTICE, ircmParameters = [nick],
                       ircmComment = Just comment }
        return sendResponse
      return $ do async . atomically $ do responseValue <- CM.waitSend sendResponse
                                          writeTMVar response responseValue
                  return True
    UserStop (UserStopResponse response) -> do
      I.unregisterUser (userInterface user) user
      writeTVar (userActive user) False
      writeTMVar response (Right ())
      return $ return False

-- | Handle event.
handleEvent :: User -> STM (AM Bool)
handleEvent user = do
  delayEvents <- readTVar $ userDelayEvents user
  if delayEvents
    then retry
    else return ()
  event <- do
    event <- peekTQueue $ userInjectedEvents user
    case event of
     Just event -> do
       readTQueue $ userInjectedEvents user
       return event
     Nothing -> CM.recv $ userSubscription user
  nick <- readTVar $ userNick user
  ownNick <- CM.getNick $ userConnectionManager user
  case event of
   ComaMessage message
     | ircmCommand message == cmd_PRIVMSG && (extractNick $ ircmPrefix message) == Just nick
       && ircmParameters message == [ownNick] ->
         case ircmComment message of
          Just comment -> do
            writeTChan (userEvents user) $ UserRecvMessage nick comment
            return $ return True
          Nothing -> return $ return True
     | ircmCommand message == cmd_NOTICE && (extractNick $ ircmPrefix message) == Just nick
       && ircmParameters message == [ownNick] ->
         case ircmComment message of
          Just comment -> do
            writeTChan (userEvents user) $ UserRecvNotice nick comment
            return $ return True
          Nothing -> return $ return True
     | ircmCommand message == cmd_QUIT && (extractNick $ ircmPrefix message) == Just nick -> do
         writeTChan (userEvents user) $ UserRecvQuit nick (ircmPrefix message) (ircmComment message)
         return $ return True
   ComaRecvNick oldNick newNick
     | oldNick == nick ->
         writeTVar (userNick user) newNick
         writeTChan (userEvents user) $ UserRecvNick nick newNick
         return $ return True
   ComaRecvCtcpRequest fromNick dest comment
     | fromNick == nick && dest == CnonNick ownNick -> do
         writeTChan (userEvents user) $ UserRecvCtcpRequest fromNick comment
         return $ return True
   ComaRecvCtcpReply fromNick dest comment
     | fromNick == nick && dest == CnonNick ownNick -> do
         writeTChan (userEvents user) $ UserRecvCtcpReply fromNick comment
         return $ return True
   ComaRecvSelfNick oldNick newNick -> do
     writeTChan (userEvents user) $ UserRecvSelfNick oldNick newNick
     return $ return True
   ComaSelfMessage selfNick dest comment
     | dest == CnonNick nick -> do
         writeTChan (userEvents user) $ UserSelfMessage selfNick comment
         return $ return True
   ComaSelfNotice selfNick dest comment
     | dest == CnonNick nick -> do
         writeTChan (userEvents user) $ UserSelfNotice selfNick comment
         return $ return True
   ComaSelfCtcpRequest selfNick dest comment
     | dest == CnonNick nick -> do
         writeTChan (userEvents user) $ UserSelfCtcpRequest selfNick comment
         return $ return True
   ComaSelfCtcpReply selfNick dest comment
     | dest == CnonNick nick -> do
         writeTChan (userEvents user) $ UserSelfCtcpReply selfNick comment
         return $ return True
   ComaDisconected error -> do
     writeTChan (userEvents user) (UserDisconnected error)
     return $ return True
   _ -> return $ return True

