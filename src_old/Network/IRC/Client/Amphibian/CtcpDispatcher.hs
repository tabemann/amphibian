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

module Network.IRC.Client.Amphibian.CtcpDispatcher

       (CtcpDispatcher,
        CtcpDispatcherStopResponse,
        CtcpDispatcherRequestHandler,
        new,
        start,
        stop,
        waitStop,
        registerRequestHandler,
        unregisterRequestHandler)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import Network.IRC.Client.Amphibian.Ctcp
import Network.IRC.Client.Amphibian.Commands
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import Control.Concurrrent.STM (STM,
                                TVar,
                                TMVar,
                                atomically,
                                orElse,
                                retry,
                                newTVar,
                                readTVar,
                                writeTVar,
                                newEmptyTMVar,
                                putTMVar,
                                readTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

-- | Create a CTCP dispatcher.
new :: Interface -> STM CtcpDispatcher
new intf = do
  running <- newTVar False
  actions <- newTQueue
  subscription <- I.subscribe intf
  connectionManagers <- newTVar []
  requestHandlers = newTVar HM.empty
  return $ CtcpDispatcher { ctdiInterface = interface,
                            ctdiRunning = running,
                            ctdiActions = actions,
                            ctdiSubscription = subscription,
                            ctdiConnectionManagers = connectionManagers }

-- | Start a CTCP dispatcher.
start :: CtcpDispatcher -> AM ()
start dispatcher = do
  join . liftIO . atomically $ do
    running <- readTVar $ ctdiRunning dispatcher
    if not running
    then do
      writeTVar (ctdiRunning dispatcher) True
      I.registerCtcpDispatcher (ctdiInterface dispatcher) dispatcher
      return $ do
        intf <- getInterface
        async $ runAM (runCtcpDispatcher dispatcher) intf
    else return $ return ()

-- | Stop a CTCP dispatcher.
stop :: CtcpDispatcher -> STM CtcpDispatcherStopResponse
stop dispatcher = do
  response <- CtcpDispatcherStopResponse <$> newEmptyTMVar
  writeTQueue (ctdiActions dispatcher) $ CthaStop response
  return response

-- | Wait for a CTCP dispatcher to stop.
waitStop :: CtcpDispatcherStopResponse -> STM (Either Error ())
waitStop (CtcpDispatcherStopResponse response) = readTMVar response

-- | Register a request handler with an input dispatcher.
registerRequestHandler :: CtcpDispatcher -> CtcpCommand ->
                          (ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM Bool) ->
                          STM CtcpDispatcherRequestHandler
registerRequestHandler dispatcher command handler = do
  handler' <- newTVar handler
  let handler = CtcpDispatcherRequestHandler { cdrhDispatcher = dispatcher,
                                               cdrhCommand = command,
                                               cdrhHandler = handler' }
  handlerMap <- readTVar $ ctdiRequestHandlers dispatcher
  let handlers = HM.lookupDefault [] command handlerMap
  writeTVar (ctdiRequestHandlers dispatcher) (HM.insert request (handler : handler) handlerMap)
  return handler

-- | Unregister a request handler from an input dispatcher.
unregisterRequestHandler :: CtcpDispatcherRequestHandler -> STM ()
unregisterRequestHandler handler = do
  let dispatcher = cdrhDispatcher handler
  handlerMap <- readTVar $ ctdiRequestHandlers dispatcher
  writeTVar (ctdiRequestHandlers dispatcher) (HM.adjust (filter (/= handler)) (cdrhCommand handler) handlerMap)

-- | Run a CTCP dispatcher.
runCtcpDispatcher :: CtcpDispatcher -> AM ()
runCtcpDispatcher dispatcher = do
  continue <- join . liftIO . atomically $ do
    mappings <- readTVar $ ctdiConnectionManagers dispatcher
    handleAction dispatcher `orElse` handleInterface dispatcher `orElse`
      foldr (\mapping y -> handleMapping dispatcher mapping `orElse` y) retry mappings
  if continue
  then runCtcpDispatcher dispatcher
  else return ()

-- | Handle a CTCP dispatcher action.
handleAction :: CtcpDispatcher -> STM (AM Bool)
handleAction dispatcher = do
  action <- readTQueue $ ctdiActions dispatcher
  case action of
    CthaStop (CtcpDispatcherStopResponse response) -> do
      I.unregisterCtcpHandler (ctdiInterface dispatcher) dispatcher
      writeTVar (ctdiRunning dispatcher) False
      putTMVar response $ Right ()
      return $ return False

-- | Handle a CTCP dispatcher interface event.
handleInterface :: CtcpDispatcher -> STM (AM Bool)
handleInterface dispatcher = do
  event <- I.recv $ ctdiSubscription dispatcher
  case event of
    IntfConnectionManagerRegistered manager -> do
      mappings <- readTVar $ ctdiConnectionManagers dispatcher
      if not any (\mapping -> ctdmConnectionManager mapping == manager) mappings
      then do
        subscription <- CM.subscribe manager
        let mapping = CtcpDispatcherMapping { ctdmConnectionManager = manager,
                                              ctdmSubscription = subscription }
        writeTVar (ctdiConnectionManagers dispatcher) (mapping : mappings)
      else return ()
    IntfConnectionManagerUnregistered manager -> do
      mappings <- readTVar $ ctdiConnectionManagers dispatcher
      writeTVar (ctdiConnectionManagers) $ filter (\mapping -> ctdmConnectionManager /= manager) mappings
    _ -> return ()
  return $ return True
  
-- | Handle a CTCP dispatcher mapping.
handleMapping :: CtcpDispatcher -> CtcpDispatcherMapping -> STM (AM Bool)
handleMapping dispatcher mapping = do
  event <- CM.recv $ ctdmSubscription mapping
  let manager = ctdmConnectionManager mapping
      intf = ctdiInterface dispatcher
  case event of
    ComaRecvCtcpRequest nick _ comment ->
      case parseCtcp comment of
        Just (command, argument)
          | command == ctcp_CLIENTINFO -> handleClientInfo dispatcher manager nick
          | otherwise -> dispatchRequest intf manager nick command argument
    _ -> return $ return True

-- | Dispatch request.
dispatchRequest :: CtcpDispatcher -> ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> STM (AM Bool)
dispatchRequest dispatcher manager nick command argument = do
  requestHandlers <- HM.lookup command <$> readTVar $ ctdiRequestHandlers dispatcher
  case commandHandlers of
    [] -> return $ do
            handleUnsupported manager nick command argument
            return True
    requestHandlers -> return $ dispatchRequest' requestHandlers manager nick command argument
  where dispatchRequest' (handler : rest) manager nick command argument = do
          handled <- (cdrhHandler handler) manager nick command argument
          if not handled
          then dispatchRequest' rest manager nick command argument
          else return True
        dispatchRequest' [] manager nick command argument = do
          handleUnsupported manager nick command argument
          return True

-- | Handle CLIENTINFO CTCP request.
handleClientInfo :: CtcpDispatcher -> ConnectionManager -> Nick -> STM (AM Bool)
handleClientInfo dispatcher manager nick = do
  let intf = ctdiInterface dispatcher
  keys <- unique <$> map fst <$> HM.toList <$> readTVar $ ctdiRequestHandlers dispatcher
  CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_CLIENTINFO . Just .
                                   B.append (BC.singleton ':') $ B.intercalate (BC.singleton ' ') keys }
  return $ return True


-- | Handle unsupported CTCP request.
handleUnsupported :: ConnectionManager -> Nick -> CtcpCommand -> Maybe CtcpArgument -> AM ()
handleUnsupported manager nick command argument = do
  intf <- getInterface
  let errorMessage = encode intf manager <$> lookupText "Unknown/unsupported CTCP request"
  let comment = case argument of
    Just argument ->
      formatCtcp ctcp_ERRMSG (B.concat [command, BC.singleton ' ', argument, BC.pack " :", errorMesage])
    Nothing ->
      formatCtcp ctcp_ERRMSG (B.concat [command, BC.pack " :", errorMessage])
  liftIO . atomically $ CM.send manager $ IRCMessage { ircmPrefix = Nothing,
                                 ircmCommand = cmd_NOTICE,
                                 ircmParameters = [nick],
                                 ircmComment = Just . formatCtcp ctcp_ERRMSG $ Just comment }
  return ()

-- | Encode text sent to a connection manager.
encode :: Interface -> ConnectionManager -> T.Text -> STM B.ByteString
encode intf manager text = do
  config <- I.getConnectionConfig intf manager
  case config of
    Just config -> return $ (encoEncoder $ cocoEncoding config) text
    Nothing -> return B.empty
  Nothing -> return B.empty
