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

module Network.IRC.Client.Amphibian.InputDispatcher
       
       (InputDispatcher,
        InputDispatcherStopResponse,
        InputDispatcherMessageHandler,
        InputDispatcherCommandHandler,
        new,
        start,
        stop,
        waitStop,
        registerMessageHandler,
        unregisterMessageHandler,
        registerCommandHandler,
        unregisterCommandHandler)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.FrameMessage as FM
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar,
                               atomically,
                               orElse,
                               retry,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newEmptyTMVar,
                               readTMVar,
                               putTMVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      readTQueue,
                                      writeTQueue)
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (isSpace)

-- | Create an input dispatcher.
new :: Interface -> STM InputDispatcher
new intf = do
  running <- newTVar False
  actions <- newTQueue
  interfaceSubscription <- I.subscribe intf
  frames <- newTVar []
  messageHandlers <- newTVar []
  commandHandlers <- newTVar HM.empty
  return $ InputDispatcher { indiRunning = running,
                             indiActions = actions,
                             indiInterface = interface,
                             indiInterfaceSubscription = interfaceSubscription,
                             indiFrames = frames,
                             indiMessageHandlers = messageHandlers,
                             indiCommandHandlers = commandHandlers }

-- | Start an input dispatcher.
start :: InputDispatcher -> AM ()
start dispatcher = do
  intf <- getInterface
  join . liftIO . atomically $ do
    running <- readTVar $ indiRunning dispatcher
    if not running
    then do
      writeTVar (indiRunning dispatcher) True
      I.registerInputDispatcher intf dispatcher
      return $ async (runAM (runDispatcher dispatcher) intf)
    else return $ return ()
  return ()

-- | Stop an input dispatcher.
stop :: InputDispatcher -> STM InputDispatcherStopResponse
stop dispatcher = do
  response <- InputDispatcherStopResponse <$> newEmptyTMVar
  writeTQueue (indiActions dispatcher) (IndaStop response)
  return response

-- | Wait for an input dispatcher to stop.
waitStop :: InputDispatcher -> STM (Either Error ())
waitStop (InputDispatcherStopResponse response) = readTMVar response

-- | Register a message handler with an input dispatcher.
registerMessageHandler :: InputDispatcher -> (Frame -> StyledText -> AM Bool) ->
                          STM InputDispatcherMessageHandler
registerMessageHandler dispatcher handler = do
  handler' <- newTVar handler
  let handler = InputDispatcherMessageHandler { idmhDispatcher = dispatcher,
                                                idmhHandler = handler' }
  handlers <- readTVar $ indiMessageHandlers dispatcher
  writeTVar (indiMessageHandlers dispatcher) (handler : handlers)
  return handler

-- | Unregister a message handler from an input dispatcher.
unregisterMessageHandler :: InputDispatcherMessageHandler -> STM ()
unregisterMessageHandler handler = do
  let dispatcher = idmhDispatcher handler
  handlers <- readTVar $ indiMessageHandlers dispatcher
  writeTVar (indiMessageHandlers dispatcher) (filter (/= handler) handlers)

-- | Register a command handler with an input dispatcher.
registerCommandHandler :: InputDispatcher -> Text -> (Frame -> Text -> StyledText -> AM Bool) ->
                          STM InputDispatcherCommandHandler
registerCommandHandler dispatcher command handler = do
  handler' <- newTVar handler
  let handler = InputDispatcherCommandHandler { idchDispatcher = dispatcher,
                                                idchCommand = command,
                                                idchHandler = handler' }
  handlerMap <- readTVar $ indiCommandHandlers dispatcher
  let handlers = HM.lookupDefault [] command handlerMap
  writeTVar (indiCommandHandlers dispatcher) (HM.insert command (handler : handler) handlerMap)
  return handler

-- | Unregister a command handler from an input dispatcher.
unregisterCommandHandler :: InputDispatcherCommandHandler -> STM ()
unregisterCommandHandler handler = do
  let dispatcher = idchDispatcher handler
  handlerMap <- readTVar $ indiCommandHandlers dispatcher
  writeTVar (indiCommandHandlers dispatcher) (HM.adjust (filter (/= handler)) (idchCommand handler) handlerMap)

-- | Run input dispatcher.
runDispatcher :: InputDispatcher -> AM ()
runDispatcher dispatcher = do
  continue <- join . liftIO . atomically $ do
    frames <- readTVar $ indiFrames dispatcher
    handleAction dispatcher `orElse` handleInterface dispatcher `orElse`
      (foldr (\mapping y -> handleFrame dispatcher mapping `orElse` y) retry frames)
  if continue
  then runDispatcher dispatcher
  else return()

-- | Handle action.
handleAction :: InputDispatcher -> STM (AM Bool)
handleAction dispatcher = do
  action <- readTQueue $ indiActions dispatcher
  case action of
    IndaStop (InputDispatcherStopResponse response) -> do
      I.unregisterInputDispatcher (indiInterface dispatcher) dispatcher
      writeTVar (indiRunning dispatcher) False
      putTMVar response $ Right ()
      return $ return False

-- | Handle interface event.
handleInterface :: InputDispatcher -> STM (AM Bool)
handleInterface dispatcher = do
  event <- I.recv $ indiInterfaceSubscription dispatcher
  case event of
    IntfFrameRegistered frame -> do
      subscription <- F.subscribeInput frame
      let mapping = InputDispatcherFrameMapping { idfmFrame = frame,
                                                  idfmSubscription = subscription }
      frames <- readTVar $ indiFrames dispatcher
      writeTVar (indiFrames dispatcher) (mapping : frames)
    IntfFrameUnregisterd frame -> do
      frames <- readTVar $ indiFrames dispatcher
      writeTVar (indiFrames dispatcher) (filter (\mapping -> idfmFrame mapping /= frame) frames)
    _ -> return ()
  return $ return True

-- | Handle frame input event.
handleFrame :: InputDispatcher -> InputDispatcherFrameMapping -> STM (AM Bool)
handleFrame dispatcher mapping = do
  event <- F.recvInput $ idfmSubscription mapping
  case event of
    FievLine text ->
      case text of
        (StyledTextElement style firstPart : rest) ->
          case T.uncons firstPart of
            '/' : firstPartRest ->
              case T.uncons firstPartRest of
                '/' : _ ->
                  dispatchMessage dispatcher (idfmFrame mapping) (StyledTextElement style firstPartRest : rest)
                _ ->
                  let (command, commandRest) = T.breakOn " " firstPartRest in
                  let (_, commandRest) = T.span isSpace commandRest in
                  dispatchCommand dispatcher (idfmFrame mapping) command
                  (StyledTextElement style commandRest : rest)
            _ -> dispatchMessage dispatcher (idfmFrame mapping) text
        [] -> return $ return True
    _ -> return $ return True

-- | Dispatch message.
dispatchMessage :: InputDispatcher -> Frame -> StyledText -> STM (AM Bool)
dispatchMessage dispatcher frame text =
  messageHandlers <- readTVar $ indiMessageHandlers dispatcher
  return $ dispatchMessage' messageHandlers frame text
  where dispatchMessage' (handler : rest) frame text = do
          handled <- (idmhHandler handler) frame text
          if not handled
          then dispatchMessage' rest frame text
          else return True
        dispatchMessage' [] _ _ = return True

-- | Dispatch command.
dispatchCommand :: InputDispatcher -> Frame -> T.Text -> StyledText -> STM (AM Bool)
dispatchCommand dispatcher frame command text = do
  commandHandlers <- HM.lookup command <$> readTVar $ indiCommandHandlers dispatcher
  case commandHandlers of
    [] -> return $ do
            FM.unknownCommandMessage frame command
            return True
    commandHandlers -> return $ dispatchCommand' commandHandlers frame command text
  where dispatchCommand' (handler : rest) frame command text = do
          handled <- (idchHandler handler) frame command text
          if not handled
          then dispatchCommand' rest frame command text
          else return True
        dispatchCommand' [] frame command _ = do
          FM.unknownCommandMessage frame command
          return True
