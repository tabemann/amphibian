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

module Network.IRC.Client.Amphibian.Frontend

       (new,
        start,
        subscribeInput,
        subscribeOutput,
        recvInput,
        recvOutput,
        peekInput,
        peekOutput,
        getActive,
        stop,
        stopped)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import Data.Functor ((<$>))
import Control.Concurrent.STM (STM,
                               TVar,
                               TChan,
                               newTVar,
                               readTVar,
                               writeTVar,
                               newBroadcastTChan,
                               dupTChan,
                               readTChan,
                               peekTChan,
                               writeTChan)

-- | Create a new frontend interface.
new :: Interface -> STM Frontend
new intf  = do
  active <- newTVar True
  inputEvents <- newBroadcastTChan
  outputEvents <- newBroadcastTChan
  return $ Frontend { fronInterface = intf,
                      fronActive = active,
                      fronInputEvents = inputEvents,
                      fronOutputEvents = outputEvents }

-- | Start a frontend interface.
start :: Frontend -> STM ()
start frontend = I.registerFrontend (fronInterface frontend) frontend

-- | Subscribe to input events from a frontend interface.
subscribeInput :: Frontend -> STM FrontendInputSubscription
subscribeInput frontend = FrontendInputSubscription <$> dupTChan $ fronInputEvents frontend

-- | Subscribe to output events from a frontend interface.
subscribeOutput :: Frontend -> STM FrontendOutputSubscription
subscribeOutput frontend = FrontendOutputSubscription <$> dupTChan $ fronOutputEvents frontend

-- | Receive input events from a frontend interface input subscription.
recvInput :: FrontendInputSubscription -> STM FrontendInputEvent
recvInput (FrontendInputSubscription subscription) = readTChan subscription

-- | Receive output events from a frontend interface output subscription.
recvOutput :: FrontendOutputSubscription -> STM FrontendOutputEvent
recvOutput (FrontendOutputSubscription subscription) = readTChan subscription

-- | Peek input events from a frontend interface input subscription.
peekInput :: FrontendInputSubscription -> STM FrontendInputEvent
peekInput (FrontendInputSubscription subscription) = peekTChan subscription

-- | Peek output events from a frontend interface output subscription.
peekOutput :: FrontendOutputSubscription -> STM FrontendOutputEvent
peekOutput (FrontendOutputSubscription subscription) = peekTChan subscription

-- | Signal that a frontend should stop.
stop :: Frontend -> STM ()
stop frontend = writeTChan (fronOutputEvents frontend) FroeStop

-- | Signal that a frontend has stopped.
stopped :: Frontend -> STM ()
stopped frontend = writeTChan (fronInputEvents frontend) FrieStopped
