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
