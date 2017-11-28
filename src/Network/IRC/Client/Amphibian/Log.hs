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

module Network.IRC.Client.Amphibian.Log

  (Response(..),
   Error(..),
   Log,
   newLog,
   startLog,
   stopLog,
   writeLog,
   readLog,
   getLogState)

where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Network.Socket as NS
import Data.Functor ((<$>))
import Data.Sequence ((|>))
import Data.Foldable (toList)
import System.IO (stderr,
                  openFile,
                  hClose,
                  hFlush,
                  IOMode(..),
                  Handle)
import Data.Text.IO (hPutStr,
                     readFile)
import Text.Printf (printf)
import Control.Concurrent.Async (Async,
                                 async,
                                 cancel)
import Control.Concurrent.STM (STM,
                               atomically,
                               orElse,
                               retry,
                               TVar,
                               newTVar,
                               writeTVar,
                               readTVar)
import Control.Concurrent.STM.TQueue (TQueue,
                                      newTQueue,
                                      writeTQueue,
                                      readTQueue)
import Control.Concurrent.STM.TMVar (TMVar,
                                     newEmptyTMVar,
                                     putTMVar)
import Control.Exception (catch,
                          IOException,
                          SomeException)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath.Posix ((</>))
import System.Directory (createDirectoryIfMissing)
import Prelude hiding (readFile)

-- | Create a new log.
newLog :: NS.HostName -> NS.PortNumber -> B.ByteString -> STM Log
newLog hostname port nickOrName = do
  handle <- newTVar Nothing
  text <- newTVar S.empty
  state <- newTVar LogNotStarted
  actions <- newTQueue
  return Log { logHandle = handle,
               logText = text,
               logHostname = hostname,
               logPort = port,
               logNickOrName = nickOrName,
               logState = state,
               logActions = actions }

-- | Start a log.
startLog :: Log -> IO (Either Error ())
startLog log = do
  alreadyRunning <- atomically $ do
    state <- readTVar $ logState log
    if state == LogNotStarted
      then do writeTVar (logState log) LogStarted
              return False
      else return True
  if not alreadyRunning
    then do async $ do runLog log
            return $ Right ()
    else return . Left $ Error "log already started"

-- | Stop a log.
stopLog :: Log -> STM (Response ())
stopLog log = do
  state <- readTVar $ logState log
  response <- newEmptyTMVar
  let response' = Response response
  case state of
    LogStarted -> writeTQueue (logActions log) $ StopLog response'
    LogNotStarted -> putTMVar response $ Right ()
  return response'

-- | Write to a log.
writeLog :: Log -> T.Text -> STM (Response ())
writeLog log text = do
  response <- Response <$> newEmptyTMVar
  writeTQueue (logActions log) $ WriteLog text response
  return response

-- | Read from a log.
readLog :: Log -> STM (Response T.Text)
readLog log = do
  response <- Response <$> newEmptyTMVar
  writeTQueue (logActions log) $ ReadLog response
  return response

-- | Get log state.
getLogState :: Log -> STM LogState
getLogState log = readTVar $ logState log

-- | Load log from file.
loadLog :: Log -> IO ()
loadLog log = do
  loadLog' `catch` (\e -> return $ const () (e :: IOException))
  where loadLog' = do
          logDir <- getUserDataDir $ "amphibian" </> "log" </>
                    printf "%s:%d" (logHostname log)
                    (fromIntegral (logPort log) :: Int)
          createDirectoryIfMissing True logDir
          let filePath = logDir </>
                         (T.unpack . ourDecodeUtf8 $ logNickOrName log)
          text <- readFile filePath `catch`
                  (\e -> return $ const "" (e :: SomeException))
          atomically . writeTVar (logText log) $ S.singleton text
          atomically . writeTVar (logHandle log) =<< Just <$>
            openFile filePath AppendMode

-- | Run log.
runLog :: Log -> IO ()
runLog log = do
  action <- atomically . readTQueue $ logActions log
  case action of
    WriteLog text response -> do
      handleWriteLog log text response
      runLog log
    ReadLog response -> do
      handleReadLog log response
      runLog log
    StopLog response ->
      handleStopLog log response

-- | Handle write log.
handleWriteLog :: Log -> T.Text -> Response () -> IO ()
handleWriteLog log text (Response response) = do
  handle <- atomically $ do
    logText' <- readTVar $ logText log
    writeTVar (logText log) $ logText' |> text
    readTVar $ logHandle log
  case handle of
    Just handle -> do
      hPutStr handle text
      hFlush handle
    Nothing -> return ()
  atomically . putTMVar response $ Right ()

-- | Handle read log.
handleReadLog :: Log -> Response T.Text -> IO ()
handleReadLog log (Response response) = do
  logLoaded <- atomically $ do
    handle <- readTVar $ logHandle log
    return $ handle /= Nothing
  if not logLoaded
    then loadLog log
    else return ()
  atomically $ do
    text <- T.concat . toList <$> (readTVar $ logText log)
    writeTVar (logText log) $ S.singleton text
    putTMVar response $ Right text

-- | Handle stop log.
handleStopLog :: Log -> Response () -> IO ()
handleStopLog log (Response response) = do
  logHandle' <- atomically $ do
    logHandle' <- readTVar $ logHandle log
    writeTVar (logHandle log) Nothing
    return logHandle'
  case logHandle' of
    Just logHandle' -> hClose logHandle'
    Nothing -> return ()
  atomically $ do
    writeTVar (logState log) LogNotStarted
    writeTVar (logText log) S.empty
    putTMVar response $ Right ()
  
