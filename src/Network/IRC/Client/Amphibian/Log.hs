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
   loadLog,
   writeLog,
   readLog,
   getLogLoaded,
   getLogRunning)

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

-- | Log state type
data LogData = LogData
  { logHandle :: Maybe Handle,
    logText :: S.Seq T.Text,
    logLoaded :: Bool }

-- | Chunk size
chunkSize :: Int
chunkSize = 4096

-- | Create a new log.
newLog :: Int -> STM Log
newLog initialMaxLines = do
  running <- newTVar False
  actions <- newTQueue
  return Log { logRunning = running,
               logActions = actions,
               logInitialMaxLines = initialMaxLines }

-- | Start a log.
startLog :: Log -> IO (Either Error ())
startLog log = do
  alreadyRunning <- atomically $ do
    running <- readTVar $ logRunning log
    if not running
      then do writeTVar (logRunning log) True
              return False
      else return True
  if not alreadyRunning
    then do let state = LogData { logHandle = Nothing,
                                  logText = S.empty,
                                  logLoaded = False }
            async $ do runLog log state
            return $ Right ()
    else return . Left $ Error "log already started"

-- | Stop a log.
stopLog :: Log -> STM (Response ())
stopLog log = do
  running <- readTVar $ logRunning log
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response $ Right ()
    else writeTQueue (logActions log) $ StopLog response'
  return response'

-- | Load log.
loadLog :: Log -> NS.HostName -> NS.PortNumber -> B.ByteString ->
           STM (Response ())
loadLog log hostname port nickOrName = do
  running <- readTVar $ logRunning log
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "log not started"
    else writeTQueue (logActions log) $
         LoadLog hostname port nickOrName response'
  return response'

-- | Write to a log.
writeLog :: Log -> T.Text -> STM (Response ())
writeLog log text = do
  running <- readTVar $ logRunning log
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "log not started"
    else writeTQueue (logActions log) $ WriteLog text response'
  return response'

-- | Read from a log.
readLog :: Log -> STM (Response T.Text)
readLog log = do
  running <- readTVar $ logRunning log
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "log not started"
    else writeTQueue (logActions log) $ ReadLog response'
  return response'

-- | Get whether a log is loaded.
getLogLoaded :: Log -> STM (Response Bool)
getLogLoaded log = do
  running <- readTVar $ logRunning log
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "log not started"
    else writeTQueue (logActions log) $ GetLogLoaded response'
  return response'

-- | Get whether a log is running.
getLogRunning :: Log -> STM Bool
getLogRunning = readTVar . logRunning

-- | Run log.
runLog :: Log -> LogData -> IO ()
runLog outer log = do
  action <- atomically . readTQueue $ logActions outer
  case action of
    LoadLog hostname port nickOrName response -> do
      log <- handleLoadLog log hostname port nickOrName $
             logInitialMaxLines outer
      runLog outer log
    WriteLog text response -> do
      log <- handleWriteLog log text response
      runLog outer log
    ReadLog response -> do
      log <- handleReadLog log response
      runLog outer log
    GetLogLoaded response -> do
      log <- handleGetLogLoaded log response
      runLog outer log
    StopLog response ->
      handleStopLog log outer response

-- | Load log from file.
handleLoadLog :: LogData -> NS.HostName -> NS.PortNumber -> B.ByteString ->
                 Int -> IO LogData
handleLoadLog log hostname port nickOrName initialMaxLines =
  if not $ logLoaded log
  then loadLog' `catch` (\e -> return $ const log (e :: IOException))
  else return log
  where loadLog' = do
          logDir <- getUserDataDir $ "amphibian" </> "log" </> hostname
          createDirectoryIfMissing True logDir
          let filePath = logDir </> (T.unpack . ourDecodeUtf8 $ nickOrName)
          text <- readFile filePath `catch`
                  (\e -> return $ const "" (e :: SomeException))
          let text' = shortenText text initialMaxLines
          handle <- openFile filePath AppendMode
          return $ log { logText = logText log |> text',
                         logHandle = Just handle,
                         logLoaded = True }
        shortenText text initialMaxLines =
          shortenText' (reverse $ T.chunksOf chunkSize text) initialMaxLines
          [""] ""
        shortenText' chunks count parts section =
          if count > 0
          then
            let (prev, part) = T.breakOnEnd "\n" section
            in if prev /= ""
               then shortenText' chunks (count - 1) (part : parts)
                    (T.dropEnd 1 prev)
               else
                 case chunks of
                   chunk : rest -> shortenText' rest count parts $
                                   T.append chunk part
                   [] -> T.intercalate "\n" $ part : parts
          else T.intercalate "\n" parts

-- | Handle write log.
handleWriteLog :: LogData -> T.Text -> Response () -> IO LogData
handleWriteLog log text (Response response) = do
  case logHandle log of
    Just handle -> do
      hPutStr handle text
      hFlush handle
    Nothing -> return ()
  atomically . putTMVar response $ Right ()
  return $ log { logText = logText log |> text }

-- | Handle read log.
handleReadLog :: LogData -> Response T.Text -> IO LogData
handleReadLog log (Response response) = do
  let text = T.concat . toList $ logText log
  atomically . putTMVar response $ Right text
  return $ log { logText = S.singleton text }

-- | Get whether the log is loaded.
handleGetLogLoaded :: LogData -> Response Bool -> IO LogData
handleGetLogLoaded log (Response response) = do
  atomically . putTMVar response . Right $ logLoaded log
  return log

-- | Handle stop log.
handleStopLog :: LogData -> Log -> Response () -> IO ()
handleStopLog log outer (Response response) = do
  case logHandle log of
    Just handle -> hClose handle
    Nothing -> return ()
  atomically $ do
    writeTVar (logRunning outer) False
    putTMVar response $ Right ()
  
