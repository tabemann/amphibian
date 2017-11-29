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

module Network.IRC.Client.Amphibian.History

  (Response(..),
   Error(..),
   History,
   newHistory,
   startHistory,
   stopHistory,
   loadHistory,
   addHistory,
   getPrevHistory,
   getNextHistory,
   getHistoryState)

where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Network.Socket as NS
import Data.Functor ((<$>))
import Data.Sequence ((|>),
                      (><))
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
import Data.Text.Encoding (encodeUtf8)
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
import Data.Char (isSpace)
import Prelude hiding (readFile)

-- | Create a new history.
newHistory :: STM History
newHistory = do
  handle <- newTVar Nothing
  lines <- newTVar S.empty
  position <- newTVar Nothing
  nickOrName <- newTVar Nothing
  state <- newTVar HistoryNotStarted
  actions <- newTQueue
  return History { historyHandle = handle,
                   historyLines = lines,
                   historyPosition = position,
                   historyNickOrName = nickOrName,
                   historyState = state,
                   historyActions = actions }

-- | Start a history.
startHistory :: History -> IO (Either Error ())
startHistory history = do
  alreadyRunning <- atomically $ do
    state <- readTVar $ historyState history
    if state == HistoryNotStarted
      then do writeTVar (historyState history) HistoryStarted
              return False
      else return True
  if not alreadyRunning
    then do async $ do runHistory history
            return $ Right ()
    else return . Left $ Error "history already started"

-- | Stop a history.
stopHistory :: History -> STM (Response ())
stopHistory history = do
  state <- readTVar $ historyState history
  response <- newEmptyTMVar
  let response' = Response response
  case state of
    HistoryNotStarted -> putTMVar response $ Right ()
    _ -> writeTQueue (historyActions history) $ StopHistory response'
  return response'

-- | Load history.
loadHistory :: History -> NS.HostName -> NS.PortNumber -> Maybe B.ByteString ->
               STM (Response ())
loadHistory history hostname port nickOrName = do
  response <- Response <$> newEmptyTMVar
  writeTQueue (historyActions history) $
    LoadHistory hostname port nickOrName response
  return response

-- | Add to history.
addHistory :: History -> T.Text -> STM (Response ())
addHistory history text = do
  response <- Response <$> newEmptyTMVar
  writeTQueue (historyActions history) $ AddHistory text response
  return response

-- | Get the previous history entry.
getPrevHistory :: History -> STM (Response (Maybe T.Text))
getPrevHistory history = do
  response <- Response <$> newEmptyTMVar
  writeTQueue (historyActions history) $ GetPrevHistory response
  return response

-- | Get the next history entry.
getNextHistory :: History -> STM (Response (Maybe T.Text))
getNextHistory history = do
  response <- Response <$> newEmptyTMVar
  writeTQueue (historyActions history) $ GetNextHistory response
  return response

-- | Get history state.
getHistoryState :: History -> STM HistoryState
getHistoryState history = readTVar $ historyState history

-- | Run history.
runHistory :: History -> IO ()
runHistory history = do
  action <- atomically . readTQueue $ historyActions history
  case action of
    LoadHistory hostname port nickOrName response -> do
      handleLoadHistory history hostname port nickOrName response
      runHistory history
    AddHistory text response -> do
      handleAddHistory history text response
      runHistory history
    GetPrevHistory response -> do
      handleGetPrevHistory history response
      runHistory history
    GetNextHistory response -> do
      handleGetNextHistory history response
      runHistory history
    StopHistory response ->
      handleStopHistory history response

-- | Load history from file.
handleLoadHistory :: History -> NS.HostName -> NS.PortNumber ->
                     Maybe B.ByteString -> Response () -> IO ()
handleLoadHistory history hostname port nickOrName (Response response) = do
  state <- atomically . readTVar $ historyState history
  case state of
    HistoryStarted ->
      loadHistory' `catch` (\e -> return $ const () (e :: IOException))
    HistoryLoaded ->
      atomically . putTMVar response . Left $ Error "history already loaded"
    HistoryNotStarted -> error "impossible"
  where loadHistory' = do
          historyDir <- getUserDataDir $ "amphibian" </> "history" </>
                    printf "%s:%d" hostname (fromIntegral port :: Int)
          createDirectoryIfMissing True historyDir
          let filePath =
                case nickOrName of
                  Just nickOrName ->
                    historyDir </> (T.unpack $ ourDecodeUtf8 nickOrName)
                  Nothing -> historyDir ++ ".session"
          oldLines <- S.filter (not . T.null) . S.fromList . T.splitOn "\n" <$>
            (readFile filePath `catch` (\e -> return $ const ""
                                              (e :: SomeException)))
          atomically $ do
            newLines <- readTVar $ historyLines history
            writeTVar (historyLines history) $ oldLines >< newLines
            writeTVar (historyState history) HistoryLoaded
            writeTVar (historyNickOrName history) nickOrName
          atomically . writeTVar (historyHandle history) =<< Just <$>
            openFile filePath AppendMode
          atomically . putTMVar response $ Right ()

-- | Handle add history.
handleAddHistory :: History -> T.Text -> Response () -> IO ()
handleAddHistory history text (Response response) = do
  handle <- atomically . readTVar $ historyHandle history
  case handle of
    Just handle -> do
      nickOrName <- atomically . readTVar $ historyNickOrName history
      let text' = filterLineForHistoryFile nickOrName text
      hPutStr handle . T.pack $ printf "%s\n" text'
      hFlush handle
    Nothing -> return ()
  atomically $ do
    lines <- readTVar $ historyLines history
    writeTVar (historyLines history) $ lines |> text
    writeTVar (historyPosition history) Nothing
    putTMVar response $ Right ()

-- | Filter a line before it goes in the history file.
filterLineForHistoryFile :: Maybe B.ByteString -> T.Text -> T.Text
filterLineForHistoryFile nickOrName text =
  let text' =
        case nickOrName of
          Just nickOrName -> filterMessageText nickOrName text
          Nothing -> text
      (part0, part1) = T.span isSpace text'
      (part1', part2) = T.span (not . isSpace) part1
      (part2', part3) = T.span isSpace part2
      (part3', part4) = T.span (not . isSpace) part3
      (part4', part5) = T.span isSpace part4
  in if part1' == "/msg"
     then
       T.concat [part0, part1', part2', part3', part4',
                 filterMessageText (encodeUtf8 part3') part5]
     else text'

-- | Get previous history.
handleGetPrevHistory :: History -> Response (Maybe T.Text) -> IO ()
handleGetPrevHistory history (Response response) = do
  atomically $ do
    position <- readTVar $ historyPosition history
    lines <- readTVar $ historyLines history
    let newPosition =
          case position of
            Just position -> position + 1
            Nothing -> 0
    let historyLength = S.length lines
    if newPosition < historyLength
      then case S.lookup ((historyLength - 1) - newPosition) lines of
             Just line -> do
               putTMVar response . Right  $ Just line
               writeTVar (historyPosition history) $ Just newPosition
             Nothing -> error "impossible"
      else putTMVar response . Right $ Nothing

-- | Get the next history.
handleGetNextHistory :: History -> Response (Maybe T.Text) -> IO ()
handleGetNextHistory history (Response response) = do
  atomically $ do
    position <- readTVar $ historyPosition history
    lines <- readTVar $ historyLines history
    case position of
      Nothing -> putTMVar response $ Right Nothing
      Just 0 -> do
        writeTVar (historyPosition history) Nothing
        putTMVar response . Right $ Just ""
      Just position -> do
        let newPosition = position - 1
        case S.lookup ((S.length lines - 1) - newPosition) lines of
          Just line -> do
            putTMVar response . Right $ Just line
            writeTVar (historyPosition history) $ Just newPosition
          Nothing -> error "impossible"

-- | Handle stop history.
handleStopHistory :: History -> Response () -> IO ()
handleStopHistory history (Response response) = do
  historyHandle' <- atomically $ do
    historyHandle' <- readTVar $ historyHandle history
    writeTVar (historyHandle history) Nothing
    return historyHandle'
  case historyHandle' of
    Just historyHandle' -> hClose historyHandle'
    Nothing -> return ()
  atomically $ do
    writeTVar (historyState history) HistoryNotStarted
    writeTVar (historyLines history) S.empty
    putTMVar response $ Right ()
  
