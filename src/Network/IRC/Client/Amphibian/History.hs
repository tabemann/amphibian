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
   getHistoryLoaded)

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

-- | Private history state type.
data HistoryState = HistoryState
  { historyHandle :: Maybe Handle,
    historyLines :: S.Seq T.Text,
    historyPosition :: Maybe Int,
    historyNickOrName :: Maybe B.ByteString,
    historyLoaded :: Bool }

-- | Create a new history.
newHistory :: STM History
newHistory = do
  running <- newTVar False
  actions <- newTQueue
  return History { historyRunning = running,
                   historyActions = actions }

-- | Start a history.
startHistory :: History -> IO (Either Error ())
startHistory history = do
  alreadyRunning <- atomically $ do
    running <- readTVar $ historyRunning history
    if not running
      then do writeTVar (historyRunning history) True
              return False
      else return True
  if not alreadyRunning
    then do let state = HistoryState { historyHandle = Nothing,
                                       historyLines = S.empty,
                                       historyPosition = Nothing,
                                       historyNickOrName = Nothing,
                                       historyLoaded = False }
            async $ do runHistory history state
            return $ Right ()
    else return . Left $ Error "history already started"

-- | Stop a history.
stopHistory :: History -> STM (Response ())
stopHistory history = do
  running <- readTVar $ historyRunning history
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response $ Right ()
    else writeTQueue (historyActions history) $ StopHistory response'
  return response'

-- | Load history.
loadHistory :: History -> NS.HostName -> NS.PortNumber -> Maybe B.ByteString ->
               STM (Response ())
loadHistory history hostname port nickOrName = do
  running <- readTVar $ historyRunning history
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "history not started"
    else writeTQueue (historyActions history) $
         LoadHistory hostname port nickOrName response'
  return response'

-- | Add to history.
addHistory :: History -> T.Text -> STM (Response ())
addHistory history text = do
  running <- readTVar $ historyRunning history
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "history not started"
    else writeTQueue (historyActions history) $ AddHistory text response'
  return response'

-- | Get the previous history entry.
getPrevHistory :: History -> STM (Response (Maybe T.Text))
getPrevHistory history = do
  running <- readTVar $ historyRunning history
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "history not started"
    else writeTQueue (historyActions history) $ GetPrevHistory response'
  return response'

-- | Get the next history entry.
getNextHistory :: History -> STM (Response (Maybe T.Text))
getNextHistory history = do
  running <- readTVar $ historyRunning history
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "history not started"
    else writeTQueue (historyActions history) $ GetNextHistory response'
  return response'

-- | Get whether the history is loaded.
getHistoryLoaded :: History -> STM (Response Bool)
getHistoryLoaded history = do
  running <- readTVar $ historyRunning history
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "history not started"
    else writeTQueue (historyActions history) $ GetHistoryLoaded response'
  return response'

-- | Run history.
runHistory :: History -> HistoryState -> IO ()
runHistory outer history = do
  action <- atomically . readTQueue $ historyActions outer
  case action of
    LoadHistory hostname port nickOrName response -> do
      history <- handleLoadHistory history hostname port nickOrName response
      runHistory outer history
    AddHistory text response -> do
      history <- handleAddHistory history text response
      runHistory outer history
    GetPrevHistory response -> do
      history <- handleGetPrevHistory history response
      runHistory outer history
    GetNextHistory response -> do
      history <- handleGetNextHistory history response
      runHistory outer history
    GetHistoryLoaded response -> do
      history <- handleGetHistoryLoaded history response
      runHistory outer history
    StopHistory response -> do
      handleStopHistory history outer response

-- | Load history from file.
handleLoadHistory :: HistoryState -> NS.HostName -> NS.PortNumber ->
                     Maybe B.ByteString -> Response () -> IO HistoryState
handleLoadHistory history hostname port nickOrName (Response response) =
  if not $ historyLoaded history
  then loadHistory' `catch` (\e -> return $ const history (e :: IOException))
  else do
    atomically . putTMVar response . Left $ Error "history already loaded"
    return history
  where loadHistory' = do
          historyDir <- getUserDataDir $ "amphibian" </> "history" </> hostname
          createDirectoryIfMissing True historyDir
          let filePath =
                case nickOrName of
                  Just nickOrName ->
                    historyDir </> (T.unpack $ ourDecodeUtf8 nickOrName)
                  Nothing -> historyDir ++ ".session"
          oldLines <- S.filter (not . T.null) . S.fromList . T.splitOn "\n" <$>
            (readFile filePath `catch` (\e -> return $ const ""
                                              (e :: SomeException)))
          let newLines = historyLines history
          handle <- openFile filePath AppendMode
          let history' = history { historyLines = oldLines >< newLines,
                                   historyNickOrName = nickOrName,
                                   historyHandle = Just handle,
                                   historyLoaded = True }
          atomically . putTMVar response $ Right ()
          return history'

-- | Handle add history.
handleAddHistory :: HistoryState -> T.Text -> Response () -> IO HistoryState
handleAddHistory history text (Response response) = do
  case historyHandle history of
    Just handle -> do
      let text' = filterLineForHistoryFile (historyNickOrName history) text
      hPutStr handle . T.pack $ printf "%s\n" text'
      hFlush handle
    Nothing -> return ()
  let history' = history { historyLines = historyLines history |> text,
                           historyPosition = Nothing }
  atomically . putTMVar response $ Right ()
  return history'

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
handleGetPrevHistory :: HistoryState -> Response (Maybe T.Text) ->
                        IO HistoryState
handleGetPrevHistory history (Response response) = do
  let newPosition =
        case historyPosition history of
          Just position -> position + 1
          Nothing -> 0
      lines = historyLines history
  let historyLength = S.length lines
  if newPosition < historyLength
    then case S.lookup ((historyLength - 1) - newPosition) lines of
           Just line -> do
             atomically . putTMVar response . Right  $ Just line
             return $ history { historyPosition = Just newPosition }
           Nothing -> error "impossible"
    else do atomically . putTMVar response . Right $ Nothing
            return history

-- | Get the next history.
handleGetNextHistory :: HistoryState -> Response (Maybe T.Text) ->
                        IO HistoryState
handleGetNextHistory history (Response response) = do
  let lines = historyLines history
  case historyPosition history of
    Nothing -> do
      atomically . putTMVar response $ Right Nothing
      return history
    Just 0 -> do
      atomically . putTMVar response . Right $ Just ""
      return $ history { historyPosition = Nothing }
    Just position -> do
      let newPosition = position - 1
      case S.lookup ((S.length lines - 1) - newPosition) lines of
        Just line -> do
          atomically . putTMVar response . Right $ Just line
          return $ history { historyPosition = Just newPosition }
        Nothing -> error "impossible"

-- | Get whether the history is loaded.
handleGetHistoryLoaded :: HistoryState -> Response Bool -> IO HistoryState
handleGetHistoryLoaded history (Response response) = do
  atomically . putTMVar response . Right $ historyLoaded history
  return history

-- | Handle stop history.
handleStopHistory :: HistoryState -> History -> Response () -> IO ()
handleStopHistory history outer (Response response) = do
  case historyHandle history of
    Just handle -> hClose handle
    Nothing -> return ()  
  atomically $ do
    writeTVar (historyRunning outer) False
    putTMVar response $ Right ()
