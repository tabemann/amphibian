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

module Network.IRC.Client.Amphibian.IgnoreList

  (Response(..),
   Error(..),
   IgnoreList,
   UserEventType,
   newIgnoreList,
   startIgnoreList,
   stopIgnoreList,
   loadIgnoreList,
   updateIgnoreList,
   filterWithIgnoreList,
   getIgnoreListEntries,
   getIgnoreListRunning,
   getIgnoreListLoaded)

where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Utility
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Sequence as S
import Data.Monoid ((<>),
                    mempty)
import Data.Functor (fmap,
                     (<$>))
import Data.Sequence ((|>),
                     ViewL(..))
import Data.Foldable (foldl')
import Data.List (elem)
import System.IO (openFile,
                  hClose,
                  hFlush,
                  hSeek,
                  SeekMode(..),
                  hSetFileSize,
                  IOMode(..),
                  Handle)
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

-- | Private ignore list state type.
data IgnoreListData = IgnoreListData
  { ignoreListHandle :: Maybe Handle,
    ignoreListEntries :: S.Seq (B.ByteString, S.Seq UserEventType),
    ignoreListLoaded :: Bool }

-- | Create a new ignore list.
newIgnoreList :: STM IgnoreList
newIgnoreList = do
  running <- newTVar False
  actions <- newTQueue
  return IgnoreList { ignoreListRunning = running,
                      ignoreListActions = actions }

-- | Start an ignore list.
startIgnoreList :: IgnoreList -> IO (Either Error ())
startIgnoreList ignoreList = do
  alreadyRunning <- atomically $ do
    running <- readTVar $ ignoreListRunning ignoreList
    if not running
      then do writeTVar (ignoreListRunning ignoreList) True
              return False
      else return True
  if not alreadyRunning
    then do let state = IgnoreListData { ignoreListHandle = Nothing,
                                         ignoreListEntries = S.empty,
                                         ignoreListLoaded = False }
            async $ do runIgnoreList ignoreList state
            return $ Right ()
    else return . Left $ Error "ignoreList already started"

-- | Stop an ignore list.
stopIgnoreList :: IgnoreList -> STM (Response ())
stopIgnoreList ignoreList = do
  running <- readTVar $ ignoreListRunning ignoreList
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response $ Right ()
    else writeTQueue (ignoreListActions ignoreList) $ StopIgnoreList response'
  return response'

-- | Load an ignore list.
loadIgnoreList :: IgnoreList -> STM (Response ())
loadIgnoreList ignoreList = do
  running <- readTVar $ ignoreListRunning ignoreList
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "ignore list not started"
    else writeTQueue (ignoreListActions ignoreList) $ LoadIgnoreList response'
  return response'

-- | Update the ignore list.
updateIgnoreList :: IgnoreList -> B.ByteString -> S.Seq UserEventType ->
                    STM (Response ())
updateIgnoreList ignoreList mask userEventTypes = do
  running <- readTVar $ ignoreListRunning ignoreList
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "ignore list not started"
    else writeTQueue (ignoreListActions ignoreList) $
         UpdateIgnoreList mask userEventTypes response'
  return response'

-- | Filter with ignore list.
filterWithIgnoreList :: IgnoreList -> B.ByteString -> S.Seq UserEventType ->
                        STM (Response Bool)
filterWithIgnoreList ignoreList nickUsernameAndHost userEventTypes = do
  running <- readTVar $ ignoreListRunning ignoreList
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "ignore list not started"
    else writeTQueue (ignoreListActions ignoreList) $
         FilterWithIgnoreList nickUsernameAndHost userEventTypes response'
  return response'

-- | Get the ignore list entries.
getIgnoreListEntries :: IgnoreList ->
                        STM (Response (S.Seq (B.ByteString,
                                              S.Seq UserEventType)))
getIgnoreListEntries ignoreList = do
  running <- readTVar $ ignoreListRunning ignoreList
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "ignore list not started"
    else writeTQueue (ignoreListActions ignoreList) $
         GetIgnoreListEntries response'
  return response'

-- | Get whether the ignore list is loaded.
getIgnoreListLoaded :: IgnoreList -> STM (Response Bool)
getIgnoreListLoaded ignoreList = do
  running <- readTVar $ ignoreListRunning ignoreList
  response <- newEmptyTMVar
  let response' = Response response
  if not running
    then putTMVar response . Left $ Error "ignore list not started"
    else writeTQueue (ignoreListActions ignoreList) $
         GetIgnoreListLoaded response'
  return response'

-- | Get whether the ignore list is running.
getIgnoreListRunning :: IgnoreList -> STM Bool
getIgnoreListRunning = readTVar . ignoreListRunning

-- | Run ignore list.
runIgnoreList :: IgnoreList -> IgnoreListData -> IO ()
runIgnoreList outer ignoreList = do
  action <- atomically . readTQueue $ ignoreListActions outer
  case action of
    LoadIgnoreList response -> do
      ignoreList <- handleLoadIgnoreList ignoreList response
      runIgnoreList outer ignoreList
    UpdateIgnoreList mask userEventTypes response -> do
      ignoreList <- handleUpdateIgnoreList ignoreList mask userEventTypes
                    response
      runIgnoreList outer ignoreList
    FilterWithIgnoreList nickUsernameAndHost userEventTypes response -> do
      ignoreList <- handleFilterWithIgnoreList ignoreList nickUsernameAndHost
                    userEventTypes response
      runIgnoreList outer ignoreList
    GetIgnoreListEntries response -> do
      ignoreList <- handleGetIgnoreListEntries ignoreList response
      runIgnoreList outer ignoreList
    GetIgnoreListLoaded response -> do
      ignoreList <- handleGetIgnoreListLoaded ignoreList response
      runIgnoreList outer ignoreList
    StopIgnoreList response ->
      handleStopIgnoreList ignoreList outer response

-- | Load ignore list from file.
handleLoadIgnoreList :: IgnoreListData -> Response () -> IO IgnoreListData
handleLoadIgnoreList ignoreList (Response response) = do
  amphibianDir <- getUserDataDir "amphibian"
  createDirectoryIfMissing True amphibianDir
  let filePath = amphibianDir </> "ignore"
  if not $ ignoreListLoaded ignoreList
    then do
      ignoreList <- loadIgnoreList' ignoreList filePath
                    `catch` (\e -> return $ const ignoreList (e :: IOException))
      ignoreList <- openIgnoreList ignoreList filePath
        `catch` (\e -> return $ const ignoreList (e :: IOException))
      atomically . putTMVar response $ Right ()
      return $ ignoreList { ignoreListLoaded = True }
    else do
      atomically . putTMVar response . Left $ Error "ignore list already loaded"
      return ignoreList
  where loadIgnoreList' ignoreList filePath = do
          lines <- S.filter (not . B.null) . S.fromList .
                   B.split (byteOfChar '\n') <$> B.readFile filePath
          let parts = fmap (\line -> S.filter (not . B.null) . S.fromList $
                                     B.split (byteOfChar ' ') line) lines
              parts' = S.filter (\line ->
                                   case S.viewl line of
                                     mask :< types ->
                                       verifyMask mask &&
                                       verifyUserEventTypes types &&
                                       not (S.null types)
                                     EmptyL -> False) parts
              entries = fmap (\line ->
                                case S.viewl line of
                                  mask :< types ->
                                    (mask, parseUserEventTypes types)
                                  EmptyL -> error "impossible") parts'
          return $ ignoreList { ignoreListEntries = entries }
        openIgnoreList ignoreList filePath = do
          handle <- openFile filePath ReadWriteMode
          return $ ignoreList { ignoreListHandle = Just handle }

-- | Update ignore list.
handleUpdateIgnoreList :: IgnoreListData -> B.ByteString ->
                          S.Seq UserEventType -> Response () ->
                          IO IgnoreListData
handleUpdateIgnoreList ignoreList mask userEventTypes (Response response) = do
  case verifyAndNormalizeMask mask of
    Just mask -> do
      let entries = S.filter (\(mask', _) -> mask /= mask') $
                    ignoreListEntries ignoreList
          entries' =
            if S.null userEventTypes
            then entries
            else entries |> (mask, normalizeUserEventTypes userEventTypes)
      ignoreList <- saveIgnoreList $ ignoreList { ignoreListEntries = entries' }
      atomically . putTMVar response $ Right ()
      return ignoreList
    Nothing -> do
      atomically . putTMVar response . Left $ Error "invalid mask"
      return ignoreList

-- | Filter a nick, username, and host against an ignore list.
handleFilterWithIgnoreList :: IgnoreListData -> B.ByteString ->
                              S.Seq UserEventType -> Response Bool ->
                              IO IgnoreListData
handleFilterWithIgnoreList ignoreList nickUsernameAndHost userEventTypes
  (Response response) = do
  case extractParts nickUsernameAndHost of
    Just (nick, username, host) ->
      atomically . putTMVar response . Right . not .
      matchFilters nick username host $ ignoreListEntries ignoreList
    Nothing -> atomically . putTMVar response $ Right False
  return ignoreList
  where matchFilters nick username host entries =
          case S.viewl entries of
            (mask, userEventTypes') :< rest ->
              case extractParts mask of
                Just (nick', username', host') ->
                  if matchByteStringGlob nick' nick &&
                     matchByteStringGlob username' username &&
                     matchByteStringGlob host' host
                  then
                    UserEventAll `elem` userEventTypes' ||
                    foldl' (\matching userEventType ->
                              matching || (userEventType `elem` userEventTypes))
                    False userEventTypes'
                  else matchFilters nick username host rest
                Nothing -> matchFilters nick username host rest
            EmptyL -> False

-- | Handle getting ignore list entries.
handleGetIgnoreListEntries :: IgnoreListData ->
                              Response (S.Seq (B.ByteString,
                                               S.Seq UserEventType)) ->
                              IO IgnoreListData
handleGetIgnoreListEntries ignoreList (Response response) = do
  atomically . putTMVar response . Right $ ignoreListEntries ignoreList
  return ignoreList

-- | Handle getting whether ignore list is loaded.
handleGetIgnoreListLoaded :: IgnoreListData -> Response Bool ->
                             IO IgnoreListData
handleGetIgnoreListLoaded ignoreList (Response response) = do
  atomically . putTMVar response . Right $ ignoreListLoaded ignoreList
  return ignoreList

-- | Handle stopping an ignore list.
handleStopIgnoreList :: IgnoreListData -> IgnoreList -> Response () -> IO ()
handleStopIgnoreList ignoreList outer (Response response) = do
  case ignoreListHandle ignoreList of
    Just handle -> hClose handle
    Nothing -> return ()
  atomically $ do
    writeTVar (ignoreListRunning outer) False
    putTMVar response $ Right ()

-- | Save ignore list.
saveIgnoreList :: IgnoreListData -> IO IgnoreListData
saveIgnoreList ignoreList =
  case ignoreListHandle ignoreList of
    Just handle -> do
      hSeek handle AbsoluteSeek 0
      hSetFileSize handle 0
      BB.hPutBuilder handle . foldl' formatEntries mempty $
        ignoreListEntries ignoreList
      hFlush handle
      return ignoreList
    Nothing -> return ignoreList
  where formatEntries builder (mask, userEventTypes) =
          let builder' = builder <> BB.byteString mask
              builder'' =
                foldl' (\builder userEventType ->
                          builder <> BB.word8 (byteOfChar ' ') <>
                          BB.byteString (byteStringOfUserEventType
                                         userEventType))
                builder' userEventTypes
          in builder'' <> BB.word8 (byteOfChar '\n')

-- | Verify and normalize mask.
verifyAndNormalizeMask :: B.ByteString -> Maybe B.ByteString
verifyAndNormalizeMask byteString
  | isNick byteString = Just . B.append byteString $ encodeUtf8 "!*@*"
  | verifyMask byteString = Just byteString
  | otherwise = Nothing

-- | Check whether a mask is really just a nick.
isNick :: B.ByteString -> Bool
isNick byteString =
  B.notElem (byteOfChar '!') byteString &&
  B.notElem (byteOfChar '@') byteString

-- | Verify mask.
verifyMask :: B.ByteString -> Bool
verifyMask mask =
  let (first, second) = B.breakSubstring (encodeUtf8 "!") mask
  in case B.uncons second of
    Just (delimiter, second')
      | delimiter == byteOfChar '!' ->
        let (second'', third) = B.breakSubstring (encodeUtf8 "@") second'
        in case B.uncons third of
          Just (delimiter', third')
           | delimiter' == byteOfChar '@' ->
             B.notElem (byteOfChar '@') first &&
             B.notElem (byteOfChar '!') second'' &&
             B.notElem (byteOfChar '@') third'
          _ -> False
    _ -> False

-- | Extract parts from a fully qualified user nick, name, and host or a mask.
extractParts :: B.ByteString -> Maybe (B.ByteString, B.ByteString, B.ByteString)
extractParts mask =
  let (first, second) = B.breakSubstring (encodeUtf8 "!") mask
  in case B.uncons second of
    Just (delimiter, second')
      | delimiter == byteOfChar '!' ->
        let (second'', third) = B.breakSubstring (encodeUtf8 "@") second'
        in case B.uncons third of
          Just (delimiter', third')
           | delimiter' == byteOfChar '@' &&
             B.notElem (byteOfChar '@') first &&
             B.notElem (byteOfChar '!') second'' &&
             B.notElem (byteOfChar '@') third' -> Just (first, second'', third')
          _ -> Nothing
    _ -> Nothing

-- | Verify ignore types.
verifyUserEventTypes :: S.Seq B.ByteString -> Bool
verifyUserEventTypes = foldl' verifyType True
  where verifyType matching byteString =
          matching && (userEventTypeOfByteString byteString /= Nothing)

-- | Parse ignore types.
parseUserEventTypes :: S.Seq B.ByteString -> S.Seq UserEventType
parseUserEventTypes =
  normalizeUserEventTypes . foldl' parseUserEventTypes' S.empty
  where parseUserEventTypes' types byteString =
          case userEventTypeOfByteString byteString of
            Just userEventType -> types |> userEventType
            Nothing -> types

-- | Normalize ignore types.
normalizeUserEventTypes :: S.Seq UserEventType -> S.Seq UserEventType
normalizeUserEventTypes userEventTypes =
  if UserEventAll `elem` userEventTypes
  then S.singleton UserEventAll
  else
    let ignoreChannel = UserEventChannel `elem` userEventTypes
        ignorePrivate = UserEventPrivate `elem` userEventTypes
        ignoreNotice = UserEventNotice `elem` userEventTypes
        ignoreCtcp = UserEventCtcp `elem` userEventTypes
        ignoreDcc = UserEventDcc `elem` userEventTypes
        ignoreInvite = UserEventInvite `elem` userEventTypes
        ignoreStatus = UserEventStatus `elem` userEventTypes
    in if ignoreChannel && ignorePrivate && ignoreNotice && ignoreCtcp &&
          ignoreDcc && ignoreInvite && ignoreStatus
       then S.singleton UserEventAll
       else
         let userEventTypes =
               condAppend ignoreChannel S.empty UserEventChannel
             userEventTypes' =
               condAppend ignorePrivate userEventTypes UserEventPrivate
             userEventTypes'' =
               condAppend ignoreNotice userEventTypes' UserEventNotice
             userEventTypes''' =
               condAppend ignoreCtcp userEventTypes'' UserEventCtcp
             userEventTypes'''' =
               condAppend ignoreDcc userEventTypes''' UserEventDcc
             userEventTypes''''' =
               condAppend ignoreInvite userEventTypes'''' UserEventInvite
         in condAppend ignoreStatus userEventTypes''''' UserEventStatus
  where condAppend cond xs x = if cond then xs |> x else xs

-- | Get ignore type of bytestring.
userEventTypeOfByteString :: B.ByteString -> Maybe UserEventType
userEventTypeOfByteString byteString
  | byteString == encodeUtf8 "CHAN" = Just UserEventChannel
  | byteString == encodeUtf8 "PRIV" = Just UserEventPrivate
  | byteString == encodeUtf8 "NOTI" = Just UserEventNotice
  | byteString == encodeUtf8 "CTCP" = Just UserEventCtcp
  | byteString == encodeUtf8 "DCC" = Just UserEventDcc
  | byteString == encodeUtf8 "INVI" = Just UserEventInvite
  | byteString == encodeUtf8 "STAT" = Just UserEventStatus
  | byteString == encodeUtf8 "ALL" = Just UserEventAll
  | otherwise = Nothing

-- | Bytestring of ignore type.
byteStringOfUserEventType :: UserEventType -> B.ByteString
byteStringOfUserEventType UserEventChannel = encodeUtf8 "CHAN"
byteStringOfUserEventType UserEventPrivate = encodeUtf8 "PRIV"
byteStringOfUserEventType UserEventNotice = encodeUtf8 "NOTI"
byteStringOfUserEventType UserEventCtcp = encodeUtf8 "CTCP"
byteStringOfUserEventType UserEventDcc = encodeUtf8 "DCC"
byteStringOfUserEventType UserEventInvite = encodeUtf8 "INVI"
byteStringOfUserEventType UserEventStatus = encodeUtf8 "STAT"
byteStringOfUserEventType UserEventAll = encodeUtf8 "ALL"
