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

module Network.IRC.Client.Amphibian.InputHandler

       (installHandlers)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import Network.IRC.Client.Amphibian.Utility
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.Channel as C
import qualified Network.IRC.Client.Amphibian.User as U
import qualified Network.IRC.Client.Amphibian.FrameMessage as FM
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import qualified Network.IRC.Client.Amphibian.InputDispatcher as ID
import qualified Network.IRC.Client.Amphibian.Command as Cmd
import qualified Network.IRC.Client.Amphibian.Frontend as Fr
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Control.Monad (join,
                      (=<<),
                      forM)
import Data.Char (isSpace)

-- | Install handlers.
installHandlers :: Interface -> STM ()
installHandlers intf = do
  dispatcher <- I.getInputDispatcher intf
  case dispatcher of
    Just dispatcher -> do
      ID.registerMessageHandler dispatcher defaultMessageHandler
      ID.registerCommandHandler dispatcher "msg" msgHandler
      ID.registerCommandHandler dispatcher "notice" noticeHandler
      ID.registerCommandHandler dispatcher "join" joinHandler
      ID.registerCommandHandler dispatcher "quit" quitHandler
    Nothing -> return ()

-- | Default message handler.
defaultMessageHandler :: Frame -> StyledText -> AM Bool
defaultMessageHandler frame text = do
  comment <- encode frame $ ST.encode text
  mapping <- liftIO . atomically $ F.getMapping frame
  case mapping of
    FrmaChannel channel -> do
      response <- liftIO . atomically $ C.message channel comment
      response' <- liftIO . atomically $ c.waitMessage response
      case response' of
        Left error -> do
          advisoryText <- lookupText "Unable to send message to channel"
          FM.errorMessage frame advisoryText error
        Right -> return ()
      return True
    FrmaUser user ->
      response <- liftIO . atomically $ U.message user' comment
      response' <- liftIO . atomically $ U.waitMessage response
      case response' of
        Left error -> do
          advisoryText <- lookupText "Unable to send message to user"
          FM.errorMessage frame advisoryText error
        Right -> return ()
      return True
    FrmaConnectionManager _ -> do
      advisoryText <- lookupText "Not a channel or user"
      FM.errorMessage frame advisoryText (Error [])
    _ -> return False

-- | Msg command handler.
msgHandler :: Frame -> T.Text -> StyledText -> AM Bool
msgHandler frame command text
  | command == "msg" = do
      let (dest, text') = ST.break isSpace text
      let dest' = ST.removeStyle dest
      intf <- getInterface
      dest'' <- liftIO . atomically $ encodeFrame intf frame dest'
      text'' <- liftIO . atomically . encodeFrame intf frame . ST.encode $ ST.drop 1 text'
      case T.uncons dest' of
       Just ('#', _) ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              channel <- findChannel intf manager dest''
              case channel of
               Just channel -> do
                 response <- C.message channel text''
                 return $ do
                   response' <- liftIO . atomically $ C.waitMessage response
                   case response' of
                    Right () -> return ()
                    Left error -> do
                      messageText <- lookupText "Unable to send message to channel"
                      FM.errorMessage frame messageText error
               Nothing -> return $ FM.notInChannelMessage frame dest''
            Nothing -> return $ FM.unboundFrameMessage frame
       Just _ ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              user <- findOrCreateUser intf manager dest''
              response <- U.message user text''
              return $ do
                response' <- liftIO . atomically $ U.waitMessage response
                case response' of
                 Right () -> return ()
                 Left error -> do
                   messageText <- lookupText "Unable to send message to user"
                   FM.errorMessage frame messageText error
            Nothing -> return $ FM.unboundFrameMessage frame
       Nothing -> do
         syntaxText <- lookupText "/notice <destination> <text>"
         FM.badCommandSyntaxMessage frame syntaxText
  | otherwise = return False

-- | Notice command handler.
noticeHandler :: Frame -> T.Text -> StyledText -> AM Bool
noticeHandler frame command text
  | command == "notice" = do
      let (dest, text') = ST.break isSpace text
      let dest' = ST.removeStyle dest
      intf <- getInterface
      dest'' <- liftIO . atomically $ encodeFrame intf frame dest'
      text'' <- liftIO . atomically . encodeFrame intf frame . ST.encode $ ST.drop 1 text'
      case T.uncons dest' of
       Just ('#', _) ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              channel <- findChannel intf manager dest''
              case channel of
               Just channel -> do
                 response <- C.notice channel text''
                 return $ do
                   response' <- liftIO . atomically $ C.waitNotice response
                   case response' of
                    Right () -> return ()
                    Left error -> do
                      messageText <- lookupText "Unable to send notice to channel"
                      FM.errorMessage frame messageText error
               Nothing -> return $ FM.notInChannelMessage frame dest''
            Nothing -> return $ FM.unboundFrameMessage frame
       Just _ ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              user <- findOrCreateUser intf manager dest''
              response <- U.notice user text''
              return $ do
                response' <- liftIO . atomically $ U.waitNotice response
                case response' of
                 Right () -> return ()
                 Left error -> do
                   messageText <- lookupText "Unable to send notice to user"
                   FM.errorMessage frame messageText error
            Nothing -> return $ FM.unboundFrameMessage frame
       Nothing -> do
         syntaxText <- lookupText "/notice <destination> <text>"
         FM.badCommandSyntaxMessage frame syntaxText
  | otherwise = return False

-- | Join command handler.
joinHandler :: Frame -> T.Text -> StyledText -> AM Bool
joinHandler frame command text
  | command == "join" = do
      let (name, key) = ST.break isSpace text
      let name' = ST.removeStyle name
      let key' = ST.removeStyle key
      intf <- getInterface
      name'' <- liftIO . atomically $ encodeFrame intf frame name'
      key'' <- liftIO . atomically . encodeFrame intf frame $ T.drop 1 key'
      let key''' = if key'' /= B.empty then Just key'' else Nothing
      case T.uncons name' of
       Just ('#', _) ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              channel <- do
                channel <- findChannel intf manager name''
                case channel of
                 Just channel -> return channel
            Nothing -> return $ FM.unboundFrameMessage frame
       Just _ -> FM.notAChannelMessage frame dest'
       Nothing -> do
         syntaxText <- lookupText "/join <channel> [<key>]"
         FM.badCommandSyntaxMessage frame syntaxText
  | otherwise = return False

-- | Quit command handler.
quitHandler :: Frame -> T.Text -> StyledText -> AM Bool
quitHandler frame command text
  | command == "quit" = do
      intf <- getInterface
      async $ runAM (doQuit text) intf
      return True
  | otherwise = return False
  where doQuit text = do
          managers <- getConnectionManagers
          forM managers $ \manager -> do
            config <- getConnectionConfig manager
            let encodedText =
                  case config of
                   Just config -> (encoEncoder $ cocoEncoding config) $ ST.encode text
                   Nothing -> B.empty
            response <- Cmd.quit manager encodedText
            Cmd.quitQuit response
          frontend <- getFrontend
          case frontend of
           Just frontend -> do
             subscription <- liftIO . atomically $ Fr.subscribeInput frontend
             liftIO . atomically $ Fr.stop frontend
             waitM (== FrieStopped) . liftIO . atomically $ Fr.recvInput subscription
           Nothing -> return ()
          liftIO . atomically . I.exit =<< getInterface

