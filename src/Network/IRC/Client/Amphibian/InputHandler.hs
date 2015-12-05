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
import Control.Concurrent.Async (Async,
                                 async)
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))
import Control.Monad (join,
                      (=<<),
                      forM)
import Data.Char (isSpace)
import Text.Read (readMaybe)

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
      ID.registerCommandHandler dispatcher "part" partHandler
      ID.registerCommandHandler dispatcher "server" serverHandler
      ID.registerCommandHandler dispatcher "reconnect" reconnectHandler
      ID.registerCommandHandler dispatcher "disconnect" disconnectHandler
      ID.registerCommandHandler dispatcher "quit" quitHandler
    Nothing -> return ()

-- | Default message handler.
defaultMessageHandler :: Frame -> StyledText -> AM Bool
defaultMessageHandler frame text = do
  comment <- encode frame $ ST.encode text
  mapping <- liftIO . atomically $ F.getMapping frame
  intf <- getInterface
  case mapping of
    FrmaChannel channel -> do
      response <- liftIO . atomically $ C.message channel comment
      async . flip runAM intf $ do
        response' <- liftIO . atomically $ c.waitMessage response
        case response' of
         Left error -> do
           advisoryText <- lookupText "Unable to send message to channel"
           FM.errorMessage frame advisoryText error
         Right -> return ()
      return True
    FrmaUser user -> do
      response <- liftIO . atomically $ U.message user' comment
      async . flip runAM intf $ do
        response' <- liftIO . atomically $ U.waitMessage response
        case response' of
         Left error -> do
           advisoryText <- lookupText "Unable to send message to user"
           FM.errorMessage frame advisoryText error
         Right -> return ()
      return True
    FrmaConnectionManager _ -> do
      FM.notChannelOrUserFrameMessage frame
      return True
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
                   async . flip runAM intf $ do
                     response' <- liftIO . atomically $ C.waitMessage response
                     case response' of
                      Right () -> return ()
                      Left error -> do
                        messageText <- lookupText "Unable to send message to channel"
                        FM.errorMessage frame messageText error
                   return True
               Nothing -> return $ do FM.notInChannelMessage frame dest''
                                      return True
            Nothing -> return $ do FM.unboundFrameMessage frame
                                   return True
       Just _ ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              user <- findOrCreateUser intf manager dest''
              response <- U.message user text''
              return $ do
                async . flip runAM intf $ do
                  response' <- liftIO . atomically $ U.waitMessage response
                  case response' of
                   Right () -> return ()
                   Left error -> do
                     messageText <- lookupText "Unable to send message to user"
                     FM.errorMessage frame messageText error
                return True
            Nothing -> return $ do FM.unboundFrameMessage frame
                                   return True
       Nothing -> do
         syntaxText <- lookupText "/notice <destination> <text>"
         FM.badCommandSyntaxMessage frame syntaxText
         return True
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
                   async . flip runAM intf $ do
                     response' <- liftIO . atomically $ C.waitNotice response
                     case response' of
                      Right () -> return ()
                      Left error -> do
                        messageText <- lookupText "Unable to send notice to channel"
                        FM.errorMessage frame messageText error
                   return True
               Nothing -> do return $ FM.notInChannelMessage frame dest''
                             return True
            Nothing -> do return $ FM.unboundFrameMessage frame
                          return True
       Just _ ->
         join . liftIO . atomically $ do
           manager <- F.getConnectionManager frame
           case manager of
            Just manager -> do
              user <- findOrCreateUser intf manager dest''
              response <- U.notice user text''
              return $ do
                async . flip runAM intf $ do
                  response' <- liftIO . atomically $ U.waitNotice response
                  case response' of
                   Right () -> return ()
                   Left error -> do
                     messageText <- lookupText "Unable to send notice to user"
                     FM.errorMessage frame messageText error
                return True
            Nothing -> do return $ FM.unboundFrameMessage frame
                          return True
       Nothing -> do
         syntaxText <- lookupText "/notice <destination> <text>"
         FM.badCommandSyntaxMessage frame syntaxText
         return True
  | otherwise = return False

-- | Join command handler.
joinHandler :: Frame -> T.Text -> StyledText -> AM Bool
joinHandler frame command text
  | command == "join" = do
      let text' = ST.removeStyle text
      let (name, rest) = T.break isSpace text
      let (key, rest') = T.break isSpace $ T.stripStart rest
      intf <- getInterface
      name' <- liftIO . atomically $ encodeFrame intf frame name
      key' <- do if key' /= T.empty
                   then liftIO . atomically . encodeFrame intf frame key
                   else return  Nothing
      case T.uncons name' of
       Just ('#', _)
         | ST.removeStyle rest' == "" ->
           join . liftIO . atomically $ do
             manager <- F.getConnectionManager frame
             case manager of
              Just manager -> do
                channel <- do
                  channel <- findChannel intf manager name'
                  case channel of
                   Just channel -> do
                     C.setKey channel key'
                     return channel
                   Nothing -> do
                     channel <- C.new intf manager name' key'
                     C.start channel
                     return channel
                findOrCreateChannelFrame intf channel
                autoJoin <- C.getAutoJoin channel
                if not autoJoin
                  then do response <- C.join channel
                          return $ do
                            async . flip runAM intf $ do
                              response' <- liftIO . atomically $ C.waitJoin response
                              case response' of
                               Right () -> return ()
                               Left error -> do
                                 messageText <- lookupText "Unable to join channel"
                                 FM.errorMessage frame messageText error
                            return True
                  else return $ return True
              Nothing -> return $ do FM.unboundFrameMessage frame
                                     return True
         | otherwise -> do
             syntaxText <- lookupText "/join <channel> [<key>]"
             FM.badCommandSyntaxMessage frame syntaxText
             return True
       Just _ -> do FM.notAChannelMessage frame dest'
                    return True
       Nothing -> do
         syntaxText <- lookupText "/join <channel> [<key>]"
         FM.badCommandSyntaxMessage frame syntaxText
         return True
  | otherwise = return False

-- | Part command handler.
partHandler :: Frame -> T.Text -> StyledText -> AM Bool
partHandler frame command text
  | command = "part" = do
      let comment = liftIO . atomically . encodeFrame intf frame $ ST.encode text
      let comment' = if message == B.empty then Nothing else Just message
      join . liftIO . atomically $ do
        mapping <- F.getMapping frame
        case mapping of
         FrmaChannel channel -> do
           response <- C.part channel comment'
           return $ do
             async . flip runAM intf $ do
               response' <- liftIO . atomically $ C.waitPart response
               case response' of
                Right () -> return ()
                Left error -> do
                  messageText <- lookupText "Unable to part channel"
                  FM.errorMessage frame messageText error
             return True
         _ -> return $ do FM.notChannelFrameMessage frame
                          return True
  | otherwise = return False

-- | Server command handler.
serverHandler :: Frame -> T.Text -> StyledText -> AM Bool
serverHandler frame command text
  | command == "server" = do
      let parts = filter (/= "") . T.splitOn " " $ ST.removeStyle text
      case parts of
       hostName : rest -> do
         let (port, rest') = uncons rest
         let (nick, rest'') = uncons rest'
         let (userName, rest''') = uncons rest''
         let (password, rest'''') = uncons rest'''
         let (port', validPort) = maybe (Nothing, True) (\port -> parsePort) port
         case validPort of
          True -> do
            config <- getConfig
            let encoding = confDefaultEncoding config
            let port'' = maybe (confDefaultPort config) id port'
                allNicks = map (encoEncoder encoding) $ maybe (confDefaultAllNicks config) (: []) nick
                userName' = encoEncoder encoding $ maybe (confDefaultUserName config) id userName
                password = maybe Nothing (encoEncoder encoding) $ maybe (confDefaultPassword config) Just password
                name = encoEncoder encoding $ confDefaultName config
                mode = confDefaultMode config
                ctcpUserInfo = confDefaultCtcpUserInfo config
            response <- liftIO . atomically $ do
              manager <- findOrCreateConnectionManager intf
              case I.getConnectionConfig intf manager of
               Nothing -> I.setConnectionConfig $ ConnectionConfig { cocoEncoding = encoding,
                                                                     cocoCtcpUserInfo = ctcpUserInfo }
               Just _ -> return ()
              findOrCreateConnectionFrame intf manager
              CM.connect manager $ ConnectionManagerSetup { comaServerName = hostName,
                                                            comaName = name,
                                                            comaOriginalHost = T.unpack hostName,
                                                            comaPort = port'',
                                                            comaUserName = userName',
                                                            comaAllNicks = allNicks,
                                                            comaPassword = password,
                                                            comaMode = mode }
            async . flip runAM intf $ do
              response' <- liftIO . atomically $ CM.waitConnect response
              case response of
                Right () -> return ()
                Left error -> do
                  messageText <- lookupText "Unable to connect to server"
                  FM.errorMessage frame messageText error
            return True
          False -> do
            syntaxText <- lookupText "/server <hostname> [<port> [<nick> [<username> [<password>]]]]"
            FM.badCommandSyntaxMessage frame syntaxText
            return True
       [] -> do
         syntaxText <- lookupText "/server <hostname> [<port> [<nick> [<username> [<password>]]]]"
         FM.badCommandSyntaxMessage frame syntaxText
         return True
  | otherwise = return False
  where parsePort port =
          case readMaybe $ T.unpack port of
           Just port | port >= 1 && port <= 65535 -> (Just port, True)
           _ -> (Nothing, False)

-- | Reconnect command handler.
reconnectHandler :: Frame -> T.Text -> StyledText -> AM Bool
reconnectHandler frame command text
  | command == "reconnect" = do
      intf <- getInterface
      let text' = liftIO . atomically . encodeFrame intf frame $ ST.encode text
      let comment = if text' == B.empty then Nothing else Just text'
      manager <- F.getConnectionManager frame
      case manager of
       Just manager ->
         async . flip runAM intf $ do
           connected <- liftIO . atomically $ CM.isConnected manager
           if connected
             then do quitResponse <- Cmd.quitNoWait manager comment
                     quitResponse' <- liftIO . atomically $ Cmd.waitQuitNoWait quitResponse
                     case quitResponse' of
                      QuitNoWaitSuccess -> return ()
                      QuitNoWaitError error -> do
                        messageText <- lookupText "Unable to send QUIT message to server"
                        FM.errorMessage frame messageText error
             else return ()
           reconnectResponse <- CM.reconnect manager
           reconnectResponse' <- liftIO . atomically $ CM.waitReconnect reconnectResponse
           case reconnectResponse' of
            Right () -> return ()
            Left error -> do
              messageText <- lookupText "Failed to reconnect to server"
              FM.errorMessage frame messageText error
       Nothing -> FM.unboundFrameMessage frame
      return True
  | otherwise = return False

-- | Disonnect command handler.
disconnectHandler :: Frame -> T.Text -> StyledText -> AM Bool
disconnectHandler frame command text
  | command == "reconnect" = do
      intf <- getInterface
      let text' = liftIO . atomically . encodeFrame intf frame $ ST.encode text
      let comment = if text' == B.empty then Nothing else Just text'
      manager <- F.getConnectionManager frame
      case manager of
       Just manager ->
         async . flip runAM intf $ do
           connected <- liftIO . atomically $ CM.isConnected manager
           if connected
             then do quitResponse <- Cmd.quitNoWait manager comment
                     quitResponse' <- liftIO . atomically $ Cmd.waitQuitNoWait quitResponse
                     case quitResponse' of
                      QuitNoWaitSuccess -> return ()
                      QuitNoWaitError error -> do
                        messageText <- lookupText "Unable to send QUIT message to server"
                        FM.errorMessage frame messageText error
                     disconnectResponse <- CM.disconnect manager
                     disconnectResponse' <- liftIO . atomically $ CM.waitDisconnect disconnectResponse
                     case reconnectResponse' of
                      Right () -> return ()
                      Left error -> do
                        messageText <- lookupText "Failed to disconnect from server"
                        FM.errorMessage frame messageText error
             else FM.notConnectedMessage frame
       Nothing -> FM.unboundFrameMessage frame
      return True
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
                   Just config ->
                     let text' = (encoEncoder $ cocoEncoding config) $ ST.encode text in
                     if text == B.empty then Nothing else Just text
                   Nothing -> Nothing
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

