module Network.IRC.Client.Amphibian.InputHandler

       (installHandlers)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Monad
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frame as F
import qualified Network.IRC.Client.Amphibian.ConnectionManager as CM
import qualified Network.IRC.Client.Amphibian.Channel as C
import qualified Network.IRC.Client.Amphibian.User as U
import qualified Network.IRC.Client.Amphibian.FrameMessage as FM
import qualified Network.IRC.Client.Amphibian.StyledText as ST
import qualified Network.IRC.Client.Amphibian.InputDispatcher as ID
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Concurrent.STM (STM,
                               atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<$>))

-- | Install handlers.
installHandlers :: Interface -> STM ()
installHandlers intf = do
  dispatcher <- I.getInputDispatcher intf
  case dispatcher of
    Just dispatcher -> do
      ID.registerMessageHandler dispatcher defaultMessageHandler
      ID.registerCommandHandler dispatcher (T.pack "notice") noticeHandler
    Nothing -> return ()

-- | Encode text sent from a frame.
encode :: Frame -> T.Text -> AM B.ByteString
encode frame text = do
  manager <- liftIO . atomically $ F.getConnectionManager frame
  case manager of
    Just manager -> do
      config <- getConnectionConfig manager
      case config of
        Just config -> return $ (encoEncoder $ cocoEncoding config) text
        Nothing -> return B.empty
    Nothing -> return B.empty

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

-- | Notice command handler.
noticeHandler :: Frame -> T.Text -> StyledText -> AM Bool
noticeHandler frame command text =
  if command == T.pack "notice"
  then do
    comment <- encode frame $ ST.encode text
    
  else return False