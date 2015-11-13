module Network.IRC.Client.Amphibian.Frontend.Vty.VtyKeys

       (installHandlers)

       where

import Network.IRC.Client.Amphibian.Types
import Network.IRC.Client.Amphibian.Frontend.Vty.Types
import qualified Network.IRC.Client.Amphibian.Interface as I
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyFrontend as VF
import qualified Network.IRC.Client.Amphibian.Frontend.Vty.VtyWindow as VW
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Control.Concurrent.STM (STM,
                               atomically)
import qualified Graphics.Vty.Input as VI
import Data.Char (toUpper,
                  isControl)

-- | Install key handlers for the Vty frontend.
installHandlers :: VtyFrontend -> STM ()
installHandlers vtyFrontend = do
  VF.registerUnmappedKeyHandler vtyFrontend handleUnmapped
  VF.registerKeyHandler vtyFrontend VI.KLeft [] handleLeft
  VF.registerKeyHandler vtyFrontend VI.KRight [] handleRight
  VF.registerKeyHandler vtyFrontend VI.KUp [] handleUp
  VF.registerKeyHandler vtyFrontend VI.KDown [] handleDown
  VF.registerKeyHandler vtyFrontend VI.KPageUp [] handlePageUp
  VF.registerKeyHandler vtyFrontend VI.KPageDown [] handlePageDown

-- | Handle unmapped key.
handleUnmapped :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleUnmapped vtyFrontend (VI.KChar char) []
  | not $ isControl char =
      join . liftIO . atomically $ do
        currentWindow <- VF.getCurrentWindow vtyFrontend
        case currentWindow of
         Just currentWindow -> do
           VW.enterChar currentWindow char
           return $ VF.redraw vtyFrontend
         Nothing -> return $ return ()
handleUnmapped vtyFrontend (VI.KChar char) [VI.MShift]
  | not $ isControl char =
      join . liftIO . atomically $ do
        currentWindow <- VF.getCurrentWindow vtyFrontend
        case currentWindow of
         Just currentWindow -> do
           VW.enterChar currentWindow $ toUpper char
           return $ VF.redraw vtyFrontend
         Nothing -> return $ return ()
handleUnmapped _ _ _ = return ()

-- | Handle left key.
handleLeft :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleLeft vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.moveLeft currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle right key.
handleRight :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleRight vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.moveRight currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle up key.
handleUp :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleUp vtyFrontend _ - = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.prevInput currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle down key.
handleDown :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handleDown vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.nextInput currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle page up key.
handlePageUp :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handlePageUp vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.scrollUp currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

-- | Handle page down key.
handlePageDown :: VtyFrontend -> VI.Key -> [VI.Modifier] -> AM ()
handlePageDown vtyFrontend _ _ = do
  join . liftIO . atomically $ do
    curentWindow <- VF.getCurrentWindow vtyFrontend
    case currentWindow of
     Just currentWindow -> do
       VW.scrollDown currentWindow
       return $ VF.redraw vtyFrontend
     Nothing -> return $ return ()
  return True

