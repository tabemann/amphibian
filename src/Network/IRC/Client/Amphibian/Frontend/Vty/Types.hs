module Network.IRC.Client.Amphibian.Frontend.Vty.Types

       (VtyFrontend(..),
        VtyWindow(..))

       where

import Network.IRC.Client.Aphibian.Types
import Graphics.Vty (Vty)
import Graphics.Vty.Input (Key,
                           Modifier)
import Control.Concurrent.STM (STM,
                               TVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Map.Strict (Map)

-- | Vty frontend type.
data VtyFrontend =
  VtyFrontend { vtfrInterface :: Interface,
                vtfrFrontend :: TVar (Maybe Frontend)
                vtfrFrontendSubscription :: TVar (Maybe FrontendOutputSubscription),
                vtfrRunning :: TVar Bool,
                vtfrVty :: TVar (Maybe Vty),
                vtfrHeight :: TVar Int,
                vtfrWidth :: TVar Int,
                vtfrCurrentWindow :: TVar (Maybe VtyWindow),
                vtfrWindows :: TVar [VtyWindow],
                vtfrKeyMappings :: TVar (Map VtyKeyCombination [VtyKeyHandler]),
                vtfrUnmappedKeyHandlers :: TVar [VtyUnmappedKeyHandler] }

-- | Vty window type.
data VtyWindow =
  VtyWindow { vtwiFrontend :: VtyFrontend,
              vtwiFrame :: Frame,
              vtwiBufferLines :: TVar (Seq FrameLine),
              vtwiBufferPosition :: TVar VtyBufferPosition,
              vtwiPrevInputBuffer :: TVar (Seq StyledText),
              vtwiPrevInputPosition :: TVar Int,
              vtwiInputText :: TVar StyledText,
              vtwiInputCursorPosition :: TVar Int,
              vtwiInputVisiblePosition :: TVar Int }

-- | Vty buffer position type.
data VtyBufferPosition = VtbpFixed Int
                       | VtbpDynamic
                       deriving Eq

-- | Vty key combination type.
data VtyKeyCombination =
  VtyKeyCombination { vtkcKey :: Key,
                      vtkcModifiers :: [Modifier] }
  deriving (Eq, Ord)

-- | Vty key handler type.
data VtyKeyHandler =
  VtyKeyHandler ( vtkhFrontend :: VtyFrontend,
                  vtkhKeyCombo :: VtyKeyCombination,
                  vtkhHandler :: TVar (VtyFrontend -> Key -> [Modifier] -> AM Bool) )
  deriving Eq

-- | Vty unmapped key handler type.
data VtyUnmappedKeyHandler =
  VtyUnmappedKeyHandler ( vukhFrontend :: VtyFrontend,
                          vukhHandler :: TVar (VtyFrontend -> Key -> [Modifier] -> AM Bool) )
  deriving Eq

