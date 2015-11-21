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

module Network.IRC.Client.Amphibian.Frontend.Vty.Types

       (VtyFrontend(..),
        VtyWindow(..))

       where

import Network.IRC.Client.Aphibian.Types
import Graphics.Vty (Vty)
import Graphics.Vty.Input (Key,
                           Modifier)
import Control.Concurrent.STM (STM,
                               TVar,
                               TMVar)
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
                vtfrCurrentWindowIndex :: TVar (Maybe Int),
                vtfrWindows :: TVar (Seq VtyWindow),
                vtfrWindowServer :: TVar (Maybe VtyWindowServer),
                vtfrKeyMappings :: TVar (Map VtyKeyCombination [VtyKeyHandler]),
                vtfrUnmappedKeyHandlers :: TVar [VtyUnmappedKeyHandler] }
  deriving Eq

-- | Vty window type.
data VtyWindow =
  VtyWindow { vtwiFrontend :: VtyFrontend,
              vtwiFrame :: Frame,
              vtwiSubscription :: FrameOutputSubscription,
              vtwiActive :: TVar Bool,
              vtwiBufferLines :: TVar (Seq FrameLine),
              vtwiBufferPosition :: TVar VtyBufferPosition,
              vtwiPrevInputBuffer :: TVar (Seq StyledText),
              vtwiPrevInputPosition :: TVar Int,
              vtwiInputText :: TVar StyledText,
              vtwiInputCursorPosition :: TVar Int,
              vtwiInputVisiblePosition :: TVar Int }
  deriving Eq

-- | Vty window server type.
data VtyWindowServer =
  VtyWindowServer { vtwsFrontend :: VtyFrontend,
                    vtwsRunning :: TVar Bool,
                    vtwsActions :: TQueue VtyWindowServerAction }
  deriving Eq

-- | Vty window server action type.
data VtyWindowServerAction = VtwsStartWindow VtyWindow VtyWindowStartResponse
                           | VtwsStop VtyWindowServerStopResponse
                           deriving Eq

-- | Vty window server stop response type.
newtype VtyWindowServerStopResponse = VtyWindowServerStopResponse (TMVar (Either Error ()))

-- | Vty window start response type.
newtype VtyWindowStartResponse = VtyWindowStartResponse (TMVar (Either Error ()))

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

