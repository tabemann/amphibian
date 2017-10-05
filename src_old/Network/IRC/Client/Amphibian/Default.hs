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

module Network.IRC.Client.Amphibian.Default

  (getDefaultConfig)

  where

import Network.IRC.Client.Amphibian.Types
import qualified Network.IRC.Client.Amphibian.Encoding as E
import qualified Data.Text as T
import System.Locale (TimeLocale,
                      defaultTimeLocale)
import System.IO (FilePath)
import System.Directory (createDirectoryIfMissing,
                         doesDirectoryExist,
                         getAppUserDataDirectory)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)

-- | Application name
applicationName :: T.Text
applicationName = "amphibian"

-- | Get default configuration.
getDefaultConfig :: IO Config
getDefaultConfig = do
  configDir <- getDirectory getUserConfigDir
  dataDir <- getDirectory getUserDataDir
  return $ Config { confConfigDir = configDir,
                    confDataDir = dataDir,
                    confConfigPluginName = "amphibian.hs",
                    confPluginError = Right (),
                    confServerList = [],
                    confDefaultScriptEntry = "entry",
                    confScriptCompileOptions = ["-O2"],
                    confScriptEvalOptions = ["-02"],
                    confLanguage = "en",
                    confTimeLocale = defaultTimeLocale,
                    confLightBackground = False,
                    confCtcpVersion = "Amphibian 0.1",
                    confCtcpSource = "http://github.com/tabemann/amphibian",
                    confDefaultPort = 6667,
                    confDefaultUserName = "jrandomhacker",
                    confDefaultName = "J. Random Hacker",
                    confDefaultAllNicks = ["jrandomhacker"],
                    confDefaultPassword = Nothing,
                    confDefaultMode = [],
                    confDefaultEncoding = E.defaultEncoding,
                    confDefaultCtcpUserInfo = "I am J. Random Hacker." }

-- | Get directory.
getDirectory :: (String -> IO FilePath) -> IO FilePath
getDirectory query = do
  newDir <- query $ T.unpack applicationName
  newDirExists <- doesDirectoryExist newDir
  if newDirExists
  then return newDir
  else do
    oldDir <- getAppUserDataDirectory $ T.upack applicationName
    oldDirExists <- doesDirectoryExist oldDir
    if oldDirExists
    then return oldDir
    else do
      createDirectoryIfMissing True newDir
      return newDir
