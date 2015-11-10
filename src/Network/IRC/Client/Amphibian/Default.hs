module Network.IRC.Client.Amphibian.Default

  (getDefaultConfig)

  where

import Network.IRC.Client.Amphibian.Types
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
applicationName = T.pack "amphibian"

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
                    confDefaultScriptEntry = T.pack "entry",
                    confScriptCompileOptions = [T.pack "-O2"],
                    confScriptEvalOptions = [T.pack "-02"],
                    confLanguage = T.pack "en",
                    confTimeLocale = defaultTimeLocale,
                    confLightBackground = False,
                    confCtcpVersion = T.pack "Amphibian 0.1",
                    confCtcpSource = T.pack "http://github.com/tabemann/amphibian" }

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
