module Network.IRC.Client.Amphibian.Language
       
       (defaultLanguage,
        availableLanguages)

       where

import qualified Data.Text as T

-- | Default language.
defaultLanguage :: T.Text
defaultLanguage = T.pack "en"

-- | Available languages.
availableLanguages :: [T.Text]
availableLanguages = [T.pack "en"]
