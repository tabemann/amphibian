module Network.IRC.Client.Amphibian.Utility
       
       (extractNick,
        convertAddRemove,
        isError,
        unique)

       where

import Network.IRC.Client.Amphibian.Types
import Text.Read (readMaybe)

-- | Extract nick.
extractNick :: Maybe MessagePrefix -> Maybe Nick
extractNick (Just prefix) = Just . first $ BUTF8.break (== '!') prefix
extractNick Nothing = Nothing

-- | Convert add/remove into a bytestring.
convertAddRemove :: AddRemove -> B.ByteString
convertAddRemove Add = BUTF8.fromString "+"
convertAddRemove Remove = BUTF8.fromString "-"

-- | Check whether command corresponds to an error
isError :: MessageCommand -> Bool
isError command =
  case readMaybe (BUTF8.toString command) :: Maybe Integer of
    Just numeric -> numeric >= 400
    Nothing -> False

-- | Make a list containing all the unique entries of a list, favoring the first unique entries in the list.
unique :: Eq a => [a] -> [a]
unique (x : xs) = x : unique (filter (/= x) xs)
unique [] = []