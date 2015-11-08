module Network.IRC.Client.Amphibian.Ctcp
       
       (checkCtcp,
        parseCtcp,
        formatCtcp,
        escapeCtcp,
        unescapeCtcp)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BUTF8
import Data.Char (isSpace)
import Data.Strings (byteToChar)

-- | Is a message or notice a CTCP request or reply?
checkCtcp :: MessageComment -> Maybe MessageComment
checkCtcp comment =
  case BUTF8.uncons $ unescapeCtcp comment of
    Just ('\x1', rest) ->
      let len = BUTF8.length rest of
      if len > 0
      then let (body, end) = BUTF8.splitAt (len - 1) rest in
           if end == '\x1'
           then Just body
           else Nothing
      else Nothing
    Nothing -> Nothing

-- | Parse CTCP request or response.
parseCtcp :: MessageComment -> Maybe (CtcpCommand, Maybe CtcpArgument)
parseCtcp comment =
  let (command, rest) = BUTF8.break isSpace comment in
  if command /= B.empty
  then case BUTF8.uncons rest of
    Just (_, argument) -> Just (command, Just argument)
    Nothing -> Just (command, Nothing)
  else Nothing

-- | Format CTCP request or response.
formatCtcp :: CtcpCommand -> Maybe CtcpArgument -> MessageComment
formatCtcp command (Just argument) =
  escapeCtcp $ B.concat [BC.singleton "\x1", command, BC.singleton " ", argument, BC.singleton "\x1"]
formatCtcp command nothing =
  escapeCtcp $ B.concat [BC.singleton "\x1", command, BC.singleton "\x1"]

-- | Escape CTCP data.
escapeCtcp :: B.ByteString -> MessageComment
escapeCtcp data =
  B.concat $ B.foldr' doEscape [] data
  where doEscape byte accum =
    case byteToChar byte of
      '\r' -> B.append (BC.singleton '\x10') (BC.singleton 'r') : accum
      '\n' -> B.append (BC.singleton '\x10') (BC.singleton 'n') : accum
      '\x0' -> B.append (BC.singleton '\x10') (BC.singleton '0') : accum
      '\x10' -> B.append (BC.singleton '\x10') (BC.singleton '\x10') : accum
      _ -> B.singleton byte : accum

-- | Unescape CTCP data.
unescapeCtcp :: MessageComment -> B.ByteString
unescapeCtcp comment = unescapeCtcp' comment []
  where unescapeCtcp' data accum =
    case B.uncons data of
      Just (byte, rest)
        | byteToChar byte == '\x10' ->
          case B.uncons rest of
            Just (byte, rest)
              | byteToChar byte == 'r' -> unescapeCtcp' rest (BC.singleton '\r' : accum)
              | byteToChar byte == 'n' -> unescapeCtcp' rest (BC.singleton '\n' : accum)
              | byteToChar byte == '0' -> unescapeCtcp' rest (BC.singleton '\x0' : accum)
              | byteToChar byte == '\x10' -> unescapeCtcp' rest (BC.singleton '\x10' : accum)
            _ -> unescapeCtcp' rest (BC.singleton '\x10' : accum)
        | otherwise -> unescapeCtcp' data (B.singleton byte : accum)
      Nothing -> B.concat $ reverse accum