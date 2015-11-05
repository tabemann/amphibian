module Network.IRC.Client.Amphibian.Encoding

       (defaultEncoding,
        availableEncodings)

       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Default encoding.
defaultEncoding :: Encoding
defaultEncoding = utf8Encoding

-- | Available encodings.
availableEncodings :: [Encoding]
availableEncodings = [utf8Encoding, latin1Encoding]

-- | UTF-8 encoding.
utf8Encoding :: Encoding
utf8Encoding = Encoding { encoName = T.pack "UTF-8",
                          encoEncoder = TE.encodeUtf8,
                          encoDecoder = TE.decodeUtf8 }

-- | latin-1 encoding.
latin1Encoding :: Encoding
latin1Encoding = Encoding { encoName = T.pack "latin-1",
                            encoEncoder = BC.pack . T.unpack,
                            encoDecoder = TE.decodeLatin1 }