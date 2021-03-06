module Apollo.Crypto where

import Apollo.Types ( Sha1Hash(..) )

import Crypto.Hash
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( pack )

sha1 :: ByteString -> Sha1Hash
sha1 = Sha1Hash . pack . show . h where
  h :: ByteString -> Digest SHA1
  h = hash
