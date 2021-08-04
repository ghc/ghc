-- |
-- Module      : Crypto.MAC.HMAC
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- provide the HMAC (Hash based Message Authentification Code) base algorithm.
-- <http://en.wikipedia.org/wiki/HMAC>
--
module Crypto.MAC.HMAC
    ( hmac
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Bits (xor)

-- | compute a MAC using the supplied hashing function
--
-- An incremental API can be found in the module "Crypto.Hash".
--
hmac :: (ByteString -> ByteString) -- ^ hash function
     -> Int -- ^ block size
     -> ByteString -- ^ secret
     -> ByteString -- ^ message
     -> ByteString
hmac hashF blockSize secret msg = hashF $ B.append opad (hashF $ B.append ipad msg)
    where opad = B.map (xor 0x5c) k'
          ipad = B.map (xor 0x36) k'

          k'  = B.append kt pad
          kt  = if B.length secret > fromIntegral blockSize then hashF secret else secret
          pad = B.replicate (fromIntegral blockSize - B.length kt) 0
