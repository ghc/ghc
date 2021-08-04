-- |
-- Module      : Crypto.Cipher.DES
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
module Crypto.Cipher.DES
    ( DES
    ) where

import           Data.Word
import           Crypto.Error
import           Crypto.Cipher.Types
import           Crypto.Cipher.DES.Primitive
import           Crypto.Internal.ByteArray (ByteArrayAccess)
import qualified Crypto.Internal.ByteArray as B
import           Data.Memory.Endian

-- | DES Context
data DES = DES Word64
    deriving (Eq)

instance Cipher DES where
    cipherName    _ = "DES"
    cipherKeySize _ = KeySizeFixed 8
    cipherInit k    = initDES k

instance BlockCipher DES where
    blockSize _ = 8
    ecbEncrypt (DES key) = B.mapAsWord64 (unBlock . encrypt key . Block)
    ecbDecrypt (DES key) = B.mapAsWord64 (unBlock . decrypt key . Block)

initDES :: ByteArrayAccess key => key -> CryptoFailable DES
initDES k
    | len == 8  = CryptoPassed $ DES key
    | otherwise = CryptoFailed $ CryptoError_KeySizeInvalid
  where len = B.length k
        key = fromBE $ B.toW64BE k 0
