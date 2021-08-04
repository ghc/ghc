-- |
-- Module      : Crypto.Cipher.CAST5
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : stable
-- Portability : good
--
module Crypto.Cipher.CAST5
    ( CAST5
    ) where

import           Crypto.Error
import           Crypto.Cipher.Types
import           Crypto.Cipher.CAST5.Primitive
import           Crypto.Internal.ByteArray (ByteArrayAccess)
import qualified Crypto.Internal.ByteArray as B

-- | CAST5 block cipher (also known as CAST-128).  Key is between
-- 40 and 128 bits.
newtype CAST5 = CAST5 Key

instance Cipher CAST5 where
    cipherName    _ = "CAST5"
    cipherKeySize _ = KeySizeRange 5 16
    cipherInit      = initCAST5

instance BlockCipher CAST5 where
    blockSize _ = 8
    ecbEncrypt (CAST5 k) = B.mapAsWord64 (encrypt k)
    ecbDecrypt (CAST5 k) = B.mapAsWord64 (decrypt k)

initCAST5 :: ByteArrayAccess key => key -> CryptoFailable CAST5
initCAST5 bs
    | len <   5 = CryptoFailed CryptoError_KeySizeInvalid
    | len <  16 = CryptoPassed (CAST5 $ buildKey short padded)
    | len == 16 = CryptoPassed (CAST5 $ buildKey False bs)
    | otherwise = CryptoFailed CryptoError_KeySizeInvalid
  where
    len   = B.length bs
    short = len <= 10

    padded :: B.Bytes
    padded = B.convert bs `B.append` B.replicate (16 - len) 0
