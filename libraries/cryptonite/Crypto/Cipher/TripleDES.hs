-- |
-- Module      : Crypto.Cipher.TripleDES
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ???

module Crypto.Cipher.TripleDES
    ( DES_EEE3
    , DES_EDE3
    , DES_EEE2
    , DES_EDE2
    ) where

import           Data.Word
import           Crypto.Error
import           Crypto.Cipher.Types
import           Crypto.Cipher.DES.Primitive
import           Crypto.Internal.ByteArray (ByteArrayAccess)
import qualified Crypto.Internal.ByteArray as B
import           Data.Memory.Endian

-- | 3DES with 3 different keys used all in the same direction
data DES_EEE3 = DES_EEE3 Word64 Word64 Word64
    deriving (Eq)

-- | 3DES with 3 different keys used in alternative direction
data DES_EDE3 = DES_EDE3 Word64 Word64 Word64 
    deriving (Eq)

-- | 3DES where the first and third keys are equal, used in the same direction
data DES_EEE2 = DES_EEE2 Word64 Word64 -- key1 and key3 are equal
    deriving (Eq)

-- | 3DES where the first and third keys are equal, used in alternative direction
data DES_EDE2 = DES_EDE2 Word64 Word64 -- key1 and key3 are equal
    deriving (Eq)

instance Cipher DES_EEE3 where
    cipherName    _ = "3DES_EEE"
    cipherKeySize _ = KeySizeFixed 24
    cipherInit k    = init3DES DES_EEE3 k

instance Cipher DES_EDE3 where
    cipherName    _ = "3DES_EDE"
    cipherKeySize _ = KeySizeFixed 24
    cipherInit k    = init3DES DES_EDE3 k

instance Cipher DES_EDE2 where
    cipherName    _ = "2DES_EDE"
    cipherKeySize _ = KeySizeFixed 16
    cipherInit k    = init2DES DES_EDE2 k

instance Cipher DES_EEE2 where
    cipherName    _ = "2DES_EEE"
    cipherKeySize _ = KeySizeFixed 16
    cipherInit k    = init2DES DES_EEE2 k

instance BlockCipher DES_EEE3 where
    blockSize _ = 8
    ecbEncrypt (DES_EEE3 k1 k2 k3) = B.mapAsWord64 (unBlock . (encrypt k3 . encrypt k2 . encrypt k1) . Block)
    ecbDecrypt (DES_EEE3 k1 k2 k3) = B.mapAsWord64 (unBlock . (decrypt k1 . decrypt k2 . decrypt k3) . Block)

instance BlockCipher DES_EDE3 where
    blockSize _ = 8
    ecbEncrypt (DES_EDE3 k1 k2 k3) = B.mapAsWord64 (unBlock . (encrypt k3 . decrypt k2 . encrypt k1) . Block)
    ecbDecrypt (DES_EDE3 k1 k2 k3) = B.mapAsWord64 (unBlock . (decrypt k1 . encrypt k2 . decrypt k3) . Block)

instance BlockCipher DES_EEE2 where
    blockSize _ = 8
    ecbEncrypt (DES_EEE2 k1 k2) = B.mapAsWord64 (unBlock . (encrypt k1 . encrypt k2 . encrypt k1) . Block)
    ecbDecrypt (DES_EEE2 k1 k2) = B.mapAsWord64 (unBlock . (decrypt k1 . decrypt k2 . decrypt k1) . Block)

instance BlockCipher DES_EDE2 where
    blockSize _ = 8
    ecbEncrypt (DES_EDE2 k1 k2) = B.mapAsWord64 (unBlock . (encrypt k1 . decrypt k2 . encrypt k1) . Block)
    ecbDecrypt (DES_EDE2 k1 k2) = B.mapAsWord64 (unBlock . (decrypt k1 . encrypt k2 . decrypt k1) . Block)

init3DES :: ByteArrayAccess key => (Word64 -> Word64 -> Word64 -> a) -> key -> CryptoFailable a
init3DES constr k
    | len == 24 = CryptoPassed $ constr k1 k2 k3
    | otherwise = CryptoFailed CryptoError_KeySizeInvalid
  where len = B.length k
        (k1, k2, k3) = (fromBE $ B.toW64BE k 0, fromBE $ B.toW64BE k 8, fromBE $ B.toW64BE k 16)

init2DES :: ByteArrayAccess key => (Word64 -> Word64 -> a) -> key -> CryptoFailable a
init2DES constr k
    | len == 16 = CryptoPassed $ constr k1 k2
    | otherwise = CryptoFailed CryptoError_KeySizeInvalid
  where len = B.length k
        (k1, k2) = (fromBE $ B.toW64BE k 0, fromBE $ B.toW64BE k 8)
