module Crypto.Cipher.Twofish
    ( Twofish128
    , Twofish192
    , Twofish256
    ) where

import Crypto.Cipher.Twofish.Primitive
import Crypto.Cipher.Types
import Crypto.Cipher.Utils

newtype Twofish128 = Twofish128 Twofish

instance Cipher Twofish128 where
    cipherName    _ = "Twofish128"
    cipherKeySize _ = KeySizeFixed 16
    cipherInit key  = Twofish128 <$> (initTwofish =<< validateKeySize (undefined :: Twofish128) key)

instance BlockCipher Twofish128 where
    blockSize                 _ = 16
    ecbEncrypt (Twofish128 key) = encrypt key
    ecbDecrypt (Twofish128 key) = decrypt key

newtype Twofish192 = Twofish192 Twofish

instance Cipher Twofish192 where
    cipherName    _ = "Twofish192"
    cipherKeySize _ = KeySizeFixed 24
    cipherInit key  = Twofish192 <$> (initTwofish =<< validateKeySize (undefined :: Twofish192) key)

instance BlockCipher Twofish192 where
    blockSize                 _ = 16
    ecbEncrypt (Twofish192 key) = encrypt key
    ecbDecrypt (Twofish192 key) = decrypt key

newtype Twofish256 = Twofish256 Twofish

instance Cipher Twofish256 where
    cipherName    _ = "Twofish256"
    cipherKeySize _ = KeySizeFixed 32
    cipherInit key  = Twofish256 <$> (initTwofish =<< validateKeySize (undefined :: Twofish256) key)

instance BlockCipher Twofish256 where
    blockSize                 _ = 16
    ecbEncrypt (Twofish256 key) = encrypt key
    ecbDecrypt (Twofish256 key) = decrypt key
