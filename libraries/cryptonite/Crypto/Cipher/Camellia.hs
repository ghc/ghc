-- |
-- Module      : Crypto.Cipher.Camellia
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Camellia support. only 128 bit variant available for now.

module Crypto.Cipher.Camellia
    ( Camellia128
    ) where

import Crypto.Cipher.Camellia.Primitive
import Crypto.Cipher.Types

-- | Camellia block cipher with 128 bit key
newtype Camellia128 = Camellia128 Camellia

instance Cipher Camellia128 where
    cipherName    _ = "Camellia128"
    cipherKeySize _ = KeySizeFixed 16
    cipherInit k    = Camellia128 `fmap` initCamellia k

instance BlockCipher Camellia128 where
    blockSize _ = 16
    ecbEncrypt (Camellia128 key) = encrypt key
    ecbDecrypt (Camellia128 key) = decrypt key
