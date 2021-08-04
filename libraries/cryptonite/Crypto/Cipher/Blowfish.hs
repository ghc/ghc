-- |
-- Module      : Crypto.Cipher.Blowfish
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.Cipher.Blowfish
    ( Blowfish
    , Blowfish64
    , Blowfish128
    , Blowfish256
    , Blowfish448
    ) where

import Crypto.Internal.Imports
import Crypto.Cipher.Types
import Crypto.Cipher.Blowfish.Primitive

-- | variable keyed blowfish state
newtype Blowfish = Blowfish Context
    deriving (NFData)

-- | 64 bit keyed blowfish state
newtype Blowfish64 = Blowfish64 Context
    deriving (NFData)

-- | 128 bit keyed blowfish state
newtype Blowfish128 = Blowfish128 Context
    deriving (NFData)

-- | 256 bit keyed blowfish state
newtype Blowfish256 = Blowfish256 Context
    deriving (NFData)

-- | 448 bit keyed blowfish state
newtype Blowfish448 = Blowfish448 Context
    deriving (NFData)

instance Cipher Blowfish where
    cipherName _    = "blowfish"
    cipherKeySize _ = KeySizeRange 6 56
    cipherInit k    = Blowfish `fmap` initBlowfish k

instance BlockCipher Blowfish where
    blockSize _ = 8
    ecbEncrypt (Blowfish bf) = encrypt bf
    ecbDecrypt (Blowfish bf) = decrypt bf

#define INSTANCE_CIPHER(CSTR, NAME, KEYSIZE) \
instance Cipher CSTR where \
    { cipherName _ = NAME \
    ; cipherKeySize _ = KeySizeFixed KEYSIZE \
    ; cipherInit k = CSTR `fmap` initBlowfish k \
    }; \
instance BlockCipher CSTR where \
    { blockSize _ = 8 \
    ; ecbEncrypt (CSTR bf) = encrypt bf \
    ; ecbDecrypt (CSTR bf) = decrypt bf \
    };

INSTANCE_CIPHER(Blowfish64, "blowfish64", 8)
INSTANCE_CIPHER(Blowfish128, "blowfish128", 16)
INSTANCE_CIPHER(Blowfish256, "blowfish256", 32)
INSTANCE_CIPHER(Blowfish448, "blowfish448", 56)
