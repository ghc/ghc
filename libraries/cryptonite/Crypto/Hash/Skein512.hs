-- |
-- Module      : Crypto.Hash.Skein512
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Skein512 cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Skein512
    (  Skein512_224 (..), Skein512_256 (..), Skein512_384 (..), Skein512_512 (..)
    ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)


-- | Skein512 (224 bits) cryptographic hash algorithm
data Skein512_224 = Skein512_224
    deriving (Show,Data)

instance HashAlgorithm Skein512_224 where
    type HashBlockSize           Skein512_224 = 64
    type HashDigestSize          Skein512_224 = 28
    type HashInternalContextSize Skein512_224 = 160
    hashBlockSize  _          = 64
    hashDigestSize _          = 28
    hashInternalContextSize _ = 160
    hashInternalInit p        = c_skein512_init p 224
    hashInternalUpdate        = c_skein512_update
    hashInternalFinalize p    = c_skein512_finalize p 224

-- | Skein512 (256 bits) cryptographic hash algorithm
data Skein512_256 = Skein512_256
    deriving (Show,Data)

instance HashAlgorithm Skein512_256 where
    type HashBlockSize           Skein512_256 = 64
    type HashDigestSize          Skein512_256 = 32
    type HashInternalContextSize Skein512_256 = 160
    hashBlockSize  _          = 64
    hashDigestSize _          = 32
    hashInternalContextSize _ = 160
    hashInternalInit p        = c_skein512_init p 256
    hashInternalUpdate        = c_skein512_update
    hashInternalFinalize p    = c_skein512_finalize p 256

-- | Skein512 (384 bits) cryptographic hash algorithm
data Skein512_384 = Skein512_384
    deriving (Show,Data)

instance HashAlgorithm Skein512_384 where
    type HashBlockSize           Skein512_384 = 64
    type HashDigestSize          Skein512_384 = 48
    type HashInternalContextSize Skein512_384 = 160
    hashBlockSize  _          = 64
    hashDigestSize _          = 48
    hashInternalContextSize _ = 160
    hashInternalInit p        = c_skein512_init p 384
    hashInternalUpdate        = c_skein512_update
    hashInternalFinalize p    = c_skein512_finalize p 384

-- | Skein512 (512 bits) cryptographic hash algorithm
data Skein512_512 = Skein512_512
    deriving (Show,Data)

instance HashAlgorithm Skein512_512 where
    type HashBlockSize           Skein512_512 = 64
    type HashDigestSize          Skein512_512 = 64
    type HashInternalContextSize Skein512_512 = 160
    hashBlockSize  _          = 64
    hashDigestSize _          = 64
    hashInternalContextSize _ = 160
    hashInternalInit p        = c_skein512_init p 512
    hashInternalUpdate        = c_skein512_update
    hashInternalFinalize p    = c_skein512_finalize p 512


foreign import ccall unsafe "cryptonite_skein512_init"
    c_skein512_init :: Ptr (Context a) -> Word32 -> IO ()

foreign import ccall "cryptonite_skein512_update"
    c_skein512_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_skein512_finalize"
    c_skein512_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()
