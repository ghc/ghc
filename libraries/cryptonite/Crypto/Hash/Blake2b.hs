-- |
-- Module      : Crypto.Hash.Blake2b
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Blake2b cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Blake2b
    (  Blake2b_160 (..), Blake2b_224 (..), Blake2b_256 (..), Blake2b_384 (..), Blake2b_512 (..)
    ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)


-- | Blake2b (160 bits) cryptographic hash algorithm
data Blake2b_160 = Blake2b_160
    deriving (Show,Data)

instance HashAlgorithm Blake2b_160 where
    type HashBlockSize           Blake2b_160 = 128
    type HashDigestSize          Blake2b_160 = 20
    type HashInternalContextSize Blake2b_160 = 248
    hashBlockSize  _          = 128
    hashDigestSize _          = 20
    hashInternalContextSize _ = 248
    hashInternalInit p        = c_blake2b_init p 160
    hashInternalUpdate        = c_blake2b_update
    hashInternalFinalize p    = c_blake2b_finalize p 160

-- | Blake2b (224 bits) cryptographic hash algorithm
data Blake2b_224 = Blake2b_224
    deriving (Show,Data)

instance HashAlgorithm Blake2b_224 where
    type HashBlockSize           Blake2b_224 = 128
    type HashDigestSize          Blake2b_224 = 28
    type HashInternalContextSize Blake2b_224 = 248
    hashBlockSize  _          = 128
    hashDigestSize _          = 28
    hashInternalContextSize _ = 248
    hashInternalInit p        = c_blake2b_init p 224
    hashInternalUpdate        = c_blake2b_update
    hashInternalFinalize p    = c_blake2b_finalize p 224

-- | Blake2b (256 bits) cryptographic hash algorithm
data Blake2b_256 = Blake2b_256
    deriving (Show,Data)

instance HashAlgorithm Blake2b_256 where
    type HashBlockSize           Blake2b_256 = 128
    type HashDigestSize          Blake2b_256 = 32
    type HashInternalContextSize Blake2b_256 = 248
    hashBlockSize  _          = 128
    hashDigestSize _          = 32
    hashInternalContextSize _ = 248
    hashInternalInit p        = c_blake2b_init p 256
    hashInternalUpdate        = c_blake2b_update
    hashInternalFinalize p    = c_blake2b_finalize p 256

-- | Blake2b (384 bits) cryptographic hash algorithm
data Blake2b_384 = Blake2b_384
    deriving (Show,Data)

instance HashAlgorithm Blake2b_384 where
    type HashBlockSize           Blake2b_384 = 128
    type HashDigestSize          Blake2b_384 = 48
    type HashInternalContextSize Blake2b_384 = 248
    hashBlockSize  _          = 128
    hashDigestSize _          = 48
    hashInternalContextSize _ = 248
    hashInternalInit p        = c_blake2b_init p 384
    hashInternalUpdate        = c_blake2b_update
    hashInternalFinalize p    = c_blake2b_finalize p 384

-- | Blake2b (512 bits) cryptographic hash algorithm
data Blake2b_512 = Blake2b_512
    deriving (Show,Data)

instance HashAlgorithm Blake2b_512 where
    type HashBlockSize           Blake2b_512 = 128
    type HashDigestSize          Blake2b_512 = 64
    type HashInternalContextSize Blake2b_512 = 248
    hashBlockSize  _          = 128
    hashDigestSize _          = 64
    hashInternalContextSize _ = 248
    hashInternalInit p        = c_blake2b_init p 512
    hashInternalUpdate        = c_blake2b_update
    hashInternalFinalize p    = c_blake2b_finalize p 512


foreign import ccall unsafe "cryptonite_blake2b_init"
    c_blake2b_init :: Ptr (Context a) -> Word32 -> IO ()

foreign import ccall "cryptonite_blake2b_update"
    c_blake2b_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_blake2b_finalize"
    c_blake2b_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()
