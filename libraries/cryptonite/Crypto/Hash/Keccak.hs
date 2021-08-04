-- |
-- Module      : Crypto.Hash.Keccak
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Keccak cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Keccak
    (  Keccak_224 (..), Keccak_256 (..), Keccak_384 (..), Keccak_512 (..)
    ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)


-- | Keccak (224 bits) cryptographic hash algorithm
data Keccak_224 = Keccak_224
    deriving (Show,Data)

instance HashAlgorithm Keccak_224 where
    type HashBlockSize           Keccak_224 = 144
    type HashDigestSize          Keccak_224 = 28
    type HashInternalContextSize Keccak_224 = 352
    hashBlockSize  _          = 144
    hashDigestSize _          = 28
    hashInternalContextSize _ = 352
    hashInternalInit p        = c_keccak_init p 224
    hashInternalUpdate        = c_keccak_update
    hashInternalFinalize p    = c_keccak_finalize p 224

-- | Keccak (256 bits) cryptographic hash algorithm
data Keccak_256 = Keccak_256
    deriving (Show,Data)

instance HashAlgorithm Keccak_256 where
    type HashBlockSize           Keccak_256 = 136
    type HashDigestSize          Keccak_256 = 32
    type HashInternalContextSize Keccak_256 = 344
    hashBlockSize  _          = 136
    hashDigestSize _          = 32
    hashInternalContextSize _ = 344
    hashInternalInit p        = c_keccak_init p 256
    hashInternalUpdate        = c_keccak_update
    hashInternalFinalize p    = c_keccak_finalize p 256

-- | Keccak (384 bits) cryptographic hash algorithm
data Keccak_384 = Keccak_384
    deriving (Show,Data)

instance HashAlgorithm Keccak_384 where
    type HashBlockSize           Keccak_384 = 104
    type HashDigestSize          Keccak_384 = 48
    type HashInternalContextSize Keccak_384 = 312
    hashBlockSize  _          = 104
    hashDigestSize _          = 48
    hashInternalContextSize _ = 312
    hashInternalInit p        = c_keccak_init p 384
    hashInternalUpdate        = c_keccak_update
    hashInternalFinalize p    = c_keccak_finalize p 384

-- | Keccak (512 bits) cryptographic hash algorithm
data Keccak_512 = Keccak_512
    deriving (Show,Data)

instance HashAlgorithm Keccak_512 where
    type HashBlockSize           Keccak_512 = 72
    type HashDigestSize          Keccak_512 = 64
    type HashInternalContextSize Keccak_512 = 280
    hashBlockSize  _          = 72
    hashDigestSize _          = 64
    hashInternalContextSize _ = 280
    hashInternalInit p        = c_keccak_init p 512
    hashInternalUpdate        = c_keccak_update
    hashInternalFinalize p    = c_keccak_finalize p 512


foreign import ccall unsafe "cryptonite_keccak_init"
    c_keccak_init :: Ptr (Context a) -> Word32 -> IO ()

foreign import ccall "cryptonite_keccak_update"
    c_keccak_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_keccak_finalize"
    c_keccak_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()
