-- |
-- Module      : Crypto.Hash.Blake2s
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Blake2s cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Blake2s
    (  Blake2s_160 (..), Blake2s_224 (..), Blake2s_256 (..)
    ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)


-- | Blake2s (160 bits) cryptographic hash algorithm
data Blake2s_160 = Blake2s_160
    deriving (Show,Data)

instance HashAlgorithm Blake2s_160 where
    type HashBlockSize           Blake2s_160 = 64
    type HashDigestSize          Blake2s_160 = 20
    type HashInternalContextSize Blake2s_160 = 136
    hashBlockSize  _          = 64
    hashDigestSize _          = 20
    hashInternalContextSize _ = 136
    hashInternalInit p        = c_blake2s_init p 160
    hashInternalUpdate        = c_blake2s_update
    hashInternalFinalize p    = c_blake2s_finalize p 160

-- | Blake2s (224 bits) cryptographic hash algorithm
data Blake2s_224 = Blake2s_224
    deriving (Show,Data)

instance HashAlgorithm Blake2s_224 where
    type HashBlockSize           Blake2s_224 = 64
    type HashDigestSize          Blake2s_224 = 28
    type HashInternalContextSize Blake2s_224 = 136
    hashBlockSize  _          = 64
    hashDigestSize _          = 28
    hashInternalContextSize _ = 136
    hashInternalInit p        = c_blake2s_init p 224
    hashInternalUpdate        = c_blake2s_update
    hashInternalFinalize p    = c_blake2s_finalize p 224

-- | Blake2s (256 bits) cryptographic hash algorithm
data Blake2s_256 = Blake2s_256
    deriving (Show,Data)

instance HashAlgorithm Blake2s_256 where
    type HashBlockSize           Blake2s_256 = 64
    type HashDigestSize          Blake2s_256 = 32
    type HashInternalContextSize Blake2s_256 = 136
    hashBlockSize  _          = 64
    hashDigestSize _          = 32
    hashInternalContextSize _ = 136
    hashInternalInit p        = c_blake2s_init p 256
    hashInternalUpdate        = c_blake2s_update
    hashInternalFinalize p    = c_blake2s_finalize p 256


foreign import ccall unsafe "cryptonite_blake2s_init"
    c_blake2s_init :: Ptr (Context a) -> Word32 -> IO ()

foreign import ccall "cryptonite_blake2s_update"
    c_blake2s_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_blake2s_finalize"
    c_blake2s_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()
