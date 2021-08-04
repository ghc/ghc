-- |
-- Module      : Crypto.Hash.Whirlpool
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Whirlpool cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Whirlpool ( Whirlpool (..) ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)

-- | Whirlpool cryptographic hash algorithm
data Whirlpool = Whirlpool
    deriving (Show,Data)

instance HashAlgorithm Whirlpool where
    type HashBlockSize           Whirlpool = 64
    type HashDigestSize          Whirlpool = 64
    type HashInternalContextSize Whirlpool = 168
    hashBlockSize  _          = 64
    hashDigestSize _          = 64
    hashInternalContextSize _ = 168
    hashInternalInit          = c_whirlpool_init
    hashInternalUpdate        = c_whirlpool_update
    hashInternalFinalize      = c_whirlpool_finalize

foreign import ccall unsafe "cryptonite_whirlpool_init"
    c_whirlpool_init :: Ptr (Context a)-> IO ()

foreign import ccall "cryptonite_whirlpool_update"
    c_whirlpool_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_whirlpool_finalize"
    c_whirlpool_finalize :: Ptr (Context a) -> Ptr (Digest a) -> IO ()
