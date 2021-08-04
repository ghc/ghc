-- |
-- Module      : Crypto.Hash.SHA384
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- SHA384 cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.SHA384 ( SHA384 (..) ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)

-- | SHA384 cryptographic hash algorithm
data SHA384 = SHA384
    deriving (Show,Data)

instance HashAlgorithm SHA384 where
    type HashBlockSize           SHA384 = 128
    type HashDigestSize          SHA384 = 48
    type HashInternalContextSize SHA384 = 256
    hashBlockSize  _          = 128
    hashDigestSize _          = 48
    hashInternalContextSize _ = 256
    hashInternalInit          = c_sha384_init
    hashInternalUpdate        = c_sha384_update
    hashInternalFinalize      = c_sha384_finalize

instance HashAlgorithmPrefix SHA384 where
    hashInternalFinalizePrefix = c_sha384_finalize_prefix

foreign import ccall unsafe "cryptonite_sha384_init"
    c_sha384_init :: Ptr (Context a)-> IO ()

foreign import ccall "cryptonite_sha384_update"
    c_sha384_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_sha384_finalize"
    c_sha384_finalize :: Ptr (Context a) -> Ptr (Digest a) -> IO ()

foreign import ccall "cryptonite_sha384_finalize_prefix"
    c_sha384_finalize_prefix :: Ptr (Context a) -> Ptr Word8 -> Word32 -> Word32 -> Ptr (Digest a) -> IO ()
