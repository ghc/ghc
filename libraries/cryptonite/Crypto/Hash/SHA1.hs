-- |
-- Module      : Crypto.Hash.SHA1
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- SHA1 cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.SHA1 ( SHA1 (..) ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)

-- | SHA1 cryptographic hash algorithm
data SHA1 = SHA1
    deriving (Show,Data)

instance HashAlgorithm SHA1 where
    type HashBlockSize           SHA1 = 64
    type HashDigestSize          SHA1 = 20
    type HashInternalContextSize SHA1 = 96
    hashBlockSize  _          = 64
    hashDigestSize _          = 20
    hashInternalContextSize _ = 96
    hashInternalInit          = c_sha1_init
    hashInternalUpdate        = c_sha1_update
    hashInternalFinalize      = c_sha1_finalize

instance HashAlgorithmPrefix SHA1 where
    hashInternalFinalizePrefix = c_sha1_finalize_prefix

foreign import ccall unsafe "cryptonite_sha1_init"
    c_sha1_init :: Ptr (Context a)-> IO ()

foreign import ccall "cryptonite_sha1_update"
    c_sha1_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_sha1_finalize"
    c_sha1_finalize :: Ptr (Context a) -> Ptr (Digest a) -> IO ()

foreign import ccall "cryptonite_sha1_finalize_prefix"
    c_sha1_finalize_prefix :: Ptr (Context a) -> Ptr Word8 -> Word32 -> Word32 -> Ptr (Digest a) -> IO ()
