-- |
-- Module      : Crypto.Hash.Tiger
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Tiger cryptographic hash.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Tiger ( Tiger (..) ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)

-- | Tiger cryptographic hash algorithm
data Tiger = Tiger
    deriving (Show,Data)

instance HashAlgorithm Tiger where
    type HashBlockSize           Tiger = 64
    type HashDigestSize          Tiger = 24
    type HashInternalContextSize Tiger = 96
    hashBlockSize  _          = 64
    hashDigestSize _          = 24
    hashInternalContextSize _ = 96
    hashInternalInit          = c_tiger_init
    hashInternalUpdate        = c_tiger_update
    hashInternalFinalize      = c_tiger_finalize

foreign import ccall unsafe "cryptonite_tiger_init"
    c_tiger_init :: Ptr (Context a)-> IO ()

foreign import ccall "cryptonite_tiger_update"
    c_tiger_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_tiger_finalize"
    c_tiger_finalize :: Ptr (Context a) -> Ptr (Digest a) -> IO ()
