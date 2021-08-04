-- |
-- Module      : Crypto.Hash.SHAKE
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- SHA3 extendable output functions (SHAKE).
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Crypto.Hash.SHAKE
    (  SHAKE128 (..), SHAKE256 (..), HashSHAKE (..)
    ) where

import           Control.Monad (when)
import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable (Storable(..))
import           Data.Bits
import           Data.Data
import           Data.Word (Word8, Word32)

import           GHC.TypeLits (Nat, KnownNat, type (+))
import           Crypto.Internal.Nat

-- | Type class of SHAKE algorithms.
class HashAlgorithm a => HashSHAKE a where
    -- | Alternate finalization needed for cSHAKE
    cshakeInternalFinalize :: Ptr (Context a) -> Ptr (Digest a) -> IO ()
    -- | Get the digest bit length
    cshakeOutputLength :: a -> Int

-- | SHAKE128 (128 bits) extendable output function.  Supports an arbitrary
-- digest size, to be specified as a type parameter of kind 'Nat'.
--
-- Note: outputs from @'SHAKE128' n@ and @'SHAKE128' m@ for the same input are
-- correlated (one being a prefix of the other).  Results are unrelated to
-- 'SHAKE256' results.
data SHAKE128 (bitlen :: Nat) = SHAKE128
    deriving (Show, Data)

instance KnownNat bitlen => HashAlgorithm (SHAKE128 bitlen) where
    type HashBlockSize           (SHAKE128 bitlen)  = 168
    type HashDigestSize          (SHAKE128 bitlen) = Div8 (bitlen + 7)
    type HashInternalContextSize (SHAKE128 bitlen) = 376
    hashBlockSize  _          = 168
    hashDigestSize _          = byteLen (Proxy :: Proxy bitlen)
    hashInternalContextSize _ = 376
    hashInternalInit p        = c_sha3_init p 128
    hashInternalUpdate        = c_sha3_update
    hashInternalFinalize      = shakeFinalizeOutput (Proxy :: Proxy bitlen)

instance KnownNat bitlen => HashSHAKE (SHAKE128 bitlen) where
    cshakeInternalFinalize = cshakeFinalizeOutput (Proxy :: Proxy bitlen)
    cshakeOutputLength _ = integralNatVal (Proxy :: Proxy bitlen)

-- | SHAKE256 (256 bits) extendable output function.  Supports an arbitrary
-- digest size, to be specified as a type parameter of kind 'Nat'.
--
-- Note: outputs from @'SHAKE256' n@ and @'SHAKE256' m@ for the same input are
-- correlated (one being a prefix of the other).  Results are unrelated to
-- 'SHAKE128' results.
data SHAKE256 (bitlen :: Nat) = SHAKE256
    deriving (Show, Data)

instance KnownNat bitlen => HashAlgorithm (SHAKE256 bitlen) where
    type HashBlockSize           (SHAKE256 bitlen) = 136
    type HashDigestSize          (SHAKE256 bitlen) = Div8 (bitlen + 7)
    type HashInternalContextSize (SHAKE256 bitlen) = 344
    hashBlockSize  _          = 136
    hashDigestSize _          = byteLen (Proxy :: Proxy bitlen)
    hashInternalContextSize _ = 344
    hashInternalInit p        = c_sha3_init p 256
    hashInternalUpdate        = c_sha3_update
    hashInternalFinalize      = shakeFinalizeOutput (Proxy :: Proxy bitlen)

instance KnownNat bitlen => HashSHAKE (SHAKE256 bitlen) where
    cshakeInternalFinalize = cshakeFinalizeOutput (Proxy :: Proxy bitlen)
    cshakeOutputLength _ = integralNatVal (Proxy :: Proxy bitlen)

shakeFinalizeOutput :: KnownNat bitlen
                    => proxy bitlen
                    -> Ptr (Context a)
                    -> Ptr (Digest a)
                    -> IO ()
shakeFinalizeOutput d ctx dig = do
    c_sha3_finalize_shake ctx
    c_sha3_output ctx dig (byteLen d)
    shakeTruncate d (castPtr dig)

cshakeFinalizeOutput :: KnownNat bitlen
                     => proxy bitlen
                     -> Ptr (Context a)
                     -> Ptr (Digest a)
                     -> IO ()
cshakeFinalizeOutput d ctx dig = do
    c_sha3_finalize_cshake ctx
    c_sha3_output ctx dig (byteLen d)
    shakeTruncate d (castPtr dig)

shakeTruncate :: KnownNat bitlen => proxy bitlen -> Ptr Word8 -> IO ()
shakeTruncate d ptr =
    when (bits > 0) $ do
        byte <- peekElemOff ptr index
        pokeElemOff ptr index (byte .&. mask)
  where
    mask = (1 `shiftL` bits) - 1
    (index, bits) = integralNatVal d `divMod` 8

foreign import ccall unsafe "cryptonite_sha3_init"
    c_sha3_init :: Ptr (Context a) -> Word32 -> IO ()

foreign import ccall "cryptonite_sha3_update"
    c_sha3_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall unsafe "cryptonite_sha3_finalize_shake"
    c_sha3_finalize_shake :: Ptr (Context a) -> IO ()

foreign import ccall unsafe "cryptonite_sha3_finalize_cshake"
    c_sha3_finalize_cshake :: Ptr (Context a) -> IO ()

foreign import ccall unsafe "cryptonite_sha3_output"
    c_sha3_output :: Ptr (Context a) -> Ptr (Digest a) -> Word32 -> IO ()
