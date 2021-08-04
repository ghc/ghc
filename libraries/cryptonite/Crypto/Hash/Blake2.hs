-- |
-- Module      : Crypto.Hash.Blake2
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : experimental
-- Portability : unknown
--
-- Module containing the binding functions to work with the
-- Blake2
--
-- Implementation based from [RFC7693](https://tools.ietf.org/html/rfc7693)
--
-- Please consider the following when chosing a hash:
--
--      Algorithm     | Target | Collision | Digest Size |
--         Identifier |  Arch  |  Security |   in bytes  |
--     ---------------+--------+-----------+-------------+
--      id-blake2b160 | 64-bit |   2**80   |         20  |
--      id-blake2b256 | 64-bit |   2**128  |         32  |
--      id-blake2b384 | 64-bit |   2**192  |         48  |
--      id-blake2b512 | 64-bit |   2**256  |         64  |
--     ---------------+--------+-----------+-------------+
--      id-blake2s128 | 32-bit |   2**64   |         16  |
--      id-blake2s160 | 32-bit |   2**80   |         20  |
--      id-blake2s224 | 32-bit |   2**112  |         28  |
--      id-blake2s256 | 32-bit |   2**128  |         32  |
--     ---------------+--------+-----------+-------------+
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Crypto.Hash.Blake2
    ( Blake2s(..)
    , Blake2sp(..)
    , Blake2b(..)
    , Blake2bp(..)
    ) where

import           Crypto.Hash.Types
import           Foreign.Ptr (Ptr)
import           Data.Data
import           Data.Word (Word8, Word32)
import           GHC.TypeLits (Nat, KnownNat)
import           Crypto.Internal.Nat

-- | Fast and secure alternative to SHA1 and HMAC-SHA1
--
-- It is espacially known to target 32bits architectures.
--
-- Known supported digest sizes:
--
-- * Blake2s 160
-- * Blake2s 224
-- * Blake2s 256
--
data Blake2s (bitlen :: Nat) = Blake2s
    deriving (Show,Data)

instance (IsDivisibleBy8 bitlen, KnownNat bitlen, IsAtLeast bitlen 8, IsAtMost bitlen 256)
      => HashAlgorithm (Blake2s bitlen)
      where
    type HashBlockSize           (Blake2s bitlen) = 64
    type HashDigestSize          (Blake2s bitlen) = Div8 bitlen
    type HashInternalContextSize (Blake2s bitlen) = 136
    hashBlockSize  _          = 64
    hashDigestSize _          = byteLen (Proxy :: Proxy bitlen)
    hashInternalContextSize _ = 136
    hashInternalInit p        = c_blake2s_init p (integralNatVal (Proxy :: Proxy bitlen))
    hashInternalUpdate        = c_blake2s_update
    hashInternalFinalize p    = c_blake2s_finalize p (integralNatVal (Proxy :: Proxy bitlen))

foreign import ccall unsafe "cryptonite_blake2s_init"
    c_blake2s_init :: Ptr (Context a) -> Word32 -> IO ()
foreign import ccall "cryptonite_blake2s_update"
    c_blake2s_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()
foreign import ccall unsafe "cryptonite_blake2s_finalize"
    c_blake2s_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()

-- | Fast cryptographic hash.
--
-- It is especially known to target 64bits architectures.
--
-- Known supported digest sizes:
--
-- * Blake2b 160
-- * Blake2b 224
-- * Blake2b 256
-- * Blake2b 384
-- * Blake2b 512
--
data Blake2b (bitlen :: Nat) = Blake2b
    deriving (Show,Data)

instance (IsDivisibleBy8 bitlen, KnownNat bitlen, IsAtLeast bitlen 8, IsAtMost bitlen 512)
      => HashAlgorithm (Blake2b bitlen)
      where
    type HashBlockSize           (Blake2b bitlen) = 128
    type HashDigestSize          (Blake2b bitlen) = Div8 bitlen
    type HashInternalContextSize (Blake2b bitlen) = 248
    hashBlockSize  _          = 128
    hashDigestSize _          = byteLen (Proxy :: Proxy bitlen)
    hashInternalContextSize _ = 248
    hashInternalInit p        = c_blake2b_init p (integralNatVal (Proxy :: Proxy bitlen))
    hashInternalUpdate        = c_blake2b_update
    hashInternalFinalize p    = c_blake2b_finalize p (integralNatVal (Proxy :: Proxy bitlen))

foreign import ccall unsafe "cryptonite_blake2b_init"
    c_blake2b_init :: Ptr (Context a) -> Word32 -> IO ()
foreign import ccall "cryptonite_blake2b_update"
    c_blake2b_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()
foreign import ccall unsafe "cryptonite_blake2b_finalize"
    c_blake2b_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()

data Blake2sp (bitlen :: Nat) = Blake2sp
    deriving (Show,Data)

instance (IsDivisibleBy8 bitlen, KnownNat bitlen, IsAtLeast bitlen 8, IsAtMost bitlen 256)
      => HashAlgorithm (Blake2sp bitlen)
      where
    type HashBlockSize           (Blake2sp bitlen) = 64
    type HashDigestSize          (Blake2sp bitlen) = Div8 bitlen
    type HashInternalContextSize (Blake2sp bitlen) = 2185
    hashBlockSize  _          = 64
    hashDigestSize _          = byteLen (Proxy :: Proxy bitlen)
    hashInternalContextSize _ = 2185
    hashInternalInit p        = c_blake2sp_init p (integralNatVal (Proxy :: Proxy bitlen))
    hashInternalUpdate        = c_blake2sp_update
    hashInternalFinalize p    = c_blake2sp_finalize p (integralNatVal (Proxy :: Proxy bitlen))

foreign import ccall unsafe "cryptonite_blake2sp_init"
    c_blake2sp_init :: Ptr (Context a) -> Word32 -> IO ()
foreign import ccall "cryptonite_blake2sp_update"
    c_blake2sp_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()
foreign import ccall unsafe "cryptonite_blake2sp_finalize"
    c_blake2sp_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()

data Blake2bp (bitlen :: Nat) = Blake2bp
    deriving (Show,Data)

instance (IsDivisibleBy8 bitlen, KnownNat bitlen, IsAtLeast bitlen 8, IsAtMost bitlen 512)
      => HashAlgorithm (Blake2bp bitlen)
      where
    type HashBlockSize           (Blake2bp bitlen) = 128
    type HashDigestSize          (Blake2bp bitlen) = Div8 bitlen
    type HashInternalContextSize (Blake2bp bitlen) = 2325
    hashBlockSize  _          = 128
    hashDigestSize _          = byteLen (Proxy :: Proxy bitlen)
    hashInternalContextSize _ = 2325
    hashInternalInit p        = c_blake2bp_init p (integralNatVal (Proxy :: Proxy bitlen))
    hashInternalUpdate        = c_blake2bp_update
    hashInternalFinalize p    = c_blake2bp_finalize p (integralNatVal (Proxy :: Proxy bitlen))


foreign import ccall unsafe "cryptonite_blake2bp_init"
    c_blake2bp_init :: Ptr (Context a) -> Word32 -> IO ()
foreign import ccall "cryptonite_blake2bp_update"
    c_blake2bp_update :: Ptr (Context a) -> Ptr Word8 -> Word32 -> IO ()
foreign import ccall unsafe "cryptonite_blake2bp_finalize"
    c_blake2bp_finalize :: Ptr (Context a) -> Word32 -> Ptr (Digest a) -> IO ()
