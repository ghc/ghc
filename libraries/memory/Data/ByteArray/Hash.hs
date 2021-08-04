-- |
-- Module      : Data.ByteArray.Hash
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- provide the SipHash algorithm.
-- reference: <http://131002.net/siphash/siphash.pdf>
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteArray.Hash
    (
    -- * SipHash
      SipKey(..)
    , SipHash(..)
    , sipHash
    , sipHashWith
    -- * FNV1 and FNV1a (32 and 64 bits)
    , FnvHash32(..)
    , FnvHash64(..)
    , fnv1Hash
    , fnv1aHash
    , fnv1_64Hash
    , fnv1a_64Hash
    ) where

import           Data.Memory.Internal.Compat
import           Data.Memory.Hash.SipHash
import           Data.Memory.Hash.FNV
import qualified Data.ByteArray.Types   as B

-- | Compute the SipHash tag of a byte array for a given key.
--
-- 'sipHash` is equivalent to 'sipHashWith 2 4'
sipHash :: B.ByteArrayAccess ba
        => SipKey
        -> ba
        -> SipHash
sipHash key ba = unsafeDoIO $ B.withByteArray ba $ \p -> hash key p (B.length ba)

-- | Compute the SipHash tag of a byte array for a given key.
--
-- The user can choose the C and D numbers of rounds.
--
-- calling 'sipHash` is equivalent to 'sipHashWith 2 4'
sipHashWith :: B.ByteArrayAccess ba
            => Int    -- ^ c rounds
            -> Int    -- ^ d rounds
            -> SipKey -- ^ key
            -> ba     -- ^ data to hash
            -> SipHash
sipHashWith c d key ba = unsafeDoIO $ B.withByteArray ba $ \p -> hashWith c d key p (B.length ba)


-- | Compute the FNV1 32 bit hash value of a byte array
fnv1Hash :: B.ByteArrayAccess ba
         => ba
         -> FnvHash32
fnv1Hash ba = unsafeDoIO $ B.withByteArray ba $ \p -> fnv1 p (B.length ba)

-- | Compute the FNV1a 32 bit hash value of a byte array
fnv1aHash :: B.ByteArrayAccess ba
          => ba
          -> FnvHash32
fnv1aHash ba = unsafeDoIO $ B.withByteArray ba $ \p -> fnv1a p (B.length ba)

-- | Compute the FNV1 64 bit hash value of a byte array
fnv1_64Hash :: B.ByteArrayAccess ba
            => ba
            -> FnvHash64
fnv1_64Hash ba = unsafeDoIO $ B.withByteArray ba $ \p -> fnv1_64 p (B.length ba)

-- | Compute the FNV1a 64 bit hash value of a byte array
fnv1a_64Hash :: B.ByteArrayAccess ba
             => ba
             -> FnvHash64
fnv1a_64Hash ba = unsafeDoIO $ B.withByteArray ba $ \p -> fnv1a_64 p (B.length ba)
