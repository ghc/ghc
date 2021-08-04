-- |
-- Module      : Crypto.Random
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.Random
    (
    -- * Deterministic instances
      ChaChaDRG
    , SystemDRG
    , Seed
    -- * Seed
    , seedNew
    , seedFromInteger
    , seedToInteger
    , seedFromBinary
    -- * Deterministic Random class
    , getSystemDRG
    , drgNew
    , drgNewSeed
    , drgNewTest
    , withDRG
    , withRandomBytes
    , DRG(..)
    -- * Random abstraction
    , MonadRandom(..)
    , MonadPseudoRandom
    ) where

import Crypto.Error
import Crypto.Random.Types
import Crypto.Random.ChaChaDRG
import Crypto.Random.SystemDRG
import Data.ByteArray (ByteArray, ByteArrayAccess, ScrubbedBytes)
import qualified Data.ByteArray as B
import Crypto.Internal.Imports

import qualified Crypto.Number.Serialize as Serialize

newtype Seed = Seed ScrubbedBytes
    deriving (ByteArrayAccess)

-- Length for ChaCha DRG seed
seedLength :: Int
seedLength = 40

-- | Create a new Seed from system entropy
seedNew :: MonadRandom randomly => randomly Seed
seedNew = Seed `fmap` getRandomBytes seedLength

-- | Convert a Seed to an integer
seedToInteger :: Seed -> Integer
seedToInteger (Seed b) = Serialize.os2ip b

-- | Convert an integer to a Seed
seedFromInteger :: Integer -> Seed
seedFromInteger i = Seed $ Serialize.i2ospOf_ seedLength (i `mod` 2^(seedLength * 8))

-- | Convert a binary to a seed
seedFromBinary :: ByteArrayAccess b => b -> CryptoFailable Seed
seedFromBinary b
    | B.length b /= 40 = CryptoFailed (CryptoError_SeedSizeInvalid)
    | otherwise        = CryptoPassed $ Seed $ B.convert b

-- | Create a new DRG from system entropy
drgNew :: MonadRandom randomly => randomly ChaChaDRG
drgNew = drgNewSeed `fmap` seedNew

-- | Create a new DRG from a seed
drgNewSeed :: Seed -> ChaChaDRG
drgNewSeed (Seed seed) = initialize seed

-- | Create a new DRG from 5 Word64.
--
-- This is a convenient interface to create deterministic interface
-- for quickcheck style testing.
--
-- It can also be used in other contexts provided the input
-- has been properly randomly generated.
--
-- Note that the @Arbitrary@ instance provided by QuickCheck for 'Word64' does
-- not have a uniform distribution.  It is often better to use instead
-- @arbitraryBoundedRandom@.
--
-- System endianness impacts how the tuple is interpreted and therefore changes
-- the resulting DRG.
drgNewTest :: (Word64, Word64, Word64, Word64, Word64) -> ChaChaDRG
drgNewTest = initializeWords

-- | Generate @len random bytes and mapped the bytes to the function @f.
--
-- This is equivalent to use Control.Arrow 'first' with 'randomBytesGenerate'
withRandomBytes :: (ByteArray ba, DRG g) => g -> Int -> (ba -> a) -> (a, g)
withRandomBytes rng len f = (f bs, rng')
  where (bs, rng') = randomBytesGenerate len rng
