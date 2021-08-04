{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This library deals with the common task of pseudo-random number generation.
module System.Random
  (
  -- * Introduction
  -- $introduction

  -- * Usage
  -- $usagepure

  -- * Pure number generator interface
  -- $interfaces
    RandomGen(..)
  , uniform
  , uniformR
  , genByteString
  , Random(..)
  , Uniform
  , UniformRange
  -- ** Standard pseudo-random number generator
  , StdGen
  , mkStdGen

  -- ** Global standard pseudo-random number generator
  -- $globalstdgen
  , getStdRandom
  , getStdGen
  , setStdGen
  , newStdGen
  , randomIO
  , randomRIO

  -- * Compatibility and reproducibility
  -- ** Backwards compatibility and deprecations
  -- $deprecations

  -- ** Reproducibility
  -- $reproducibility

  -- * Notes for pseudo-random number generator implementors
  -- ** How to implement 'RandomGen'
  -- $implementrandomgen

  -- * References
  -- $references
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Int
import Data.IORef
import Data.Word
import Foreign.C.Types
import GHC.Exts
import System.IO.Unsafe (unsafePerformIO)
import System.Random.Internal
import qualified System.Random.SplitMix as SM

-- $introduction
--
-- This module provides type classes and instances for the following concepts:
--
-- [Pure pseudo-random number generators] 'RandomGen' is an interface to pure
--     pseudo-random number generators.
--
--     'StdGen', the standard pseudo-random number generator provided in this
--     library, is an instance of 'RandomGen'. It uses the SplitMix
--     implementation provided by the
--     <https://hackage.haskell.org/package/splitmix splitmix> package.
--     Programmers may, of course, supply their own instances of 'RandomGen'.
--
-- $usagepure
--
-- In pure code, use 'uniform' and 'uniformR' to generate pseudo-random values
-- with a pure pseudo-random number generator like 'StdGen'.
--
-- >>> :{
-- let rolls :: RandomGen g => Int -> g -> [Word]
--     rolls n = take n . unfoldr (Just . uniformR (1, 6))
--     pureGen = mkStdGen 137
-- in
--     rolls 10 pureGen :: [Word]
-- :}
-- [4,2,6,1,6,6,5,1,1,5]
--
-- To run use a /monadic/ pseudo-random computation in pure code with a pure
-- pseudo-random number generator, use 'runGenState' and its variants.
--
-- >>> :{
-- let rollsM :: StatefulGen g m => Int -> g -> m [Word]
--     rollsM n = replicateM n . uniformRM (1, 6)
--     pureGen = mkStdGen 137
-- in
--     runStateGen_ pureGen (rollsM 10) :: [Word]
-- :}
-- [4,2,6,1,6,6,5,1,1,5]

-------------------------------------------------------------------------------
-- Pseudo-random number generator interfaces
-------------------------------------------------------------------------------

-- $interfaces
--
-- Pseudo-random number generators come in two flavours: /pure/ and /monadic/.
--
-- ['RandomGen': pure pseudo-random number generators] These generators produce
--     a new pseudo-random value together with a new instance of the
--     pseudo-random number generator.
--
--     Pure pseudo-random number generators should implement 'split' if they
--     are /splittable/, that is, if there is an efficient method to turn one
--     generator into two. The pseudo-random numbers produced by the two
--     resulting generators should not be correlated. See [1] for some
--     background on splittable pseudo-random generators.
--
-- ['System.Random.Stateful.StatefulGen': monadic pseudo-random number generators]
--     See "System.Random.Stateful" module
--

-- | Generates a value uniformly distributed over all possible values of that
-- type.
--
-- This is a pure version of 'System.Random.Stateful.uniformM'.
--
-- ====__Examples__
--
-- >>> import System.Random
-- >>> let pureGen = mkStdGen 137
-- >>> uniform pureGen :: (Bool, StdGen)
-- (True,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
uniform :: (RandomGen g, Uniform a) => g -> (a, g)
uniform g = runStateGen g uniformM

-- | Generates a value uniformly distributed over the provided range, which
-- is interpreted as inclusive in the lower and upper bound.
--
-- *   @uniformR (1 :: Int, 4 :: Int)@ generates values uniformly from the set
--     \(\{1,2,3,4\}\)
--
-- *   @uniformR (1 :: Float, 4 :: Float)@ generates values uniformly from the
--     set \(\{x\;|\;1 \le x \le 4\}\)
--
-- The following law should hold to make the function always defined:
--
-- > uniformR (a, b) = uniformR (b, a)
--
-- This is a pure version of 'System.Random.Stateful.uniformRM'.
--
-- ====__Examples__
--
-- >>> import System.Random
-- >>> let pureGen = mkStdGen 137
-- >>> uniformR (1 :: Int, 4 :: Int) pureGen
-- (4,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
uniformR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
uniformR r g = runStateGen g (uniformRM r)

-- | Generates a 'ByteString' of the specified size using a pure pseudo-random
-- number generator. See 'uniformByteString' for the monadic version.
--
-- ====__Examples__
--
-- >>> import System.Random
-- >>> import Data.ByteString
-- >>> let pureGen = mkStdGen 137
-- >>> unpack . fst . genByteString 10 $ pureGen
-- [51,123,251,37,49,167,90,109,1,4]
--
-- @since 1.2.0
genByteString :: RandomGen g => Int -> g -> (ByteString, g)
genByteString n g = runStateGenST g (uniformByteStringM n)
{-# INLINE genByteString #-}

-- | The class of types for which uniformly distributed values can be
-- generated.
--
-- 'Random' exists primarily for backwards compatibility with version 1.1 of
-- this library. In new code, use the better specified 'Uniform' and
-- 'UniformRange' instead.
class Random a where

  -- | Takes a range /(lo,hi)/ and a pseudo-random number generator
  -- /g/, and returns a pseudo-random value uniformly distributed over the
  -- closed interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  {-# INLINE randomR #-}
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  default randomR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
  randomR r g = runStateGen g (uniformRM r)

  -- | The same as 'randomR', but using a default range determined by the type:
  --
  -- * For bounded types (instances of 'Bounded', such as 'Char'),
  --   the range is normally the whole type.
  --
  -- * For fractional types, the range is normally the semi-closed interval
  -- @[0,1)@.
  --
  -- * For 'Integer', the range is (arbitrarily) the range of 'Int'.
  {-# INLINE random #-}
  random  :: RandomGen g => g -> (a, g)
  default random :: (RandomGen g, Uniform a) => g -> (a, g)
  random g = runStateGen g uniformM

  -- | Plural variant of 'randomR', producing an infinite list of
  -- pseudo-random values instead of returning a new generator.
  {-# INLINE randomRs #-}
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = build (\cons _nil -> buildRandoms cons (randomR ival) g)

  -- | Plural variant of 'random', producing an infinite list of
  -- pseudo-random values instead of returning a new generator.
  {-# INLINE randoms #-}
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = build (\cons _nil -> buildRandoms cons random g)


-- | Produce an infinite list-equivalent of pseudo-random values.
--
-- ====__Examples__
--
-- >>> import System.Random
-- >>> let pureGen = mkStdGen 137
-- >>> (take 4 . buildRandoms (:) random $ pureGen) :: [Int]
-- [7879794327570578227,6883935014316540929,-1519291874655152001,2353271688382626589]
--
{-# INLINE buildRandoms #-}
buildRandoms :: RandomGen g
             => (a -> as -> as)  -- ^ E.g. '(:)' but subject to fusion
             -> (g -> (a,g))     -- ^ E.g. 'random'
             -> g                -- ^ A 'RandomGen' instance
             -> as
buildRandoms cons rand = go
  where
    -- The seq fixes part of #4218 and also makes fused Core simpler.
    go g = x `seq` (x `cons` go g') where (x,g') = rand g

-- Generate values in the Int range
instance Random Integer where
  random = first (toInteger :: Int -> Integer) . random
instance Random Int8
instance Random Int16
instance Random Int32
instance Random Int64
instance Random Int
instance Random Word
instance Random Word8
instance Random Word16
instance Random Word32
instance Random Word64
#if __GLASGOW_HASKELL >= 802
instance Random CBool
#endif
instance Random CChar
instance Random CSChar
instance Random CUChar
instance Random CShort
instance Random CUShort
instance Random CInt
instance Random CUInt
instance Random CLong
instance Random CULong
instance Random CPtrdiff
instance Random CSize
instance Random CWchar
instance Random CSigAtomic
instance Random CLLong
instance Random CULLong
instance Random CIntPtr
instance Random CUIntPtr
instance Random CIntMax
instance Random CUIntMax
instance Random CFloat where
  randomR (CFloat l, CFloat h) = first CFloat . randomR (l, h)
  random = first CFloat . random
instance Random CDouble where
  randomR (CDouble l, CDouble h) = first CDouble . randomR (l, h)
  random = first CDouble . random

instance Random Char
instance Random Bool
instance Random Double where
  randomR r g = runStateGen g (uniformRM r)
  random g = runStateGen g (uniformRM (0, 1))
instance Random Float where
  randomR r g = runStateGen g (uniformRM r)
  random g = runStateGen g (uniformRM (0, 1))

-------------------------------------------------------------------------------
-- Global pseudo-random number generator
-------------------------------------------------------------------------------

-- $globalstdgen
--
-- There is a single, implicit, global pseudo-random number generator of type
-- 'StdGen', held in a global variable maintained by the 'IO' monad. It is
-- initialised automatically in some system-dependent fashion. To get
-- deterministic behaviour, use 'setStdGen'.
--
-- Note that 'mkStdGen' also gives deterministic behaviour without requiring an
-- 'IO' context.

-- |Sets the global pseudo-random number generator.
setStdGen :: MonadIO m => StdGen -> m ()
setStdGen = liftIO . writeIORef theStdGen

-- |Gets the global pseudo-random number generator.
getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen

theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}

-- |Applies 'split' to the current global pseudo-random generator,
-- updates it with one of the results, and returns the other.
newStdGen :: MonadIO m => m StdGen
newStdGen = liftIO $ atomicModifyIORef' theStdGen split

{- |Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a pseudo-random integer
between 1 and 6:

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}
getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
getStdRandom f = liftIO $ atomicModifyIORef' theStdGen (swap . f)
  where swap (v, g) = (g, v)


-- | A variant of 'randomR' that uses the global pseudo-random number
-- generator.
randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
randomRIO range = liftIO $ getStdRandom (randomR range)

-- | A variant of 'random' that uses the global pseudo-random number
-- generator.
randomIO :: (Random a, MonadIO m) => m a
randomIO = liftIO $ getStdRandom random

-------------------------------------------------------------------------------
-- Notes
-------------------------------------------------------------------------------

-- $implementrandomgen
--
-- Consider these points when writing a 'RandomGen' instance for a given pure
-- pseudo-random number generator:
--
-- *   If the pseudo-random number generator has a power-of-2 modulus, that is,
--     it natively outputs @2^n@ bits of randomness for some @n@, implement
--     'genWord8', 'genWord16', 'genWord32' and 'genWord64'. See below for more
--     details.
--
-- *   If the pseudo-random number generator does not have a power-of-2
--     modulus, implement 'next' and 'genRange'. See below for more details.
--
-- *   If the pseudo-random number generator is splittable, implement 'split'.
--     If there is no suitable implementation, 'split' should fail with a
--     helpful error message.
--
-- === How to implement 'RandomGen' for a pseudo-random number generator with power-of-2 modulus
--
-- Suppose you want to implement a [permuted congruential
-- generator](https://en.wikipedia.org/wiki/Permuted_congruential_generator).
--
-- >>> data PCGen = PCGen !Word64 !Word64
--
-- It produces a full 'Word32' of randomness per iteration.
--
-- >>> import Data.Bits
-- >>> :{
-- let stepGen :: PCGen -> (Word32, PCGen)
--     stepGen (PCGen state inc) = let
--       newState = state * 6364136223846793005 + (inc .|. 1)
--       xorShifted = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27) :: Word32
--       rot = fromIntegral (state `shiftR` 59) :: Word32
--       out = (xorShifted `shiftR` (fromIntegral rot)) .|. (xorShifted `shiftL` fromIntegral ((-rot) .&. 31))
--       in (out, PCGen newState inc)
-- :}
--
-- >>> fst $ stepGen $ snd $ stepGen (PCGen 17 29)
-- 3288430965
--
-- You can make it an instance of 'RandomGen' as follows:
--
-- >>> :{
-- instance RandomGen PCGen where
--   genWord32 = stepGen
--   split _ = error "PCG is not splittable"
-- :}
--
--
-- === How to implement 'RandomGen' for a pseudo-random number generator without a power-of-2 modulus
--
-- __We do not recommend you implement any new pseudo-random number generators without a power-of-2 modulus.__
--
-- Pseudo-random number generators without a power-of-2 modulus perform
-- /significantly worse/ than pseudo-random number generators with a power-of-2
-- modulus with this library. This is because most functionality in this
-- library is based on generating and transforming uniformly pseudo-random
-- machine words, and generating uniformly pseudo-random machine words using a
-- pseudo-random number generator without a power-of-2 modulus is expensive.
--
-- The pseudo-random number generator from
-- <https://dl.acm.org/doi/abs/10.1145/62959.62969 L’Ecuyer (1988)> natively
-- generates an integer value in the range @[1, 2147483562]@. This is the
-- generator used by this library before it was replaced by SplitMix in version
-- 1.2.
--
-- >>> data LegacyGen = LegacyGen !Int32 !Int32
-- >>> :{
-- let legacyNext :: LegacyGen -> (Int, LegacyGen)
--     legacyNext (LegacyGen s1 s2) = (fromIntegral z', LegacyGen s1'' s2'') where
--       z' = if z < 1 then z + 2147483562 else z
--       z = s1'' - s2''
--       k = s1 `quot` 53668
--       s1'  = 40014 * (s1 - k * 53668) - k * 12211
--       s1'' = if s1' < 0 then s1' + 2147483563 else s1'
--       k' = s2 `quot` 52774
--       s2' = 40692 * (s2 - k' * 52774) - k' * 3791
--       s2'' = if s2' < 0 then s2' + 2147483399 else s2'
-- :}
--
-- You can make it an instance of 'RandomGen' as follows:
--
-- >>> :{
-- instance RandomGen LegacyGen where
--   next = legacyNext
--   genRange _ = (1, 2147483562)
--   split _ = error "Not implemented"
-- :}
--
-- $deprecations
--
-- Version 1.2 mostly maintains backwards compatibility with version 1.1. This
-- has a few consequences users should be aware of:
--
-- *   The type class 'Random' is only provided for backwards compatibility.
--     New code should use 'Uniform' and 'UniformRange' instead.
--
-- *   The methods 'next' and 'genRange' in 'RandomGen' are deprecated and only
--     provided for backwards compatibility. New instances of 'RandomGen' should
--     implement word-based methods instead. See below for more information
--     about how to write a 'RandomGen' instance.
--
-- *   This library provides instances for 'Random' for some unbounded types
--     for backwards compatibility. For an unbounded type, there is no way
--     to generate a value with uniform probability out of its entire domain, so
--     the 'random' implementation for unbounded types actually generates a
--     value based on some fixed range.
--
--     For 'Integer', 'random' generates a value in the 'Int' range. For 'Float'
--     and 'Double', 'random' generates a floating point value in the range @[0,
--     1)@.
--
--     This library does not provide 'Uniform' instances for any unbounded
--     types.
--
-- $reproducibility
--
-- If you have two builds of a particular piece of code against this library,
-- any deterministic function call should give the same result in the two
-- builds if the builds are
--
-- *   compiled against the same major version of this library
-- *   on the same architecture (32-bit or 64-bit)
--
-- $references
--
-- 1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast
-- splittable pseudorandom number generators. In Proceedings of the 2014 ACM
-- International Conference on Object Oriented Programming Systems Languages &
-- Applications (OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
-- <https://doi.org/10.1145/2660193.2660195>

-- $setup
--
-- >>> import Control.Monad (replicateM)
-- >>> import Data.List (unfoldr)
