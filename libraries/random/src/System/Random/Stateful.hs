{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  System.Random.Stateful
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
--
-- This library deals with the common task of pseudo-random number generation.
module System.Random.Stateful
  (
  -- * Pure Random Generator
  module System.Random
  -- * Monadic Random Generator
  -- $introduction

  -- * Usage
  -- $usagemonadic

  -- * Mutable pseudo-random number generator interfaces
  -- $interfaces
  , StatefulGen(..)
  , FrozenGen(..)
  , RandomGenM(..)
  , withMutableGen
  , withMutableGen_
  , randomM
  , randomRM
  , splitGenM

  -- * Monadic adapters for pure pseudo-random number generators #monadicadapters#
  -- $monadicadapters

  -- ** Pure adapter
  , StateGen(..)
  , StateGenM(..)
  , runStateGen
  , runStateGen_
  , runStateGenT
  , runStateGenT_
  , runStateGenST
  -- ** Mutable adapter with atomic operations
  , AtomicGen(..)
  , AtomicGenM(..)
  , newAtomicGenM
  , applyAtomicGen
  -- ** Mutable adapter in 'IO'
  , IOGen(..)
  , IOGenM(..)
  , newIOGenM
  , applyIOGen
  -- ** Mutable adapter in 'ST'
  , STGen(..)
  , STGenM(..)
  , newSTGenM
  , applySTGen
  , runSTGen
  , runSTGen_

  -- * Pseudo-random values of various types
  -- $uniform
  , Uniform(..)
  , uniformListM
  , UniformRange(..)

  -- * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  , uniformByteStringM
  , uniformDouble01M
  , uniformDoublePositive01M
  , uniformFloat01M
  , uniformFloatPositive01M

  -- * Appendix

  -- ** How to implement 'StatefulGen'
  -- $implementmonadrandom

  -- ** Floating point number caveats #fpcaveats#
  -- $floating

  -- * References
  -- $references
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.IORef
import Data.STRef
import Foreign.Storable
import System.Random
import System.Random.Internal

-- $introduction
--
-- This module provides type classes and instances for the following concepts:
--
-- [Monadic pseudo-random number generators] 'StatefulGen' is an interface to
--     monadic pseudo-random number generators.
--
-- [Monadic adapters] 'StateGenM', 'AtomicGenM', 'IOGenM' and 'STGenM' turn a
--     'RandomGen' instance into a 'StatefulGen' instance.
--
-- [Drawing from a range] 'UniformRange' is used to generate a value of a
--     type uniformly within a range.
--
--     This library provides instances of 'UniformRange' for many common
--     numeric types.
--
-- [Drawing from the entire domain of a type] 'Uniform' is used to generate a
--     value of a type uniformly over all possible values of that type.
--
--     This library provides instances of 'Uniform' for many common bounded
--     numeric types.
--
-- $usagemonadic
--
-- In monadic code, use the relevant 'Uniform' and 'UniformRange' instances to
-- generate pseudo-random values via 'uniformM' and 'uniformRM', respectively.
--
-- As an example, @rollsM@ generates @n@ pseudo-random values of @Word@ in the
-- range @[1, 6]@ in a 'StatefulGen' context; given a /monadic/ pseudo-random
-- number generator, you can run this probabilistic computation as follows:
--
-- >>> :{
-- let rollsM :: StatefulGen g m => Int -> g -> m [Word]
--     rollsM n = replicateM n . uniformRM (1, 6)
-- in do
--     monadicGen <- MWC.create
--     rollsM 10 monadicGen :: IO [Word]
-- :}
-- [3,4,3,1,4,6,1,6,1,4]
--
-- Given a /pure/ pseudo-random number generator, you can run the monadic
-- pseudo-random number computation @rollsM@ in an 'IO' or 'ST' context by
-- applying a monadic adapter like 'AtomicGenM', 'IOGenM' or 'STGenM'
-- (see [monadic-adapters](#monadicadapters)) to the pure pseudo-random number
-- generator.
--
-- >>> :{
-- let rollsM :: StatefulGen g m => Int -> g -> m [Word]
--     rollsM n = replicateM n . uniformRM (1, 6)
--     pureGen = mkStdGen 42
-- in
--     newIOGenM pureGen >>= rollsM 10 :: IO [Word]
-- :}
-- [1,1,3,2,4,5,3,4,6,2]

-------------------------------------------------------------------------------
-- Pseudo-random number generator interfaces
-------------------------------------------------------------------------------

-- $interfaces
--
-- Pseudo-random number generators come in two flavours: /pure/ and /monadic/.
--
-- ['System.Random.RandomGen': pure pseudo-random number generators]
--     See "System.Random" module.
--
-- ['StatefulGen': monadic pseudo-random number generators] These generators
--     mutate their own state as they produce pseudo-random values. They
--     generally live in 'ST' or 'IO' or some transformer that implements
--     @PrimMonad@.
--

-------------------------------------------------------------------------------
-- Monadic adapters
-------------------------------------------------------------------------------

-- $monadicadapters
--
-- Pure pseudo-random number generators can be used in monadic code via the
-- adapters 'StateGenM', 'AtomicGenM', 'IOGenM' and 'STGenM'.
--
-- *   'StateGenM' can be used in any state monad. With strict 'StateT' there is
--     no performance overhead compared to using the 'RandomGen' instance
--     directly. 'StateGenM' is /not/ safe to use in the presence of exceptions
--     and concurrency.
--
-- *   'AtomicGenM' is safe in the presence of exceptions and concurrency since
--     it performs all actions atomically.
--
-- *   'IOGenM' is a wrapper around an 'IORef' that holds a pure generator.
--     'IOGenM' is safe in the presence of exceptions, but not concurrency.
--
-- *   'STGenM' is a wrapper around an 'STRef' that holds a pure generator.
--     'STGenM' is safe in the presence of exceptions, but not concurrency.

-- | Interface to operations on 'RandomGen' wrappers like 'IOGenM' and 'StateGenM'.
--
-- @since 1.2.0
class (RandomGen r, StatefulGen g m) => RandomGenM g r m | g -> r where
  applyRandomGenM :: (r -> (a, r)) -> g -> m a

-- | Splits a pseudo-random number generator into two. Overwrites the mutable
-- wrapper with one of the resulting generators and returns the other.
--
-- @since 1.2.0
splitGenM :: RandomGenM g r m => g -> m r
splitGenM = applyRandomGenM split

instance (RandomGen r, MonadIO m) => RandomGenM (IOGenM r) r m where
  applyRandomGenM = applyIOGen

instance (RandomGen r, MonadIO m) => RandomGenM (AtomicGenM r) r m where
  applyRandomGenM = applyAtomicGen

instance (RandomGen r, MonadState r m) => RandomGenM (StateGenM r) r m where
  applyRandomGenM f _ = state f

instance RandomGen r => RandomGenM (STGenM r s) r (ST s) where
  applyRandomGenM = applySTGen


-- | Runs a mutable pseudo-random number generator from its 'Frozen' state.
--
-- ====__Examples__
--
-- >>> import Data.Int (Int8)
-- >>> withMutableGen (IOGen (mkStdGen 217)) (uniformListM 5) :: IO ([Int8], IOGen StdGen)
-- ([-74,37,-50,-2,3],IOGen {unIOGen = StdGen {unStdGen = SMGen 4273268533320920145 15251669095119325999}})
--
-- @since 1.2.0
withMutableGen :: FrozenGen f m => f -> (MutableGen f m -> m a) -> m (a, f)
withMutableGen fg action = do
  g <- thawGen fg
  res <- action g
  fg' <- freezeGen g
  pure (res, fg')

-- | Same as 'withMutableGen', but only returns the generated value.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> withMutableGen_ (IOGen pureGen) (uniformRM (1 :: Int, 6 :: Int))
-- 4
--
-- @since 1.2.0
withMutableGen_ :: FrozenGen f m => f -> (MutableGen f m -> m a) -> m a
withMutableGen_ fg action = fst <$> withMutableGen fg action


-- | Generates a list of pseudo-random values.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newIOGenM pureGen
-- >>> uniformListM 10 g :: IO [Bool]
-- [True,True,True,True,False,True,True,False,False,False]
--
-- @since 1.2.0
uniformListM :: (StatefulGen g m, Uniform a) => Int -> g -> m [a]
uniformListM n gen = replicateM n (uniformM gen)

-- | Generates a pseudo-random value using monadic interface and `Random` instance.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newIOGenM pureGen
-- >>> randomM g :: IO Double
-- 0.5728354935654512
--
-- @since 1.2.0
randomM :: (RandomGenM g r m, Random a) => g -> m a
randomM = applyRandomGenM random

-- | Generates a pseudo-random value using monadic interface and `Random` instance.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newIOGenM pureGen
-- >>> randomRM (1, 100) g :: IO Int
-- 52
--
-- @since 1.2.0
randomRM :: (RandomGenM g r m, Random a) => (a, a) -> g -> m a
randomRM r = applyRandomGenM (randomR r)

-- | Wraps an 'IORef' that holds a pure pseudo-random number generator. All
-- operations are performed atomically.
--
-- *   'AtomicGenM' is safe in the presence of exceptions and concurrency.
-- *   'AtomicGenM' is the slowest of the monadic adapters due to the overhead
--     of its atomic operations.
--
-- @since 1.2.0
newtype AtomicGenM g = AtomicGenM { unAtomicGenM :: IORef g}


-- | Frozen version of mutable `AtomicGenM` generator
--
-- @since 1.2.0
newtype AtomicGen g = AtomicGen { unAtomicGen :: g}
  deriving (Eq, Ord, Show, RandomGen, Storable, NFData)

-- | Creates a new 'AtomicGenM'.
--
-- @since 1.2.0
newAtomicGenM :: MonadIO m => g -> m (AtomicGenM g)
newAtomicGenM = fmap AtomicGenM . liftIO . newIORef

instance (RandomGen g, MonadIO m) => StatefulGen (AtomicGenM g) m where
  uniformWord32R r = applyAtomicGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyAtomicGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyAtomicGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyAtomicGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyAtomicGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyAtomicGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applyAtomicGen (genShortByteString n)


instance (RandomGen g, MonadIO m) => FrozenGen (AtomicGen g) m where
  type MutableGen (AtomicGen g) m = AtomicGenM g
  freezeGen = fmap AtomicGen . liftIO . readIORef . unAtomicGenM
  thawGen (AtomicGen g) = newAtomicGenM g

-- | Atomically applies a pure operation to the wrapped pseudo-random number
-- generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newAtomicGenM pureGen
-- >>> applyAtomicGen random g :: IO Int
-- 7879794327570578227
--
-- @since 1.2.0
applyAtomicGen :: MonadIO m => (g -> (a, g)) -> (AtomicGenM g) -> m a
applyAtomicGen op (AtomicGenM gVar) =
  liftIO $ atomicModifyIORef' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE applyAtomicGen #-}

-- | Wraps an 'IORef' that holds a pure pseudo-random number generator.
--
-- *   'IOGenM' is safe in the presence of exceptions, but not concurrency.
-- *   'IOGenM' is slower than 'StateGenM' due to the extra pointer indirection.
-- *   'IOGenM' is faster than 'AtomicGenM' since the 'IORef' operations used by
--     'IOGenM' are not atomic.
--
-- An example use case is writing pseudo-random bytes into a file:
--
-- >>> import UnliftIO.Temporary (withSystemTempFile)
-- >>> import Data.ByteString (hPutStr)
-- >>> let ioGen g = withSystemTempFile "foo.bin" $ \_ h -> uniformRM (0, 100) g >>= flip uniformByteStringM g >>= hPutStr h
--
-- and then run it:
--
-- >>> newIOGenM (mkStdGen 1729) >>= ioGen
--
-- @since 1.2.0
newtype IOGenM g = IOGenM { unIOGenM :: IORef g }

-- | Frozen version of mutable `IOGenM` generator
--
-- @since 1.2.0
newtype IOGen g = IOGen { unIOGen :: g }
  deriving (Eq, Ord, Show, RandomGen, Storable, NFData)


-- | Creates a new 'IOGenM'.
--
-- @since 1.2.0
newIOGenM :: MonadIO m => g -> m (IOGenM g)
newIOGenM = fmap IOGenM . liftIO . newIORef

instance (RandomGen g, MonadIO m) => StatefulGen (IOGenM g) m where
  uniformWord32R r = applyIOGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applyIOGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applyIOGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applyIOGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applyIOGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applyIOGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applyIOGen (genShortByteString n)


instance (RandomGen g, MonadIO m) => FrozenGen (IOGen g) m where
  type MutableGen (IOGen g) m = IOGenM g
  freezeGen = fmap IOGen . liftIO . readIORef . unIOGenM
  thawGen (IOGen g) = newIOGenM g


-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> g <- newIOGenM pureGen
-- >>> applyIOGen random g :: IO Int
-- 7879794327570578227
--
-- @since 1.2.0
applyIOGen :: MonadIO m => (g -> (a, g)) -> IOGenM g -> m a
applyIOGen f (IOGenM ref) = liftIO $ do
  g <- readIORef ref
  case f g of
    (!a, !g') -> a <$ writeIORef ref g'
{-# INLINE applyIOGen #-}

-- | Wraps an 'STRef' that holds a pure pseudo-random number generator.
--
-- *   'STGenM' is safe in the presence of exceptions, but not concurrency.
-- *   'STGenM' is slower than 'StateGenM' due to the extra pointer indirection.
--
-- @since 1.2.0
newtype STGenM g s = STGenM { unSTGenM :: STRef s g }

-- | Frozen version of mutable `STGenM` generator
--
-- @since 1.2.0
newtype STGen g = STGen { unSTGen :: g }
  deriving (Eq, Ord, Show, RandomGen, Storable, NFData)

-- | Creates a new 'STGenM'.
--
-- @since 1.2.0
newSTGenM :: g -> ST s (STGenM g s)
newSTGenM = fmap STGenM . newSTRef


instance RandomGen g => StatefulGen (STGenM g s) (ST s) where
  uniformWord32R r = applySTGen (genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r = applySTGen (genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 = applySTGen genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = applySTGen genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = applySTGen genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = applySTGen genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n = applySTGen (genShortByteString n)

instance RandomGen g => FrozenGen (STGen g) (ST s) where
  type MutableGen (STGen g) (ST s) = STGenM g s
  freezeGen = fmap STGen . readSTRef . unSTGenM
  thawGen (STGen g) = newSTGenM g


-- | Applies a pure operation to the wrapped pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> (runSTGen pureGen (\g -> applySTGen random g)) :: (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
applySTGen :: (g -> (a, g)) -> STGenM g s -> ST s a
applySTGen f (STGenM ref) = do
  g <- readSTRef ref
  case f g of
    (!a, !g') -> a <$ writeSTRef ref g'
{-# INLINE applySTGen #-}

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> (runSTGen pureGen (\g -> applySTGen random g)) :: (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})
--
-- @since 1.2.0
runSTGen :: RandomGen g => g -> (forall s . STGenM g s -> ST s a) -> (a, g)
runSTGen g action = unSTGen <$> runST (withMutableGen (STGen g) action)

-- | Runs a monadic generating action in the `ST` monad using a pure
-- pseudo-random number generator. Returns only the resulting pseudo-random
-- value.
--
-- ====__Examples__
--
-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> (runSTGen_ pureGen (\g -> applySTGen random g)) :: Int
-- 7879794327570578227
--
-- @since 1.2.0
runSTGen_ :: RandomGen g => g -> (forall s . STGenM g s -> ST s a) -> a
runSTGen_ g action = fst $ runSTGen g action


-- $uniform
--
-- This library provides two type classes to generate pseudo-random values:
--
-- *   'UniformRange' is used to generate a value of a type uniformly within a
--     range.
-- *   'Uniform' is used to generate a value of a type uniformly over all
--     possible values of that type.
--
-- Types may have instances for both or just one of 'UniformRange' and
-- 'Uniform'. A few examples illustrate this:
--
-- *   'Int', 'Word16' and 'Bool' are instances of both 'UniformRange' and
--     'Uniform'.
-- *   'Integer', 'Float' and 'Double' each have an instance for 'UniformRange'
--     but no 'Uniform' instance.
-- *   A hypothetical type @Radian@ representing angles by taking values in the
--     range @[0, 2π)@ has a trivial 'Uniform' instance, but no 'UniformRange'
--     instance: the problem is that two given @Radian@ values always span /two/
--     ranges, one clockwise and one anti-clockwise.
-- *   It is trivial to construct a @Uniform (a, b)@ instance given
--     @Uniform a@ and @Uniform b@ (and this library provides this tuple
--     instance).
-- *   On the other hand, there is no correct way to construct a
--     @UniformRange (a, b)@ instance based on just @UniformRange a@ and
--     @UniformRange b@.

-------------------------------------------------------------------------------
-- Notes
-------------------------------------------------------------------------------

-- $floating
--
-- The 'UniformRange' instances for 'Float' and 'Double' use the following
-- procedure to generate a random value in a range for @uniformRM (a, b) g@:
--
-- If \(a = b\), return \(a\). Otherwise:
--
-- 1.  Generate \(x\) uniformly such that \(0 \leq x \leq 1\).
--
--     The method by which \(x\) is sampled does not cover all representable
--     floating point numbers in the unit interval. The method never generates
--     denormal floating point numbers, for example.
--
-- 2.  Return \(x \cdot a + (1 - x) \cdot b\).
--
--     Due to rounding errors, floating point operations are neither
--     associative nor distributive the way the corresponding operations on
--     real numbers are. Additionally, floating point numbers admit special
--     values @NaN@ as well as negative and positive infinity.
--
-- For pathological values, step 2 can yield surprising results.
--
-- *   The result may be greater than @max a b@.
--
--     >>> :{
--     let (a, b, x) = (-2.13238e-29, -2.1323799e-29, 0.27736077)
--         result = x * a + (1 - x) * b :: Float
--     in (result, result > max a b)
--     :}
--     (-2.1323797e-29,True)
--
-- *   The result may be smaller than @min a b@.
--
--     >>> :{
--     let (a, b, x) = (-1.9087862, -1.908786, 0.4228573)
--         result = x * a + (1 - x) * b :: Float
--     in (result, result < min a b)
--     :}
--     (-1.9087863,True)
--
-- What happens when @NaN@ or @Infinity@ are given to 'uniformRM'? We first
-- define them as constants:
--
-- >>> nan = read "NaN" :: Float
-- >>> inf = read "Infinity" :: Float
--
-- *   If at least one of \(a\) or \(b\) is @NaN@, the result is @NaN@.
--
--     >>> let (a, b, x) = (nan, 1, 0.5) in x * a + (1 - x) * b
--     NaN
--     >>> let (a, b, x) = (-1, nan, 0.5) in x * a + (1 - x) * b
--     NaN
--
-- *   If \(a\) is @-Infinity@ and \(b\) is @Infinity@, the result is @NaN@.
--     >>> let (a, b, x) = (-inf, inf, 0.5) in x * a + (1 - x) * b
--     NaN
--
-- *   Otherwise, if \(a\) is @Infinity@ or @-Infinity@, the result is \(a\).
--
--     >>> let (a, b, x) = (inf, 1, 0.5) in x * a + (1 - x) * b
--     Infinity
--     >>> let (a, b, x) = (-inf, 1, 0.5) in x * a + (1 - x) * b
--     -Infinity
--
-- *   Otherwise, if \(b\) is @Infinity@ or @-Infinity@, the result is \(b\).
--
--     >>> let (a, b, x) = (1, inf, 0.5) in x * a + (1 - x) * b
--     Infinity
--     >>> let (a, b, x) = (1, -inf, 0.5) in x * a + (1 - x) * b
--     -Infinity
--
-- Note that the [GCC 10.1.0 C++ standard library](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libstdc%2B%2B-v3/include/bits/random.h;h=19307fbc3ca401976ef6823e8fda893e4a263751;hb=63fa67847628e5f358e7e2e7edb8314f0ee31f30#l1859),
-- the [Java 10 standard library](https://docs.oracle.com/javase/10/docs/api/java/util/Random.html#doubles%28double,double%29)
-- and [CPython 3.8](https://github.com/python/cpython/blob/3.8/Lib/random.py#L417)
-- use the same procedure to generate floating point values in a range.
--
-- $implementmonadrandom
--
-- Typically, a monadic pseudo-random number generator has facilities to save
-- and restore its internal state in addition to generating pseudo-random numbers.
--
-- Here is an example instance for the monadic pseudo-random number generator
-- from the @mwc-random@ package:
--
-- > instance (s ~ PrimState m, PrimMonad m) => StatefulGen (MWC.Gen s) m where
-- >   uniformWord8 = MWC.uniform
-- >   uniformWord16 = MWC.uniform
-- >   uniformWord32 = MWC.uniform
-- >   uniformWord64 = MWC.uniform
-- >   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
--
-- > instance PrimMonad m => FrozenGen MWC.Seed m where
-- >   type MutableGen MWC.Seed m = MWC.Gen (PrimState m)
-- >   thawGen = MWC.restore
-- >   freezeGen = MWC.save
--
-- === @FrozenGen@
--
-- `FrozenGen` gives us ability to use any stateful pseudo-random number generator in its
-- immutable form, if one exists that is. This concept is commonly known as a seed, which
-- allows us to save and restore the actual mutable state of a pseudo-random number
-- generator. The biggest benefit that can be drawn from a polymorphic access to a
-- stateful pseudo-random number generator in a frozen form is the ability to serialize,
-- deserialize and possibly even use the stateful generator in a pure setting without
-- knowing the actual type of a generator ahead of time. For example we can write a
-- function that accepts a frozen state of some pseudo-random number generator and
-- produces a short list with random even integers.
--
-- >>> import Data.Int (Int8)
-- >>> :{
-- myCustomRandomList :: FrozenGen f m => f -> m [Int8]
-- myCustomRandomList f =
--   withMutableGen_ f $ \gen -> do
--     len <- uniformRM (5, 10) gen
--     replicateM len $ do
--       x <- uniformM gen
--       pure $ if even x then x else x + 1
-- :}
--
-- and later we can apply it to a frozen version of a stateful generator, such as `STGen`:
--
-- >>> print $ runST $ myCustomRandomList (STGen (mkStdGen 217))
-- [-50,-2,4,-8,-58,-40,24,-32,-110,24]
--
-- or a @Seed@ from @mwc-random@:
--
-- >>> import Data.Vector.Primitive as P
-- >>> print $ runST $ myCustomRandomList (MWC.toSeed (P.fromList [1,2,3]))
-- [24,40,10,40,-8,48,-78,70,-12]
--
-- Alternatively, instead of discarding the final state of the generator, as it happens
-- above, we could have used `withMutableGen`, which together with the result would give
-- us back its frozen form. This would allow us to store the end state of our generator
-- somewhere for the later reuse.
--
--
-- $references
--
-- 1. Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast
-- splittable pseudorandom number generators. In Proceedings of the 2014 ACM
-- International Conference on Object Oriented Programming Systems Languages &
-- Applications (OOPSLA '14). ACM, New York, NY, USA, 453-472. DOI:
-- <https://doi.org/10.1145/2660193.2660195>

-- $setup
-- >>> import Control.Monad.Primitive
-- >>> import qualified System.Random.MWC as MWC
--
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
--
-- >>> :{
-- instance (s ~ PrimState m, PrimMonad m) => StatefulGen (MWC.Gen s) m where
--   uniformWord8 = MWC.uniform
--   uniformWord16 = MWC.uniform
--   uniformWord32 = MWC.uniform
--   uniformWord64 = MWC.uniform
--   uniformShortByteString n g = unsafeSTToPrim (genShortByteStringST n (MWC.uniform g))
-- instance PrimMonad m => FrozenGen MWC.Seed m where
--   type MutableGen MWC.Seed m = MWC.Gen (PrimState m)
--   thawGen = MWC.restore
--   freezeGen = MWC.save
-- :}
--
