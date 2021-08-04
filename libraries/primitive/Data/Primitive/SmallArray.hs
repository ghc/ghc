{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module : Data.Primitive.SmallArray
-- Copyright: (c) 2015 Dan Doel
-- License: BSD3
--
-- Maintainer: libraries@haskell.org
-- Portability: non-portable
--
-- Small arrays are boxed (im)mutable arrays.
--
-- The underlying structure of the 'Array' type contains a card table, allowing
-- segments of the array to be marked as having been mutated. This allows the
-- garbage collector to only re-traverse segments of the array that have been
-- marked during certain phases, rather than having to traverse the entire
-- array.
--
-- 'SmallArray' lacks this table. This means that it takes up less memory and
-- has slightly faster writes. It is also more efficient during garbage
-- collection so long as the card table would have a single entry covering the
-- entire array. These advantages make them suitable for use as arrays that are
-- known to be small.
--
-- The card size is 128, so for uses much larger than that, 'Array' would likely
-- be superior.
--
-- The underlying type, 'SmallArray#', was introduced in GHC 7.10, so prior to
-- that version, this module simply implements small arrays as 'Array'.

module Data.Primitive.SmallArray
  ( SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , copySmallArray
  , copySmallMutableArray
  , indexSmallArray
  , indexSmallArrayM
  , indexSmallArray##
  , cloneSmallArray
  , cloneSmallMutableArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , thawSmallArray
  , runSmallArray
  , unsafeThawSmallArray
  , sizeofSmallArray
  , sizeofSmallMutableArray
#if MIN_VERSION_base(4,14,0)
  , shrinkSmallMutableArray
#endif
  , smallArrayFromList
  , smallArrayFromListN
  , mapSmallArray'
  , traverseSmallArrayP
  ) where


#if (__GLASGOW_HASKELL__ >= 710)
#define HAVE_SMALL_ARRAY 1
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Exts hiding (toList)
import qualified GHC.Exts
#endif

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Zip
import Data.Data
import Data.Foldable as Foldable
import Data.Functor.Identity
#if !(MIN_VERSION_base(4,10,0))
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import qualified GHC.ST as GHCST
import qualified Data.Semigroup as Sem
#endif
import Text.ParserCombinators.ReadP
#if MIN_VERSION_base(4,10,0)
import GHC.Exts (runRW#)
#elif MIN_VERSION_base(4,9,0)
import GHC.Base (runRW#)
#endif

#if !(HAVE_SMALL_ARRAY)
import Data.Primitive.Array
import Data.Traversable
import qualified Data.Primitive.Array as Array
#endif

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq1(..),Ord1(..),Show1(..),Read1(..))
#endif

#if HAVE_SMALL_ARRAY
data SmallArray a = SmallArray (SmallArray# a)
  deriving Typeable
#else
newtype SmallArray a = SmallArray (Array a) deriving
  ( Eq
  , Ord
  , Show
  , Read
  , Foldable
  , Traversable
  , Functor
  , Applicative
  , Alternative
  , Monad
  , MonadPlus
  , MonadZip
  , MonadFix
  , Monoid
  , NFData
#if MIN_VERSION_deepseq(1,4,3)
  , NFData1
#endif
  , Typeable
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
  , Eq1
  , Ord1
  , Show1
  , Read1
#endif
  )

#if MIN_VERSION_base(4,7,0)
instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  fromListN n l = SmallArray (fromListN n l)
  fromList l = SmallArray (fromList l)
  toList a = Foldable.toList a
#endif
#endif

#if HAVE_SMALL_ARRAY
#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 SmallArray where
  liftRnf r = foldl' (\_ -> r) ()
#endif

instance NFData a => NFData (SmallArray a) where
  rnf = foldl' (\_ -> rnf) ()
#endif

#if HAVE_SMALL_ARRAY
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)
  deriving Typeable
#else
newtype SmallMutableArray s a = SmallMutableArray (MutableArray s a)
  deriving (Eq, Typeable)
#endif

-- | Create a new small mutable array.
--
-- /Note:/ this function does not check if the input is non-negative.
newSmallArray
  :: PrimMonad m
  => Int -- ^ size
  -> a   -- ^ initial contents
  -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
newSmallArray (I# i#) x = primitive $ \s ->
  case newSmallArray# i# x s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
#else
newSmallArray n e = SmallMutableArray `liftM` newArray n e
#endif
{-# INLINE newSmallArray #-}

-- | Read the element at a given index in a mutable array.
--
-- /Note:/ this function does not do bounds checking.
readSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ array
  -> Int                               -- ^ index
  -> m a
#if HAVE_SMALL_ARRAY
readSmallArray (SmallMutableArray sma#) (I# i#) =
  primitive $ readSmallArray# sma# i#
#else
readSmallArray (SmallMutableArray a) = readArray a
#endif
{-# INLINE readSmallArray #-}

-- | Write an element at the given idex in a mutable array.
--
-- /Note:/ this function does not do bounds checking.
writeSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ array
  -> Int                               -- ^ index
  -> a                                 -- ^ new element
  -> m ()
#if HAVE_SMALL_ARRAY
writeSmallArray (SmallMutableArray sma#) (I# i#) x =
  primitive_ $ writeSmallArray# sma# i# x
#else
writeSmallArray (SmallMutableArray a) = writeArray a
#endif
{-# INLINE writeSmallArray #-}

-- | Look up an element in an immutable array.
--
-- The purpose of returning a result using a monad is to allow the caller to
-- avoid retaining references to the array. Evaluating the return value will
-- cause the array lookup to be performed, even though it may not require the
-- element of the array to be evaluated (which could throw an exception). For
-- instance:
--
-- > data Box a = Box a
-- > ...
-- >
-- > f sa = case indexSmallArrayM sa 0 of
-- >   Box x -> ...
--
-- 'x' is not a closure that references 'sa' as it would be if we instead
-- wrote:
--
-- > let x = indexSmallArray sa 0
--
-- And does not prevent 'sa' from being garbage collected.
--
-- Note that 'Identity' is not adequate for this use, as it is a newtype, and
-- cannot be evaluated without evaluating the element.
--
-- /Note:/ this function does not do bounds checking.
indexSmallArrayM
  :: Monad m
  => SmallArray a -- ^ array
  -> Int          -- ^ index
  -> m a
#if HAVE_SMALL_ARRAY
indexSmallArrayM (SmallArray sa#) (I# i#) =
  case indexSmallArray# sa# i# of
    (# x #) -> pure x
#else
indexSmallArrayM (SmallArray a) = indexArrayM a
#endif
{-# INLINE indexSmallArrayM #-}

-- | Look up an element in an immutable array.
--
-- /Note:/ this function does not do bounds checking.
indexSmallArray
  :: SmallArray a -- ^ array
  -> Int          -- ^ index
  -> a
#if HAVE_SMALL_ARRAY
indexSmallArray sa i = runIdentity $ indexSmallArrayM sa i
#else
indexSmallArray (SmallArray a) = indexArray a
#endif
{-# INLINE indexSmallArray #-}

-- | Read a value from the immutable array at the given index, returning
-- the result in an unboxed unary tuple. This is currently used to implement
-- folds.
indexSmallArray## :: SmallArray a -> Int -> (# a #)
#if HAVE_SMALL_ARRAY
indexSmallArray## (SmallArray ary) (I# i) = indexSmallArray# ary i
#else
indexSmallArray## (SmallArray a) = indexArray## a
#endif
{-# INLINE indexSmallArray## #-}

-- | Create a copy of a slice of an immutable array.
--
-- /Note:/ The provided Array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallArray
  :: SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> SmallArray a
#if HAVE_SMALL_ARRAY
cloneSmallArray (SmallArray sa#) (I# i#) (I# j#) =
  SmallArray (cloneSmallArray# sa# i# j#)
#else
cloneSmallArray (SmallArray a) i j = SmallArray $ cloneArray a i j
#endif
{-# INLINE cloneSmallArray #-}

-- | Create a copy of a slice of a mutable array.
--
-- /Note:/ The provided Array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneSmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
cloneSmallMutableArray (SmallMutableArray sma#) (I# o#) (I# l#) =
  primitive $ \s -> case cloneSmallMutableArray# sma# o# l# s of
    (# s', smb# #) -> (# s', SmallMutableArray smb# #)
#else
cloneSmallMutableArray (SmallMutableArray ma) i j =
  SmallMutableArray `liftM` cloneMutableArray ma i j
#endif
{-# INLINE cloneSmallMutableArray #-}

-- | Create an immutable array corresponding to a slice of a mutable array.
--
-- This operation copies the portion of the array to be frozen.
freezeSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallArray a)
#if HAVE_SMALL_ARRAY
freezeSmallArray (SmallMutableArray sma#) (I# i#) (I# j#) =
  primitive $ \s -> case freezeSmallArray# sma# i# j# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)
#else
freezeSmallArray (SmallMutableArray ma) i j =
  SmallArray `liftM` freezeArray ma i j
#endif
{-# INLINE freezeSmallArray #-}

-- | Render a mutable array immutable.
--
-- This operation performs no copying, so care must be taken not to modify the
-- input array after freezing.
unsafeFreezeSmallArray
  :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
#if HAVE_SMALL_ARRAY
unsafeFreezeSmallArray (SmallMutableArray sma#) =
  primitive $ \s -> case unsafeFreezeSmallArray# sma# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)
#else
unsafeFreezeSmallArray (SmallMutableArray ma) =
  SmallArray `liftM` unsafeFreezeArray ma
#endif
{-# INLINE unsafeFreezeSmallArray #-}

-- | Create a mutable array corresponding to a slice of an immutable array.
--
-- This operation copies the portion of the array to be thawed.
thawSmallArray
  :: PrimMonad m
  => SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
thawSmallArray (SmallArray sa#) (I# o#) (I# l#) =
  primitive $ \s -> case thawSmallArray# sa# o# l# s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
#else
thawSmallArray (SmallArray a) off len =
  SmallMutableArray `liftM` thawArray a off len
#endif
{-# INLINE thawSmallArray #-}

-- | Render an immutable array mutable.
--
-- This operation performs no copying, so care must be taken with its use.
unsafeThawSmallArray
  :: PrimMonad m => SmallArray a -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
unsafeThawSmallArray (SmallArray sa#) =
  primitive $ \s -> case unsafeThawSmallArray# sa# s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
#else
unsafeThawSmallArray (SmallArray a) = SmallMutableArray `liftM` unsafeThawArray a
#endif
{-# INLINE unsafeThawSmallArray #-}

-- | Copy a slice of an immutable array into a mutable array.
--
-- /Note:/ this function does not do bounds or overlap checking.
copySmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ destination
  -> Int                               -- ^ destination offset
  -> SmallArray a                      -- ^ source
  -> Int                               -- ^ source offset
  -> Int                               -- ^ length
  -> m ()
#if HAVE_SMALL_ARRAY
copySmallArray
  (SmallMutableArray dst#) (I# do#) (SmallArray src#) (I# so#) (I# l#) =
    primitive_ $ copySmallArray# src# so# dst# do# l#
#else
copySmallArray (SmallMutableArray dst) i (SmallArray src) = copyArray dst i src
#endif
{-# INLINE copySmallArray #-}

-- | Copy a slice of one mutable array into another.
--
-- /Note:/ this function does not do bounds or overlap checking.
copySmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ destination
  -> Int                               -- ^ destination offset
  -> SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ source offset
  -> Int                               -- ^ length
  -> m ()
#if HAVE_SMALL_ARRAY
copySmallMutableArray
  (SmallMutableArray dst#) (I# do#)
  (SmallMutableArray src#) (I# so#)
  (I# l#) =
    primitive_ $ copySmallMutableArray# src# so# dst# do# l#
#else
copySmallMutableArray (SmallMutableArray dst) i (SmallMutableArray src) =
  copyMutableArray dst i src
#endif
{-# INLINE copySmallMutableArray #-}

sizeofSmallArray :: SmallArray a -> Int
#if HAVE_SMALL_ARRAY
sizeofSmallArray (SmallArray sa#) = I# (sizeofSmallArray# sa#)
#else
sizeofSmallArray (SmallArray a) = sizeofArray a
#endif
{-# INLINE sizeofSmallArray #-}

sizeofSmallMutableArray :: SmallMutableArray s a -> Int
#if HAVE_SMALL_ARRAY
sizeofSmallMutableArray (SmallMutableArray sa#) =
  I# (sizeofSmallMutableArray# sa#)
#else
sizeofSmallMutableArray (SmallMutableArray ma) = sizeofMutableArray ma
#endif
{-# INLINE sizeofSmallMutableArray #-}

-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- *one* result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
traverseSmallArrayP
  :: PrimMonad m
  => (a -> m b)
  -> SmallArray a
  -> m (SmallArray b)
#if HAVE_SMALL_ARRAY
traverseSmallArrayP f = \ !ary ->
  let
    !sz = sizeofSmallArray ary
    go !i !mary
      | i == sz
      = unsafeFreezeSmallArray mary
      | otherwise
      = do
          a <- indexSmallArrayM ary i
          b <- f a
          writeSmallArray mary i b
          go (i + 1) mary
  in do
    mary <- newSmallArray sz badTraverseValue
    go 0 mary
#else
traverseSmallArrayP f (SmallArray ar) = SmallArray `liftM` traverseArrayP f ar
#endif
{-# INLINE traverseSmallArrayP #-}

-- | Strict map over the elements of the array.
mapSmallArray' :: (a -> b) -> SmallArray a -> SmallArray b
#if HAVE_SMALL_ARRAY
mapSmallArray' f sa = createSmallArray (length sa) (die "mapSmallArray'" "impossible") $ \smb ->
  fix ? 0 $ \go i ->
    when (i < length sa) $ do
      x <- indexSmallArrayM sa i
      let !y = f x
      writeSmallArray smb i y *> go (i+1)
#else
mapSmallArray' f (SmallArray ar) = SmallArray (mapArray' f ar)
#endif
{-# INLINE mapSmallArray' #-}

#ifndef HAVE_SMALL_ARRAY
runSmallArray
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray a
runSmallArray m = SmallArray $ runArray $
  m >>= \(SmallMutableArray mary) -> return mary

#elif !MIN_VERSION_base(4,9,0)
runSmallArray
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray a
runSmallArray m = runST $ m >>= unsafeFreezeSmallArray

#else
-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
-- The only downside is that separately created 0-length arrays won't share
-- their Array constructors, although they'll share their underlying
-- Array#s.
runSmallArray
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray a
runSmallArray m = SmallArray (runSmallArray# m)

runSmallArray#
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray# a
runSmallArray# m = case runRW# $ \s ->
  case unST m s of { (# s', SmallMutableArray mary# #) ->
  unsafeFreezeSmallArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

#endif

#if HAVE_SMALL_ARRAY
-- See the comment on runSmallArray for why we use emptySmallArray#.
createSmallArray
  :: Int
  -> a
  -> (forall s. SmallMutableArray s a -> ST s ())
  -> SmallArray a
createSmallArray 0 _ _ = SmallArray (emptySmallArray# (# #))
createSmallArray n x f = runSmallArray $ do
  mary <- newSmallArray n x
  f mary
  pure mary

emptySmallArray# :: (# #) -> SmallArray# a
emptySmallArray# _ = case emptySmallArray of SmallArray ar -> ar
{-# NOINLINE emptySmallArray# #-}

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.SmallArray." ++ fun ++ ": " ++ problem

emptySmallArray :: SmallArray a
emptySmallArray =
  runST $ newSmallArray 0 (die "emptySmallArray" "impossible")
            >>= unsafeFreezeSmallArray
{-# NOINLINE emptySmallArray #-}


infixl 1 ?
(?) :: (a -> b -> c) -> (b -> a -> c)
(?) = flip
{-# INLINE (?) #-}

noOp :: a -> ST s ()
noOp = const $ pure ()

smallArrayLiftEq :: (a -> b -> Bool) -> SmallArray a -> SmallArray b -> Bool
smallArrayLiftEq p sa1 sa2 = length sa1 == length sa2 && loop (length sa1 - 1)
  where
  loop i
    | i < 0
    = True
    | (# x #) <- indexSmallArray## sa1 i
    , (# y #) <- indexSmallArray## sa2 i
    = p x y && loop (i-1)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Eq1 SmallArray where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftEq = smallArrayLiftEq
#else
  eq1 = smallArrayLiftEq (==)
#endif
#endif

instance Eq a => Eq (SmallArray a) where
  sa1 == sa2 = smallArrayLiftEq (==) sa1 sa2

instance Eq (SmallMutableArray s a) where
  SmallMutableArray sma1# == SmallMutableArray sma2# =
    isTrue# (sameSmallMutableArray# sma1# sma2#)

smallArrayLiftCompare :: (a -> b -> Ordering) -> SmallArray a -> SmallArray b -> Ordering
smallArrayLiftCompare elemCompare a1 a2 = loop 0
  where
  mn = length a1 `min` length a2
  loop i
    | i < mn
    , (# x1 #) <- indexSmallArray## a1 i
    , (# x2 #) <- indexSmallArray## a2 i
    = elemCompare x1 x2 `mappend` loop (i+1)
    | otherwise = compare (length a1) (length a2)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Ord1 SmallArray where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftCompare = smallArrayLiftCompare
#else
  compare1 = smallArrayLiftCompare compare
#endif
#endif

-- | Lexicographic ordering. Subject to change between major versions.
instance Ord a => Ord (SmallArray a) where
  compare sa1 sa2 = smallArrayLiftCompare compare sa1 sa2

instance Foldable SmallArray where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !ary ->
    let
      !sz = sizeofSmallArray ary
      go i
        | i == sz = z
        | (# x #) <- indexSmallArray## ary i
        = f x (go (i+1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !ary ->
    let
      go i
        | i < 0 = z
        | (# x #) <- indexSmallArray## ary i
        = f (go (i-1)) x
    in go (sizeofSmallArray ary - 1)
  {-# INLINE foldl #-}
  foldr1 f = \ !ary ->
    let
      !sz = sizeofSmallArray ary - 1
      go i =
        case indexSmallArray## ary i of
          (# x #) | i == sz -> x
                  | otherwise -> f x (go (i+1))
    in if sz < 0
       then die "foldr1" "Empty SmallArray"
       else go 0
  {-# INLINE foldr1 #-}
  foldl1 f = \ !ary ->
    let
      !sz = sizeofSmallArray ary - 1
      go i =
        case indexSmallArray## ary i of
          (# x #) | i == 0 -> x
                  | otherwise -> f (go (i - 1)) x
    in if sz < 0
       then die "foldl1" "Empty SmallArray"
       else go sz
  {-# INLINE foldl1 #-}
  foldr' f = \z !ary ->
    let
      go i !acc
        | i == -1 = acc
        | (# x #) <- indexSmallArray## ary i
        = go (i-1) (f x acc)
    in go (sizeofSmallArray ary - 1) z
  {-# INLINE foldr' #-}
  foldl' f = \z !ary ->
    let
      !sz = sizeofSmallArray ary
      go i !acc
        | i == sz = acc
        | (# x #) <- indexSmallArray## ary i
        = go (i+1) (f acc x)
    in go 0 z
  {-# INLINE foldl' #-}
  null a = sizeofSmallArray a == 0
  {-# INLINE null #-}
  length = sizeofSmallArray
  {-# INLINE length #-}
  maximum ary | sz == 0   = die "maximum" "Empty SmallArray"
              | (# frst #) <- indexSmallArray## ary 0
              = go 1 frst
   where
     sz = sizeofSmallArray ary
     go i !e
       | i == sz = e
       | (# x #) <- indexSmallArray## ary i
       = go (i+1) (max e x)
  {-# INLINE maximum #-}
  minimum ary | sz == 0   = die "minimum" "Empty SmallArray"
              | (# frst #) <- indexSmallArray## ary 0
              = go 1 frst
   where sz = sizeofSmallArray ary
         go i !e
           | i == sz = e
           | (# x #) <- indexSmallArray## ary i
           = go (i+1) (min e x)
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}

newtype STA a = STA {_runSTA :: forall s. SmallMutableArray# s a -> ST s (SmallArray a)}

runSTA :: Int -> STA a -> SmallArray a
runSTA !sz = \ (STA m) -> runST $ newSmallArray_ sz >>=
                        \ (SmallMutableArray ar#) -> m ar#
{-# INLINE runSTA #-}

newSmallArray_ :: Int -> ST s (SmallMutableArray s a)
newSmallArray_ !n = newSmallArray n badTraverseValue

badTraverseValue :: a
badTraverseValue = die "traverse" "bad indexing"
{-# NOINLINE badTraverseValue #-}

instance Traversable SmallArray where
  traverse f = traverseSmallArray f
  {-# INLINE traverse #-}

traverseSmallArray
  :: Applicative f
  => (a -> f b) -> SmallArray a -> f (SmallArray b)
traverseSmallArray f = \ !ary ->
  let
    !len = sizeofSmallArray ary
    go !i
      | i == len
      = pure $ STA $ \mary -> unsafeFreezeSmallArray (SmallMutableArray mary)
      | (# x #) <- indexSmallArray## ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writeSmallArray (SmallMutableArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in if len == 0
     then pure emptySmallArray
     else runSTA len <$> go 0
{-# INLINE [1] traverseSmallArray #-}

{-# RULES
"traverse/ST" forall (f :: a -> ST s b). traverseSmallArray f = traverseSmallArrayP f
"traverse/IO" forall (f :: a -> IO b). traverseSmallArray f = traverseSmallArrayP f
"traverse/Id" forall (f :: a -> Identity b). traverseSmallArray f =
   (coerce :: (SmallArray a -> SmallArray (Identity b))
           -> SmallArray a -> Identity (SmallArray b)) (fmap f)
 #-}


instance Functor SmallArray where
  fmap f sa = createSmallArray (length sa) (die "fmap" "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < length sa) $ do
        x <- indexSmallArrayM sa i
        writeSmallArray smb i (f x) *> go (i+1)
  {-# INLINE fmap #-}

  x <$ sa = createSmallArray (length sa) x noOp

instance Applicative SmallArray where
  pure x = createSmallArray 1 x noOp

  sa *> sb = createSmallArray (la*lb) (die "*>" "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < la) $
        copySmallArray smb 0 sb 0 lb *> go (i+1)
   where
   la = length sa ; lb = length sb

  a <* b = createSmallArray (sza*szb) (die "<*" "impossible") $ \ma ->
    let fill off i e = when (i < szb) $
                         writeSmallArray ma (off+i) e >> fill off (i+1) e
        go i = when (i < sza) $ do
                 x <- indexSmallArrayM a i
                 fill (i*szb) 0 x
                 go (i+1)
     in go 0
   where sza = sizeofSmallArray a ; szb = sizeofSmallArray b

  ab <*> a = createSmallArray (szab*sza) (die "<*>" "impossible") $ \mb ->
    let go1 i = when (i < szab) $
            do
              f <- indexSmallArrayM ab i
              go2 (i*sza) f 0
              go1 (i+1)
        go2 off f j = when (j < sza) $
            do
              x <- indexSmallArrayM a j
              writeSmallArray mb (off + j) (f x)
              go2 off f (j + 1)
    in go1 0
   where szab = sizeofSmallArray ab ; sza = sizeofSmallArray a

instance Alternative SmallArray where
  empty = emptySmallArray

  sl <|> sr =
    createSmallArray (length sl + length sr) (die "<|>" "impossible") $ \sma ->
      copySmallArray sma 0 sl 0 (length sl)
        *> copySmallArray sma (length sl) sr 0 (length sr)

  many sa | null sa   = pure []
          | otherwise = die "many" "infinite arrays are not well defined"

  some sa | null sa   = emptySmallArray
          | otherwise = die "some" "infinite arrays are not well defined"

data ArrayStack a
  = PushArray !(SmallArray a) !(ArrayStack a)
  | EmptyStack
-- TODO: This isn't terribly efficient. It would be better to wrap
-- ArrayStack with a type like
--
-- data NES s a = NES !Int !(SmallMutableArray s a) !(ArrayStack a)
--
-- We'd copy incoming arrays into the mutable array until we would
-- overflow it. Then we'd freeze it, push it on the stack, and continue.
-- Any sufficiently large incoming arrays would go straight on the stack.
-- Such a scheme would make the stack much more compact in the case
-- of many small arrays.

instance Monad SmallArray where
  return = pure
  (>>) = (*>)

  sa >>= f = collect 0 EmptyStack (la-1)
   where
   la = length sa
   collect sz stk i
     | i < 0 = createSmallArray sz (die ">>=" "impossible") $ fill 0 stk
     | (# x #) <- indexSmallArray## sa i
     , let sb = f x
           lsb = length sb
       -- If we don't perform this check, we could end up allocating
       -- a stack full of empty arrays if someone is filtering most
       -- things out. So we refrain from pushing empty arrays.
     = if lsb == 0
       then collect sz stk (i-1)
       else collect (sz + lsb) (PushArray sb stk) (i-1)

   fill _ EmptyStack _ = return ()
   fill off (PushArray sb sbs) smb =
     copySmallArray smb off sb 0 (length sb)
       *> fill (off + length sb) sbs smb

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Fail.MonadFail SmallArray where
  fail _ = emptySmallArray

instance MonadPlus SmallArray where
  mzero = empty
  mplus = (<|>)

zipW :: String -> (a -> b -> c) -> SmallArray a -> SmallArray b -> SmallArray c
zipW nm = \f sa sb -> let mn = length sa `min` length sb in
  createSmallArray mn (die nm "impossible") $ \mc ->
    fix ? 0 $ \go i -> when (i < mn) $ do
      x <- indexSmallArrayM sa i
      y <- indexSmallArrayM sb i
      writeSmallArray mc i (f x y)
      go (i+1)
{-# INLINE zipW #-}

instance MonadZip SmallArray where
  mzip = zipW "mzip" (,)
  mzipWith = zipW "mzipWith"
  {-# INLINE mzipWith #-}
  munzip sab = runST $ do
    let sz = length sab
    sma <- newSmallArray sz $ die "munzip" "impossible"
    smb <- newSmallArray sz $ die "munzip" "impossible"
    fix ? 0 $ \go i ->
      when (i < sz) $ case indexSmallArray sab i of
        (x, y) -> do writeSmallArray sma i x
                     writeSmallArray smb i y
                     go $ i+1
    (,) <$> unsafeFreezeSmallArray sma
        <*> unsafeFreezeSmallArray smb

instance MonadFix SmallArray where
  mfix f = createSmallArray (sizeofSmallArray (f err))
                            (die "mfix" "impossible") $ flip fix 0 $
    \r !i !mary -> when (i < sz) $ do
                      writeSmallArray mary i (fix (\xi -> f xi `indexSmallArray` i))
                      r (i + 1) mary
    where
      sz = sizeofSmallArray (f err)
      err = error "mfix for Data.Primitive.SmallArray applied to strict function."

#if MIN_VERSION_base(4,9,0)
-- | @since 0.6.3.0
instance Sem.Semigroup (SmallArray a) where
  (<>) = (<|>)
  sconcat = mconcat . toList
#endif

instance Monoid (SmallArray a) where
  mempty = empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<|>)
#endif
  mconcat l = createSmallArray n (die "mconcat" "impossible") $ \ma ->
    let go !_  [    ] = return ()
        go off (a:as) =
          copySmallArray ma off a 0 (sizeofSmallArray a) >> go (off + sizeofSmallArray a) as
     in go 0 l
   where n = sum . fmap length $ l

instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  fromListN = smallArrayFromListN
  fromList = smallArrayFromList
  toList = Foldable.toList

smallArrayLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> SmallArray a -> ShowS
smallArrayLiftShowsPrec elemShowsPrec elemListShowsPrec p sa = showParen (p > 10) $
  showString "fromListN " . shows (length sa) . showString " "
    . listLiftShowsPrec elemShowsPrec elemListShowsPrec 11 (toList sa)

-- this need to be included for older ghcs
listLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> [a] -> ShowS
listLiftShowsPrec _ sl _ = sl

instance Show a => Show (SmallArray a) where
  showsPrec p sa = smallArrayLiftShowsPrec showsPrec showList p sa

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Show1 SmallArray where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftShowsPrec = smallArrayLiftShowsPrec
#else
  showsPrec1 = smallArrayLiftShowsPrec showsPrec showList
#endif
#endif

smallArrayLiftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (SmallArray a)
smallArrayLiftReadsPrec _ listReadsPrec p = readParen (p > 10) . readP_to_S $ do
  () <$ string "fromListN"
  skipSpaces
  n <- readS_to_P reads
  skipSpaces
  l <- readS_to_P listReadsPrec
  return $ smallArrayFromListN n l

instance Read a => Read (SmallArray a) where
  readsPrec = smallArrayLiftReadsPrec readsPrec readList

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | @since 0.6.4.0
instance Read1 SmallArray where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftReadsPrec = smallArrayLiftReadsPrec
#else
  readsPrec1 = smallArrayLiftReadsPrec readsPrec readList
#endif
#endif



smallArrayDataType :: DataType
smallArrayDataType =
  mkDataType "Data.Primitive.SmallArray.SmallArray" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr smallArrayDataType "fromList" [] Prefix

instance Data a => Data (SmallArray a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = smallArrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> die "gunfold" "SmallArray"
  gfoldl f z m = z fromList `f` toList m

instance (Typeable s, Typeable a) => Data (SmallMutableArray s a) where
  toConstr _ = die "toConstr" "SmallMutableArray"
  gunfold _ _ = die "gunfold" "SmallMutableArray"
  dataTypeOf _ = mkNoRepType "Data.Primitive.SmallArray.SmallMutableArray"
#endif

-- | Create a 'SmallArray' from a list of a known length. If the length
--   of the list does not match the given length, this throws an exception.
smallArrayFromListN :: Int -> [a] -> SmallArray a
#if HAVE_SMALL_ARRAY
smallArrayFromListN n l =
  createSmallArray n
      (die "smallArrayFromListN" "uninitialized element") $ \sma ->
  let go !ix [] = if ix == n
        then return ()
        else die "smallArrayFromListN" "list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeSmallArray sma ix x
          go (ix+1) xs
        else die "smallArrayFromListN" "list length greater than specified size"
  in go 0 l
#else
smallArrayFromListN n l = SmallArray (Array.fromListN n l)
#endif

-- | Create a 'SmallArray' from a list.
smallArrayFromList :: [a] -> SmallArray a
smallArrayFromList l = smallArrayFromListN (length l) l

#if MIN_VERSION_base(4,14,0)
-- | Shrink the mutable array in place. The size given must be equal to
-- or less than the current size of the array. This is not checked.
shrinkSmallMutableArray :: PrimMonad m
  => SmallMutableArray (PrimState m) a
  -> Int
  -> m ()
{-# inline shrinkSmallMutableArray #-}
shrinkSmallMutableArray (SmallMutableArray x) (I# n) = primitive
  (\s0 -> case GHC.Exts.shrinkSmallMutableArray# x n s0 of
    s1 -> (# s1, () #)
  )
#endif
