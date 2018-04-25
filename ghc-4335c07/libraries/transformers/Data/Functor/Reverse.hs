{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Reverse
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors whose elements are notionally in the reverse order
-- from the original functor.
-----------------------------------------------------------------------------

module Data.Functor.Reverse (
    Reverse(..),
  ) where

import Control.Applicative.Backwards
import Data.Functor.Classes

import Prelude hiding (foldr, foldr1, foldl, foldl1, null, length)
import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Data.Foldable
import Data.Traversable
import Data.Monoid

-- | The same functor, but with 'Foldable' and 'Traversable' instances
-- that process the elements in the reverse order.
newtype Reverse f a = Reverse { getReverse :: f a }

instance (Eq1 f) => Eq1 (Reverse f) where
    liftEq eq (Reverse x) (Reverse y) = liftEq eq x y
    {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (Reverse f) where
    liftCompare comp (Reverse x) (Reverse y) = liftCompare comp x y
    {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (Reverse f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Reverse" Reverse

instance (Show1 f) => Show1 (Reverse f) where
    liftShowsPrec sp sl d (Reverse x) =
        showsUnaryWith (liftShowsPrec sp sl) "Reverse" d x

instance (Eq1 f, Eq a) => Eq (Reverse f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Reverse f a) where compare = compare1
instance (Read1 f, Read a) => Read (Reverse f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Reverse f a) where showsPrec = showsPrec1

-- | Derived instance.
instance (Functor f) => Functor (Reverse f) where
    fmap f (Reverse a) = Reverse (fmap f a)
    {-# INLINE fmap #-}

-- | Derived instance.
instance (Applicative f) => Applicative (Reverse f) where
    pure a = Reverse (pure a)
    {-# INLINE pure #-}
    Reverse f <*> Reverse a = Reverse (f <*> a)
    {-# INLINE (<*>) #-}

-- | Derived instance.
instance (Alternative f) => Alternative (Reverse f) where
    empty = Reverse empty
    {-# INLINE empty #-}
    Reverse x <|> Reverse y = Reverse (x <|> y)
    {-# INLINE (<|>) #-}

-- | Derived instance.
instance (Monad m) => Monad (Reverse m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = Reverse (return a)
    {-# INLINE return #-}
#endif
    m >>= f = Reverse (getReverse m >>= getReverse . f)
    {-# INLINE (>>=) #-}
    fail msg = Reverse (fail msg)
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (Reverse m) where
    fail msg = Reverse (Fail.fail msg)
    {-# INLINE fail #-}
#endif

-- | Derived instance.
instance (MonadPlus m) => MonadPlus (Reverse m) where
    mzero = Reverse mzero
    {-# INLINE mzero #-}
    Reverse x `mplus` Reverse y = Reverse (x `mplus` y)
    {-# INLINE mplus #-}

-- | Fold from right to left.
instance (Foldable f) => Foldable (Reverse f) where
    foldMap f (Reverse t) = getDual (foldMap (Dual . f) t)
    {-# INLINE foldMap #-}
    foldr f z (Reverse t) = foldl (flip f) z t
    {-# INLINE foldr #-}
    foldl f z (Reverse t) = foldr (flip f) z t
    {-# INLINE foldl #-}
    foldr1 f (Reverse t) = foldl1 (flip f) t
    {-# INLINE foldr1 #-}
    foldl1 f (Reverse t) = foldr1 (flip f) t
    {-# INLINE foldl1 #-}
#if MIN_VERSION_base(4,8,0)
    null (Reverse t) = null t
    length (Reverse t) = length t
#endif

-- | Traverse from right to left.
instance (Traversable f) => Traversable (Reverse f) where
    traverse f (Reverse t) =
        fmap Reverse . forwards $ traverse (Backwards . f) t
    {-# INLINE traverse #-}
