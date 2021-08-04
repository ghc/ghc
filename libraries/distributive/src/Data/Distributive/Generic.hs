{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Distributive
-- Copyright   :  (C) 2011-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Distributive.Generic
  ( GDistributive(..)
  , genericCollect
  , genericDistribute
  ) where

import Data.Distributive
import GHC.Generics
import Data.Coerce

-- | 'collect' derived from a 'Generic1' type
--
-- This can be used to easily produce a 'Distributive' instance for a
-- type with a 'Generic1' instance,
--
-- > data V2 a = V2 a a deriving (Show, Functor, Generic1)
-- > instance Distributive V2' where collect = genericCollect
genericCollect :: (Functor f, Generic1 g, GDistributive (Rep1 g))
               => (a -> g b) -> f a -> g (f b)
genericCollect f = to1 . gcollect (from1 . f)

-- | 'distribute' derived from a 'Generic1' type
--
-- It's often more efficient to use 'genericCollect' instead.
genericDistribute  :: (Functor f, Generic1 g, GDistributive (Rep1 g)) => f (g a) -> g (f a)
genericDistribute = to1 . gdistribute . fmap from1


-- Can't distribute over,
--   * sums (:+:)
--   * K1
--   * V1
class GDistributive g where
  gcollect :: Functor f => (a -> g b) -> f a -> g (f b)

gdistribute :: (GDistributive g, Functor f) => f (g b) -> g (f b)
gdistribute = gcollect id
{-# INLINE gdistribute #-}

instance GDistributive U1 where
  gcollect _ _ = U1
  {-# INLINE gcollect #-}

instance (GDistributive a, GDistributive b) => GDistributive (a :*: b) where
  -- It might be tempting to fuse `gcollect fstP (fmap f x)` into
  -- `gcollect (fstP . f) x`, but this would lead to a loss of sharing.
  gcollect f x = gcollect fstP x' :*: gcollect sndP x' where
    x' = fmap f x
    fstP (l :*: _) = l
    sndP (_ :*: r) = r
  {-# INLINE gcollect #-}

instance (Distributive a, GDistributive b) => GDistributive (a :.: b) where
  gcollect f = Comp1 . fmap gdistribute . collect (coerce f)
  {-# INLINE gcollect #-}

instance GDistributive Par1 where
  gcollect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f => (a -> Par1 b) -> f a -> Par1 (f b)
  {-# INLINE gcollect #-}

instance Distributive f => GDistributive (Rec1 f) where
  gcollect = coerce (collect :: (a -> f b) -> g a -> f (g b))
    :: forall g a b . Functor g
    => (a -> Rec1 f b) -> g a -> Rec1 f (g b)
  {-# INLINE gcollect #-}

instance GDistributive f => GDistributive (M1 i c f) where
  gcollect = coerce (gcollect :: (a -> f b) -> g a -> f (g b))
    :: forall g a b . Functor g
    => (a -> M1 i c f b) -> g a -> M1 i c f (g b)
  {-# INLINE gcollect #-}
