{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
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
module Data.Distributive
  ( Distributive(..)
  , cotraverse
  , comapM
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad (liftM)
#if __GLASGOW_HASKELL__ < 707
import Control.Monad.Instances ()
#endif
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import qualified Data.Monoid as Monoid
import Data.Orphans ()

#if MIN_VERSION_base(4,4,0)
import Data.Complex
#endif
#if __GLASGOW_HASKELL__ >= 707 || defined(MIN_VERSION_tagged)
import Data.Proxy
#endif
#if __GLASGOW_HASKELL__ >= 800 || defined(MIN_VERSION_semigroups)
import qualified Data.Semigroup as Semigroup
#endif
#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (U1(..), (:*:)(..), (:.:)(..), Par1(..), Rec1(..), M1(..))
#endif

#ifdef HLINT
{-# ANN module "hlint: ignore Use section" #-}
#endif

-- | This is the categorical dual of 'Traversable'.
--
-- Due to the lack of non-trivial comonoids in Haskell, we can restrict
-- ourselves to requiring a 'Functor' rather than
-- some Coapplicative class. Categorically every 'Distributive'
-- functor is actually a right adjoint, and so it must be 'Representable'
-- endofunctor and preserve all limits. This is a fancy way of saying it
-- is isomorphic to @(->) x@ for some x.
--
-- To be distributable a container will need to have a way to consistently
-- zip a potentially infinite number of copies of itself. This effectively
-- means that the holes in all values of that type, must have the same
-- cardinality, fixed sized vectors, infinite streams, functions, etc.
-- and no extra information to try to merge together.
--
class Functor g => Distributive g where
#if __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL distribute | collect #-}
#endif
  -- | The dual of 'Data.Traversable.sequenceA'
  --
  -- >>> distribute [(+1),(+2)] 1
  -- [2,3]
  --
  -- @
  -- 'distribute' = 'collect' 'id'
  -- 'distribute' . 'distribute' = 'id'
  -- @
  distribute  :: Functor f => f (g a) -> g (f a)
  distribute  = collect id

  -- |
  -- @
  -- 'collect' f = 'distribute' . 'fmap' f
  -- 'fmap' f = 'runIdentity' . 'collect' ('Identity' . f)
  -- 'fmap' 'distribute' . 'collect' f = 'getCompose' . 'collect' ('Compose' . f)
  -- @

  collect     :: Functor f => (a -> g b) -> f a -> g (f b)
  collect f   = distribute . fmap f

  -- | The dual of 'Data.Traversable.sequence'
  --
  -- @
  -- 'distributeM' = 'fmap' 'unwrapMonad' . 'distribute' . 'WrapMonad'
  -- @
  distributeM :: Monad m => m (g a) -> g (m a)
  distributeM = fmap unwrapMonad . distribute . WrapMonad

  -- |
  -- @
  -- 'collectM' = 'distributeM' . 'liftM' f
  -- @
  collectM    :: Monad m => (a -> g b) -> m a -> g (m b)
  collectM f  = distributeM . liftM f

-- | The dual of 'Data.Traversable.traverse'
--
-- @
-- 'cotraverse' f = 'fmap' f . 'distribute'
-- @
cotraverse :: (Distributive g, Functor f) => (f a -> b) -> f (g a) -> g b
cotraverse f = fmap f . distribute

-- | The dual of 'Data.Traversable.mapM'
--
-- @
-- 'comapM' f = 'fmap' f . 'distributeM'
-- @
comapM :: (Distributive g, Monad m) => (m a -> b) -> m (g a) -> g b
comapM f = fmap f . distributeM

instance Distributive Identity where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall a b f . Functor f => (a -> Identity b) -> f a -> Identity (f b)
  distribute = Identity . fmap runIdentity

#if __GLASGOW_HASKELL__ >= 707 || defined(MIN_VERSION_tagged)
instance Distributive Proxy where
  collect _ _ = Proxy
  distribute _ = Proxy
#endif

#if defined(MIN_VERSION_tagged)
instance Distributive (Tagged t) where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall a b f . Functor f => (a -> Tagged t b) -> f a -> Tagged t (f b)
  distribute = Tagged . fmap unTagged
#endif

instance Distributive ((->)e) where
  distribute a e = fmap ($e) a
  collect f q e = fmap (flip f e) q

instance Distributive g => Distributive (ReaderT e g) where
  distribute a = ReaderT $ \e -> collect (flip runReaderT e) a
  collect f x = ReaderT $ \e -> collect (\a -> runReaderT (f a) e) x

instance Distributive g => Distributive (IdentityT g) where
  collect = coerce (collect :: (a -> g b) -> f a -> g (f b))
            :: forall a b f . Functor f => (a -> IdentityT g b) -> f a -> IdentityT g (f b)

instance (Distributive f, Distributive g) => Distributive (Compose f g) where
  distribute = Compose . fmap distribute . collect getCompose
  collect f = Compose . fmap distribute . collect (coerce f)

instance (Distributive f, Distributive g) => Distributive (Product f g) where
  -- It might be tempting to write a 'collect' implementation that
  -- composes the passed function with fstP and sndP. This could be bad,
  -- because it would lead to the passed function being evaluated twice
  -- for each element of the underlying functor.
  distribute wp = Pair (collect fstP wp) (collect sndP wp) where
    fstP (Pair a _) = a
    sndP (Pair _ b) = b


instance Distributive f => Distributive (Backwards f) where
  distribute = Backwards . collect forwards
  collect = coerce (collect :: (a -> f b) -> g a -> f (g b))
    :: forall g a b . Functor g
    => (a -> Backwards f b) -> g a -> Backwards f (g b)

instance Distributive f => Distributive (Reverse f) where
  distribute = Reverse . collect getReverse
  collect = coerce (collect :: (a -> f b) -> g a -> f (g b))
    :: forall g a b . Functor g
    => (a -> Reverse f b) -> g a -> Reverse f (g b)

instance Distributive Monoid.Dual where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Monoid.Dual b) -> f a -> Monoid.Dual (f b)
  distribute = Monoid.Dual . fmap Monoid.getDual

instance Distributive Monoid.Product where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Monoid.Product b) -> f a -> Monoid.Product (f b)
  distribute = Monoid.Product . fmap Monoid.getProduct

instance Distributive Monoid.Sum where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Monoid.Sum b) -> f a -> Monoid.Sum (f b)
  distribute = Monoid.Sum . fmap Monoid.getSum

#if __GLASGOW_HASKELL__ >= 800 || defined(MIN_VERSION_semigroups)
instance Distributive Semigroup.Min where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Semigroup.Min b) -> f a -> Semigroup.Min (f b)
  distribute = Semigroup.Min . fmap Semigroup.getMin

instance Distributive Semigroup.Max where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Semigroup.Max b) -> f a -> Semigroup.Max (f b)
  distribute = Semigroup.Max . fmap Semigroup.getMax

instance Distributive Semigroup.First where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Semigroup.First b) -> f a -> Semigroup.First (f b)
  distribute = Semigroup.First . fmap Semigroup.getFirst

instance Distributive Semigroup.Last where
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f
    => (a -> Semigroup.Last b) -> f a -> Semigroup.Last (f b)
  distribute = Semigroup.Last . fmap Semigroup.getLast
#endif

#if MIN_VERSION_base(4,4,0)
instance Distributive Complex where
  distribute wc = fmap realP wc :+ fmap imagP wc where
    -- Redefine realPart and imagPart to avoid incurring redundant RealFloat
    -- constraints on older versions of base
    realP (r :+ _) = r
    imagP (_ :+ i) = i
#endif

instance (Distributive m, Monad m) => Distributive (WrappedMonad m) where
  collect f = WrapMonad . collect (coerce f)

#if __GLASGOW_HASKELL__ >= 702
instance Distributive U1 where
  distribute _ = U1

instance (Distributive a, Distributive b) => Distributive (a :*: b) where
  -- It might be tempting to write a 'collect' implementation that
  -- composes the passed function with fstP and sndP. This could be bad,
  -- because it would lead to the passed function being evaluated twice
  -- for each element of the underlying functor.
  distribute f = collect fstP f :*: collect sndP f where
    fstP (l :*: _) = l
    sndP (_ :*: r) = r

instance (Distributive a, Distributive b) => Distributive (a :.: b) where
  distribute = Comp1 . fmap distribute . collect unComp1
  collect f = Comp1 . fmap distribute . collect (coerce f)

instance Distributive Par1 where
  distribute = Par1 . fmap unPar1
  collect = coerce (fmap :: (a -> b) -> f a -> f b)
    :: forall f a b . Functor f => (a -> Par1 b) -> f a -> Par1 (f b)

instance Distributive f => Distributive (Rec1 f) where
  distribute = Rec1 . collect unRec1
  collect = coerce (collect :: (a -> f b) -> g a -> f (g b))
    :: forall g a b . Functor g
    => (a -> Rec1 f b) -> g a -> Rec1 f (g b)

instance Distributive f => Distributive (M1 i c f) where
  distribute = M1 . collect unM1
  collect = coerce (collect :: (a -> f b) -> g a -> f (g b))
    :: forall g a b . Functor g
    => (a -> M1 i c f b) -> g a -> M1 i c f (g b)
#endif
