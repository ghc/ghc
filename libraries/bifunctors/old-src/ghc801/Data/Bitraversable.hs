{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bitraversable
  ( Bitraversable(..)
  , bisequenceA
  , bisequence
  , bimapM
  , bifor
  , biforM
  , bimapAccumL
  , bimapAccumR
  , bimapDefault
  , bifoldMapDefault
  ) where

import Control.Applicative
import Control.Monad.Trans.Instances ()
import Data.Bifunctor
import Data.Bifoldable
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Orphans ()

#if MIN_VERSION_base(4,7,0)
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

import Data.Semigroup (Arg(..))

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (K1(..))
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
import Data.Typeable
#endif

-- | 'Bitraversable' identifies bifunctorial data structures whose elements can
-- be traversed in order, performing 'Applicative' or 'Monad' actions at each
-- element, and collecting a result structure with the same shape.
--
-- As opposed to 'Traversable' data structures, which have one variety of
-- element on which an action can be performed, 'Bitraversable' data structures
-- have two such varieties of elements.
--
-- A definition of 'bitraverse' must satisfy the following laws:
--
-- [/naturality/]
--   @'bitraverse' (t . f) (t . g) ≡ t . 'bitraverse' f g@
--   for every applicative transformation @t@
--
-- [/identity/]
--   @'bitraverse' 'Identity' 'Identity' ≡ 'Identity'@
--
-- [/composition/]
--   @'Compose' . 'fmap' ('bitraverse' g1 g2) . 'bitraverse' f1 f2
--     ≡ 'bitraverse' ('Compose' . 'fmap' g1 . f1) ('Compose' . 'fmap' g2 . f2)@
--
-- where an /applicative transformation/ is a function
--
-- @t :: ('Applicative' f, 'Applicative' g) => f a -> g a@
--
-- preserving the 'Applicative' operations:
--
-- @
-- t ('pure' x) = 'pure' x
-- t (f '<*>' x) = t f '<*>' t x
-- @
--
-- and the identity functor 'Identity' and composition functors 'Compose' are
-- defined as
--
-- > newtype Identity a = Identity { runIdentity :: a }
-- >
-- > instance Functor Identity where
-- >   fmap f (Identity x) = Identity (f x)
-- >
-- > instance Applicative Identity where
-- >   pure = Identity
-- >   Identity f <*> Identity x = Identity (f x)
-- >
-- > newtype Compose f g a = Compose (f (g a))
-- >
-- > instance (Functor f, Functor g) => Functor (Compose f g) where
-- >   fmap f (Compose x) = Compose (fmap (fmap f) x)
-- >
-- > instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- >   pure = Compose . pure . pure
-- >   Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
--
-- Some simple examples are 'Either' and '(,)':
--
-- > instance Bitraversable Either where
-- >   bitraverse f _ (Left x) = Left <$> f x
-- >   bitraverse _ g (Right y) = Right <$> g y
-- >
-- > instance Bitraversable (,) where
-- >   bitraverse f g (x, y) = (,) <$> f x <*> g y
--
-- 'Bitraversable' relates to its superclasses in the following ways:
--
-- @
-- 'bimap' f g ≡ 'runIdentity' . 'bitraverse' ('Identity' . f) ('Identity' . g)
-- 'bifoldMap' f g = 'getConst' . 'bitraverse' ('Const' . f) ('Const' . g)
-- @
--
-- These are available as 'bimapDefault' and 'bifoldMapDefault' respectively.
class (Bifunctor t, Bifoldable t) => Bitraversable t where
  -- | Evaluates the relevant functions at each element in the structure, running
  -- the action, and builds a new structure with the same shape, using the
  -- elements produced from sequencing the actions.
  --
  -- @'bitraverse' f g ≡ 'bisequenceA' . 'bimap' f g@
  --
  -- For a version that ignores the results, see 'bitraverse_'.
  bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bitraverse f g = bisequenceA . bimap f g
  {-# INLINE bitraverse #-}


-- | Sequences all the actions in a structure, building a new structure with the
-- same shape using the results of the actions. For a version that ignores the
-- results, see 'bisequenceA_'.
--
-- @'bisequenceA' ≡ 'bitraverse' 'id' 'id'@
bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = bitraverse id id
{-# INLINE bisequenceA #-}

-- | As 'bitraverse', but uses evidence that @m@ is a 'Monad' rather than an
-- 'Applicative'. For a version that ignores the results, see 'bimapM_'.
--
-- @
-- 'bimapM' f g ≡ 'bisequence' . 'bimap' f g
-- 'bimapM' f g ≡ 'unwrapMonad' . 'bitraverse' ('WrapMonad' . f) ('WrapMonad' . g)
-- @
bimapM :: (Bitraversable t, Monad m) => (a -> m c) -> (b -> m d) -> t a b -> m (t c d)
bimapM f g = unwrapMonad . bitraverse (WrapMonad . f) (WrapMonad . g)
{-# INLINE bimapM #-}

-- | As 'bisequenceA', but uses evidence that @m@ is a 'Monad' rather than an
-- 'Applicative'. For a version that ignores the results, see 'bisequence_'.
--
-- @
-- 'bisequence' ≡ 'bimapM' 'id' 'id'
-- 'bisequence' ≡ 'unwrapMonad' . 'bisequenceA' . 'bimap' 'WrapMonad' 'WrapMonad'
-- @
bisequence :: (Bitraversable t, Monad m) => t (m a) (m b) -> m (t a b)
bisequence = bimapM id id
{-# INLINE bisequence #-}

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Bitraversable
#endif

instance Bitraversable Arg where
  bitraverse f g (Arg a b) = Arg <$> f a <*> g b

instance Bitraversable (,) where
  bitraverse f g ~(a, b) = (,) <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,) x) where
  bitraverse f g ~(x, a, b) = (,,) x <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,,) x y) where
  bitraverse f g ~(x, y, a, b) = (,,,) x y <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,,,) x y z) where
  bitraverse f g ~(x, y, z, a, b) = (,,,,) x y z <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,,,,) x y z w) where
  bitraverse f g ~(x, y, z, w, a, b) = (,,,,,) x y z w <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,,,,,) x y z w v) where
  bitraverse f g ~(x, y, z, w, v, a, b) = (,,,,,,) x y z w v <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b
  {-# INLINE bitraverse #-}

instance Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a
  {-# INLINE bitraverse #-}

instance Bitraversable Constant where
  bitraverse f _ (Constant a) = Constant <$> f a
  {-# INLINE bitraverse #-}

#if __GLASGOW_HASKELL__ >= 702
instance Bitraversable (K1 i) where
  bitraverse f _ (K1 c) = K1 <$> f c
  {-# INLINE bitraverse #-}
#endif

#ifdef MIN_VERSION_tagged
instance Bitraversable Tagged where
  bitraverse _ g (Tagged b) = Tagged <$> g b
  {-# INLINE bitraverse #-}
#endif

-- | 'bifor' is 'bitraverse' with the structure as the first argument. For a
-- version that ignores the results, see 'bifor_'.
bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t
{-# INLINE bifor #-}

-- | 'biforM' is 'bimapM' with the structure as the first argument. For a
-- version that ignores the results, see 'biforM_'.
biforM :: (Bitraversable t, Monad m) =>  t a b -> (a -> m c) -> (b -> m d) -> m (t c d)
biforM t f g = bimapM f g t
{-# INLINE biforM #-}

-- | left-to-right state transformer
newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
  fmap f (StateL k) = StateL $ \ s ->
    let (s', v) = k s in (s', f v)
  {-# INLINE fmap #-}

instance Applicative (StateL s) where
  pure x = StateL (\ s -> (s, x))
  {-# INLINE pure #-}
  StateL kf <*> StateL kv = StateL $ \ s ->
    let (s', f) = kf s
        (s'', v) = kv s'
    in (s'', f v)
  {-# INLINE (<*>) #-}

-- | The 'bimapAccumL' function behaves like a combination of 'bimap' and
-- 'bifoldl'; it traverses a structure from left to right, threading a state
-- of type @a@ and using the given actions to compute new elements for the
-- structure.
bimapAccumL :: Bitraversable t => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumL f g s t = runStateL (bitraverse (StateL . flip f) (StateL . flip g) t) s
{-# INLINE bimapAccumL #-}

-- | right-to-left state transformer
newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
  fmap f (StateR k) = StateR $ \ s ->
    let (s', v) = k s in (s', f v)
  {-# INLINE fmap #-}

instance Applicative (StateR s) where
  pure x = StateR (\ s -> (s, x))
  {-# INLINE pure #-}
  StateR kf <*> StateR kv = StateR $ \ s ->
    let (s', v) = kv s
        (s'', f) = kf s'
    in (s'', f v)
  {-# INLINE (<*>) #-}

-- | The 'bimapAccumR' function behaves like a combination of 'bimap' and
-- 'bifoldl'; it traverses a structure from right to left, threading a state
-- of type @a@ and using the given actions to compute new elements for the
-- structure.
bimapAccumR :: Bitraversable t => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumR f g s t = runStateR (bitraverse (StateR . flip f) (StateR . flip g) t) s
{-# INLINE bimapAccumR #-}

-- | A default definition of 'bimap' in terms of the 'Bitraversable' operations.
--
-- @'bimapDefault' f g ≡
--     'runIdentity' . 'bitraverse' ('Identity' . f) ('Identity' . g)@
bimapDefault :: forall t a b c d . Bitraversable t
             => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault = coerce
  (bitraverse :: (a -> Identity b)
              -> (c -> Identity d) -> t a c -> Identity (t b d))
{-# INLINE bimapDefault #-}

-- | A default definition of 'bifoldMap' in terms of the 'Bitraversable' operations.
--
-- @'bifoldMapDefault' f g ≡
--    'getConst' . 'bitraverse' ('Const' . f) ('Const' . g)@
bifoldMapDefault :: forall t m a b . (Bitraversable t, Monoid m)
                 => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault = coerce
  (bitraverse :: (a -> Const m ())
              -> (b -> Const m ()) -> t a b -> Const m (t () ()))
{-# INLINE bifoldMapDefault #-}

#if !(MIN_VERSION_base(4,7,0))
coerce :: a -> b
coerce = unsafeCoerce
#endif
