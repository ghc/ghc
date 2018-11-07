{-# LANGUAGE KindSignatures, RankNTypes, TypeFamilies, MultiParamTypeClasses, FlexibleInstances,UndecidableInstances #-}

module T13585a where

import Data.Monoid (First(..))
import Data.Functor.Identity
import Data.Kind (Type)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  {-# INLINE lmap #-}

  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
  {-# INLINE rmap #-}


data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}



withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity undefined bt)
{-# INLINE withIso #-}

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a
type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

class    (Rewrapped s t, Rewrapped t s) => Rewrapping s t
instance (Rewrapped s t, Rewrapped t s) => Rewrapping s t


instance (t ~ First b) => Rewrapped (First a) t
instance Wrapped (First a) where
    type Unwrapped (First a) = Maybe a
    _Wrapped' = iso getFirst First
    {-# INLINE _Wrapped' #-}

class Wrapped s => Rewrapped (s :: Type) (t :: Type)

class Wrapped s where
    type Unwrapped s :: Type
    _Wrapped' :: Iso' s (Unwrapped s)

_Wrapping :: Rewrapping s t => (Unwrapped s -> s) -> Iso s t (Unwrapped s) (Unwrapped t)
_Wrapping _ = _Wrapped
{-# INLINE _Wrapping #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

_Wrapped :: Rewrapping s t => Iso s t (Unwrapped s) (Unwrapped t)
_Wrapped = withIso _Wrapped' $ \ sa _ -> withIso _Wrapped' $ \ _ bt -> iso sa bt
{-# INLINE _Wrapped #-}

au :: Functor f => AnIso s t a b -> ((b -> t) -> f s) -> f a
au k = withIso k $ \ sa bt f -> fmap sa (f bt)
{-# INLINE au #-}

ala :: (Functor f, Rewrapping s t) => (Unwrapped s -> s) -> ((Unwrapped t -> t) -> f s) -> f (Unwrapped s)
ala = au . _Wrapping
{-# INLINE ala #-}
