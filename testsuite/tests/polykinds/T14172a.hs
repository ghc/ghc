{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T14172a where

import Data.Kind
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Identity

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  (#.) :: Coercible c b => (b -> c) -> p a b -> p a c

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  (#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
  {-# INLINE (#.) #-}

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  (#.) _ = coerce
  {-# INLINE ( #. ) #-}

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)
{-# INLINE withIso #-}

class Wrapped s where
  type Unwrapped s :: Type
  _Wrapped' :: Iso' s (Unwrapped s)

class Wrapped s => Rewrapped (s :: Type) (t :: Type)

class    (Rewrapped s t, Rewrapped t s) => Rewrapping s t
instance (Rewrapped s t, Rewrapped t s) => Rewrapping s t

instance (t ~ Compose f' g' a') => Rewrapped (Compose f g a) t
instance Wrapped (Compose f g a) where
  type Unwrapped (Compose f g a) = f (g a)
  _Wrapped' = iso getCompose Compose

_Wrapping :: Rewrapping s t => (Unwrapped s -> s) -> Iso s t (Unwrapped s) (Unwrapped t)
_Wrapping _ = _Wrapped
{-# INLINE _Wrapping #-}

_Wrapped :: Rewrapping s t => Iso s t (Unwrapped s) (Unwrapped t)
_Wrapped = withIso _Wrapped' $ \ sa _ -> withIso _Wrapped' $ \ _ bt -> iso sa bt
{-# INLINE _Wrapped #-}
