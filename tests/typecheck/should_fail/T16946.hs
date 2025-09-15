{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, TypeFamilies, PolyKinds, FunctionalDependencies #-}
module T16946 where

import Data.Kind

class CatMonad (c :: k -> k -> Type) (m :: forall (x :: k) (y :: k). c x y -> Type -> Type) | c -> m where
  type Id c :: c x x

  xpure :: a -> m (Id c) a

boom :: forall k (c :: k -> k -> Type) (m :: forall (x :: k) (y :: k). c x y -> Type -> Type) a. CatMonad c m => a -> m (Id c) a
boom = xpure
