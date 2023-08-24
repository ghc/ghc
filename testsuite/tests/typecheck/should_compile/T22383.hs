{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}

module T22383 where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))

-- | @IsType k@ witnesses that @k ~ Type at .
data IsType k where
  IsType :: IsType Type

---------------------
--   Using a GADT
---------------------

data FromType where
  FromType :: forall (f :: Type -> Type). FromType

-- | @FunRep (f b)@ witnesses that @b :: Type at .
data FunRep a where
  AppK ::
    forall (k :: Type) (f :: k -> Type) (b :: k).
    IsType k ->
    Proxy f ->
    FunRep (f b)

-- Could not deduce: k ~ *
isMaybeF :: forall (a :: Type). FunRep a -> FromType
isMaybeF = \case
  AppK @_ @f @_ t (Proxy @g :: Proxy h) ->
    case t of
      IsType -> FromType @f

-- Could not deduce: k ~ *
isMaybeG :: forall (a :: Type). FunRep a -> FromType
isMaybeG = \case
  AppK @_ @f @_ t (Proxy @g :: Proxy h) ->
    case t of
      IsType -> FromType @g

-- Works fine
isMaybeH :: forall (a :: Type). FunRep a -> FromType
isMaybeH = \case
  AppK @_ @f @_ t (Proxy @g :: Proxy h) ->
    case t of
      IsType -> FromType @h


---------------------
--   Not using a GADT
---------------------

data FunRep2 a where
  AppK2 ::
    forall k (b :: k).
    IsType k ->
    Proxy k ->
    FunRep2 b

data FromType2 where
  FromType2 :: forall (b :: Type). FromType2

-- Could not deduce: k ~ *
isMaybeF2 :: forall k (a :: k). FunRep2 a -> FromType2
isMaybeF2 = \case
  AppK2 @_ @f t (Proxy @g :: Proxy h) ->
    case t of
      IsType -> FromType2 @f

-- Works fine
isMaybeG2 :: forall k (a :: k). FunRep2 a -> FromType2
isMaybeG2 = \case
  AppK2 @_ @f t (Proxy @g :: Proxy h) ->
    case t of
      IsType -> FromType2 @g

-- Works fine
isMaybeH2 :: forall k (a :: k). FunRep2 a -> FromType2
isMaybeH2 = \case
  AppK2 @_ @f t (Proxy @g :: Proxy h) ->
    case t of
      IsType -> FromType2 @h
