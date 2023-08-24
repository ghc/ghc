{-# LANGUAGE TypeAbstractions #-}

module T23514c where
import Data.Kind


type P1 :: forall k (a :: k) . k -> Type
data P1 :: k -> Type

type P2 :: forall k (a :: k) . k -> Type
data P2 @k :: k -> Type

type P3 :: forall k (a :: k) . k -> Type
data P3 @k @a :: k -> Type

type P4 :: forall k (a :: k) . k -> Type
data P4 :: forall k (a :: k) . k -> Type

type P5 :: forall k (a :: k) . k -> Type
data P5 :: forall a . k -> Type

type P6 :: forall k (a :: k) . k -> Type
data P6 @k :: forall a . k -> Type
