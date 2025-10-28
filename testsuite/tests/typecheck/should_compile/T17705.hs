{-# LANGUAGE TypeAbstractions #-}

module T17705 where

import Data.Kind

type F :: forall a (b :: a -> Type). a -> Type
type F @_ @f x = f x

type G :: forall a. forall (b :: a -> Type) -> a -> Type
type G @_ f x = f x

type H :: forall a (f :: a -> a -> Type). a -> a -> Type
type H @_ @f a b = f a b

