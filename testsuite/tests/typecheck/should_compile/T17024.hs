{-# language TypeFamilies, FunctionalDependencies, GADTs, DataKinds, TypeOperators, ScopedTypeVariables, FlexibleInstances , UndecidableInstances, PartialTypeSignatures #-}

module T17024 where

infixr 6 :::

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

class AppHList ts o f | ts f -> o, ts o -> f where
   appHList :: f -> HList ts -> o
instance AppHList '[] o o where
   appHList x HNil = x
instance AppHList ts o f => AppHList (t : ts) o (t -> f) where
   appHList f (x ::: xs) = appHList (f x) xs

foo :: (a -> b -> c) -> HList '[a, b] -> _
foo = appHList
