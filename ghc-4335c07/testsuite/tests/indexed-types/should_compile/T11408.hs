{-# LANGUAGE TypeFamilies #-}
module T11408 where

type family UL a
type family UR a
type family MT a b

mkMerge :: a -> UL a -> UR a -> Int
mkMerge = undefined

merger :: a -> b -> MT a b
merger = undefined

{-
merge ::
 forall a b. (UL (MT a b) ~ a, UR (MT a b) ~ b) => a -> b -> Int
or
 forall t. (MT (UL t) (UR t) ~ t) => UL t -> UR t -> Int

These types are equivalent, and in fact neither is ambiguous,
but the solver has to work quite hard to prove that.
-}
merge x y = mkMerge (merger x y) x y
