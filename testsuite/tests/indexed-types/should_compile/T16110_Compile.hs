{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T16110_Compile where

class C a where
  type T1 a b
  type forall a b. T1 a b = Either a b

  type T2 a b
  type forall x y. T2 x y = Either x y

  type T3 a b
  type forall. T3 _ _ = Int
