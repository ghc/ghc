{-# LANGUAGE GADTs #-}
module T23298 where

import Data.Kind (Type)

type HList :: Type -> Type
data HList a where
  HCons :: HList x -> HList (Maybe x)

eq :: HList a -> Bool
eq x = case x of
         HCons ms -> True

go (HCons x) = go x

{- go :: HList alpha -> beta

Under HCons
  [G] alpha ~ Maybe x
  [W] HList x ~ HList alpha
==>
  [W] x ~ alpha
==>
  [W] x ~ Maybe x
-}
