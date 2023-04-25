{-# LANGUAGE GADTs, LinearTypes #-}
module T23298 where

import Data.Kind (Type)

type HList :: Type -> Type
data HList a where
  HCons :: HList x %1 -> HList (Maybe x)

eq :: HList a -> Bool
eq x = case x of
         HCons ms -> let go (HCons x) = go x
                     in go ms
