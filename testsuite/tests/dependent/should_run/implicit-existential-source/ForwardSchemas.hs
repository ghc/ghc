{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RequiredTypeArguments #-}
module ForwardSchemas (HiddenBar, Restricted, barValue, barIsEmpty, restrict) where

import qualified GHC.Exists as Exists
import Vector

type HiddenBar a = exists (n :: Nat) | Vec n a
type Restricted n a = exists (m :: Nat). (Vec n a, Vec m a)

barValue :: HiddenBar a
barValue = Exists.Pack (type 'Z) Nil

barIsEmpty :: HiddenBar a -> Bool
barIsEmpty p = case p of
    Exists.Pack _ xs -> case xs of
        Nil -> True
        Cons _ _ -> False

restrict :: Vec n a -> Vec m a -> Restricted n a
restrict ls rs = (ls, rs)
