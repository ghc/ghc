{-# LANGUAGE TypeInType, TypeFamilies, KindSignatures, RankNTypes #-}

module T15740 where

import Data.Kind

type family F2 :: forall k. k -> Type
data SBool :: Bool -> Type
data Nat
data SNat :: Nat -> Type
type instance F2 = SBool
type instance F2 = SNat
