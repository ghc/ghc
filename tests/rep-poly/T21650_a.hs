{-# LANGUAGE DataKinds, DatatypeContexts, MagicHash, UnliftedNewtypes, TypeFamilies #-}

module T21650_a where

import Data.Kind
import GHC.Exts

type R :: Type -> RuntimeRep
type family R a where
  R Float  = FloatRep
  R Double = DoubleRep

type F :: forall (a :: Type) -> TYPE (R a)
type family F a where
  F Float  = Float#
  F Double = Double#

type C :: Type -> Constraint
class C a where {}

type D :: Type -> Constraint
class D a where
  data family N a :: TYPE (R a)

--type N :: forall (a :: Type) -> TYPE (R a)
instance D a where
  newtype instance C a => N a = MkN (F a)

foo1 :: C Float => F Float -> N Float
foo1 = MkN

foo2 :: C Double => () -> F Double -> N Double
foo2 _ = MkN
