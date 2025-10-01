{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module T26400b where

import Data.Kind
import GHC.TypeLits ( TypeError, ErrorMessage(..) )

type F :: Type -> Type -> Type
type family F a b where
  F Float _ = Bool
  F _     a = a

type C :: Type -> Type -> Constraint
class C a b
instance C () b

type Boom :: Type -> Type
type family Boom a where
  Boom () = TypeError (Text "boom")

type TF :: Type -> ( Type -> Type -> Constraint )
type family TF a where
  TF () = C

type G :: Type -> Type -> Type
data G a b where
  G1 :: a -> F b (Boom a) -> G a b
  G2 :: C a (Boom a) => a -> G a b
  G3 :: (TF b) a (Boom a) => a -> G a b
  G4 :: (b ~ Boom a) => G a b

g :: G () b -> Int
g (G1 {}) = 1 -- not redundant: F b (TypeError ...) can be solved if F reduces
g (G2 {}) = 2 -- not redundant: C () (TypeError ...) is not insoluble
g (G3 {}) = 3 -- not redundant: TF b () (TypeError ...) could reduce to C () (TypeError ...)
g (G4 {}) = 4 -- not redundant: b ~ TypeError ... could be solved depending on instantiation of b
