{-# LANGUAGE GHC2024, TypeAbstractions, AllowAmbiguousTypes, TypeFamilies #-}
module T26543 where

import Data.Kind (Type)

withObProd :: forall a r. ((Eq a) => r) -> r
withObProd = withObProd

type Ap :: k -> Type
data Ap a where
  Ap :: forall {k} (a :: k). Ap a

type family Ap2 :: Type -> Type
type instance Ap2 = Ap

f :: forall (a :: Type). Ap2 a -> Bool
f (Ap @x') = withObProd @x' $ True


-- ($) :: (p->q) -> p -> q
-- QL sees that: p = Eq a => Bool
--               q = Bool
