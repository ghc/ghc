-- This test is from the Description of #26543

{-# LANGUAGE GHC2024, TypeAbstractions, AllowAmbiguousTypes, NoImplicitPrelude,
                      TypeFamilies, UndecidableInstances #-}
module T26543_orig where

import Data.Kind
import Control.Applicative (Applicative(..))
import Prelude (type (~), ($))

type CAT k = k -> k -> Type

type family (~>) :: CAT k
type family Ob (a :: k) :: Constraint
type family UN (w :: j -> k) (wa :: k) :: j

class HasBinaryProducts k where
  type (a :: k) && (b :: k) :: k
  withObProd :: (Ob (a :: k), Ob b) => ((Ob (a && b)) => r) -> r
  (&&&) :: ((a :: k) ~> x) -> (a ~> y) -> (a ~> (x && y))

data AP (f :: Type -> Type) k = A k
type instance UN A (A k) = k

type Ap :: CAT (AP f k)
data Ap a b where
  Ap :: forall {k} a b f. (Ob a, Ob b) => f (a ~> b) -> Ap (A a :: AP f k) (A b)

type instance (~>) = Ap
type instance Ob a = (a ~ A (UN A a), Ob (UN A a))

instance (Applicative f, HasBinaryProducts k) => HasBinaryProducts (AP f k) where
  type a && b = A (UN A a && UN A b)
  withObProd @(A a) @(A b) r = withObProd @k @a @b r
  -- (&&&) :: Ap (a :: AP f k) x -> Ap a y -> Ap a (x && y)
  Ap @_ @x f &&& Ap @_ @y g = withObProd @k @x @y $ Ap (liftA2 (&&&) f g)
