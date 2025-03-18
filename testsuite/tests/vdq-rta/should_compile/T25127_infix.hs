{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
module T25127_infix where

import Data.Kind
import GHC.TypeLits

data T a where
  (:@) :: forall a -> a -> T a

t1 = Int :@ 42
t2 = String :@ "hello"
t3 = (Int -> Bool) :@ even

type T1 = Nat :@ 42
type T2 = Symbol :@ "hello"
type T3 = (Type -> Constraint) :@ Num

f1 (a :@ x) = x :: a
f2 (Int :@ n) = n*2
f3 (((->) w Bool) :@ g) = not . g
