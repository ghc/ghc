{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
module T25127_newtype where

import Data.Kind
import GHC.TypeLits

newtype T a where
  Typed :: forall a -> a -> T a

t1 = Typed Int 42
t2 = Typed String "hello"
t3 = Typed (Int -> Bool) even

type T1 = Typed Nat 42
type T2 = Typed Symbol "hello"
type T3 = Typed (Type -> Constraint) Num

f1 (Typed a x) = x :: a
f2 (Typed Int n) = n*2
f3 (Typed ((->) w Bool) g) = not . g
