module T15079_fail_a where

import Data.Kind (Type)

newtype Foo (f :: forall (a :: Type). a -> Type) = MkFoo (f Int)
data InferredProxy a = MkInferredProxy

foo :: Foo InferredProxy
foo = MkFoo MkInferredProxy