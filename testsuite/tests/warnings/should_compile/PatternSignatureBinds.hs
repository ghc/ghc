module PatternSignatureBinds where

import Data.Kind (Type)

f :: forall b . b -> b
f (x :: a) =
  undefined

data Imp (a :: c) :: forall b . b -> c -> Type
