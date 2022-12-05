{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.TypeNats (Nat, type (<=))

f :: (1 <= w)
  => IO (SymBV' sym w)
  -> IO (SymBV sym w)
f = coerce

----

data BaseType = BaseBVType Nat
type family SymExpr (sym :: Type) :: BaseType -> Type
type SymBV sym n = SymExpr sym (BaseBVType n)
newtype SymBV' sym w = MkSymBV' (SymBV sym w)
