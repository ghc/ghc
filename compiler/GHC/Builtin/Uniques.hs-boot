module GHC.Builtin.Uniques where

import GHC.Prelude
import GHC.Types.Unique
import GHC.Types.Name
import GHC.Types.Basic

-- Needed by GHC.Builtin.Types
knownUniqueName :: Unique -> Maybe Name

mkSumTyConUnique :: Arity -> Unique
mkSumDataConUnique :: ConTagZ -> Arity -> Unique

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleDataConUnique :: Arity -> Unique

mkTupleTyConUnique :: Boxity -> Arity -> Unique
mkTupleDataConUnique :: Boxity -> Arity -> Unique
