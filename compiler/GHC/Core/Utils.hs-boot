module GHC.Core.Utils where

import GHC.Core.Multiplicity
import GHC.Core.Type

mkFunctionType :: Mult -> Type -> Type -> Type
