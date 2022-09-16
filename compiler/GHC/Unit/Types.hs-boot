{-# LANGUAGE KindSignatures #-}
module GHC.Unit.Types where

import GHC.Prelude ()
import Language.Haskell.Syntax.Module.Name (ModuleName)
import Data.Kind (Type)

data UnitId
data GenModule (unit :: Type)
data GenUnit (uid :: Type)

type Module      = GenModule  Unit
type Unit        = GenUnit    UnitId

moduleName :: GenModule a -> ModuleName
moduleUnit :: GenModule a -> a
