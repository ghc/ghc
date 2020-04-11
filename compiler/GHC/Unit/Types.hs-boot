module GHC.Unit.Types where

import GHC.Prelude ()
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Module.Name

data UnitId
data GenModule unit
data GenUnit uid
data Indefinite unit

type Module      = GenModule  Unit
type Unit        = GenUnit    UnitId
type IndefUnitId = Indefinite UnitId

moduleName :: GenModule a -> ModuleName
moduleUnit :: GenModule a -> a
pprModule :: Module -> SDoc
