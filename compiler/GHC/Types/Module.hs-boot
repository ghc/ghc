module GHC.Types.Module where

import GHC.Prelude

data ModuleName
data UnitId
data GenModule a
data GenUnit a
data Indefinite unit

type Unit        = GenUnit UnitId
type IndefUnitId = Indefinite UnitId
type Module      = GenModule Unit

moduleName :: GenModule a -> ModuleName
moduleUnit :: GenModule a -> a
unitString :: Unit -> String
