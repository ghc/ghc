module GHC.Types.Module where

import GhcPrelude

data GenModule a
data ModuleName
data Unit
data UnitId
data IndefUnitId
type Module = GenModule Unit

moduleName :: GenModule a -> ModuleName
moduleUnit :: GenModule a -> a
unitString :: Unit -> String
