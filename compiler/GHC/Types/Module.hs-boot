module GHC.Types.Module where

import GhcPrelude

data GenModule a
data ModuleName
data UnitId
data InstalledUnitId
data IndefUnitId
type Module = GenModule UnitId

moduleName :: GenModule a -> ModuleName
moduleUnit :: GenModule a -> a
unitIdString :: UnitId -> String
