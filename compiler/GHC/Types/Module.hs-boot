module GHC.Types.Module where

import GhcPrelude

data Module
data ModuleName
data UnitId
data InstalledUnitId
data ComponentId

moduleName :: Module -> ModuleName
moduleUnitId :: Module -> UnitId
unitIdString :: UnitId -> String
