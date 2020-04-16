module GHC.Unit.State where
import GhcPrelude
import FastString
import {-# SOURCE #-} GHC.Unit.Types(UnitId,Unit,IndefUnitId)
data PackageState
data UnitInfoMap
data PackageDatabase unit
emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
displayUnitId :: PackageState -> UnitId -> Maybe String
improveUnit :: UnitInfoMap -> Unit -> Unit
unitInfoMap :: PackageState -> UnitInfoMap
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
