module GHC.Types.Id.Info where
import GHC.Prelude
import GHC.Utils.Outputable
data IdInfo
data IdDetails

vanillaIdInfo :: IdInfo
coVarDetails :: IdDetails
isCoVarDetails :: IdDetails -> Bool
pprIdDetails :: IdDetails -> SDoc

