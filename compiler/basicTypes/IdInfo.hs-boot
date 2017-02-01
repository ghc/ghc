module IdInfo where
import BasicTypes
import Outputable
data IdInfo
data IdDetails

vanillaIdInfo :: IdInfo
coVarDetails :: IdDetails
isCoVarDetails :: IdDetails -> Bool
isJoinIdDetails_maybe :: IdDetails -> Maybe JoinArity
pprIdDetails :: IdDetails -> SDoc

