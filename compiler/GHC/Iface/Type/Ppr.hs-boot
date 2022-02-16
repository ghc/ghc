module GHC.Iface.Type.Ppr
   ( IfaceTypePpr, IfaceBndrPpr
   , IfaceCoercionPpr, IfaceAppArgsPpr
   )
where

-- Empty import to influence the compilation ordering.
-- See Note [Depend on GHC.Num.Integer] in GHC.Base
import GHC.Base ()

data IfaceType' p
data IfaceAppArgs' p
data IfaceBndr' p
data IfaceCoercion' p

data TtgIfacePpr

type IfaceTypePpr = IfaceType' TtgIfacePpr
type IfaceAppArgsPpr = IfaceAppArgs' TtgIfacePpr
type IfaceBndrPpr = IfaceBndr' TtgIfacePpr
type IfaceCoercionPpr = IfaceCoercion' TtgIfacePpr
