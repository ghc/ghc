module GHC.Iface.Type
   ( IfaceType, IfaceTyCon, IfaceBndr
   , IfaceCoercion, IfaceTyLit, IfaceAppArgs
   )
where

-- Empty import to influence the compilation ordering.
-- See Note [Depend on GHC.Num.Integer] in GHC.Base
import GHC.Base ()

data IfaceType' p
data IfaceAppArgs' p
data IfaceBndr' p
data IfaceCoercion' p

data IfaceTyCon
data IfaceTyLit

data TtgIface

type IfaceType = IfaceType' TtgIface
type IfaceAppArgs = IfaceAppArgs' TtgIface
type IfaceBndr = IfaceBndr' TtgIface
type IfaceCoercion = IfaceCoercion' TtgIface
