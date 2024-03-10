module GHC.Iface.Type
   ( IfaceType, IfaceTyCon, IfaceBndr
   , IfaceCoercion, IfaceTyLit, IfaceAppArgs
   , ShowSub
   )
where

-- Empty import to influence the compilation ordering.
-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Base ()

data IfaceAppArgs

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
data IfaceBndr
data ShowSub
