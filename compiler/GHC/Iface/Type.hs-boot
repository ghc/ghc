module GHC.Iface.Type
   ( IfaceType, IfaceTyCon, IfaceForAllBndr
   , IfaceCoercion, IfaceTyLit, IfaceAppArgs
   )
where

import GHC.Types.Var (VarBndr, ArgFlag)

data IfaceAppArgs

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
data IfaceBndr
type IfaceForAllBndr  = VarBndr IfaceBndr ArgFlag
