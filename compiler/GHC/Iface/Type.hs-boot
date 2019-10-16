module GHC.Iface.Type where

import Var (VarBndr, ArgFlag)

data IfaceAppArgs

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
data IfaceBndr
type IfaceForAllBndr  = VarBndr IfaceBndr ArgFlag
data IfacePred
