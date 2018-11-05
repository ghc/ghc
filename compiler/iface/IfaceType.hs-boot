-- Used only by ToIface.hs-boot

module IfaceType( IfaceType, IfaceTyCon, IfaceForAllBndr
                , IfaceCoercion, IfaceTyLit, IfaceAppArgs ) where

import Var (VarBndr, ArgFlag)

data IfaceAppArgs

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
data IfaceBndr
type IfaceForAllBndr  = VarBndr IfaceBndr ArgFlag
