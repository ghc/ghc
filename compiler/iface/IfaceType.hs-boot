-- Used only by ToIface.hs-boot

module IfaceType( IfaceType, IfaceTyCon, IfaceForAllBndr
                , IfaceCoercion, IfaceTyLit, IfaceTcArgs ) where

import Var (TyVarBndr, ArgFlag)
import FastString (FastString)

data IfaceTcArgs
type IfLclName = FastString
type IfaceKind = IfaceType

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
type IfaceTvBndr      = (IfLclName, IfaceKind)
type IfaceForAllBndr  = TyVarBndr IfaceTvBndr ArgFlag
