module HscTypes where

import Module
import TyCoRep
import NameEnv

data HscEnv

type HomePackageTable = DModuleNameEnv HomeModInfo
data HomeModInfo

type PackageTypeEnv = TypeEnv
type TypeEnv = NameEnv TyThing
