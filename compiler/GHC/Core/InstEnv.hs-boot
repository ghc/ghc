module GHC.Core.InstEnv where

import GHC.Prelude (Bool, Maybe, Either)
import GHC.Core.TyCo.Rep
import GHC.Core.Class
import GHC.Unit.Module.Env
import GHC.Core.RoughMap

data ClsInst

data InstEnvs

type DFunInstType = Maybe Type

type InstMatch = (ClsInst, [DFunInstType])

type ClsInstLookupResult = ([InstMatch], PotentialUnifiers, [InstMatch])

type VisibleOrphanModules = ModuleSet

lookupUniqueInstEnv :: InstEnvs -> Class -> [Type] -> Either LookupInstanceErrReason (ClsInst, [Type])

data LookupInstanceErrReason

data PotentialUnifiers

lookupInstEnv :: Bool -> InstEnvs -> Class -> [Type] -> ClsInstLookupResult
