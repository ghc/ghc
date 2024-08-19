module GHC.Core.FamInstEnv where

import Language.Haskell.Syntax.Basic
import GHC.Core.Reduction
import GHC.Core.TyCo.Rep

normaliseType :: FamInstEnvs -> Role -> Type -> Reduction

type FamInstEnvs = (FamInstEnv, FamInstEnv)

data FamInstEnv

data FamInst
