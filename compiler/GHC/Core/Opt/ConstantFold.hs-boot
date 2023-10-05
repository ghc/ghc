module GHC.Core.Opt.ConstantFold where

import GHC.Prelude
import GHC.Core
import GHC.Builtin.PrimOps
import GHC.Types.Name

primOpRules ::  Name -> PrimOp -> Maybe CoreRule
