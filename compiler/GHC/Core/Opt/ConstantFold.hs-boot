module GHC.Core.Opt.ConstantFold where

import GHC.Prelude ( Maybe )
import GHC.Core.Rules( CoreRule )
import GHC.Builtin.PrimOps ( PrimOp )
import GHC.Types.Name ( Name )

primOpRules ::  Name -> PrimOp -> Maybe CoreRule
