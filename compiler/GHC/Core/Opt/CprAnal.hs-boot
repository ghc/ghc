module GHC.Core.Opt.CprAnal ( exprTerminates ) where

import GHC.Prelude
import GHC.Core

exprTerminates :: CoreExpr -> Bool
