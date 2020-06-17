module GHC.Core.SimpleOpt where

import GHC.Core
import {-# SOURCE #-} GHC.Core.Unfold
import GHC.Utils.Misc (HasDebugCallStack)

data SimpleOpts

so_uf_opts :: SimpleOpts -> UnfoldingOpts

simpleOptExpr :: HasDebugCallStack => SimpleOpts -> CoreExpr -> CoreExpr
