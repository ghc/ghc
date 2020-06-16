module GHC.Core.SimpleOpt where

import GHC.Core
import {-# SOURCE #-} GHC.Core.Unfold
import GHC.Utils.Misc (HasDebugCallStack)

data SimpleOptOpts

so_uf_opts :: SimpleOptOpts -> UnfoldingOpts

simpleOptExpr :: HasDebugCallStack => SimpleOptOpts -> CoreExpr -> CoreExpr
