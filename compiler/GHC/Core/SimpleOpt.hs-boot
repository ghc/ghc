module GHC.Core.SimpleOpt where

import GHC.Core
import {-# SOURCE #-} GHC.Core.Unfold
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Types.Basic (StateHackFlag)

data SimpleOpts

so_uf_opts :: SimpleOpts -> UnfoldingOpts
so_st_hack :: SimpleOpts -> StateHackFlag

simpleOptExpr :: HasDebugCallStack => SimpleOpts -> CoreExpr -> CoreExpr
