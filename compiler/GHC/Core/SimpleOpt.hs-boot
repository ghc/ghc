module GHC.Core.SimpleOpt where

import GHC.Core
import {-# SOURCE #-} GHC.Core.Unfold

data SimpleOptOpts

so_uf_opts :: SimpleOptOpts -> UnfoldingOpts

simpleOptExpr :: SimpleOptOpts -> CoreExpr -> CoreExpr
