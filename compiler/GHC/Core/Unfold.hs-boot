module GHC.Core.Unfold (
        mkUnfolding, mkInlineUnfolding
    ) where

import GHC.Prelude
import GHC.Core
import GHC.Driver.Session

mkInlineUnfolding :: CoreExpr -> Unfolding

mkUnfolding :: DynFlags
            -> UnfoldingSource
            -> Bool
            -> Bool
            -> CoreExpr
            -> Unfolding
