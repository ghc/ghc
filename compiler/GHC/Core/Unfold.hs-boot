module GHC.Core.Unfold (
        mkUnfolding
    ) where

import GhcPrelude
import GHC.Core
import DynFlags

mkUnfolding :: DynFlags
            -> UnfoldingSource
            -> Bool
            -> Bool
            -> CoreExpr
            -> Unfolding
