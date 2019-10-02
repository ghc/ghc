module CoreUnfold (
        mkUnfolding, mkInlineUnfolding
    ) where

import GhcPrelude
import CoreSyn
import DynFlags

mkInlineUnfolding :: CoreExpr -> Unfolding

mkUnfolding :: DynFlags
            -> UnfoldingSource
            -> Bool
            -> Bool
            -> CoreExpr
            -> Unfolding
