module CoreUnfold (
        mkUnfolding
    ) where

import GhcPrelude
import CoreSyn
import DynFlags

mkUnfolding :: DynFlags
            -> UnfoldingSource
            -> Bool
            -> Bool
            -> CoreExpr
            -> Unfolding
