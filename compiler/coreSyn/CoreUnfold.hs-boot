module CoreUnfold (
        mkUnfolding, mkInlineUnfolding
    ) where

import GhcPrelude
import CoreSyn
import GHC.Driver.Session

mkInlineUnfolding :: CoreExpr -> Unfolding

mkUnfolding :: DynFlags
            -> UnfoldingSource
            -> Bool
            -> Bool
            -> CoreExpr
            -> Unfolding
