module CoreUnfold (
        mkUnfolding
    ) where

import GhcPrelude
import CoreSyn
import GHC.Driver.Session

mkUnfolding :: DynFlags
            -> UnfoldingSource
            -> Bool
            -> Bool
            -> CoreExpr
            -> Unfolding
