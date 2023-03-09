module GHC.Tc.Types.ErrCtxt where

import GHC.Prelude
import GHC.Types.Var.Env
import GHC.Tc.Zonk.Monad (ZonkM)
import GHC.Utils.Outputable

-- | Additional context to include in an error message, e.g.
-- "In the type signature ...", "In the ambiguity check for ...", etc.
type ErrCtxt = (Bool, TidyEnv -> ZonkM (TidyEnv, SDoc))
        -- Monadic so that we have a chance
        -- to deal with bound type variables just before error
        -- message construction

        -- Bool:  True <=> this is a landmark context; do not
        --                 discard it when trimming for display
