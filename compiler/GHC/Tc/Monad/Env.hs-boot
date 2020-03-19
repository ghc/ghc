module GHC.Tc.Monad.Env where

import GHC.Tc.Types( TcM )
import GHC.Types.Var.Env( TidyEnv )

-- Annoyingly, there's a recursion between tcInitTidyEnv
-- (which does zonking and hence needs GHC.Tc.Utils.Monadic) and
-- addErrTc etc which live in GHC.Tc.Monad.  Rats.
tcInitTidyEnv :: TcM TidyEnv

