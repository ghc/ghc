module GHC.Tc.Utils.Env where

import GHC.Tc.Types( TcM )
import GHC.Types.Var.Env( TidyEnv )
import GHC.Core.TyCo.Rep (Type)
import GHC.Types.Var (EvVar)

-- Annoyingly, there's a recursion between tcInitTidyEnv
-- (which does zonking and hence needs GHC.Tc.Utils.TcMType) and
-- addErrTc etc which live in GHC.Tc.Utils.Monad.  Rats.
tcInitTidyEnv :: TcM TidyEnv
emitTypeable :: Type -> TcM EvVar

