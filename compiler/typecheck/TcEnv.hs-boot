module TcEnv where

import TcRnTypes( TcM )
import VarEnv( TidyEnv )

-- Annoyingly, there's a recursion between tcInitTidyEnv
-- (which does zonking and hence needs TcMType) and
-- addErrTc etc which live in TcRnMonad.  Rats.
tcInitTidyEnv :: TcM TidyEnv

