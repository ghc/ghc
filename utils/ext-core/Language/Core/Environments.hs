module Language.Core.Environments where

import Language.Core.Env
import Language.Core.Core
import Language.Core.Printer()

{- Environments. -}
type Tvenv = Env Tvar Kind                    -- type variables  (local only)
type Tcenv = Env Tcon KindOrCoercion          -- type constructors
type Cenv = Env Dcon Ty 		      -- data constructors
type Venv = Env Var Ty 			      -- values
type Menv = Env AnMname Envs		      -- modules
data Envs = Envs {tcenv_::Tcenv,cenv_::Cenv,venv_::Venv} -- all the exportable envs
  deriving Show

{- Extend an environment, checking for illegal shadowing of identifiers (for term
   variables -- shadowing type variables is allowed.) -}
data EnvType = Tv | NotTv
  deriving Eq
