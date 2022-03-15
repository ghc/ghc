module GHC.Core.Unify where

import GHC.Core.TyCo.Subst (TCvSubst)
import GHC.Core.Type (Type)


import Data.Maybe (Maybe)

tcMatchTys :: [Type] -> [Type] -> Maybe TCvSubst
