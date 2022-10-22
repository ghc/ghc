module GHC.Core.Unify where

import GHC.Core.TyCo.Subst (Subst)
import GHC.Core.Type (Type)


import Data.Maybe (Maybe)

tcMatchTys :: [Type] -> [Type] -> Maybe Subst
