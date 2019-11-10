module TyCoFVs where

import TyCoRep (Type)
import FreeVarStrategy

typeFVs :: FreeVarStrategy m => Type -> m

