module TyCoFVs where

import TyCoRep (Type)
import FVM

typeFVs :: FVM m => Type -> m

