module PatSyn where

import BasicTypes (Arity)
import {-# SOURCE #-} GHC.Core.TyCoRep (Type)
import Var (TyVar)
import Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
