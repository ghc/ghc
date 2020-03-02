module GHC.Core.PatSyn where

import BasicTypes (Arity)
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type)
import Var (TyVar)
import Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
