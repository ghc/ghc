module GHC.Core.PatSyn where

import GHC.Types.Basic (Arity)
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type)
import GHC.Types.Var (TyVar)
import GHC.Types.Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
