{-# LANGUAGE NoPolyKinds #-}
module GHC.Types.Var where

import {-# SOURCE #-} GHC.Types.Name
import Language.Haskell.Syntax.Specificity (Specificity)

data FunTyFlag
data Var
instance NamedThing Var
data VarBndr var argf
type TyVar = Var
type Id    = Var
type TyCoVar = Id
type TcTyVar = Var
type InvisTVBinder = VarBndr TyVar Specificity
