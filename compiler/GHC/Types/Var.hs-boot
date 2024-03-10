{-# LANGUAGE NoPolyKinds #-}
module GHC.Types.Var where

import {-# SOURCE #-} GHC.Types.Name

data ForAllTyFlag
data FunTyFlag
data Var
instance NamedThing Var
data VarBndr var argf
data Specificity
type TyVar = Var
type Id    = Var
type TyCoVar = Id
type TcTyVar = Var
type InvisTVBinder = VarBndr TyVar Specificity
