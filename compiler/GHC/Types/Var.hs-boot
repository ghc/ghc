{-# LANGUAGE NoPolyKinds #-}
module GHC.Types.Var where

import GHC.Prelude ()
import {-# SOURCE #-} GHC.Types.Name
  -- We compile this GHC with -XNoImplicitPrelude, so if there are no imports
  -- it does not seem to depend on anything. But it does! We must, for
  -- example, compile GHC.Types in the ghc-prim library first. So this
  -- otherwise-unnecessary import tells the build system that this module
  -- depends on GhcPrelude, which ensures that GHC.Type is built first.

data ArgFlag
data AnonArgFlag
data Var
instance NamedThing Var
data VarBndr var argf
data Specificity
type TyVar = Var
type Id    = Var
type TyCoVar = Id
type TcTyVar = Var
type InvisTVBinder = VarBndr TyVar Specificity
