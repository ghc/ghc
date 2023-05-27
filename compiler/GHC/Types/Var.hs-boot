{-# LANGUAGE NoPolyKinds #-}
module GHC.Types.Var where

import GHC.Prelude (Either)
import {-# SOURCE #-} GHC.Types.Name
  -- We compile this GHC with -XNoImplicitPrelude, so if there are no imports
  -- it does not seem to depend on anything. But it does! We must, for
  -- example, compile GHC.Types in the ghc-prim library first. So this
  -- otherwise-unnecessary import tells the build system that this module
  -- depends on GhcPrelude, which ensures that GHC.Type is built first.

data ForAllTyFlag
data FunTyFlag
data Var
instance NamedThing Var
instance NamedThing Id
instance NamedThing TyVar
instance NamedThing TcTyVar
data VarBndr var argf
data Specificity
data TyVar
data Id
type TyCoVar = Either TyVar Id
data TcTyVar
type InvisTVBinder = VarBndr TyVar Specificity
