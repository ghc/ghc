module TyCoRep where

import GhcPrelude

import Outputable ( SDoc', HasPprConfig )
import Data.Data  ( Data )
import {-# SOURCE #-} Var( Var, ArgFlag, AnonArgFlag )
import {-# SOURCE #-} Packages (HasPackageState)
import NameSuppress ( HasNameSuppress )
import TypeSuppress

data Type
data TyThing
data Coercion
data UnivCoProvenance
data TCvSubst
data TyLit
data TyCoBinder
data MCoercion

type PredType = Type
type Kind = Type
type ThetaType = [PredType]
type CoercionN = Coercion
type MCoercionN = MCoercion

pprKind
  :: ( HasPprConfig r
     , HasNameSuppress r
     , HasTypeSuppress r
     , HasPackageState r
     )
  => Kind -> SDoc' r
pprType
  :: ( HasPprConfig r
     , HasNameSuppress r
     , HasTypeSuppress r
     , HasPackageState r
     )
  => Type -> SDoc' r
mkFunTy   :: AnonArgFlag -> Type -> Type -> Type
mkForAllTy :: Var -> ArgFlag -> Type -> Type

isRuntimeRepTy :: Type -> Bool

instance Data Type  -- To support Data instances in CoAxiom
