module TyCoRep where

import GhcPrelude

import Outputable ( SDoc )
import Data.Data  ( Data )
import {-# SOURCE #-} Var( Var, ArgFlag, AnonArgFlag )

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

pprKind :: Kind -> SDoc
pprType :: Type -> SDoc
mkFunTy   :: AnonArgFlag -> Type -> Type -> Type
mkForAllTy :: Var -> ArgFlag -> Type -> Type

isRuntimeRepTy :: Type -> Bool

instance Data Type  -- To support Data instances in CoAxiom
