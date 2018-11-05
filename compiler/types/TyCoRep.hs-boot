module TyCoRep where

import GhcPrelude

import Outputable ( SDoc )
import Data.Data  ( Data )
import Weight

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

isRuntimeRepTy :: Type -> Bool
isMultiplicityTy :: Type -> Bool

instance Data Type
  -- To support Data instances in CoAxiom

type Rig = GMult Type
type Weighted = GWeighted Type
instance Multable Type