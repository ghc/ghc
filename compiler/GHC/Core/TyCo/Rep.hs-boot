module GHC.Core.TyCo.Rep where

import GHC.Utils.Outputable ( Outputable )
import Data.Data  ( Data )
import {-# SOURCE #-} GHC.Types.Var( Var, ArgFlag, AnonArgFlag )

data Type
data TyThing
data Coercion
data UnivCoProvenance
data TyLit
data TyCoBinder
data MCoercion

data Scaled a
type Mult = Type

type PredType = Type
type Kind = Type
type ThetaType = [PredType]
type CoercionN = Coercion
type MCoercionN = MCoercion

mkFunTyMany :: AnonArgFlag -> Type -> Type -> Type
mkForAllTy :: Var -> ArgFlag -> Type -> Type

instance Data Type  -- To support Data instances in GHC.Core.Coercion.Axiom
instance Outputable Type
