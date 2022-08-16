{-# LANGUAGE NoPolyKinds #-}
module GHC.Core.TyCo.Rep where

import GHC.Utils.Outputable ( Outputable )
import Data.Data  ( Data )
import {-# SOURCE #-} GHC.Types.Var( Var, ArgFlag, AnonArgFlag )
import {-# SOURCE #-} GHC.Core.TyCon ( TyCon )

data Type
data Coercion
data CoSel
data UnivCoProvenance
data TyLit
data TyCoBinder
data MCoercion

data Scaled a
type Mult = Type

type PredType = Type
type RuntimeRepType = Type
type Kind = Type
type ThetaType = [PredType]
type CoercionN = Coercion
type MCoercionN = MCoercion

mkForAllTy       :: Var -> ArgFlag -> Type -> Type
mkNakedTyConTy   :: TyCon -> Type
mkNakedKindFunTy :: AnonArgFlag -> Type -> Type -> Type

instance Data Type  -- To support Data instances in GHC.Core.Coercion.Axiom
instance Outputable Type
