{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Coercion where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.TyCo.Rep
import {-# SOURCE #-} GHC.Core.TyCon

import GHC.Types.Basic ( LeftOrRight )
import GHC.Core.Coercion.Axiom
import GHC.Types.Var
import GHC.Data.Pair
import GHC.Utils.Misc

mkReflCo :: Role -> Type -> Coercion
mkTyConAppCo :: HasDebugCallStack => Role -> TyCon -> [Coercion] -> Coercion
mkAppCo :: Coercion -> Coercion -> Coercion
mkForAllCo :: HasDebugCallStack => TyCoVar -> ForAllTyFlag -> ForAllTyFlag -> Coercion -> Coercion -> Coercion
mkFunCo      :: Role -> FunTyFlag -> CoercionN -> Coercion -> Coercion -> Coercion
mkNakedFunCo :: Role -> FunTyFlag -> CoercionN -> Coercion -> Coercion -> Coercion
mkFunCo2     :: Role -> FunTyFlag -> FunTyFlag -> CoercionN -> Coercion -> Coercion -> Coercion
mkCoVarCo :: CoVar -> Coercion
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkUnivCo :: UnivCoProvenance -> [Coercion] -> Role -> Type -> Type -> Coercion
mkSymCo :: Coercion -> Coercion
mkTransCo :: HasDebugCallStack => Coercion -> Coercion -> Coercion
mkSelCo :: HasDebugCallStack => CoSel -> Coercion -> Coercion
mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkInstCo :: Coercion -> Coercion -> Coercion
mkGReflCo :: Role -> Type -> MCoercionN -> Coercion
mkNomReflCo :: Type -> Coercion
mkKindCo :: Coercion -> Coercion
mkSubCo :: HasDebugCallStack => Coercion -> Coercion
mkProofIrrelCo :: Role -> Coercion -> Coercion -> Coercion -> Coercion
mkAxiomCo :: CoAxiomRule -> [Coercion] -> Coercion

funRole :: Role -> FunSel -> Role

isGReflCo :: Coercion -> Bool
isReflCo :: Coercion -> Bool
isReflexiveCo :: Coercion -> Bool
decomposePiCos :: HasDebugCallStack => Coercion -> Pair Type -> [Type] -> ([Coercion], Coercion)
coVarTypesRole :: HasDebugCallStack => CoVar -> (Type, Type, Role)
coVarRole :: CoVar -> Role

mkCoercionType :: Role -> Type -> Type -> Type

seqCo :: Coercion -> ()

coercionKind  :: HasDebugCallStack => Coercion -> Pair Type
coercionLKind :: HasDebugCallStack => Coercion -> Type
coercionRKind :: HasDebugCallStack => Coercion -> Type
coercionType :: Coercion -> Type

topNormaliseNewType_maybe :: Type -> Maybe (Coercion, Type)
  -- used to look through newtypes to the right of
  -- function arrows, in 'GHC.Core.Type.getRuntimeArgTys'
