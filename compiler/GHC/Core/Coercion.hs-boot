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
mkForAllCo :: TyCoVar -> Coercion -> Coercion -> Coercion
mkFunCo :: Role -> CoercionN -> Coercion -> Coercion -> Coercion
mkCoVarCo :: CoVar -> Coercion
mkAxiomInstCo :: CoAxiom Branched -> BranchIndex -> [Coercion] -> Coercion
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkUnivCo :: UnivCoProvenance -> Role -> Type -> Type -> Coercion
mkSymCo :: Coercion -> Coercion
mkTransCo :: Coercion -> Coercion -> Coercion
mkNthCo :: HasDebugCallStack => Role -> Int -> Coercion -> Coercion
mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkInstCo :: Coercion -> Coercion -> Coercion
mkGReflCo :: Role -> Type -> MCoercionN -> Coercion
mkNomReflCo :: Type -> Coercion
mkKindCo :: Coercion -> Coercion
mkSubCo :: HasDebugCallStack => Coercion -> Coercion
mkProofIrrelCo :: Role -> Coercion -> Coercion -> Coercion -> Coercion
mkAxiomRuleCo :: CoAxiomRule -> [Coercion] -> Coercion

isGReflCo :: Coercion -> Bool
isReflCo :: Coercion -> Bool
isReflexiveCo :: Coercion -> Bool
decomposePiCos :: HasDebugCallStack => Coercion -> Pair Type -> [Type] -> ([Coercion], Coercion)
coVarKindsTypesRole :: HasDebugCallStack => CoVar -> (Kind, Kind, Type, Type, Role)
coVarRole :: CoVar -> Role

mkCoercionType :: Role -> Type -> Type -> Type

data LiftingContext
liftCoSubst :: HasDebugCallStack => Role -> LiftingContext -> Type -> Coercion
seqCo :: Coercion -> ()

coercionKind :: Coercion -> Pair Type
coercionLKind :: Coercion -> Type
coercionRKind :: Coercion -> Type
coercionType :: Coercion -> Type

topNormaliseNewType_maybe :: Type -> Maybe (Coercion, Type)
  -- used to look through newtypes to the right of
  -- function arrows, in 'GHC.Core.Type.getRuntimeArgTys'
