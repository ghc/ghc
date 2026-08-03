module GHC.Core.Coercion where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.TyCo.Rep
import {-# SOURCE #-} GHC.Core.TyCon

import GHC.Types.Basic ( LeftOrRight )
import GHC.Core.Coercion.Axiom
import GHC.Types.Var
import GHC.Data.Pair
import GHC.Utils.Misc

mkTyConAppCo :: HasDebugCallStack => Role -> TyCon -> [Coercion] -> Coercion
mkAppCo  :: Coercion -> Coercion -> Coercion
mkAppCos :: Coercion -> [Coercion] -> Coercion
mkSelCo :: HasDebugCallStack => CoSel -> Coercion -> Coercion
mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkInstCo :: Coercion -> Coercion -> Coercion

isKindCo :: Coercion -> Bool
isReflexiveCo :: Coercion -> Bool
decomposePiCos  :: HasDebugCallStack => Coercion -> [Type] -> ([Coercion], Coercion)
decomposePiCosK :: HasDebugCallStack => Coercion -> Pair Type -> [Type] -> ([Coercion], Coercion)
coVarTypesRole :: HasDebugCallStack => CoVar -> (Type, Type, Role)
coVarRole :: CoVar -> Role

mkCoercionType :: Role -> Type -> Type -> Type

coercionKind  :: HasDebugCallStack => Coercion -> Pair Type
coercionLKind :: HasDebugCallStack => Coercion -> Type
coercionRKind :: HasDebugCallStack => Coercion -> Type
coercionRole  :: Coercion -> Role
coercionType  :: Coercion -> Type

topNormaliseNewType_maybe :: Type -> Maybe (Coercion, Type)
  -- used to look through newtypes to the right of
  -- function arrows, in 'GHC.Core.Type.getRuntimeArgTys'

assertGoodForAllCo :: HasDebugCallStack
                   => TyCoVar -> ForAllTyFlag -> ForAllTyFlag
                   -> KindMCoercion -> Coercion -> a -> a
