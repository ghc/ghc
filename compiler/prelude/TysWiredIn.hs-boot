module TysWiredIn where

import Var( TyVar, ArgFlag )
import {-# SOURCE #-} TyCon      ( TyCon )
import {-# SOURCE #-} TyCoRep    (Type, Kind)


mkFunKind :: Kind -> Kind -> Kind
mkForAllKind :: TyVar -> ArgFlag -> Kind -> Kind

listTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type

coercibleTyCon, heqTyCon :: TyCon

unitTy :: Type

liftedTypeKind :: Kind
constraintKind :: Kind

runtimeRepTyCon, vecCountTyCon, vecElemTyCon :: TyCon
runtimeRepTy :: Type

liftedRepDataConTyCon, vecRepDataConTyCon, tupleRepDataConTyCon :: TyCon

liftedRepDataConTy, unliftedRepDataConTy, intRepDataConTy, int8RepDataConTy,
  int16RepDataConTy, word16RepDataConTy,
  wordRepDataConTy, int64RepDataConTy, word8RepDataConTy, word64RepDataConTy,
  addrRepDataConTy, floatRepDataConTy, doubleRepDataConTy :: Type

vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy :: Type

int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
  int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
  word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,
  doubleElemRepDataConTy :: Type

anyTypeOfKind :: Kind -> Type
unboxedTupleKind :: [Type] -> Type
mkPromotedListTy :: Type -> [Type] -> Type
