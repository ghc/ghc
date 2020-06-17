module GHC.Builtin.Types where

import {-# SOURCE #-} GHC.Core.TyCon    ( TyCon )
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type, Kind)

import GHC.Types.Basic (Arity, TupleSort)
import GHC.Types.Name (Name)

listTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type

coercibleTyCon, heqTyCon :: TyCon

unitTy :: Type

liftedTypeKind :: Kind
liftedTypeKindTyCon :: TyCon

constraintKind :: Kind

runtimeRepTyCon, vecCountTyCon, vecElemTyCon :: TyCon
runtimeRepTy :: Type

liftedRepDataConTyCon, vecRepDataConTyCon, tupleRepDataConTyCon :: TyCon

liftedRepDataConTy, unliftedRepDataConTy,
  intRepDataConTy,
  int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
  wordRepDataConTy,
  word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
  addrRepDataConTy,
  floatRepDataConTy, doubleRepDataConTy :: Type

vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy :: Type

int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
  int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
  word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,
  doubleElemRepDataConTy :: Type

anyTypeOfKind :: Kind -> Type
unboxedTupleKind :: [Type] -> Type
mkPromotedListTy :: Type -> [Type] -> Type

multiplicityTyCon :: TyCon
multiplicityTy :: Type
oneDataConTy :: Type
oneDataConTyCon :: TyCon
manyDataConTy :: Type
manyDataConTyCon :: TyCon
unrestrictedFunTyCon :: TyCon
multMulTyCon :: TyCon

tupleTyConName :: TupleSort -> Arity -> Name

integerTy, naturalTy :: Type
