module GHC.Builtin.Types where

import {-# SOURCE #-} GHC.Core.TyCon    ( TyCon )
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type, Kind)
import {-# SOURCE #-} GHC.Core.DataCon  ( DataCon )

import GHC.Types.Basic (Arity, TupleSort, Boxity, ConTag)
import {-# SOURCE #-} GHC.Types.Name (Name)

listTyCon :: TyCon
typeSymbolKind :: Type
charTy :: Type
mkBoxedTupleTy :: [Type] -> Type

coercibleTyCon, heqTyCon :: TyCon

unitTy :: Type

liftedTypeKindTyConName :: Name

liftedTypeKind :: Kind
unliftedTypeKind :: Kind

liftedTypeKindTyCon :: TyCon
unliftedTypeKindTyCon :: TyCon

liftedRepTyCon :: TyCon
unliftedRepTyCon :: TyCon

constraintKind :: Kind

runtimeRepTyCon, levityTyCon, vecCountTyCon, vecElemTyCon :: TyCon
runtimeRepTy, levityTy :: Type

boxedRepDataConTyCon :: TyCon
liftedDataConTyCon :: TyCon
vecRepDataConTyCon, tupleRepDataConTyCon :: TyCon

liftedRepTy, unliftedRepTy :: Type
liftedDataConTy, unliftedDataConTy :: Type

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

promotedTupleDataCon :: Boxity -> Arity -> TyCon

tupleDataCon :: Boxity -> Arity -> DataCon
tupleTyCon   :: Boxity -> Arity -> TyCon

cTupleDataCon :: Arity -> DataCon
cTupleDataConName :: Arity -> Name
cTupleTyConName :: Arity -> Name
cTupleSelIdName :: ConTag -> Arity -> Name

sumDataCon :: ConTag -> Arity -> DataCon
sumTyCon :: Arity -> TyCon
