module GHC.Builtin.Types where

import {-# SOURCE #-} GHC.Core.TyCon    ( TyCon )
import {-# SOURCE #-} GHC.Core.TyCo.Rep (Type, Kind, RuntimeRepType)
import {-# SOURCE #-} GHC.Core.DataCon  ( DataCon )

import GHC.Types.Basic (Arity, TupleSort, Boxity, ConTag)
import {-# SOURCE #-} GHC.Types.Name (Name)
import GHC.Unit.Types (WiredIn)

listTyCon :: TyCon
typeSymbolKind :: WiredIn Type
charTy :: WiredIn Type
mkBoxedTupleTy :: [Type] -> Type

coercibleTyCon, heqTyCon :: TyCon

unitTy :: Type

liftedTypeKindTyConName :: WiredIn Name
constraintKindTyConName :: WiredIn Name

liftedTypeKind, unliftedTypeKind, zeroBitTypeKind :: WiredIn Kind

liftedTypeKindTyCon, unliftedTypeKindTyCon :: WiredIn TyCon

liftedRepTyCon, unliftedRepTyCon :: WiredIn TyCon

constraintKind :: WiredIn Kind

runtimeRepTyCon, levityTyCon, vecCountTyCon, vecElemTyCon :: WiredIn TyCon
runtimeRepTy, levityTy :: WiredIn Type

boxedRepDataConTyCon, liftedDataConTyCon :: WiredIn TyCon
vecRepDataConTyCon, tupleRepDataConTyCon :: WiredIn TyCon

liftedRepTy, unliftedRepTy, zeroBitRepTy :: WiredIn RuntimeRepType
liftedDataConTy, unliftedDataConTy :: WiredIn Type

intRepDataConTy,
  int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
  wordRepDataConTy,
  word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
  addrRepDataConTy,
  floatRepDataConTy, doubleRepDataConTy :: WiredIn RuntimeRepType

vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy :: WiredIn Type

int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
  int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
  word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,
  doubleElemRepDataConTy :: WiredIn Type

anyTypeOfKind :: Kind -> WiredIn Type
unboxedTupleKind :: [Type] -> WiredIn Type

multiplicityTyCon :: WiredIn TyCon
multiplicityTy :: WiredIn Type
oneDataConTy :: Type
oneDataConTyCon :: TyCon
manyDataConTy :: Type
manyDataConTyCon :: TyCon
unrestrictedFunTyCon :: TyCon
multMulTyCon :: TyCon

tupleTyConName :: TupleSort -> Arity -> Name
tupleDataConName :: Boxity -> Arity -> Name

integerTy, naturalTy :: WiredIn Type

promotedTupleDataCon :: Boxity -> Arity -> WiredIn TyCon

tupleDataCon :: Boxity -> Arity -> WiredIn DataCon
tupleTyCon   :: Boxity -> Arity -> WiredIn TyCon

cTupleDataCon :: Arity -> WiredIn DataCon
cTupleDataConName :: Arity -> WiredIn Name
cTupleTyConName :: Arity -> WiredIn Name
cTupleSelIdName :: ConTag -> Arity -> WiredIn Name

sumDataCon :: ConTag -> Arity -> WiredIn DataCon
sumTyCon :: Arity -> WiredIn TyCon
