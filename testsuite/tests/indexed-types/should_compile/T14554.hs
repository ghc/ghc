{-# Language UndecidableInstances, DataKinds, TypeOperators,
             KindSignatures, PolyKinds, TypeInType, TypeFamilies,
             GADTs, LambdaCase, ScopedTypeVariables #-}

module T14554 where

import Data.Kind
import Data.Proxy

type a ~> b = (a, b) -> Type

data IdSym0 :: (Type,Type) -> Type

data KIND = X | FNARR KIND KIND

data TY :: KIND -> Type where
  ID    :: TY (FNARR X X)
  FNAPP :: TY (FNARR k k') -> TY k -> TY k'

data TyRep (kind::KIND) :: TY kind -> Type where
  TID    :: TyRep (FNARR X X)  ID
  TFnApp :: TyRep (FNARR k k') f
         -> TyRep k            a
         -> TyRep k'           (FNAPP f a)

type family IK (kind::KIND) :: Type where
  IK X            = Type
  IK (FNARR k k') = IK k ~> IK k'

type family IT (ty::TY kind) :: IK kind

zero :: TyRep X a -> IT a
zero x = case x of
            TFnApp TID a -> undefined
