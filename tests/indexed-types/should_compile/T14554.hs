{-# Language UndecidableInstances, DataKinds, TypeOperators, TypeFamilies,
             PolyKinds, GADTs, LambdaCase, ScopedTypeVariables #-}

module T14554 where

import Data.Kind
import Data.Proxy

type a ~> b = (a, b) -> Type

data IdSym0 :: (Type,Type) -> Type

data KIND = X | FNARR KIND KIND

data TY :: KIND -> Type where
  ID    :: TY (FNARR X X)
  FNAPP :: TY (FNARR k k') -> TY k -> TY k'

type TyRep :: forall (kind::KIND) -> TY kind -> Type
data TyRep k t where
  TID    :: TyRep (FNARR X X)  ID
  TFnApp :: TyRep (FNARR k k') f
         -> TyRep k            a
         -> TyRep k'           (FNAPP f a)

type IK :: KIND -> Type
type family IK k where
  IK X            = Type
  IK (FNARR k k') = IK k ~> IK k'

type IT :: TY kind -> IK kind
type family IT t

zero :: TyRep X a -> IT a
zero x = case x of
            TFnApp TID a -> undefined
