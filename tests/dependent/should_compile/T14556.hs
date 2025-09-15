{-# Language UndecidableInstances, DataKinds, TypeOperators, PolyKinds,
             TypeFamilies, GADTs, LambdaCase, ScopedTypeVariables #-}

module T14556 where

import Data.Kind
import Data.Proxy

data Fn a b where
  IdSym :: Fn Type Type

type (@@) :: Fn k k' -> k -> k'
type family f @@ a where
  IdSym @@ a = a

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
  IK (FNARR k k') = Fn (IK k) (IK k')

type IT :: TY kind -> IK kind
type family IT t where
  IT ID          = IdSym
  IT (FNAPP f x) = IT f @@ IT x

zero :: TyRep X a -> IT a
zero = undefined
