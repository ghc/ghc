{-# Language UndecidableInstances, DataKinds, TypeOperators, KindSignatures, PolyKinds, TypeInType, TypeFamilies, GADTs, LambdaCase, ScopedTypeVariables #-}

module T14556 where

import Data.Kind
import Data.Proxy

data Fn a b where
  IdSym :: Fn Type Type

type family
  (@@) (f::Fn k k') (a::k)::k' where
  IdSym @@ a = a

data KIND = X | FNARR KIND KIND

data TY :: KIND -> Type where
  ID    :: TY (FNARR X X)
  FNAPP :: TY (FNARR k k') -> TY k -> TY k'

data TyRep (kind::KIND) :: TY kind -> Type where
  TID    :: TyRep (FNARR X X)  ID
  TFnApp :: TyRep (FNARR k k') f
         -> TyRep k            a
         -> TyRep k'           (FNAPP f a)

type family
  IK (kind::KIND) :: Type where
  IK X            = Type
  IK (FNARR k k') = Fn (IK k) (IK k')

type family
  IT (ty::TY kind) :: IK kind where
  IT ID          = IdSym
  IT (FNAPP f x) = IT f @@ IT x

zero :: TyRep X a -> IT a
zero = undefined
