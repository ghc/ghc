{-# LANGUAGE GADTs, TypeOperators, DataKinds, TypeFamilies, PolyKinds, TypeFamilyDependencies #-}

module T14749 where

import Data.Kind

data KIND = STAR | KIND :> KIND

data Ty :: KIND -> Type where
  TMaybe :: Ty (STAR :> STAR)
  TApp   :: Ty (a :> b) -> (Ty a -> Ty b)

type family IK (k :: KIND) = (res :: Type) where
  IK STAR   = Type
  IK (a:>b) = IK a -> IK b

type family I (t :: Ty k) = (res :: IK k)  where
  I TMaybe     = Maybe
  I (TApp f a) = (I f) (I a)

data TyRep (k :: KIND) (t :: Ty k) where
  TyMaybe :: TyRep (STAR:>STAR) TMaybe
  TyApp   :: TyRep (a:>b) f -> TyRep a x -> TyRep b (TApp f x)

zero :: TyRep STAR a -> I a
zero x = case x of
            TyApp TyMaybe _ -> Nothing
