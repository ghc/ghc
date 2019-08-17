{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeFamilies, ExplicitForAll,
             TemplateHaskell, UndecidableInstances, ScopedTypeVariables #-}

module KindEqualities2 where

import Data.Kind
import GHC.Exts ( Any )

data Kind = Star | Arr Kind Kind

data Ty :: Kind -> Type where
  TInt :: Ty Star
  TBool :: Ty Star
  TMaybe :: Ty (Arr Star Star)
  TApp :: Ty (Arr k1 k2) -> Ty k1 -> Ty k2


data TyRep (k :: Kind) (t :: Ty k) where
  TyInt :: TyRep Star TInt
  TyBool :: TyRep Star TBool
  TyMaybe :: TyRep (Arr Star Star) TMaybe
  TyApp :: TyRep (Arr k1 k2) a -> TyRep k1 b -> TyRep k2 (TApp a b)

type family IK (k :: Kind)
type instance IK Star = Type
type instance IK (Arr k1 k2) = IK k1 -> IK k2

$(return [])  -- necessary because the following instances depend on the
              -- previous ones.

type family I (t :: Ty k) :: IK k
type instance I TInt = Int
type instance I TBool = Bool
type instance I TMaybe = Maybe
type instance I (TApp a b) = (I a) (I b)

zero :: forall (a :: Ty 'Star). TyRep Star a -> I a
zero TyInt = 0
zero TyBool = False
zero (TyApp TyMaybe TyInt) = Nothing

main = print $ zero (TyApp TyMaybe TyInt)
