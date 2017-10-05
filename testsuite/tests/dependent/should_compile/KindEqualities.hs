{-# LANGUAGE TypeInType, GADTs, ExplicitForAll #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module KindEqualities where

import Data.Kind

data TyRep1 :: * -> * where
  TyInt1 :: TyRep1 Int
  TyBool1 :: TyRep1 Bool

zero1 :: forall a. TyRep1 a -> a
zero1 TyInt1 = 0
zero1 TyBool1 = False

data Proxy (a :: k) = P

data TyRep :: forall k. k -> * where
  TyInt :: TyRep Int
  TyBool :: TyRep Bool
  TyMaybe :: TyRep Maybe
  TyApp :: TyRep a -> TyRep b -> TyRep (a b)

zero :: forall (a :: *). TyRep a -> a
zero TyInt = 0
zero TyBool = False
zero (TyApp TyMaybe _) = Nothing
