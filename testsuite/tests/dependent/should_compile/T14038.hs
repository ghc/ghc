{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T14038 where

import Data.Kind (Type)

data family Sing (a :: k)
data instance Sing (z :: [a]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x:xs)

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type a @@ b = Apply a b
infixl 9 @@

data FunArrow = (:->) -- ^ '(->)'
              | (:~>) -- ^ '(~>)'

class FunType (arr :: FunArrow) where
  type Fun (k1 :: Type) arr (k2 :: Type) :: Type

class FunType arr => AppType (arr :: FunArrow) where
  type App k1 arr k2 (f :: Fun k1 arr k2) (x :: k1) :: k2

type FunApp arr = (FunType arr, AppType arr)

instance FunType (:->) where
  type Fun k1 (:->) k2 = k1 -> k2

instance AppType (:->) where
  type App k1 (:->) k2 (f :: k1 -> k2) x = f x

instance FunType (:~>) where
  type Fun k1 (:~>) k2 = k1 ~> k2

instance AppType (:~>) where
  type App k1 (:~>) k2 (f :: k1 ~> k2) x = f @@ x

infixr 0 -?>
type (-?>) (k1 :: Type) (k2 :: Type) (arr :: FunArrow) = Fun k1 arr k2

elimList :: forall (a :: Type) (p :: [a] -> Type) (l :: [a]).
            Sing l
         -> p '[]
         -> (forall (x :: a) (xs :: [a]). Sing x -> Sing xs -> p xs -> p (x:xs))
         -> p l
elimList = elimListPoly @(:->)

elimListTyFun :: forall (a :: Type) (p :: [a] ~> Type) (l :: [a]).
                 Sing l
              -> p @@ '[]
              -> (forall (x :: a) (xs :: [a]). Sing x -> Sing xs -> p @@ xs -> p @@ (x:xs))
              -> p @@ l
elimListTyFun = elimListPoly @(:~>) @_ @p

elimListPoly :: forall (arr :: FunArrow) (a :: Type) (p :: ([a] -?> Type) arr) (l :: [a]).
                FunApp arr
             => Sing l
             -> App [a] arr Type p '[]
             -> (forall (x :: a) (xs :: [a]). Sing x -> Sing xs -> App [a] arr Type p xs -> App [a] arr Type p (x:xs))
             -> App [a] arr Type p l
elimListPoly SNil                      pNil _     = pNil
elimListPoly (SCons x (xs :: Sing xs)) pNil pCons = pCons x xs (elimListPoly @arr @a @p @xs xs pNil pCons)
