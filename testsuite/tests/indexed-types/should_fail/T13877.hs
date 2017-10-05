{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T13877 where

import Data.Kind

data family Sing (a :: k)
data instance Sing (z :: [a]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x:xs)

data TyFun :: * -> * -> *
type a ~> b = TyFun a b -> *
infixr 0 ~>

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type a @@ b = Apply a b
infixl 9 @@

data FunArrow = (:->) | (:~>)

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

listElim :: forall (a :: Type) (p :: [a] -> Type) (l :: [a]).
            Sing l
         -> p '[]
         -> (forall (x :: a) (xs :: [a]). Sing x -> Sing xs -> p xs -> p (x:xs))
         -> p l
listElim = listElimPoly @(:->) @a @p @l

listElimTyFun :: forall (a :: Type) (p :: [a] ~> Type) (l :: [a]).
                 Sing l
              -> p @@ '[]
              -> (forall (x :: a) (xs :: [a]). Sing x -> Sing xs -> p @@ xs -> p @@ (x:xs))
              -> p @@ l
listElimTyFun = listElimPoly @(:->) @a @p @l

listElimPoly :: forall (arr :: FunArrow) (a :: Type) (p :: ([a] -?> Type) arr) (l :: [a]).
                FunApp arr
             => Sing l
             -> App [a] arr Type p '[]
             -> (forall (x :: a) (xs :: [a]). Sing x -> Sing xs -> App [a] arr Type p xs -> App [a] arr Type p (x:xs))
             -> App [a] arr Type p l
listElimPoly SNil                      pNil _     = pNil
listElimPoly (SCons x (xs :: Sing xs)) pNil pCons = pCons x xs (listElimPoly @arr @a @p @xs xs pNil pCons)
