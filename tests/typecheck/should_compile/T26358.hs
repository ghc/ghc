{-# LANGUAGE TypeFamilies #-}

module T26358 where
import Data.Kind
import Data.Proxy

{- Two failing tests, described in GHC.Core.Unify
     Note [Shortcomings of the apartness test]

Explanation for TF2
* We try to reduce
    (TF2 (F (G Float)) (F Int) (G Float))
* We can only do so if those arguments are apart from the first
  equation of TF2, namely (Bool,Char,Int).
* So we try to unify
    [F (G Float), F Int, G Float] ~ [Bool, Char, Int]
* They really are apart, but we can't quite spot that yet;
  hence #26358

TF1 is similar.
-}


type TF1 :: Type -> Type -> Type -> Type
type family TF1 a b c where
  TF1 Bool Char a = Word
  TF1 a    b    c = (a,b,c)

type F :: Type -> Type
type family F a where

foo :: Proxy a
    -> Proxy (TF1 (F a) (F Int) Int)
    -> Proxy (F a, F Int, Int)
foo _ px = px

type TF2 :: Type -> Type -> Type -> Type
type family TF2 a b c where
  TF2 Bool Char Int = Word
  TF2 a    b    c   = (a,b,c)

type G :: Type -> Type
type family G a where

bar :: Proxy (TF2 (F (G Float)) (F Int) (G Float))
    -> Proxy (F (G Float), F Int, G Float)
bar px = px

