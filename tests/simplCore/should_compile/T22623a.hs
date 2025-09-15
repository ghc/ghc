{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T22623a where

import Data.Kind

type Id :: Type -> Type
type family Id x
type instance Id x = x

data Q
data SQ (x :: Q)

data NonEmpty where
  (:|) :: Q -> [Q] -> NonEmpty

type Tail :: NonEmpty -> [Q]
type family Tail y where
  Tail ('(:|) _ y) = y
type MyHead :: Q -> NonEmpty -> Q
type family MyHead x y where
  MyHead _ ('(:|) c _) = c

type SList :: [Q] -> Type
data SList z where
  SNil  :: SList '[]
  SCons :: SQ x -> SList xs -> SList (x:xs)

type SNonEmpty :: NonEmpty -> Type
data SNonEmpty z where
  (:%|) :: SQ x -> SList xs -> SNonEmpty (x :| xs)

data TyFun
type F = TyFun -> Type

type Apply :: F -> Q -> NonEmpty
type family Apply f x

type ConstSym1 :: NonEmpty -> F
data ConstSym1 (x :: NonEmpty) :: F
type instance Apply (ConstSym1 x) _ = x

type SLambda :: F -> Type
newtype SLambda (f :: F) =
  SLambda { applySing :: forall t. SQ t -> SNonEmpty (f `Apply` t) }

type Foldr2 :: Q -> NonEmpty -> [Q] -> [Q]
type family Foldr2 a c x where
  Foldr2 _ _ '[] = '[]
  Foldr2 a c (_:ys) = MyHead a c : Foldr2 a c ys

type (++) :: [Q] -> [Q] -> [Q]
type family (++) xs ys where
  (++) '[] ys = ys
  (++) ('(:) x xs) ys = '(:) x (xs ++ ys)

(%++) :: forall (x :: [Q]) (y :: [Q]). SList x -> SList y -> SList (x ++ y)
(%++) SNil sYs = sYs
(%++) (SCons sX sXs) sYs = SCons sX (sXs %++ sYs)
