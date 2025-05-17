{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T18065 where

import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))

type family Sing :: k -> Type
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>
type family Apply (f :: a ~> b) (x :: a) :: b

type SingFunction1 f = forall t. Sing t -> Sing (f `Apply` t)
singFun1 :: forall f. SingFunction1 f -> Sing f
singFun1 f = SLambda f

type SingFunction2 f = forall t1 t2. Sing t1 -> Sing t2 -> Sing (f `Apply` t1 `Apply` t2)
singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = SLambda (\x -> singFun1 (f x))

newtype SLambda (f :: a ~> b) =
  SLambda { applySing :: forall t. Sing t -> Sing (f `Apply` t) }
type instance Sing @(_ ~> _) = SLambda

data SList :: forall a. [a] -> Type where
  SNil  :: SList '[]
  SCons :: Sing x -> Sing xs -> SList (x:xs)
type instance Sing @[a] = SList

data SNonEmpty :: forall a. NonEmpty a -> Type where
  (:%|) :: Sing x -> Sing xs -> SNonEmpty (x:|xs)
type instance Sing @(NonEmpty a) = SNonEmpty

type family Id (x :: a) :: a where
  Id x = x
data IdSym0 :: a ~> a
type instance Apply IdSym0 x = Id x
sId :: forall a (x :: a). Sing x -> Sing (Id x)
sId sx = sx

type family (.) (f :: b ~> c) (g :: a ~> b) (x :: a) :: c where
  (f . g) x = f `Apply` (g `Apply` x)
data (.@#@$)   :: (b ~> c) ~> (a ~> b) ~> a ~> c
data (.@#@$$)  :: (b ~> c) -> (a ~> b) ~> a ~> c
data (.@#@$$$) :: (b ~> c) -> (a ~> b) -> a ~> c
type instance Apply  (.@#@$)   f       = (.@#@$$)  f
type instance Apply ((.@#@$$)  f) g    = (.@#@$$$) f g
type instance Apply ((.@#@$$$) f  g) x = (f . g) x
(%.) :: forall b c a (f :: b ~> c) (g :: a ~> b) (x :: a).
        Sing f -> Sing g -> Sing x -> Sing ((f . g) x)
(%.) sf sg sx = sf `applySing` (sg `applySing` sx)

type family Go (k :: a ~> b ~> b) (z :: b) (l :: [a]) :: b where
  Go _ z '[]    = z
  Go k z (y:ys) = k `Apply` y `Apply` Go k z ys
data GoSym :: (a ~> b ~> b) -> b -> [a] ~> b
type instance Apply (GoSym k z) l = Go k z l
type family Listfoldr (k :: a ~> b ~> b) (z :: b) (l :: [a]) :: b where
  Listfoldr k z l = Go k z l
sListfoldr :: forall a b (k :: a ~> b ~> b) (z :: b) (l :: [a]).
              Sing k -> Sing z -> Sing l -> Sing (Listfoldr k z l)
sListfoldr sk sz = sGo
  where
    sGo :: forall l'. Sing l' -> Sing (GoSym k z `Apply` l')
    sGo SNil             = sz
    sGo (sy `SCons` sys) = sk `applySing` sy `applySing` sGo sys

class PMonoid a where
  type Mempty :: a
  type Mappend (x :: a) (y :: a) :: a
data MappendSym0 :: a ~> a ~> a
data MappendSym1 :: a -> a ~> a
type instance Apply  MappendSym0 x    = MappendSym1 x
type instance Apply (MappendSym1 x) y = Mappend x y
class SMonoid a where
  sMempty  :: Sing (Mempty :: a)
  sMappend :: forall (x :: a) (y :: a). Sing x -> Sing y -> Sing (Mappend x y)

class PFoldable t where
  type Foldr (f :: a ~> b ~> b) (z :: b) (l :: t a) :: b
instance PFoldable [] where
  type Foldr f z l = Listfoldr f z l
class SFoldable t where
  sFoldr :: forall a b (f :: a ~> b ~> b) (z :: b) (l :: t a).
            Sing f -> Sing z -> Sing l -> Sing (Foldr f z l)
instance SFoldable [] where
  sFoldr = sListfoldr

type family FoldMap (f :: a ~> m) (l :: t a) :: m where
  FoldMap f l = Foldr (MappendSym0 .@#@$$$ f) Mempty l
sFoldMap :: forall t a m (f :: a ~> m) (l :: t a).
            (SFoldable t, SMonoid m)
         => Sing f -> Sing l -> Sing (FoldMap f l)
sFoldMap sf = sFoldr (singFun2 @((.@#@$$) MappendSym0) (singFun2 @MappendSym0 sMappend %.) `applySing` sf) sMempty

type family NEFold (l :: NonEmpty m) :: m where
  NEFold (a :| as) = a `Mappend` FoldMap IdSym0 as
sNEFold :: forall m (l :: NonEmpty m). SMonoid m
        => Sing l -> Sing (NEFold l)
sNEFold (sa :%| sas) = sa `sMappend` sFoldMap (singFun1 @IdSym0 sId) sas
