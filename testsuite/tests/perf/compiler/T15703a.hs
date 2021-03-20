{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T15703a where

import Data.Kind
import Data.Type.Equality
import GHC.Generics
import Lib2

-----
-- The important bits
-----

-- DefaultSignatures-based instances
instance PMonoid a => PApplicative ((,) a)
instance SMonoid a => SApplicative ((,) a)
instance VMonoid a => VApplicative ((,) a)

instance SApplicative f => SApplicative (M1 i c f) where
  sPure x = singFun3 @(.@#@$) (%.) @@ singFun1 @M1Sym0 SM1 @@ (singFun1 @PureSym0 sPure) @@ x
  -- If I change the implementation of sPure above to be this:
  --
  --   sPure x = SM1 (sPure x)
  --
  -- Then Lib.hs compiles quickly again (< 1 second) with -O1.

  SM1 f %<*> SM1 x = SM1 (f %<*> x)

-------------------------------------------------------------------------------

type family GenericPure (x :: a) :: f a where
  Pure x = To1 (Pure x)

type GenericPureC (f :: Type -> Type) (x :: a) =
  (Pure x :: f a) ~ (GenericPure x :: f a)

type family (g :: f (a ~> b)) `GenericAp` (x :: f a) :: f b where
  g `GenericAp` x = To1 (From1 g <*> From1 x)

type GenericApC (g :: f (a ~> b)) (x :: f a) =
  (g <*> x) ~ (g `GenericAp` x)

class PApplicative f where
  type Pure (x :: a) :: f a
  type Pure x = GenericPure x

  type (g :: f (a ~> b)) <*> (x :: f a) :: f b
  type g <*> x = g `GenericAp` x

data PureSym0 :: forall f a. a ~> f a
type instance Apply PureSym0 x = Pure x

data (<*>@#@$) :: forall f a b. f (a ~> b) ~> f a ~> f b
type instance Apply (<*>@#@$) g = (<*>@#@$$) g
data (<*>@#@$$) :: forall f a b. f (a ~> b) -> f a ~> f b
type instance Apply ((<*>@#@$$) g) x = g <*> x

class SApplicative f where
  sPure         :: forall a (x :: a).
                   Sing x -> Sing (Pure x :: f a)
  default sPure :: forall a (x :: a).
                   ( SGeneric1 f, SApplicative (Rep1 f)
                   , GenericPureC f x )
                => Sing x -> Sing (Pure x :: f a)
  sPure = sTo1 . sPure

  (%<*>)         :: forall a b (g :: f (a ~> b)) (x :: f a).
                    Sing g -> Sing x -> Sing (g <*> x)
  default (%<*>) :: forall a b (g :: f (a ~> b)) (x :: f a).
                    ( SGeneric1 f, SApplicative (Rep1 f)
                    , GenericApC g x )
                 => Sing g -> Sing x -> Sing (g <*> x)
  sg %<*> sx = sTo1 (sFrom1 sg %<*> sFrom1 sx)

class (PApplicative f, SApplicative f) => VApplicative f where
  applicativeHomomorphism         :: forall a b (g :: a ~> b) (x :: a).
                                     Sing g -> Sing x
                                  -> (Pure g <*> Pure x) :~: (Pure (g `Apply` x) :: f b)
  default applicativeHomomorphism :: forall a b (g :: a ~> b) (x :: a).
                                     ( VGeneric1 f, VApplicative (Rep1 f)
                                     , GenericPureC f g, GenericPureC f x, GenericPureC f (g `Apply` x)
                                     , GenericApC (Pure g :: f (a ~> b)) (Pure x :: f a)
                                     )
                                  => Sing g -> Sing x
                                  -> (Pure g <*> Pure x) :~: (Pure (g `Apply` x) :: f b)
  applicativeHomomorphism sg sx
    | Refl <- sFot1 @Type @f (sPure sg)
    , Refl <- sFot1 @Type @f (sPure sx)
    , Refl <- applicativeHomomorphism @(Rep1 f) sg sx
    , Refl <- sFot1 @Type @f pureGX
    , Refl <- sTof1 @Type @f (sTo1 pureGX)
    = Refl
    where
      pureGX :: Sing (Pure (Apply g x) :: Rep1 f b)
      pureGX = sPure (sg @@ sx)

instance PApplicative (K1 i c) where
  type Pure _ = 'K1 Mempty
  type 'K1 x <*> 'K1 y = 'K1 (x <> y)

instance SMonoid c => SApplicative (K1 i c) where
  sPure _ = SK1 sMempty
  SK1 x %<*> SK1 y = SK1 (x %<> y)

instance VMonoid c => VApplicative (K1 i c) where
  applicativeHomomorphism _ _ | Refl <- monoidLeftIdentity (sMempty @c) = Refl

instance PApplicative (M1 i c f) where
  type Pure x = 'M1 (Pure x)
  type 'M1 ff <*> 'M1 x = 'M1 (ff <*> x)

instance VApplicative f => VApplicative (M1 i c f) where
  applicativeHomomorphism sg sx
    | Refl <- applicativeHomomorphism @f sg sx
    = Refl

instance PApplicative (f :*: g) where
  type Pure a = Pure a ':*: Pure a
  type (ff ':*: gg) <*> (x ':*: y) = (ff <*> x) ':*: (gg <*> y)

instance (SApplicative f, SApplicative g) => SApplicative (f :*: g) where
  sPure a = sPure a :%*: sPure a
  (f :%*: g) %<*> (x :%*: y) = (f %<*> x) :%*: (g %<*> y)

instance (VApplicative f, VApplicative g) => VApplicative (f :*: g) where
  applicativeHomomorphism sg sx
    | Refl <- applicativeHomomorphism @f sg sx
    , Refl <- applicativeHomomorphism @g sg sx
    = Refl

instance PApplicative Par1 where
  type Pure x = 'Par1 x
  type 'Par1 f <*> 'Par1 x = 'Par1 (f @@ x)

instance SApplicative Par1 where
  sPure = SPar1
  SPar1 f %<*> SPar1 x = SPar1 (f @@ x)

instance VApplicative Par1 where
  applicativeHomomorphism _ _ = Refl
