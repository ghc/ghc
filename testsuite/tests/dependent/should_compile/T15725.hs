{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T15725 where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import GHC.Exts (Any)

-----
-- The important bits
-----

type instance Meth (x :: Identity a) = GenericMeth x
instance SC Identity

-------------------------------------------------------------------------------

data family Sing :: forall k. k -> Type
data instance Sing :: forall a. Identity a -> Type where
  SIdentity :: Sing x -> Sing ('Identity x)

newtype Par1 p = Par1 p
data instance Sing :: forall p. Par1 p -> Type where
  SPar1 :: Sing x -> Sing ('Par1 x)

type family Rep1 (f :: Type -> Type) :: Type -> Type

class PGeneric1 (f :: Type -> Type) where
  type From1 (z :: f a)      :: Rep1 f a
  type To1   (z :: Rep1 f a) :: f a

class SGeneric1 (f :: Type -> Type) where
  sFrom1 :: forall (a :: Type) (z :: f a).      Sing z -> Sing (From1 z)
  sTo1   :: forall (a :: Type) (r :: Rep1 f a). Sing r -> Sing (To1 r :: f a)

type instance Rep1 Identity = Par1

instance PGeneric1 Identity where
  type From1 ('Identity x) = 'Par1 x
  type To1   ('Par1 x)     = 'Identity x

instance SGeneric1 Identity where
  sFrom1 (SIdentity x) = SPar1 x
  sTo1 (SPar1 x) = SIdentity x

type family GenericMeth (x :: f a) :: f a where
  GenericMeth x = To1 (Meth (From1 x))

type family Meth (x :: f a) :: f a

class SC f where
  sMeth         :: forall a (x :: f a).
                   Sing x -> Sing (Meth x)
  default sMeth :: forall a (x :: f a).
                   ( SGeneric1 f, SC (Rep1 f)
                   , Meth x ~ GenericMeth x
                   )
                => Sing x -> Sing (Meth x)
  sMeth sx = sTo1 (sMeth (sFrom1 sx))

  dummy :: f a -> ()
  dummy _ = ()

type instance Meth (x :: Par1 p) = x
instance SC Par1 where
  sMeth x = x
