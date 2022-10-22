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

module DCo_T15703_aux where

import Data.Kind
import Data.Type.Equality
import GHC.Generics

data family Sing :: forall k. k -> Type
data instance Sing :: forall i k c (p :: k). K1 i c p -> Type where
  SK1 :: Sing x -> Sing ('K1 x)
data instance Sing :: forall k i (c :: Meta) (f :: k -> Type) (p :: k).
                      M1 i c f p -> Type where
  SM1 :: Sing x -> Sing ('M1 x)

data instance Sing :: forall k (f :: k -> Type) (g :: k -> Type) (p :: k).
                      (f :*: g) p -> Type where
  (:%*:) :: Sing x -> Sing y -> Sing (x ':*: y)

data instance Sing :: forall p. Par1 p -> Type where
  SPar1 :: Sing x -> Sing ('Par1 x)

class PGeneric1 (f :: k -> Type) where
  type From1 (z :: f a)      :: Rep1 f a
  type To1   (z :: Rep1 f a) :: f a

class VGeneric1 (f :: k -> Type) where
  sFot1 :: forall (a :: k) (r :: Rep1 f a). Sing r -> From1 (To1 r :: f a) :~: r

instance PGeneric1 ((,) a) where
  type From1 '(x, y) = 'M1 ('M1 ('M1 ('K1 x) ':*: 'M1 ('Par1 y)))
  type To1   ('M1 ('M1 ('M1 ('K1 x) ':*: 'M1 ('Par1 y)))) = '(x, y)

instance VGeneric1 ((,) a) where
  sFot1 (SM1 (SM1 (SM1 SK1{} :%*: SM1 SPar1{}))) = Refl
