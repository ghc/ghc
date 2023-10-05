{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module T13848 where

data N = Z | S N

data Vec1 (n :: N) a where
  VNil1  :: forall a. Vec1 Z a
  VCons1 :: forall n a. a -> Vec1 n a -> Vec1 (S n) a

data Vec2 (n :: N) a where
  VNil2  :: Vec2 Z a
  VCons2 :: a -> Vec2 n a -> Vec2 (S n) a

data Vec3 (n :: N) a
  = (n ~ Z) => VNil3
  | forall (n' :: N). (n ~ S n') => VCons3 a (Vec3 n' a)

vcons1 :: Vec1 (S Z) Int
vcons1 = VCons1 @Z @Int 1 (VNil1 @Int)

vcons2 :: Vec2 (S Z) Int
vcons2 = VCons2 @Int @Z 1 (VNil2 @Int)

vcons3 :: Vec3 (S Z) Int
vcons3 = VCons3 @(S Z) @Int @Z 1 (VNil3 @Z @Int)

newtype Result1 a s = Ok1 s

newtype Result2 a s where
  Ok2 :: s -> Result2 a s

result1 :: Result1 Int Char
result1 = Ok1 @Int @Char 'a'

result2 :: Result2 Int Char
result2 = Ok2 @Char @Int 'a'
