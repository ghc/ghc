{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module T17139 where

import T17139a
import Data.Kind

type family TypeFam f fun where
  TypeFam f ((a :: Type) -> (b :: Type)) = f a -> TypeFam f b

lift :: (a -> b) -> TypeFam f (a -> b)
lift f = \x -> _ (f <*> x)


{-
x              :: alpha
body of lambda :: beta

[W] TypeFam f (a->b) ~ (alpha -> beta)
-->
[W] (f a -> TypeFam f b) ~ (alpha -> beta)
-->
 alpha := f a
 beta  := TypeFam f b

(<*>) :: Applicative g => g (p -> q) -> g p -> g q

f <*> x

arg1
  (a->b) ~ g0 (p0->q0)
  g0 := ((->) a)
  (p0 -> q0) ~ b   <---------
arg2
  alpha ~ g0 p0
  g0 ~ f          <----------
  p0 := a
res
  g0 q0

Finish with
 [W] f ~ (->) a
 [W] b ~ (a -> q0)
 --> rewrite b
 [W] (a -> q0) ~ a -> (

_ :: g0 q0 -> beta
  :: (a -> q0) -> TypeFam f b
  :: (a -> q0) -> TypeFam ((->) a) (a -> q0)
  :: (a -> q0) -> (a->a) -> TypeFam (-> a) q0

BUT we would get different error messages if we did
   g0 := f
and then encountered [W] g0 ~ ((->) a)
-}