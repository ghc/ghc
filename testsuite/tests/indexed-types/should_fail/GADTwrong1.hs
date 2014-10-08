{-# LANGUAGE TypeFamilies, GADTs, RankNTypes, ScopedTypeVariables #-}

module ShouldFail where

type family Const a
type instance Const a = ()

data T a where T :: c -> T (Const c)

coerce :: forall a b . a -> b
coerce x = case T x :: T (Const b) of
             T y -> y

{-
  T :: forall a. forall c. (a ~ Const c) => c -> T a

  a ~ gamma        -- Instantiate T with a=alpha, c=gamma
  alpha ~ Const b  -- Result of (T x)
  alpha ~ Const gamma  -- Constraint from (T x)

  y::c
  forall c. (Const b ~ Const c) => c ~ b

==> 
  Const b ~ Const a

------------

case e of 
  T y -> y

  e :: T alpha

  Patterns
   forall c. (alpha ~ Const c) => c ~ b
   alpha ~ Const b

-}  