{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module T12444 where

data Nat = Zero | Succ Nat

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

type family (:+:) (a :: Nat) (b :: Nat) :: Nat where
  m :+: Zero = m
  m :+: (Succ n) = Succ (m :+: n)

foo :: SNat (Succ c) -> SNat b -> SNat (Succ (c :+: b))
foo _ x = x


{-
sadd :: ((Succ n1 :+: n) ~ Succ (n1 :+: n), (Succ n1) ~ m)
     => SNat m -> SNat n -> SNat (m :+: n)
sadd SZero n = n
-}

{- [G]  a ~ Succ c
        Succ c + b ~ Succ (c+b)
        a ~ Zero

-->
        Zero ~ Succ c            TyEq
        fsk1 ~ Succ fsk2         TyEq
        fsk1 ~ (Succ c) + b      FunEq
        fsk2 ~ c+b               FunEq
----
[W] b ~ a+b  --->   b ~ Succ (c+b)


Derived
       a ~ Succ c
       fsk1 ~ Succ fsk2
       b ~ Succ fsk2

work   (Succ c) + b ~ fsk1
       c+b ~ fsk2


-}

{-

[G] a ~ [b]
--> Derived shadow [D] a ~ [b]
    When adding this to the model
    don't kick out a derived shadow again!

[G] (a ~ b)  --> sc   a ~~ b, a ~# b
  Silly to then kick out (a~b) and (a~~b)
  and rewrite them to (a~a) and (a~~a)

Insoluble constraint does not halt solving;
indeed derived version stays. somehow
-}
