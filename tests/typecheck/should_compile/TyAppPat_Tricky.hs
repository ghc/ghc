{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- Test a few things at once:
-- 1. Multiple foralls (syntactically)
-- 2. Variables with "inferred" specificity
-- 3. Existentials bound before universals (e1)
-- 4. Universals bound out of order (u4, u3, u1, u2)
-- 5. Interspersed existentials and universals
-- 6. Constraints
data Foo u1 u2 u3 u4 where
  MkFoo :: forall e1.
           forall u4 {u3}.
           forall e2 e3.
           forall u1 e4 u2.
           (e1 ~ u1, e2 ~ u2, e3 ~ u3, e4 ~ u4) =>
           e1 ->
           e2 ->
           e3 ->
           e4 ->
           Foo u1 u2 u3 u4

foo :: Foo u1 u2 u3 u4 -> (u1, u2, u3, u4)
foo (MkFoo @e1 @_ @e2 @e3 @_ @e4 x1 x2 x3 x4) = (x1 :: e1, x2 :: e2, x3 :: e3, x4 :: e4)

main = print (foo (MkFoo 1 'x' True ()))
