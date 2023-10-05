{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, KindSignatures, EmptyCase, LambdaCase #-}

-- Check some GADTs
module EmptyCase004 where

import Data.Kind (Type)

data A :: Type -> Type where
  A1 :: A Int
  A2 :: A Bool

-- Non-exhaustive: Missing A2
f1 :: A Bool -> a
f1 = \case

-- Non-exhaustive: missing both A1 & A2
f2 :: A a -> b
f2 = \case

-- Exhaustive
f3 :: A [a] -> b
f3 = \case

data B :: Type -> Type -> Type where
  B1 :: Int -> B Bool Bool
  B2 ::        B Int  Bool

-- Non-exhaustive: missing (B1 _)
g1 :: B a a -> b
g1 x = case x of

-- Non-exhaustive: missing both (B1 _) & B2
g2 :: B a b -> c
g2 = \case

-- Exhaustive
g3 :: B Char a -> b
g3 = \case

-- NOTE: A lambda-case always has ONE scrutinee and a lambda case refers
-- always to the first of the arguments. Hence, the following warnings are
-- valid:

-- Non-exhaustive: Missing both A1 & A2
h1 :: A a -> A a -> b
h1 = \case

h2 :: A a -> B a b -> ()
h2 A1 = \case -- Non-exhaustive, missing B2
h2 A2 = \case -- Non-exhaustive, missing (B1 _)
