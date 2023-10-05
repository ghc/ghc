{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies, GADTs, EmptyCase, LambdaCase #-}

-- Check interaction between Newtypes and DataFamilies
module EmptyCase008 where

import Data.Kind (Type)

data family DA a

newtype Foo3 a = Foo3 (DA a)

data instance DA Int = MkDA1 Char | MkDA2

-- Non-exhaustive. Missing: MkDA1 Char, MkDA2
f11 :: Foo3 Int -> ()
f11 = \case

-- Non-exhaustive. (no info about a)
f12 :: Foo3 a -> ()
f12 = \case

data instance DA () -- Empty data type

-- Exhaustive.
f13 :: Foo3 () -> ()
f13 = \case

-- ----------------
data family DB a :: Type -> Type

data instance DB Int a where
  MkDB1 :: DB Int ()
  MkDB2 :: DB Int Bool

newtype Foo4 a b = Foo4 (DB a b)

-- Non-exhaustive. Missing: Foo4 MkDB1
f14 :: Foo4 Int () -> ()
f14 = \case

-- Exhaustive
f15 :: Foo4 Int [a] -> ()
f15 = \case

-- Non-exhaustive. Missing: (_ :: Foo4 a b) (no information about a or b)
f16 :: Foo4 a b -> ()
f16 = \case

data instance DB Char Bool -- Empty data type

-- Exhaustive (empty data type)
f17 :: Foo4 Char Bool -> ()
f17 = \case
