{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, KindSignatures, EmptyCase, LambdaCase #-}

-- Check interaction between Newtypes and GADTs
module EmptyCase006 where

import Data.Kind (Type)

data GA :: Type -> Type where
  MkGA1 :: GA Int
  MkGA2 :: GA a -> GA [a]
  MkGA3 :: GA (a,a)

newtype Foo1 a = Foo1 (GA a)

-- Non-exhaustive. Missing: Foo1 MkGA1
f01 :: Foo1 Int -> ()
f01 = \case

-- Exhaustive
f02 :: Foo1 (Int, Bool) -> ()
f02 = \case

-- Non-exhaustive. Missing: Foo1 MkGA1, Foo1 (MkGA2 _), Foo1 MkGA3
f03 :: Foo1 a -> ()
f03 = \case

-- Exhaustive
f04 :: Foo1 () -> ()
f04 = \case
