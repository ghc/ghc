{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies, GADTs, EmptyCase, LambdaCase #-}

-- Arrow Kind, Newtypes, GADTs, DataFamilies
module EmptyCase009 where

import Data.Kind (Type)

data family DB a :: Type -> Type

data instance DB Int a where
  MkDB1 :: DB Int ()
  MkDB2 :: DB Int Bool

data instance DB Char Bool -- Empty data type

newtype Bar f = Bar (f Int)

-- Non-exhaustive. Missing: (_ :: Bar f)
f17 :: Bar f -> ()
f17 x = case x of {}

-- Exhaustive (Bar (DB Int) ~ DB Int Int, incompatible with both MkDB1 & MkDB2)
f18 :: Bar (DB Int) -> ()
f18 x = case x of {}

data instance DB () a where
  MkDB1_u :: DB () ()
  MkDB2_u :: DB () Int

-- Non-exhaustive. Missing: Bar MkDB2_u
f19 :: Bar (DB ()) -> ()
f19 = \case

data GB :: Type -> Type where
  MkGB1 :: Int -> GB ()
  MkGB2 :: GB (a,a)
  MkGB3 :: GB b

-- Non-exhaustive. Missing: Bar MkGB3
f20 :: Bar GB -> ()
f20 = \case
