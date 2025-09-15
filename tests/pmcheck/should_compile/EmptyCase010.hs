{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds
           , EmptyCase, LambdaCase #-}

-- Newtypes, PolyKinds, DataKinds, GADTs, DataFamilies
module EmptyCase010 where

import Data.Kind (Type)

newtype Baz (f :: k -> Type) (a :: k) = Baz (f a)

data T = T1 | T2 T | T3 T T | T4 () -- only promoted

data GC :: T -> Type where
  MkGC1 :: GC 'T1
  MkGC2 :: T -> GC (T4 '())

-- Exhaustive: GC ('T2 'T1) is not strictly inhabited
f21 :: Baz GC ('T2 'T1) -> ()
f21 = \case

-- Non-exhaustive. Missing: Baz MkGC1, Baz (MkGC2 _)
f22 :: Baz GC a -> ()
f22 = \case

-- Non-exhaustive. Missing: Baz MkGC1
f23 :: Baz GC 'T1 -> ()
f23 = \case

data GD :: (Type -> Type) -> Type where
  MkGD1 :: GD Maybe
  MkGD2 :: GD []
  MkGD3 :: GD f

-- Non-exhaustive. Missing: Baz MkGD1, Baz MkGD3
f24 :: Baz GD Maybe -> ()
f24 = \case

-- Non-exhaustive. Missing: Baz MkGD3
f25 :: Baz GD (Either Int) -> ()
f25 x = case x of {}

-- Non-exhaustive. Missing: Baz MkGD1, Baz MkGD2, Baz MkGD3
f26 :: Baz GD f -> ()
f26 = \case

data family DC a :: Type -> Type

data instance DC () Int -- Empty type

-- Exhaustive
f27 :: Baz (DC ()) Int -> ()
f27 = \case

-- Non-exhaustive. Missing: _ :: Baz (DC ()) a (a is unknown)
f28 :: Baz (DC ()) a -> ()
f28 = \case

data instance DC Bool a where
  MkDC1 :: DC Bool Int
  MkDC2 :: DC Bool [a]

-- Exhaustive. (DC Bool Char) is not strictly inhabited
f29 :: Baz (DC Bool) Char -> ()
f29 = \case

-- Non-exhaustive. Missing: Baz MkDC2
f30 :: Baz (DC Bool) [Int] -> ()
f30 = \case

-- Non-exhaustive. Missing: Baz f a (a and f unknown (and the kind too))
f31 :: Baz f a -> ()
f31 x = case x of {}
