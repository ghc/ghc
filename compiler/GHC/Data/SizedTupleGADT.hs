{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Small data type for storing information related to MachOp args,
-- along with type-level information about the arity of the MachOp
module GHC.Data.SizedTupleGADT (
  SizedTupleGADT(..),
  Natural,
  firstOfTupleGADT,
  sizeOfTupleGADT,
  mapSizedTupleGADT_maybe,
  listToSizedTupleGADT_maybe,
) where

import Prelude

import Data.Functor.Classes
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats

data SizedTupleGADT (numElements :: Natural) (a :: Type) where
  TupleG0 :: SizedTupleGADT 0 a
  TupleG1 :: a -> SizedTupleGADT 1 a
  TupleG2 :: a -> a -> SizedTupleGADT 2 a
  TupleG3 :: a -> a -> a -> SizedTupleGADT 3 a

deriving instance Eq a => Eq (SizedTupleGADT n a)
deriving instance Ord a => Ord (SizedTupleGADT n a)
deriving instance Show a => Show (SizedTupleGADT n a)
deriving instance Functor (SizedTupleGADT n)
deriving instance Foldable (SizedTupleGADT n)
deriving instance Traversable (SizedTupleGADT n)

instance Eq1 (SizedTupleGADT n) where
  liftEq f xs ys = case (xs, ys) of
    (TupleG0, TupleG0) -> True
    (TupleG1 x1      , TupleG1 y1)
      -> f x1 y1
    (TupleG2 x1 x2   , TupleG2 y1 y2)
      -> f x1 y1 && f x2 y2
    (TupleG3 x1 x2 x3, TupleG3 y1 y2 y3)
      -> f x1 y1 && f x2 y2 && f x3 y3

firstOfTupleGADT :: (numElements > 0) => SizedTupleGADT numElements a -> a
firstOfTupleGADT = \case
  TupleG1 x -> x
  TupleG2 x _ -> x
  TupleG3 x _ _ -> x

sizeOfTupleGADT :: SizedTupleGADT numElements a -> SNat numElements
sizeOfTupleGADT = \case
  TupleG0 {} -> SNat
  TupleG1 {} -> SNat
  TupleG2 {} -> SNat
  TupleG3 {} -> SNat

-- | Treats returning @Nothing@ as meaning "Leave this element unchanged."
-- (Returns @Nothing@ itself if all elements are left unchanged.)
mapSizedTupleGADT_maybe :: (a -> Maybe a) -> SizedTupleGADT n a -> Maybe (SizedTupleGADT n a)
mapSizedTupleGADT_maybe f xs = case xs of
  TupleG0 -> Nothing
  TupleG1 x -> case f x of
    Nothing -> Nothing
    Just y  -> Just (TupleG1 y)
  TupleG2 x1 x2 -> case (f x1, f x2) of
    (Nothing, Nothing)
      -> Nothing
    (fromMaybe x1 -> y1, fromMaybe x2 -> y2)
      -> Just (TupleG2 y1 y2)
  TupleG3 x1 x2 x3 -> case (f x1, f x2, f x3) of
    (Nothing, Nothing, Nothing)
      -> Nothing
    (fromMaybe x1 -> y1, fromMaybe x2 -> y2, fromMaybe x3 -> y3)
      -> Just (TupleG3 y1 y2 y3)

listToSizedTupleGADT_maybe :: forall (n :: Natural) (a :: Type). (KnownNat n, n < 4) => [a] -> Maybe (SizedTupleGADT n a)
listToSizedTupleGADT_maybe li
  | Just Refl <- sameNat @n @0 Proxy Proxy
  , [] <- li
  = Just TupleG0

  | Just Refl <- sameNat @n @1 Proxy Proxy
  , [x1] <- li
  = Just $ TupleG1 x1

  | Just Refl <- sameNat @n @2 Proxy Proxy
  , [x1,x2] <- li
  = Just $ TupleG2 x1 x2

  | Just Refl <- sameNat @n @3 Proxy Proxy
  , [x1,x2,x3] <- li
  = Just $ TupleG3 x1 x2 x3

  | otherwise
  = Nothing
