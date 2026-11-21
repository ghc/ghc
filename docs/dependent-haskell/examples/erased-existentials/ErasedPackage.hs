{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE RequiredTypeArguments, ScopedTypeVariables #-}

-- | Observe payloads whose type witness is erased.
module ErasedPackage
  ( observation, MixedNumber, packNumber, readMixedNumber
  , Filtered, filterVec, cells, filterObservation
  ) where

import Data.Kind (Type)
import GHC.Exists
import PackedVecLibrary (Nat(..), Vec(..), toList)

type Hidden = exists (a :: Type) | Maybe a

present :: Hidden -> Bool
present package = case package of
  Pack (type a) payload ->
    case payload of
      Nothing -> False
      Just _ -> True

-- | Open two defined packages and ignore an exceptional package.
observation :: (Bool, Bool, Bool)
observation =
  ( present (Pack (type Bool) (Just True))
  , present (Pack (type Char) Nothing)
  , case error "unused package" :: Hidden of
      Pack (type a) _ -> True
  )

type NumberPackage a = exists (b :: Type) | (a, b, Int)

-- | Hide the remaining type of an explicitly packed number.
type MixedNumber = exists (a :: Type). NumberPackage a

-- | Introduce the explicit witness while keeping the outer type visible.
packNumber :: Int -> NumberPackage Bool
packNumber n = Pack (type Char) (True, 'x', n)

-- | Open both packages and read the number.
readMixedNumber :: MixedNumber -> Int
readMixedNumber p = readNumber p
  where
    readNumber :: forall a. NumberPackage a -> Int
    readNumber q = case q of
      Pack (type b) (_, _ :: b, n) -> n

-- | A vector whose result length is known only to its producer.
type Filtered a = exists (m :: Nat) | Vec a m

-- | Return passing elements in order. The result hides its length.
filterVec :: (a -> Bool) -> Vec a n -> Filtered a
filterVec _ VNil = Pack (type 'Z) VNil
filterVec p (VCons x xs)
  | p x = case filterVec p xs of
      Pack _ ys -> Pack _ (VCons x ys)
  | otherwise = filterVec p xs

-- | Read the result without recovering a runtime length.
cells :: Filtered a -> [a]
cells xs = case xs of
  Pack (type _) ys -> toList ys

-- | Observe one passing element before inspecting the input tail.
filterObservation :: ([Int], [Int])
filterObservation =
  ( take 1 (cells (filterVec odd (VCons 7 (error "unseen input tail"))))
  , cells (filterVec odd (VCons 2 (VCons 7 (VCons 9 VNil))))
  )
