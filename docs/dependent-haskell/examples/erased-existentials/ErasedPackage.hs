{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -O0 #-}

-- | Observe payloads whose type witness is erased.
module ErasedPackage (observation, Filtered, filterVec, cells, filterObservation) where

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

-- | A vector whose result length is known only to its producer.
type Filtered a = exists (m :: Nat) | Vec a m

-- | Return passing elements in order. The result hides its length.
filterVec :: (a -> Bool) -> Vec a n -> Filtered a
filterVec _ VNil = Pack (type 'Z) VNil
filterVec p (VCons x xs)
  | p x = case filterVec p xs of
      Pack (type m) ys -> Pack (type ('S m)) (VCons x ys)
  | otherwise = filterVec p xs

-- | Read the result without recovering a runtime length.
cells :: Filtered a -> [a]
cells xs = case xs of
  Pack (type m) ys -> toList ys

-- | Observe one passing element before inspecting the input tail.
filterObservation :: ([Int], [Int])
filterObservation =
  ( take 1 (cells (filterVec odd (VCons 7 (error "unseen input tail"))))
  , cells (filterVec odd (VCons 2 (VCons 7 (VCons 9 VNil))))
  )
