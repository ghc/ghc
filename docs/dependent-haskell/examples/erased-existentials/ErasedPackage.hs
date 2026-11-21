{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes #-}
{-# LANGUAGE RequiredTypeArguments, ScopedTypeVariables #-}

-- | Observe payloads whose type witness is erased.
module ErasedPackage
  ( observation, EncodedCount, encodeCount, MixedNumber, packNumber, readMixedNumber, readNumber
  , readLabelledNumber, Filtered, filterVec, cells, filterObservation
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

type LabelledNumber a label = exists (b :: Type) | (a, b, label, Int)
type NumberPackage a = LabelledNumber a String

-- | Hide the remaining type of an explicitly packed number.
type MixedNumber = exists (a :: Type). NumberPackage a

-- | Introduce the explicit witness while keeping the outer type visible.
packNumber :: Int -> NumberPackage Bool
packNumber n = Pack (type Char) (True, 'x', "count", n)

-- | Open both packages and read the number.
readMixedNumber :: MixedNumber -> Int
readMixedNumber p = readNumber p

-- | Read the number without exposing either witness in the result.
readNumber :: forall a. NumberPackage a -> Int
readNumber q = snd (readLabelledNumber @a @String q)

-- | Read the public label and number from the package.
readLabelledNumber :: forall a label. LabelledNumber a label -> (label, Int)
readLabelledNumber q = case q of
  Pack (type b) (_, _ :: b, label, n) -> (label, n)

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

-- | A state and a reader that consumes that state once.
type EncodedCount = exists a. (a, a %1 -> Int)

-- | Store a Boolean flag or an integer with its own reader.
encodeCount :: Either Bool Int -> EncodedCount
encodeCount (Left flag) = (flag, fromFlag)
  where
    fromFlag :: Bool %1 -> Int
    fromFlag False = 0
    fromFlag True = 1
encodeCount (Right count) = (count, \n -> n)
