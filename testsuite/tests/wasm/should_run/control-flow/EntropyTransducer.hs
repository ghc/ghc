module EntropyTransducer
  ( traceBits
  , rangeSelect
  )
where

-- Convert a sequence of events to a sequence of bits.
-- Also provide an inverse function that converts
-- a sequence of bits to an integer that lies in a
-- known range (for simulating `switch`).

import ControlTestMonad

import GHC.Utils.Panic

traceBits :: [Event a b] -> [Bool]
traceBits (Predicate _ b : events) = b : traceBits events
traceBits (Action _ : events) = traceBits events
traceBits (Switch _ (lo, hi) i : events) =
    inverseRangeSelect (lo, hi) i ++ traceBits events
traceBits [] = []


rangeSelect :: (Integer, Integer) -> [Bool] -> Maybe (Integer, [Bool])
rangeSelect (lo, limit) bits | lo == pred limit = Just (lo, bits)
rangeSelect _ [] = Nothing
rangeSelect (lo, limit) (bit : bits) =
    rangeSelect (if bit then (lo, mid) else (mid, limit)) bits
  where mid = (lo + limit) `div` 2

inverseRangeSelect :: (Integer, Integer) -> Integer -> [Bool]
inverseRangeSelect (lo, limit) i
    | lo == pred limit = if i == lo then [] else panic "fault in inverseRangeSelect"
    | otherwise = if i < mid then True : inverseRangeSelect (lo, mid) i
                  else False : inverseRangeSelect (mid, limit) i
  where mid = (lo + limit) `div` 2
