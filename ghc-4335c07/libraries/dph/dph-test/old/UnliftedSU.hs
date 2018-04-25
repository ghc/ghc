{-# OPTIONS -fallow-undecidable-instances #-}

import Testsuite

import Data.Array.Parallel.Unlifted

class    (Eq a, UA a) => U a
instance (Eq a, UA a) => U a



$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  -- if this doesn't work nothing else will, so run this first
  prop_fromSU_toSU :: U a => [[a]] -> Bool
  prop_fromSU_toSU xss = fromSU (toSU xss) == xss

  prop_concatSU :: U a => SUArr a -> SUArr a -> Bool
  prop_concatSU xss yss =
    (concatSU xss == concatSU yss)
    == (concat (fromSU xss) == concat (fromSU yss))

  prop_flattenSU :: U a => SUArr a -> SUArr a -> Bool
  prop_flattenSU xss yss =
    (xss == yss) == (flattenSU xss == flattenSU yss)

  -- missing: (>:)
  -- missing: segmentU

  prop_replicateSU :: U a => UArr (Int :*: a) -> Bool
  prop_replicateSU ps = let (ms :*: xs) = unzipU ps
                            ns          = mapU abs ms
                        in
    fromSU (replicateSU ns xs) == zipWith replicate (fromU ns) (fromU xs)

  prop_foldlSU :: (U a, U b) => (a -> b -> a) -> a -> SUArr b -> Bool
  prop_foldlSU f z xss =
    fromU (foldlSU f z xss) == map (foldl f z) (fromSU xss)

  -- missing: foldSU
  -- missing: loopSU

  prop_andSU :: SUArr Bool -> Bool
  prop_andSU bss =
    fromU (andSU bss) == map and (fromSU bss)

  prop_orSU :: SUArr Bool -> Bool
  prop_orSU bss =
    fromU (orSU bss) == map or (fromSU bss)

  prop_sumSU :: (U num, Num num) => SUArr num -> Bool
  prop_sumSU nss =
    fromU (sumSU nss) == map sum (fromSU nss)

  prop_productSU :: (U num, Num num) => SUArr num -> Bool
  prop_productSU nss =
    fromU (productSU nss) == map product (fromSU nss)

  -- missing: maximumSU
  -- missing: minimumSU

  -- missing: enumFromToSU
  -- missing: enumFromThenToSU
  
  -- missing: fusion rules
  |])

