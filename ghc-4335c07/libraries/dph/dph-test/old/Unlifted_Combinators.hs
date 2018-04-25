import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  prop_mapU :: (UA a, Eq b, UA b) => (a -> b) -> UArr a -> Bool
  prop_mapU f arr =
    fromU (mapU f arr) == map f (fromU arr)

  -- missing: zipWithU
  -- missing: zipWith3U
  
  prop_filterU :: (Eq a, UA a) => (a -> Bool) -> UArr a -> Bool
  prop_filterU f arr =
    fromU (filterU f arr) == filter f (fromU arr)

  prop_foldlU :: (UA a, Eq b) => (b -> a -> b) -> b -> UArr a -> Bool
  prop_foldlU f z arr =
    foldlU f z arr == foldl f z (fromU arr)

  prop_foldl1U :: (UA a, Eq a) => (a -> a -> a) -> UArr a -> Property
  prop_foldl1U f arr =
    not (nullU arr)
    ==> foldl1U f arr == foldl1 f (fromU arr)

  -- missing: foldU
  -- missing: fold1U

  prop_scanlU :: (UA a, UA b, Eq b) => (b -> a -> b) -> b -> UArr a -> Bool
  prop_scanlU f z arr =
    fromU (scanlU f z arr) == init (scanl f z (fromU arr))

  prop_scanl1U :: (UA a, Eq a) => (a -> a -> a) -> UArr a -> Property
  prop_scanl1U f arr =
    not (nullU arr)
    ==> fromU (scanl1U f arr) == init (scanl1 f (fromU arr))

  -- missing: scanU
  -- missing: scan1U
  -- missing: loopU
  |])

