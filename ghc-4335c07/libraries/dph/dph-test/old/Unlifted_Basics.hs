import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  -- if this doesn't work nothing else will, so run this first
  prop_fromU_toU :: (Eq a, UA a) => [a] -> Bool
  prop_fromU_toU xs = fromU (toU xs) == xs

  prop_lengthU :: UA a => UArr a -> Bool
  prop_lengthU arr = lengthU arr  == length (fromU arr)
  
  prop_nullU :: UA a => UArr a -> Bool
  prop_nullU arr = nullU arr == (lengthU arr == 0)
  
  prop_emptyU :: (Eq a, UA a) => a -> Bool
  prop_emptyU x = fromU emptyU == tail [x]

  prop_unitsU :: Len -> Bool
  prop_unitsU (Len n) =
    fromU (unitsU n) == replicate n ()

  prop_replicateU :: (Eq a, UA a) => Len -> a -> Bool
  prop_replicateU (Len n) x =
    fromU (replicateU n x) == replicate n x

  prop_indexU :: (Eq a, UA a) => UArr a -> Len -> Property
  prop_indexU arr (Len i) =
    i < lengthU arr
    ==> (arr !: i) == (fromU arr !! i)

  prop_appendU :: (Eq a, UA a) => UArr a -> UArr a -> Bool
  prop_appendU arr brr =
    fromU (arr +:+ brr) == fromU arr ++ fromU brr

  -- Equality
  -- --------

  prop_eqU_1 :: (Eq a, UA a) => UArr a -> Bool
  prop_eqU_1 arr = arr == arr

  prop_eqU_2 :: (Eq a, UA a) => UArr a -> UArr a -> Bool
  prop_eqU_2 arr brr = (arr == brr) == (fromU arr == fromU brr)
  |])

