
import DPH.Testsuite
import DPH.Arbitrary.Int
import Data.Array.Parallel.Unlifted as U
import Prelude as P
import System.Random as R ( Random, StdGen, randoms, randomRs )

(!:) = index "test:Basics"


$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            , "acc"     <@ [t| ( Int       ) |]
            , "num"     <@ [t| ( Int       ) |]
            , "ord"     <@ [t| ( Bool, Int ) |]
            , "enum"    <@ [t| ( Bool, Int ) |]
            ]
  [d|
  -- if this doesn't work nothing else will, so run this first
  prop_toList_fromList :: (Eq a, Elt a) => [a] -> Bool
  prop_toList_fromList xs = toList (fromList xs) == xs

  prop_length :: Elt a => Array a -> Bool
  prop_length arr = U.length arr  == P.length (toList arr)
  
  --prop_nullU :: UA a => UArr a -> Bool
  --prop_nullU arr = nullU arr == (lengthU arr == 0)
  
  prop_empty :: (Eq a, Elt a) => a -> Bool
  prop_empty x = toList empty == tail [x]

  --prop_unitsU :: Len -> Bool
  --prop_unitsU (Len n) =
  --  toList (unitsU n) == replicate n ()

  prop_replicate :: (Eq a, Elt a) => Len -> a -> Bool
  prop_replicate (Len n) x =
    toList (U.replicate n x) == P.replicate n x

  prop_repeat :: (Eq a, Elt a) => Len -> Len -> Array a -> Bool
  prop_repeat (Len n) (Len dummy) arr =
    toList (U.repeat n dummy arr) == (P.concat $ P.replicate n (toList arr))

  prop_interleave :: (Eq a, Elt a) => Array a -> Array a -> Bool
  prop_interleave arr brr =
    toList (U.interleave arr brr) == interleave (toList arr) (toList brr)
      where interleave (x:xs) (y:ys) = x : y : (interleave xs ys)
            interleave (x:_)  _      = [x]
            interleave _      _      = []

  prop_index :: (Eq a, Elt a) => Array a -> Len -> Property
  prop_index arr (Len i) =
    i < U.length arr
    ==> (arr !: i) == (toList arr !! i)

  prop_append :: (Eq a, Elt a) => Array a -> Array a -> Bool
  prop_append arr brr =
    toList (arr +:+ brr) == toList arr ++ toList brr

  prop_indexed :: (Eq a, Elt a) => Array a -> Bool
  prop_indexed arr =
    toList (indexed arr) == P.zip [0..U.length arr - 1] (toList arr)

  -- Equality
  -- --------

  prop_eqU_1 :: (Eq a, Elt a) => Array a -> Bool
  prop_eqU_1 arr = arr == arr

  prop_eqU_2 :: (Eq a, Elt a) => Array a -> Array a -> Bool
  prop_eqU_2 arr brr = (arr == brr) == (toList arr == toList brr)

  -- Randoms
  -- -------

  prop_randoms :: (Random a, Eq a, Elt a) => Proxy [a] -> Len -> StdGen -> Bool
  prop_randoms dummy (Len len) gen =
    toList (U.randoms len gen) == take len (R.randoms gen) `asProxyTypeOf` dummy

  prop_randomRs :: (Random a, Eq a, Elt a) => Proxy [a] -> Len -> (a, a) -> StdGen -> Bool
  prop_randomRs dummy (Len len) range gen =
    toList (U.randomRs len range gen) == take len (R.randomRs range gen) `asProxyTypeOf` dummy

  |])

