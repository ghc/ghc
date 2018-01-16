import DPH.Testsuite
import DPH.Arbitrary.Int
import Data.Array.Parallel.Unlifted as U
import Prelude as P

$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            , "acc"     <@ [t| ( Int       ) |]
            , "num"     <@ [t| ( Int       ) |]
            , "ord"     <@ [t| ( Bool, Int ) |]
            , "enum"    <@ [t| ( Bool, Int ) |]
            ]
  [d|
  prop_extract :: (Eq a, Elt a) => Array a -> Len -> Len -> Property
  prop_extract arr (Len i) (Len n) =
    i <= U.length arr && n <= U.length arr - i
    ==> toList (U.extract arr i n) == take n (P.drop i $ toList arr)
  
  prop_drop :: (Eq a, Elt a) => Len -> Array a -> Property
  prop_drop (Len n) arr =
    n <= U.length arr
    ==> toList (U.drop n arr) == P.drop n (toList arr)
  |])

