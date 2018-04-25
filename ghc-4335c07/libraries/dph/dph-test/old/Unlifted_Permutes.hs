import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  -- missing: permuteU
  -- missing: bpermuteU
  -- missing: bpermuteDftU
  
  prop_reverseU :: (Eq a, UA a) => UArr a -> Bool
  prop_reverseU arr =
    fromU (reverseU arr) == reverse (fromU arr)
 |])

