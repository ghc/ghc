import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  prop_sliceU :: (Eq a, UA a) => UArr a -> Len -> Len -> Property
  prop_sliceU arr (Len i) (Len n) =
    i <= lengthU arr && n <= lengthU arr - i
    ==> fromU (sliceU arr i n) == take n (drop i $ fromU arr)
  
  prop_extractU :: (Eq a, UA a) => UArr a -> Len -> Len -> Property
  prop_extractU arr (Len i) (Len n) =
    i <= lengthU arr && n <= lengthU arr - i
    ==> fromU (extractU arr i n) == take n (drop i $ fromU arr)
  
  prop_takeU :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_takeU (Len n) arr =
    n <= lengthU arr
    ==> fromU (takeU n arr) == take n (fromU arr)
  
  prop_dropU :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_dropU (Len n) arr =
    n <= lengthU arr
    ==> fromU (dropU n arr) == drop n (fromU arr)
  
  prop_splitAtU :: (Eq a, UA a) => Len -> UArr a -> Property
  prop_splitAtU (Len n) arr =
    n <= lengthU arr
    ==> let (brr, crr) = splitAtU n arr
        in (fromU brr, fromU crr) == splitAt n (fromU arr)
  |])

