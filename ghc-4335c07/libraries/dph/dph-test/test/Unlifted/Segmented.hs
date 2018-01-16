import DPH.Testsuite
import DPH.Arbitrary.Segd
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
    -- replicate each element such that the resulting array such corresponds to
    -- the given segment descriptor e.g.:
    -- replicate_s [1,3,1] [1,2,3] = [1,2,2,2,3]
    prop_replicate_s :: (Eq a, Elt a) => Array Int -> Array a -> Bool
    prop_replicate_s lens arr =
      toList (replicate_s segd arr)
      == P.concat (P.zipWith P.replicate (toList lens') (toList arr'))
      where segd = mkSegd lens' (U.scan (+) 0 lens') (U.sum lens')
            -- make the arrays equal length
            (lens', arr') = U.unzip $ U.zip (U.map (`mod` maxRepl) lens) arr
            maxRepl = 100

    -- replicate each element a given number of times
    prop_replicate_rs :: (Eq a, Elt a) => SizedInt -> Array a -> Bool
    prop_replicate_rs (SizedInt n) arr =
      toList (replicate_rs n' arr) == P.concat (P.map (P.replicate n') arr')
      where n'   = abs n
            arr' = toList arr

    -- compute sum of each segment
    prop_sum_s :: (Eq num, Num num, Elt num) => Array num -> Property
    prop_sum_s arr = forAll (segdForArray arr) $ \segd ->
      let lens' = toList $ lengthsSegd segd
          arr'  = toList $ arr
      in toList (sum_s segd arr) == P.map P.sum (nest lens' arr')

    -- compute sum of each (complete) sequence of @n@ elements
    prop_sum_r :: (Eq num, Num num, Elt num) => SizedInt -> Array num -> Property
    prop_sum_r (SizedInt n) arr =
      n > 0 -- other cases are not (yet) handled
      ==> toList (sum_r n arr) == P.map P.sum (arr' `nestBy` n)
      where xs `nestBy` n = nest (P.replicate nsegm n) xs
            nsegm = (P.length arr') `div` n
            arr'  = toList arr

    -- interleaves the segments of two arrays, e.g.:
    -- append_s (Segd [1,1,2,1]) (Segd [1,2]) [1,3,4] (Segd [1,1]) [2,5]
    --   = [1,2,3,4,5]
    prop_append_s :: (Eq a, Elt a) => Array a -> Array a -> Property
    prop_append_s arr brr =
      forAll (segdForArray arr) $ \segd1 ->
      forAll (segdForArray brr) $ \segd2 ->
      let lens1  = lengthsSegd segd1
          lens2  = lengthsSegd segd2
          lens1' = toList lens1
          lens2' = toList lens2
          arr'   = toList arr
          brr'   = toList brr
      in 
         P.length lens1' == P.length lens2' ==>
         toList (append_s (segdFrom lens1 lens2) segd1 arr segd2 brr) ==
         concat (interleave (nest lens1' arr') (nest lens2' brr'))
      where segdFrom lens1 lens2 = lengthsToSegd $ U.interleave lens1 lens2
            lengthsToSegd lens = mkSegd lens (scan (+) 0 lens) (U.sum lens)
--            interleave :: [a] -> [a] -> [a]
            interleave (x : xs) (y : ys) = x : y : interleave xs ys
            interleave (x : xs) _        = [x]
            interleave _        _        = []

    -- indexes each segment and returns the concatenation of those, e.g.:
    -- indices_s (Segd [2,0,3]) [10,10,20,20,20] = [0,1,0,1,2]
    prop_indices_s :: Segd -> Bool
    prop_indices_s segd =
      toList (indices_s segd) == concat [[0 .. n-1] | n <- lens]
      where lens = toList $ lengthsSegd segd

    -- counts the occurences of a given element in each segment
    prop_count_s :: (Eq a, Elt a) => Array a -> a -> Property
    prop_count_s arr x = forAll (segdForArray arr) $ \segd ->
      let lens' = toList $ lengthsSegd segd
          arr'  = toList arr
      in toList (count_s segd arr x) == P.map (`count` x) (nest lens' arr')
      where count xs x = P.length $ P.filter (== x) xs
  |])

