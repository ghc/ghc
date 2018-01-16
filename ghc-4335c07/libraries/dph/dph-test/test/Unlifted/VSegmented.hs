import DPH.Testsuite
import DPH.Arbitrary.Segd
import DPH.Arbitrary.VSegd
import DPH.Arbitrary.Int
import Data.Array.Parallel.Unlifted as U
import Prelude as P

import qualified Data.Vector                             as V
import qualified Data.Array.Parallel.PArray.PData        as PD
import qualified Data.Array.Parallel.PArray.PData.Nested as PDN

import qualified Data.Array.Parallel.Unlifted.Parallel.Extracts as Ex

import Debug.Trace

$(testcases [ ""        <@ [t| ( Int, Float, Double ) |]
            ]
  [d|
    -- interleaves the segments of two arrays, e.g.:
    -- append_s (Segd [1,1,2,1]) (Segd [1,2]) [1,3,4] (Segd [1,1]) [2,5]
    --   = [1,2,3,4,5]
    prop_append_vs :: (Eq a, Elt a, Elts a, Arbitrary a, Show a) => a -> Property
    prop_append_vs phantom =
      forAll (sized (\n -> return n)) $ \len ->
      forAll (vsegdOfLength len) $ \segd1 ->
      forAll (arraysForVSegd segd1 phantom) $ \arr ->
      forAll (vsegdOfLength len) $ \segd2 ->
      forAll (arraysForVSegd segd2 phantom) $ \brr ->
      let lens1  = takeLengthsOfVSegd segd1
          lens2  = takeLengthsOfVSegd segd2
          lens1' = toList lens1
          lens2' = toList lens2
          arr'   = toList $ extracts_avs segd1 arr
          brr'   = toList $ extracts_avs segd2 brr
      in 
         P.length lens1' == P.length lens2' ==>
         toList (append_vs (segdFrom lens1 lens2) segd1 arr segd2 brr) ==
         concat (interleave (nest lens1' arr') (nest lens2' brr'))
      where segdFrom lens1 lens2 = lengthsToSegd $ U.interleave lens1 lens2
            lengthsToSegd lens = mkSegd lens (scan (+) 0 lens) (U.sum lens)
            interleave (x : xs) (y : ys) = x : y : interleave xs ys
            interleave (x : xs) _        = [x]
            interleave _        _        = []

    prop_extracts :: (Eq a, Elt a, Elts a, Arbitrary a, Show a) => a -> Property
    prop_extracts phantom =
      forAll (sized (\n -> return n)) $ \len ->
      forAll (vsegdOfLength len) $ \segd ->
      forAll (arraysForVSegd segd phantom) $ \arr ->
      let xPar   = toList $ Ex.extractsFromVectorsUPVSegdP segd arr
          xSeq   = toList $ Ex.extractsFromVectorsUPVSegd  segd arr
      in 
          xPar == xSeq
  |])

