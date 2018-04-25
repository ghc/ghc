import DPH.Testsuite
import DPH.Arbitrary.Segd
import Data.Array.Parallel.Unlifted as U
import Prelude as P

$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            , "acc"     <@ [t| ( Int       ) |]
            , "num"     <@ [t| ( Int       ) |]
            , "ord"     <@ [t| ( Bool, Int ) |]
            , "enum"    <@ [t| ( Bool, Int ) |]
            ]
  [d|
    prop_lengthsToSegd :: Array Int -> Bool
    prop_lengthsToSegd arr =
      let lens = U.map (`mod` 100) arr
          segd = lengthsToSegd lens
      in checkSegd segd lens

    prop_mkSegd :: Array Int -> Bool
    prop_mkSegd arr =
      let lens = U.map (`mod` 100) arr
          ids  = U.scan (+) 0 lens
          n    = U.sum lens
          segd = mkSegd lens ids n
      in checkSegd segd lens

    prop_lengthSegd :: Segd -> Bool
    prop_lengthSegd segd =
      lengthSegd segd == U.length (lengthsSegd segd)

    -- skip: lengthsSegd (redundant)
    -- skip: indicesSegd (redundant)
    -- skip: elementsSegd (redundant)

    -- Adds two segment descriptors segment-wise
    -- TODO: decide whether we would ever be called with Segds
    --       with different number of segments in each.
    prop_plusSegd :: Segd -> Segd -> Property
    prop_plusSegd segd1 segd2 =
      let segd  = segd1 `plusSegd` segd2
          lens1 = lengthsSegd segd1
          lens2 = lengthsSegd segd2
          lens  = U.zipWith (+) lens1 lens2
      in U.length lens1 == U.length lens2
      ==> checkSegd segd lens
    
  |])