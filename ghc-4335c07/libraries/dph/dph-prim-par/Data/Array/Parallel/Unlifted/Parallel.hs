{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Parallel operations on unlifted arrays
--
--   * This is an internal API and shouldn't need to be used directly.
--     Client programs should use "Data.Array.Parallel.Unlifted"

--   NOTE: Each of the sections in the export list corresponds to one of the
--         Parallel modules, and the names are in the same order as in those
--         modules.
--
module Data.Array.Parallel.Unlifted.Parallel 
        ( -- * Basics
          lengthUP
        , nullUP
        , emptyUP
        , indexedUP
        , replicateUP
        , repeatUP
        , interleaveUP
  
          -- * Combinators
        , mapUP
        , filterUP
        , packUP
        , combineUP,  combine2UP
        , zipWithUP
        , foldUP,     fold1UP
        , foldlUP,    foldl1UP
        , scanUP
  
          -- * Enum
        , enumFromToUP
        , enumFromThenToUP
        , enumFromStepLenUP
        , enumFromStepLenEachUP
  
          -- * Permute
        , bpermuteUP
        , updateUP

          -- * Segmented
        , replicateRSUP
        , appendSUP
        , appendSUPV
        , foldRUP
        , sumRUP

          -- * Index and Extracts
        , indexsFromVector
        , indexsFromVectorsUPVSegd
        , indexsFromVectorsUPVSegdP

        , extractsFromNestedUPSSegd
        , extractsFromVectorsUPSSegd

        , extractsFromVectorsUPVSegd
        , extractsFromVectorsUPVSegdP

          -- * Subarrays
        , dropUP
  
          -- * Sums
        , andUP
        , orUP
        , allUP,     anyUP
        , sumUP,     productUP
        , maximumUP, maximumByUP
        , maximumIndexByUP)
where
import Data.Array.Parallel.Unlifted.Parallel.Basics
import Data.Array.Parallel.Unlifted.Parallel.Combinators
import Data.Array.Parallel.Unlifted.Parallel.Enum
import Data.Array.Parallel.Unlifted.Parallel.Permute
import Data.Array.Parallel.Unlifted.Parallel.Extracts
import Data.Array.Parallel.Unlifted.Parallel.Segmented
import Data.Array.Parallel.Unlifted.Parallel.Text       ()
import Data.Array.Parallel.Unlifted.Parallel.Subarrays
import Data.Array.Parallel.Unlifted.Parallel.Sums
