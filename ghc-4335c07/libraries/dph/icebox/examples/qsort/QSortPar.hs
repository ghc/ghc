{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fno-spec-constr-count #-}
--
-- TODO:
--   permute operations, which are fairly important for this algorithm, are currently
--   all sequential

module QSortPar (qsortPar)
where

import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted
import Debug.Trace

-- I'm lazy here and use the lifted qsort instead of writing a flat version
qsortPar :: UArr Double -> UArr Double
{-# NOINLINE qsortPar #-}
qsortPar = concatSU . qsortLifted . singletonSU


-- Remove the trivially sorted segments
qsortLifted:: SUArr Double -> SUArr Double
qsortLifted xssArr = 
  splitApplySUP flags qsortLifted' id xssArr
  where
    flags = mapUP ((> 1)) $ lengthsSU xssArr

-- Actual sorting
qsortLifted' xssarr = 
  if (xssLen == 0) 
    then xssarr
    else (takeCU xssLen sorted) ^+:+^  equal ^+:+^ (dropCU xssLen sorted)

  where 
  
    xssLen     = lengthSU xssarr
    xsLens     = lengthsSU xssarr
    pivots     = xssarr !:^ mapUP (flip div 2) xsLens
    pivotss    = replicateSUP xsLens pivots
    xarrLens   = zipSU xssarr pivotss 
    sorted     = qsortLifted (smaller +:+^ greater)
    smaller =  fstSU $ filterSUP (uncurryS (<)) xarrLens
    greater =  fstSU $ filterSUP (uncurryS (>)) xarrLens
    equal   =  fstSU $ filterSUP (uncurryS (==)) xarrLens



splitApplySUP:: (UA e, UA e', Show e, Show e') =>  
  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySUP #-}
splitApplySUP  flags f1 f2 xssArr = 
  if (lengthSU xssArr == 0)
    then segmentArrU emptyU emptyU 
    else combineCU flags res1 res2

  where 
    res1 = f1 $ packCUP flags xssArr 
    res2 = f2 $ packCUP (mapUP not flags) xssArr
   




