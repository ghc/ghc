{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fno-spec-constr-count #-}
--

module QSortSeq (qsortSeq, qsortList)
where

import Data.Array.Parallel.Unlifted
import Debug.Trace


qsortSeq :: UArr Double -> UArr Double
qsortSeq  xs = -- trace (show res) 
  res 
  where 
    res = concatSU $ qsortLifted $ singletonSU xs

qsortLifted:: SUArr Double -> SUArr Double
qsortLifted xssArr = splitApplySU flags qsortLifted' id xssArr
  where
    flags = mapU ((>=1)) $ lengthsSU xssArr

qsortLifted' xssarr = 
  if (xssLen == 0) 
    then   xssarr
    else (takeCU xssLen sorted) ^+:+^ equal ^+:+^  (dropCU xssLen sorted)
  where
    xssLen     = lengthSU xssarr
    xsLens     = lengthsSU xssarr
    xarrLens   = zipSU xssarr $ replicateSU xsLens $ xssarr !:^ mapU (flip div 2) xsLens
    sorted     = qsortLifted $ (mapSU fstS $ filterSU (uncurryS (<)) xarrLens)
                               +:+^    
                               (mapSU fstS $ filterSU (uncurryS (>))  xarrLens)
    equal      = mapSU fstS $ filterSU (uncurryS (==))  xarrLens

    
splitApplySU:: (UA e, UA e', Show e, Show e') =>  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySU #-}
splitApplySU  flags f1 f2 xssArr = res
                          
  where
    res  = combineCU flags res1 res2
    res1 = f1 $ packCU flags xssArr 
    res2 = f2 $ packCU (mapU not flags) xssArr
   

qsortList:: [Double] -> [Double]
qsortList =  qsortList'

qsortList' [] = []
qsortList' xs = (qsortList' smaller) ++ equal ++ (qsortList' greater) 
  where
    p = xs !! (length xs `div` 2)
    smaller = [x | x <- xs, x < p]
    equal   = [x | x <- xs, x == p]
    greater = [x | x <- xs, x > p]
