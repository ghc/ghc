module MatrixPrim
where

import Data.Array.Parallel.Unlifted as U

mmMult::Int -> U.Array Double -> U.Array Double -> U.Array Double
mmMult  order m n =
   sumR order (U.zipWith (*) mExp nTExp)
{-      sum_s (
           (lengthsToSegd  (U.replicate (order*order) order)) 
         >: (U.zipWith (*) mExp nTExp))
-}


  where 
    mExp = repeat_c (order * order * order) (U.replicate order order) 
                    (lengthsToSegd (U.replicate order order))
               m
    nTExp = U.repeat  order (order * order * order) nT
             
    nT= transposeM order n 


mmMultTL:: Int -> Int -> U.Array Double -> U.Array Double -> U.Array Double
mmMultTL order noOfSubMat ms ns = 
  sum_s ((lengthsToSegd (U.replicate (order*order*noOfSubMat) order)) >: (U.zipWith (*) msExp nsTExp))
  where
    msExp = repeat_c (order * order * order*noOfSubMat) 
                     (U.replicate (order*noOfSubMat) order) 
                     (lengthsToSegd (U.replicate (order*noOfSubMat) order))
                     ms
    nsT    = transposeML order noOfSubMat ns
    nsTExp = U.repeat_c (order*order*order*noOfSubMat) 
                        (U.replicate noOfSubMat order)
                        (lengthsToSegd (U.replicate noOfSubMat (order*order)))
                        nsT

{-
mmMultH:: Int -> U.Array Double -> U.Array Double -> U.Array Double
mmMultH order m n = m
  where
    n1 = U.bpermute n (enum
    nOrder = order `div` 2
-}

transposeM:: Int -> U.Array Double -> U.Array Double
transposeM order m = U.bpermute m inds
  where 
    inds = U.enumFromStepLenEach (order*order)
                   (U.zip3 (U.enumFromTo 0 (order-1))  
                           (U.replicate order order) 
                           (U.replicate order order))
  


transposeML:: Int -> Int -> U.Array Double -> U.Array Double
transposeML order noOfSubMat m = U.bpermute m indsExt
  where
    inds = U.enumFromStepLenEach (order*order)
                   (U.zip3 (U.enumFromTo 0 (order-1))  
                           (U.replicate order order) 
                           (U.replicate order order))
    indsExt = U.zipWith (+) 
                 (U.repeat noOfSubMat (order*order*noOfSubMat) inds )
                 (replicateEach  (order*order*noOfSubMat) (U.replicate noOfSubMat (order*order))
                                   (enumFromStepLen 0 (order*order) noOfSubMat))
{-

repeatCRC:: Int -> Int -> Int -> Int  -> U.Array Double -> U.Array Int
repeatCRC size mult seglen arr = 
  where 
    ins = enumFromStepLenEach size (U.zip3 from steps lens)
    froms = replicateEach (seglen*mult) (U.replicate mult seglen) (enumFromTo 0 (seglen -1))
    steps = U.replicate (seglen*mult) 1
    lens  = U.replicate (seglen*mult) seglen
    -}


sumR:: Int -> U.Array Double -> U.Array Double
sumR len arr = U.zipWith fn1
               (U.scan fn2 (0.0 :*: 0.0) (U.zip arr
                       (U.map (\x-> if (x `mod` len /= 0) then 1.0 else 0.0) (U.enumFromTo 0 (U.length arr -1))))) 
               (U.zip arr
                       (U.map (\x-> if (x `mod` len /= 0) then 1.0 else 0.0) (U.enumFromTo 0 (U.length arr -1))))
   where fn1 = \(x1 :*: f1) -> \(x2 :*:f2) -> f2*x1+x2
         fn2 = \(x1 :*: f1) -> \(x2 :*:f2) -> (f2*x1+x2 :*: f1*f2)
--         flarr = (U.zip arr
--                       (U.map (\x-> if (x `mod` len /= 0) then 1.0 else 0.0) (U.enumFromTo 0 (U.length arr -1))))
