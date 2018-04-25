-- !!! Array - array difference operator
-- 
--
module Main(main) where

import Data.Array
import Data.Ix

hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i,1) | i <- is , inRange bnds i]

main = 
 let 
  a1 = hist (0,10) (concat $ take 2 $ repeat [1..20])
 in
 print a1 >>
 print (a1 // [ (i,0) | i<-[0..10], odd i])



