module Main where

import Data.Typeable

f :: Typeable a => Int -> a -> [TypeRep]
f 0 a = []
f n a = typeOf a : f (n-1) [a]

-- pointwise compare 1000x1000 different TypeReps, there should be no equalities
-- (can be used as a benchmark)

main = print $ length [ t1 | t1 <- replicate 1000 (f 10 ()),
                             t2 <- replicate 1000 (f 10 'a'),
                             t1 == t2 ]
