module Main where

-- Test generation of large TypeReps
-- (can be used as a benchmark)

import Data.Typeable

f :: Typeable a => Int -> a -> TypeRep
f 0 a = typeOf a
f n a = f (n-1) [a]

main = print (f 50000 () == f 50001 ())
