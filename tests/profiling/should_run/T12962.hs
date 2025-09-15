{-# OPTIONS_GHC -fprof-auto-top #-}

-- We want to make sure that 'foo' gets a cost center
-- automatically even though it is INLINABLE. INLINE functions
-- do not get cost centers automatically.

import Data.List (zipWith3)
import Control.Exception (evaluate)

{-# INLINABLE foo #-}
foo :: Num a => a -> a -> a -> a
foo a b c = a * b + c

{-# NOINLINE niz3 #-}
niz3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
niz3 x y z = zipWith3 x y z

blah :: [Int]
blah = replicate 100 1

main = evaluate $ sum $ niz3 foo blah blah blah
