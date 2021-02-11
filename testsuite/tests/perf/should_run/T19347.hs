{-# LANGUAGE UnboxedTuples #-}

module Main where

data T = MkT !Int Int

-- An expensive recursive function
g :: Int -> Int -> (# Int, Int #)
g x 0 = (# x, 33 #)
g x n = g (x+n) (n-1)

-- 'foo' calls 'h' often
foo h 0 = 0
foo h n = h n `seq` foo h (n-1)

main = print (foo (MkT (case g 1 200 of (# a,b #) -> a))
                  200)

{- In main, we don't want to eta-expand the MkT to
  (\x. MkT (case g 1 200 of (# a,b #) -> a) x)
because then that call to g may be made more often
The faffing with unboxed tuples is to defeat full
laziness which would otherwise lift the call to g
out to top level

Before fixing #19347, running this program gave
       2,012,096 bytes allocated in the heap
after it gave
         101,712 bytes allocated in the heap
-}
