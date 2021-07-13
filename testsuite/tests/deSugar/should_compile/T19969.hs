{-# OPTIONS_GHC -dno-typeable-binds -O2 -fno-worker-wrapper #-}

module T19969 where

-- Three mutually recursive functions
-- We want to inline g, h, keeping f as the loop breaker

f [] = []
f x = reverse (g (x:: [Int])) :: [Int]

{-# INLINE g #-}
g x = reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (h x))))))))))))

{-# INLINE h #-}
h x = reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (f x))))))))))))
