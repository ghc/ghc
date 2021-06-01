module T19873 where

-- Three mutually recursive functions
-- We want to inline g, h, keeping f as the loop breaker

f x = reverse (g (x:: [Int])) :: [Int]

{-# INLINE g #-}

g x = reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (h x))))))))))))

{-# INLINE h #-}
h x = reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (reverse (f x))))))))))))
