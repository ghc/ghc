{-# OPTIONS_GHC -ddump-simpl #-}

module T21851 (g') where
import T21851a

g :: Num a => a -> a
g x = fst (f x)
{-# NOINLINE[99] g #-}

g' :: Int -> Int
g' = g

-- We should see a call to a /specialised/ verion of `f`,
-- something like
-- g' = \ (x :: Int) -> case T21851a.$w$sf x of { (# ww, ww1 #) -> ww }
