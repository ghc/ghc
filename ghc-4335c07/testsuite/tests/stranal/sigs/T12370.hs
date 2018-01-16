module T12370 where

foo :: (Int, Int) -> Int
foo (x,y) = x + y
{-# NOINLINE foo #-}

-- If the p is processed by LetUp, then we get nice use-once demands on n and m
bar n m =
    let p = (n,m)
        {-# NOINLINE p #-}
    in foo p

