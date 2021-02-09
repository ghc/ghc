module T18907 (f, g, h) where

-- | Should not unbox `x`.
-- But because of Optimistic Case Binder CPR (#19232), we will still CPR f.
f :: (Int, Int) -> (Int, Int)
f x@(_,_)
  | sum [0..24::Int] == 1 = x
  | otherwise             = (0,0)
{-# NOINLINE f #-}

-- | Ideally, we wouldn't unbox `x`, but we have a field demand on `p`, which
-- indicates that we should unbox.
g :: (Int, Int) -> (Int, Int)
g x@(p,_) = p `seq` x
{-# NOINLINE g #-}

seq' a b = seq a b
{-# NOINLINE seq' #-}

-- | Should not unbox `y`.
h :: Int -> Int -> Int
h x y = (x+1) `seq'` y
{-# NOINLINE h #-}

