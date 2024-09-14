module T21888a where

-- This tests case (B) of
-- Note [No lazy, Unboxed demands in demand signature]
-- in GHC.Core.Opt.DmdAnal

-- We should get a worker-wrapper split on g
-- and on wombat, even though f uses x boxed

{-# NOINLINE f #-}
f x = Just x

wombat :: Int -> a
wombat x = error (show (f x))

g :: Bool -> Int -> Int
g True x  | x>0 = g True (x-1)
          | otherwise = x+1
g False x = wombat x
