{-# OPTIONS_GHC -O2 -fforce-recomp #-}

-- | The point of this test is that @g@ get's a demand that says "whenever @g@
-- is called, the second component of the pair is evaluated strictly".
module T18903 where

h :: Int -> Int
h m =
  let g :: Int -> (Int,Int)
      g 1 = (m, 0)
      g n = (2 * n, 2 `div` n)
      {-# NOINLINE g #-}
  in case m of
    1 -> 0
    2 -> snd (g m)
    _ -> uncurry (+) (g m)
