{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}

module T18885b where

force :: (Int, Int) -> (Int, Int)
force p@(!x, !y) = p
{-# NOINLINE force #-}

test :: (Int, Int) -> Int -> (Int, Int)
test p z = case p of
  (x, y) | odd z     -> force p
         | otherwise -> (1, 2)
