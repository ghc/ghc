module T19246a where

f :: Ord a => [a] -> ([a], a)
{-# INLINABLE f #-}
f xs = (ys, maximum ys)
  where
    ys = reverse . reverse . reverse . reverse . reverse . reverse $ xs

