{-# OPTIONS_GHC -O #-}
module T9370b where

b n = takeWhile (<=n) $ filter odd $ map (^2) [1..]
{-# INLINE b #-}
