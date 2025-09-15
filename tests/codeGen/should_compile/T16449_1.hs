module T16449_1 where

import Data.Bits (setBit)

f :: Int
f = foldl setter 0 $ zip [0..] [()]
  where
    setter v (ix, _) = setBit v ix
