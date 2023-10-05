module T14959 where

import Data.Bits (setBit)

f = foldl setBit 0 [x | (x,_) <- zip [0..] [1]] :: Integer
