module T17140 where

import T17140a

data B = B A

mapP_B :: (Int -> Int) -> B -> B
mapP_B f (B t) = B (mapP_A f t)
