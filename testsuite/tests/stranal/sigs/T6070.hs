module T6070 where

import qualified Data.Map as M

-- Should unbox `x`, so signature 1!P(..,..)
h :: (Int, Int) -> Int -> (Int, Int)
h x y = if y > 10
         then x
         else h (case h x 0 of (y1, y2) -> (y2, y1)) (y + 1)

-- Should unbox `(a,b)`, so signature 1!P(..,..)
c :: M.Map Int Int -> (Int, Int)
c m = M.foldrWithKey (\k v (a, b) -> if k + v > 2 then (a, b) else (b, a)) (0, 1) m
