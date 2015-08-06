module Q where

import qualified Map
import Map(Map)

mymember :: Int -> Map Int a -> Bool
mymember k m = Map.member k m || Map.member (k + 1) m
