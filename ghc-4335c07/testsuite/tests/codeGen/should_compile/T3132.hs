module Spring where

import Data.Array.Unboxed

step :: UArray Int Double -> [Double]
step y = [y!1 + y!0]
