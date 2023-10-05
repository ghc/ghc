module T9580 where

import T9580a
import Data.Coerce

foo :: Dimensional Int Double -> Double
foo x = coerce x
