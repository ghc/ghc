
module T23739b where

import Data.Tuple.Experimental
import GHC.TypeLits

g1 :: Int -> Unit
g1 Int = ()

g2 :: Int
g2 = Int{}

g3 :: Int
g3 = Int
