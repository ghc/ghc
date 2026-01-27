module T20289 where

import T20289_A ( A, mkA )
import Data.Coerce ( coerce )

newtype Box = Box Int

aInt :: A Int
aInt = mkA 3

aBox :: A Box
aBox = coerce aInt
