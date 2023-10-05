module T24396b where

import GHC.Tuple
import Prelude hiding (($))

import T24396a

d1 :: C1 => D1
d1 = C1

d2 :: D2
d2 = D2

d3 :: D3
d3 = D3

d4 :: C2 => D4
d4 = C2

d5 :: D5
d5 = D5

d6 :: D6
d6 = D6

solo :: Solo $ ()
solo = Solo $ ()
