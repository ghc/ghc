-- !!! qualified + hiding type constructors (or classes)
module M where

import qualified Mod164_A hiding (T)

data T = D1 | D3

f = D1
g = Mod164_A.D1
