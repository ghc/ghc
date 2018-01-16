-- !!! hiding an entity T (where T is both a type and a dcon.)
module M where

import Mod125_A hiding (T)

--x :: T
x = T
