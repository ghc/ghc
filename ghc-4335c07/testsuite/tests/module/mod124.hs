-- !!! hiding an entity T (where T is both a type and a dcon.)
module M where

import Mod124_A hiding (T)

x :: T
x = undefined
