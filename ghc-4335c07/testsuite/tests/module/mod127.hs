-- !!! hiding an entity T (where T is both a class and a dcon.)
module M where

import Mod127_A hiding (T)

x :: T a => a -> a
x = undefined
