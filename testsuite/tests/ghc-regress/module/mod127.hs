-- !!! hiding an entity T (where T is both a class and a dcon.)
module M where

import Mod126_A hiding (T)

x :: T a => a -> a
x = undefined
