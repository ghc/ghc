
module GHC.Base (module GHC.Base, Bool(..)) where

import GHC.Prim
import GHC.Types

default ()

class Eq a where
    (==), (/=) :: a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)

instance Eq Bool where
    False == False = True
    True == True = True
    _ == _ = False

not :: Bool -> Bool
not True  = False
not False = True

inline x = x

