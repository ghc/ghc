
module GHC.Base where

import GHC.Prim

default ()

class Eq a where
    (==), (/=) :: a -> a -> Bool

    x /= y = not (x == y)
    x == y = not (x /= y)

data Bool = False | True deriving Eq

not :: Bool -> Bool
not True  = False
not False = True

