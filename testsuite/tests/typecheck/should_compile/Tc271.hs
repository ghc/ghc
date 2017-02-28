module Tc271 where

import Tc271a

class K a where
    f :: a -> a
    g :: a -> a

h :: K a => a -> a
h = f . g . h2
