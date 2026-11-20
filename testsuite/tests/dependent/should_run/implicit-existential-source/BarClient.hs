module BarClient where

import BarProvider
import Vector

opaque :: Vec n a -> Bool
opaque _ = True

bad :: Bar a -> Bool
bad p = opaque p
