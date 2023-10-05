-- !!! lazy name conflict reporting for data constructors (pt.2)
module M where

import Mod164_A as A
import Mod164_B as A

f x = 
  case x of
    A.D1 -> 'a'

