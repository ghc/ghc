-- !!! lazy name conflict reporting for data constructors
module M where

import Mod164_A
import Mod164_B

f x = 
  case x of
    D1 -> 'a'

