-- re-export all of C
module Mod118_B (C(..)) where

import Mod118_A hiding (C(m1))
import Mod118_A (C)
