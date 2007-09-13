-- !!! Conflicting re-exportation of dcon
module M (module Mod144_A,module M) where

import Mod144_A

data Foo1 = Bar

