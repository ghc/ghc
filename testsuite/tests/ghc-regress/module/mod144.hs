-- !!! Conflicting re-exportation of dcon
module M (module Mod143_A,module M) where

import Mod143_A -- yes, this is intentionally Mod143_A

data Foo1 = Bar

