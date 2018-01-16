-- !!! Conflicting re-exportation of tycon
module M (module M, module Mod143_A) where

import Mod143_A

data Foo = Baz

