-- !!! Conflicting re-exportation of var
module M (module M, module Mod142_A) where

import Mod142_A

x = 'x'
