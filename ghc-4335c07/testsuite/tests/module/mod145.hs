-- !!! Conflicting re-exportation of class methods
module Mod145(module Mod145, module Mod145_A) where

import Mod145_A

class C1 a where
  m1 :: a -> Int


