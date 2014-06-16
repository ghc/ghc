-- !!! Re-exporting a subset of a alg. data type's constructors
module Mod101 where

import Mod101_AuxB

-- ConB is not imported by Mod101_AuxB, hence not exported either.
x :: DataA
x = ConB False
