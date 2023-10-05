-- !!! Fixed bug: Re-exporting a subset of a class' methods
module Mod102 where

import Mod102_AuxB

-- methB is not imported by Mod102_AuxB, hence not exported either.
x :: Bool -> ()
x = methB False
