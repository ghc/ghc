-- !!! cumulative re-exportation of class methods
module M where

import Mod159_D

-- Mod159_D re-exports the class C using (..). C is defined
-- in Mod159_A, but (only) two of its methods are visible
-- in Mod159_D, one via Mod159_B, the other via Mod159_C.
a = m1 'a'
b = m2 'b'
