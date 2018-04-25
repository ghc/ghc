-- !!! cumulative re-exportation of data constructors, pt 2.
module M where

import Mod157_D

-- Mod157_D re-exports the type T using (..). T is defined
-- in Mod157_A, but (only) two of its constructors is visible
-- in Mod157_D, one via Mod157_B, the other via Mod157_C.
a = A
b = B
-- C is out of scope.
c = C


