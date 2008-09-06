
module ShouldCompile where

import Rn067_A

x = 'a'

-- The use of x here should not be reported as ambiguous, as it refers
-- to the local variable.
--
-- hugs Sept 2006 says:
--
-- ERROR "rn067.hs":14 - Ambiguous variable occurrence "x"
test = let x = "" in x
