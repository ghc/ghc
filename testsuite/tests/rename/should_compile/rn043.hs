-- This one should succeed; M.x is unambiguous

module ShouldCompile (module M) where

 import Rn043_A as M 	-- x, M.x
 import Rn043_B  	-- x, Rn043_A.x
