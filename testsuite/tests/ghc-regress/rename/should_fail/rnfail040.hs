-- This one should fail, because M.f is ambiguous

module M1 (module M) where

 import qualified Rnfail040_A as M 	-- M.nub
 import List as M			-- M.nub nub
 import Rnfail040_A as T		-- T.nub nub
