

module ShouldCompile where
import GHC.ST
import GHC.STRef
import GHC.Arr

-- Another 'escape' example

f:: ST t Int
f = do
    v <- newSTRef 5
    let g :: ST s Int
	-- Implicitly forall s. ST s Int
        g = readSTRef v
    g
