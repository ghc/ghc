{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where
import GHC.ST
import GHC.STRef
import GHC.Arr

-- Another 'escape' example

f:: ST t Int
f = do
    v <- newSTRef 5
    let g :: ST s Int
	-- Implicitly forall a. ST s Int
        g = readSTRef v
    g
