module Main where

-- This test used to fail in GHC 5.04.2.  See rev. 1.22 of
-- libraries/base/GHC/Base.lhs for the fix.

data FourArrow = A | B | C | D | E | ABE | AC | BD | CDE
		 deriving (Eq)

dom E   = ABE

cod A   = AC
cod B   = BD
cod C   = CDE
cod D   = CDE
cod E   = CDE
cod ABE = ABE
cod AC  = AC
cod BD  = BD
cod CDE = CDE

----
bceFour :: FourArrow -> FourArrow -> String
bceFour f g
    | dom f == dom g && cod f == cod g
    = "it works"
    | otherwise = error ("Four.bceFour: precondition fails:"
			  ++ "arrows not parallel"
			  ++ "\n")

main = print (bceFour E E)
