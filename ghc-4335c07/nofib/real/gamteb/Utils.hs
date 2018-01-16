-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Utils (xsectInterp, genRand) where

import GamtebType
import Consts
import InitTable
import Data.Array((!))

-- linear interpolation to get cross sections as f(erg)

xsectInterp :: Energy -> (Indx, Probability)
xsectInterp e =
        (eIndx, (pComp, pPair, pPhot, (pComp+pPair+pPhot)))
	where
	    logE = log e
	    eIndx = findIndx 1
		where
		    findIndx i | (i < numLev) && (logE > ergs!i) =
			findIndx (i+1)
		    findIndx i = i
	    i = (if (eIndx < 2) then 2 else eIndx)
            f = (logE - ergs!(i-1)) / (ergs!i - ergs!(i-1))
            pComp = exp (xComp!(i-1) + f*(xComp!i - xComp!(i-1)))
            pPair = exp (xPair!(i-1) + f*(xPair!i - xPair!(i-1)))
            pPhot = exp (xPhot!(i-1) + f*(xPhot!i - xPhot!(i-1)))


-- random number generator from seed

genRand :: Random -> (Random, Random)
genRand seed =
        (r1/65599, r2/71123)
	where
            r1 = (314557*seed + 2711) `fiRem` 65599
            r2 = (2711*seed + 314557) `fiRem` 71123
	    x `fiRem` m = x - fromIntegral ((truncate x `div` m) * m)
