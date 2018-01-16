-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module PhotoElec (photoElec) where

import GamtebType
import Consts
import Utils

photoElec :: Particle -> Probability -> (Particle, Value, Bool)
photoElec p prob =
	if (w' > wgtCut2)
	  then (Part pos dir w' e eIndx cell seed', absorb, False)
	  else	-- terminate particle because of weight cutoff 
	    if ((w' * fromIntegral cell) < (r1 * wgtCut))
	      then (Part pos dir w' e eIndx cell seed', absorb, True)
	      else (Part pos dir w'' e eIndx cell seed', absorb, False)
	where
	    (Part pos dir w e eIndx cell seed) = p
	    (pComp,pPair,pPhot,pTot) = prob
	    (seed', r2) = genRand seed
	    (r1, r3) = genRand r2
	    w' = w * (1 - pPhot/pTot)
	    w'' = wgtCut/fromIntegral cell
	    absorb = w - w'
