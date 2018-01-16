-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module TransPort (transPort) where

import GamtebType
import Consts
import Utils
import Distance
import RoulSplit
import PhotoElec
import Compton
import Pair
(=:) a b = (a,b)

-- transport a particle

transPort :: Particle -> Probability -> ([Result], [Stat])
transPort p prob =
	if (dColl < dSurf)
	    then (let
		    pos' = transPos pos dir dColl
	    	    p' = Part pos' dir w e eIndx cell seed
	    	    doCompton = (r1 < (pComp / (pTot-pPhot)))
		    (res, stat) = collision p' prob doCompton
	    	  in
		  (res, [nc=:1]++stat))
	    else (let
		    pos' = transPos pos dir dSurf
	            p' = Part pos' dir w e eIndx cell seed
		    (res, stat) = noCollision p' prob surf
	    	  in
		  (res, [nnc=:1]++stat))
	where
	    (Part pos dir w e eIndx cell seed) = p
	    (pComp,pPair,pPhot,pTot) = prob
	    (r, r1) = genRand seed
	    (dSurf, surf) = distSurf pos dir
	    dColl = -((log r)/ pTot)


-- no collision inside cylinder

noCollision :: Particle -> Probability -> Int -> ([Result], [Stat])
noCollision p@(Part pos dir w e eIndx cell seed) prob surf =
	case surf of
	    1 -> ([(scatter, eIndx)=:w], [ns=:1]) 
	    2 -> ([(escape, eIndx)=:w], [ne=:1]) 
	    4 -> ([(transit, eIndx)=:w], [nt=:1]) 
	    3 -> -- cross internal surface
		 -- particle will split, die in russian roulette, or continue
		 -- cell = [1..] causes roulet or split to alternate
		 if (cell == 1)
	    	    then (let
			    (p1, p2) = split p
		            (r1, s1) = transPort p1 prob
		            (r2, s2) = transPort p2 prob
		    	  in
			  (r1++r2, [nsp=:1]++s1++s2))
	    	    else
			(let
			    (p', stat', roulKill) = roulet p
			 in
			 if (roulKill)
			    then ([], stat')
			    else (let
				    (res, stat) = transPort p' prob
			    	  in
				  (res, stat++stat')))


-- collision is in cylinder, do collision physics

collision :: Particle -> Probability -> Bool -> ([Result], [Stat])
collision p prob doCompton =
	if (wgtKill) 
	  then ([], [nwk=:1])
	  else
	    if (doCompton)
	      then	-- compton scattering
	    	(let
		    (p'', prob', comptonCut) = compton p'
		 in
		 if (comptonCut)
		    then ([], [nek=:1])
		    else transPort p'' prob')
	      else	-- pair production
	    	(let
		    (p'', prob', pairCut) = pair p'
		 in
		 if (pairCut) 
		    then ([], [nek=:1])
		    else transPort p'' prob')
	where
	    (Part pos dir w e eIndx cell seed) = p
	    (pComp,pPair,pPhot,pTot) = prob
	    (p', absorb, wgtKill) = photoElec p prob


-- translate a particle position

transPos :: Point -> Point -> Value -> Point
transPos (x,y,z) (u,v,w) dist = 
	(x',y',z')
	where
	    x' = x + u*dist
	    y' = y + v*dist
	    z' = z + w*dist
