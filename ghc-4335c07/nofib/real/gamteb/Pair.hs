-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Pair (pair) where

import GamtebType
import Consts
import Utils

-- pair production after a collision

pair :: Particle -> (Particle, Probability, Bool)
pair (Part pos dir w e eIndx cell seed) =
	if (e' <= ergCut)
	    then (Part pos dir w' e' eIndx' cell seed', prob', True)
	    else (Part pos dir' w' e' eIndx' cell seed', prob', False)
	where
	    (seed', r2) = genRand seed
	    (r3, r4) = genRand r2
	    e' = 0.511008
	    (eIndx', prob') = xsectInterp e'
	    w' = 2*w
	    dir' = isos r3
	

-- sample a direction u,v,w isotropically
-- isotropic emission lab system

isos :: Random -> Point
isos r =
	isos' t1 t2 (t1*t1 + t2*t2) 
	where
	    (r1, r2) = genRand r
	    (r3, r4) = genRand r2
	    t1 = 2*r4 - 1
	    t2 = 2*r3 - 1
	    isos' t1 t2 rsq 
		| rsq > 1	=
			let
			    (r1, r2) = genRand r1
			    (r3, r4) = genRand r2
			    t1 = 2*r4 - 1
			    t2 = 2*r3 - 1
			in
			isos' t1 t2 (t1*t1 + t2*t2)
		| otherwise	=
			let
			    u = 2*rsq - 1
			    t3 = sqrt (1 - u*u) / rsq
              	 	    v = t1*t3
               		    w = t2*t3
			in
			(u,v,w)
