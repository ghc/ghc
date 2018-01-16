--
--	Patricia Fasel
--	Los Alamos National Laboratory
--	1990 August
--
module Compton (compton) where

import GamtebType
import Consts
import Utils

-- compton scattering

compton :: Particle -> (Particle, Probability, Bool)
compton (Part pos dir w e eIndx cell seed) =
	if (e' <= ergCut)
	    then (Part pos dir w e' eIndx' cell seed', prob', True)
	    else (Part pos dir' w e' eIndx' cell seed', prob', False)
	where
	    (seed', r2) = genRand seed
	    (r3, r4) = genRand r2
	    eIn = 1.956917 * e
	    eOut = klein eIn r3
	    angle = 1 + 1/eIn - 1/eOut
	    e' = 0.511008 * eOut
	    (eIndx', prob') = xsectInterp e'
	    dir' = rotas dir angle r4


-- rotate a point through a polar angle whose cosine is c
-- and through an azimuthal angle sampled uniformly

rotas :: Point -> Angle -> Random -> Point
rotas (u,v,w) a rn =
	if (r > 1)
	    then rotas (u,v,w) a rn'
	    else
		(let
		    r' = sqrt ((1 - a*a) / r)
		    t1' = t1*r'
	            t2' = t2*r'
		    wsq = 1 - w*w
		    s = sqrt wsq
		    u' = u*a + (t1'*u*w - t2'*v) / s
		    v' = v*a + (t1'*v*w - t2'*u) / s
		    w' = w*a - t1'*s
		 in
		 if (wsq < small)
		    then (t1',t2',(w*a))
		    else (u',v',w')
		)
	where
	    (r1, r2) = genRand rn
	    (rn', r3) = genRand r2
	    t1 = 2*r1 - 1
	    t2 = 2*r3 - 1
	    r = t1*t1 + t2*t2


-- sample from klein-nishina using inverse fit
-- e = energy in, units of the rest mass of an electron

klein :: Energy -> Random -> Energy
klein e r =
	if (e > 1.16666667)
	    then 
		(let
		    a' = 1.65898 + a*(0.62537*a - 1.00796)
	 	    b' = a'/f
		 in
		 if (r > b')
		    then (let
			    c' = (d-1.20397) / (1-b')
			    x' = 0.3 * exp (c'*(b'- r))
		    	  in
			  x'*e)
		    else (let
			    c' = a'/(3.63333 + a*(5.44444*a - 4.66667))
			    x' = klein1 (r/b') 2.1 c' 1.4 (0.5*a')
			  in
			  x'*e))
	    else (let
		    x' = klein1 r (3*c') a' (2*c') b'
		    a' = f/(b+c)
		    b' = 0.5*f
		    c' = 1-c
	    	  in
		  x'*e)
	where
	    a = 1/e
	    b = 2*e + 1
	    c = 1/b
	    d = log b
	    f = 2*e*(1+e)*c*c + 4*a + (1-2*a*(1+a))*d
	    klein1 x2 x3 x4 x5 x7 = 
	        1 + x2*(x2*(2*x7 + x4 - x3 + x2*(x5 - x7 - x4)) - x7)
