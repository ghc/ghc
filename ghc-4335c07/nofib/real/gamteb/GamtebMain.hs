-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
-- Copyright, 1990, The Regents of the University of California.
-- This software was produced under a U.S. Government contract (W-7405-ENG-36)
-- by the Los Alamos National Laboratory, which is operated by the University
-- of California for the U.S. Department of Energy.  The U.S. Government is
-- licensed to use, reproduce, and distribute this software.  Permission is
-- granted to the public to copy and use this software without charge, provided
-- that this notice and any statement of authorship are reproduced on all
-- copies.  Neither the Government nor the University makes any warranty,
-- express or implied, or assumes any liability for the use of this software.

module GamtebMain (gamteb) where

import GamtebType
import Consts 
import Utils
import TransPort
import Output

-- create a particle and transport it

sources :: Random -> Int -> Energy -> ([Result], [Stat])
sources seed nPart e | (nPart <= 1) =
	transPort p prob
	where
	    (eIndx, prob) = xsectInterp e
	    (seed', _) = genRand seed
	    p = (Part (0,0,0) (0,1,0) 1 e eIndx 1 seed')
sources seed nPart e =
	(res1++res2, s1++s2)
	where
	    nPart1 = nPart `div` 2
	    nPart2 = nPart - nPart1
	    (r1, r2) = genRand seed
	    (res1, s1) = sources r1 nPart1 e
	    (res2, s2) = sources r2 nPart2 e


-- scalar monte carlo code to transport .001 to 20.0 mev
-- gamma rays in a carbon cylinder of length cylLen, radius cylRad

gamteb :: Int -> [Char]
gamteb scale =	
	outGamteb nPart stats result
	where
	    seed = 0.5
	    nPart = 1*scale
	    energy = 6
	    (result, stats) = sources seed nPart energy
