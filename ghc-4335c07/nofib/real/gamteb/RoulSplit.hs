-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module RoulSplit (roulet, split) where

import GamtebType
import Consts
import Utils

-- russian roulette on existence of the particle
-- set new cell to 1 so next pass through tport is split, not roulet

roulet :: Particle -> (Particle, [Stat], Bool)
roulet (Part pos dir w e eIndx cell seed) =
	if (r1 < 0.5)
	    then -- not killed in russian roulette
		(Part pos dir (2*w) e eIndx 1 seed',
		[(nr,1), (wrg,w)], False)
	    else -- killed in russian roulette
		(Part pos dir 0 e eIndx 1 seed',
		[(nr,1), (nrk,1), (wrl,w)], True)
	where
	    (r1, r2) = genRand seed
	    (seed', r3) = genRand r2


-- split a particle into two
-- set new cells to 2 so next pass through tport is roulet, not split

split :: Particle -> (Particle, Particle)
split (Part pos dir w e eIndx cell seed) =
	(Part pos dir (0.5*w) e eIndx 2 seed1,
	 Part pos dir (0.5*w) e eIndx 2 seed2)
	where
	    (seed1, seed2) = genRand seed
