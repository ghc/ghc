-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "gamtebType.t"
#include "consts.t"
#include "utils.t"

export roulet, split;


rec	-- russian roulette on existence of the particle
	-- set new cell to 1 so next pass through tport is split, not roulet
	-- roulet :: Particle -> (Particle, [Stat], Bool)
	
    roulet (Part pos dir w e eIndx cell seed) =
	let (r1, r2) = genRand seed in
	let (seed', r3) = genRand r2 in
	if (r1 < 0.5)
	  then -- not killed in russian roulette
	    (Part pos dir (2.0*.w) e eIndx 1 seed',
	    [St nr 1.0] @ [St wrg w], false)
	  else -- killed in russian roulette
	    (Part pos dir 0.0 e eIndx 1 seed',
	    [St nr 1.0] @ [St nrk 1.0] @ [St wrl w], true)


and	-- split a particle into two
	-- set new cells to 2 so next pass through tport is roulet, not split
	-- split :: Particle -> (Particle, Particle)

    split (Part pos dir w e eIndx cell seed) =
	let (seed1, seed2) = genRand seed in
	(Part pos dir (0.5*.w) e eIndx 2 seed1,
	 Part pos dir (0.5*.w) e eIndx 2 seed2)

end
