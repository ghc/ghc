-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "gamtebType.t"
#include "consts.t"
#include "utils.t"

export photoElec;


rec
	-- photoElec :: Particle -> Prob -> (Particle, Float, Bool)

    photoElec (Part pos dir w e eIndx cell seed)
		(Pr pComp pPair pPhot pTot) =
	let (seed', r2) = genRand seed in
	let (r1, r3) = genRand r2 in
	let w' = w *. (1.0-.pPhot/.pTot) in
	let absorb = w-.w' in
	let fcell = itof cell in
	if (w' > wcp2)
	    then 
		(Part pos dir w' e eIndx cell seed', absorb, false)
	    else	-- terminate particle because of weight cutoff 
		if ((w'*.fcell) < (r1*.wcp1))
		    then (Part pos dir w' e eIndx cell seed', absorb, true)
		    else (Part pos dir (wcp1/.fcell) e eIndx cell seed', absorb, false)

end
