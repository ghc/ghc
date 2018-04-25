-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "gamtebType.t"
#include "consts.t"
#include "utils.t"
#include "distance.t"
#include "roulSplit.t"
#include "photoElec.t"
#include "compton.t"
#include "pair.t"

export transPort;


rec	-- transport a particle
	-- transPort :: Particle -> Prob -> ([Result], [Stat])

    transPort (Part pos dir w e eIndx cell seed)
	  (prob as Pr pComp pPair pPhot pTot) =
	let (r, r1) = genRand seed in
	let (dSurf, surf) = distSurf pos dir in
	let dColl = 0.0 -. log r/.pTot in
	if (dColl < dSurf)
	  then 		-- collision in cylinder
	    let pos' = transPos pos dir dColl in
	    let p = Part pos' dir w e eIndx cell seed in
	    let doCompton = (r1 < (pComp /. (pTot-.pPhot))) in
	    let (res, stat) = collision p prob doCompton in
	    (res, [St nc 1.0]@stat)
	  else 		-- no collision in cylinder
	    let pos' = transPos pos dir dSurf in
	    let p = Part pos' dir w e eIndx cell seed in
	    let (res, stat) = noCollision p prob surf in
	    (res, [St nnc 1.0]@stat)


and	-- no collision inside cylinder
	-- noCollision :: Particle -> Prob -> Int -> ([Result], [Stat])

    noCollision (p as Part pos dir w e eIndx cell seed) prob surf =
	case surf in
	    1:  ([Res scatter eIndx w], [St ns 1.0])
	||  2:	([Res escape eIndx w], [St ne 1.0])
	||  4:	([Res transit eIndx w], [St nt 1.0])
	||  3:	-- cross internal surface
		-- particle will split, die in russian roulette, or continue
		-- cell = [1..] causes roulet or split to alternate
		if (cell = 1)
	    	  then
		    let (p1, p2) = split p in
		    let (r1, s1) = transPort p1 prob in
		    let (r2, s2) = transPort p2 prob in
		    (r1@r2, [St nsp 1.0]@s1@s2)
	    	  else
		    let (p', stat, roulKill) = roulet p in
		    if (roulKill)
	    	      then ([], stat)
	    	      else let (res, stat') = transPort p' prob in
			   (res, stat@stat')
	end


and	-- collision is in cylinder, do collision physics
	-- collision :: Particle -> Prob -> Bool-> ([Result], [Stat])

    collision p prob doCompton =
	let (Part pos dir w e eIndx cell seed) = p in
	let (Pr pComp pPair pPhot pTot) = prob in
	let (p, absorb, wgtKill) = photoElec p prob in
	if (wgtKill ) 
	  then 
	        ([], [St nwk 1.0])
	  else
	    if (doCompton)
	      then	-- compton scattering
		let (p', prob', cut) = compton p in
	    	if cut
		    then ([], [St nek 1.0])
		    else transPort p' prob'
	      else	-- pair production
		let (p', prob', cut) = pair p in
	    	if cut 
		    then ([], [St nek 1.0])
		    else transPort p' prob'


and	-- translate a particle position
	-- transPos :: Point -> Point -> Float -> Point

    transPos (Pt x y z) (Pt u v w) dist = 
	let x = x +. u*.dist in
	let y = y +. v*.dist in
	let z = z +. w*.dist in
	(Pt x y z)


end
