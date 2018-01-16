-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "gamtebType.t"
#include "consts.t"
#include "utils.t"

export pair;


rec	-- pair production after a collision
	-- pair :: Particle -> (Particle, Prob, Bool)

    pair (Part pos dir w e eIndx cell seed) =
	let (seed', r2) = genRand seed in
	let (r3, r4) = genRand r2 in
	-- when energy changes, so does the index and the probabilities
	let e' = 0.511008 in
	let (eIndx', prob') = xsectInterp e' in
	let w' = 2.0*.w in
	if (e' <= energyCutoff)
	    then 
		(Part pos dir w' e' eIndx' cell seed', prob', true)
	    else 
		let dir' = isos r3 in
		(Part pos dir' w' e' eIndx' cell seed', prob', false)
	

and	-- sample a direction u,v,w isotropically
	-- isotropic emision in lab system
	-- isos :: Random -> Point

    isos r =
	let (r1, r2) = genRand r in
	let (r3, r4) = genRand r2 in
	let t1 = 2.0*.r4 -. 1.0 in
	let t2 = 2.0*.r3 -. 1.0 in
	(isos' t1 t2 (t1*.t1 +. t2*.t2) 
	    where rec
		isos' t1 t2 rsq & (rsq > 1.0) =
		    let (r1, r2) = genRand r1 in
		    let (r3, r4) = genRand r2 in
		    let t1 = 2.0*.r4 -. 1.0 in
		    let t2 = 2.0*.r3 -. 1.0 in
		    isos' t1 t2 (t1*.t1 +. t2*.t2)
	    ||  isos' t1 t2 rsq =
		    let u = 2.0*.rsq -. 1.0 in
		    let t3 = sqrt ((1.0 -. u*.u) /. rsq) in
                    let v = t1*.t3 in
                    let w = t2*.t3 in
                    (Pt u v w))

end
