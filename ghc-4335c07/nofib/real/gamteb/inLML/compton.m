-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "gamtebType.t"
#include "consts.t"
#include "utils.t"

export compton;

rec	-- compton scattering
	-- compton :: Particle -> (Particle, Prob, Bool)

    compton (Part pos dir w e eIndx cell seed) =
	let (seed', r2) = genRand seed in
	let (r3, r4) = genRand r2 in
	let eIn = 1.956917 *. e in
	let eOut = klein eIn r3 in
	let angle = 1.0 +. (1.0/.eIn) -. (1.0/.eOut) in
	let e' = 0.511008 *. eOut in
	let (eIndx', prob') = xsectInterp e' in
	if (e' <= energyCutoff)
	    then (Part pos dir w e' eIndx' cell seed', prob', true)
	    else
		let dir' = rotas dir angle r4 in
		(Part pos dir' w e' eIndx' cell seed', prob', false)


and	-- rotate a point through a polar angle whose cosine is c
	-- and through an azimuthal angle sampled uniformly
	-- rotas :: Point -> Angle -> Random -> Point

    rotas (Pt u v w) a rn =
	let (r1, r2) = genRand rn in
	let (rn', r3) = genRand r2 in
	let t1 = 2.0*.r1 -. 1.0 in
	let t2 = 2.0*.r3 -. 1.0 in
	let r = t1*.t1 +. t2*.t2 in
	if (r > 1.0)
	  then rotas (Pt u v w) a rn'
	  else
	    let wsq = 1.0 -. w*.w in
	    let r' = sqrt ((1.0 -. a*.a) /. r) in
	    let t1 = t1*.r' in
	    let t2 = t2*.r' in
	    if (wsq < small)
	      then
		(Pt t1 t2 (w*.a))
	      else
		let s = sqrt (wsq) in
		let u' = u*.a +. (t1*.u*.w -. t2*.v) /. s in
		let v' = v*.a +. (t1*.v*.w -. t2*.u) /. s in
		let w' = w*.a -. t1*.s in
		(Pt u' v' w')
	

and	-- sample from klein-nishina using inverse fit
	-- e = energy in, units of the rest mass of an electron
	-- klein :: Energy -> Random -> Energy

    klein e r =
	let a = 1.0/.e in
	let b = 2.0*.e +. 1.0 in
	let c = 1.0/.b in
	let d = log b in
	let f = 2.0*.e*.(1.0+.e)*.c*.c +. 4.0*.a +.(1.0-.2.0*.a*.(1.0+.a))*.d in
	if (e > 1.16666667)
	    then 
		let a' = 1.65898 +. a*.(0.62537*.a-.1.00796) in
	 	let b' = a'/.f in
		if (r > b')
		    then 
			let c' = (d-.1.20397) /. (1.0-.b') in
			let x' = 0.3 *. exp(c'*.(b'-.r)) in
			(x'*.e)
		    else 
			let c' = a' /. (3.63333+.a*.(5.44444*.a-.4.66667)) in
			let x' = klein1 (r/.b') 2.1 c' 1.4 (0.5*.a') in
			(x'*.e)
	    else 
		let a' = f/.(b+.c) in
		let b' = 0.5*.f in
		let c' = 1.0-.c in
		let x' = klein1 r (3.0*.c') a' (2.0*.c') b' in
		(x'*.e)
and
    klein1 x2 x3 x4 x5 x7 = 
	1.0 +. x2*.(x2*.(2.0*.x7 +. x4 -. x3 +. x2*.(x5-.x7-.x4)) -. x7)

end
