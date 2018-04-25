-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
#include "gamtebType.t"
#include "consts.t"
#include "utils.t"
#include "transPort.t"
#include "output.t"

let
rec	-- create a particle and transport it
	-- sources :: Random -> Int -> Energy -> ([Result], [Stat])

    sources seed npart e & (npart <= 1) =
	let (eIndx, prob) = xsectInterp e in
	let (seed', _) = genRand seed in
	let p = (Part (Pt 0.0 0.0 0.0) (Pt 0.0 1.0 0.0) 1.0 e eIndx 1 seed') in
	transPort p prob
||  sources seed npart e =
	let npart1 = npart/2 in
	let npart2 = npart - npart1 in
	let (r1, r2) = genRand seed in
	let (res1, s1) = sources r1 npart1 e in
	let (res2, s2) = sources r2 npart2 e in
	(res1@res2, s1@s2)


in	-- scalar monte carlo code to transport .001 to 20.0 mev
	-- gamma rays in a carbon cylinder of length cl, radius crad
	-- gamteb :: [Char] 

	let scale = (stoi (hd argv)) in
	let seed = 0.5 in
	let npart = 1*scale in  
	let energy = 6.0 in 
	let (result, stats) = sources seed npart energy in
	outGamteb npart stats result
