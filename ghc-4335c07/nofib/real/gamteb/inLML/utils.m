-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "consts.t"
#include "gamtebType.t"

export xsectInterp, genRand, showFloat, showArray, outXsectTbl;

rec     -- linear interpolation to get cross sections as f(erg)
        -- xsectInterp :: Energy -> (Indx, Prob)
 
    xsectInterp e =
	let logE = log e in
	let eIndx = (findIndx 1
	    where rec
		findIndx i & ((i < numlev) & (logE > (ergs?i))) = findIndx (i+1)
            ||  findIndx i = i) in
	let i = (if (eIndx < 2) then 2 else eIndx) in
        let f = (logE -. (ergs?(i-1))) /. ((ergs?i) -. (ergs?(i-1))) in
        let pComp = exp ((xComp?(i-1)) +. f*.((xComp?i)-.(xComp?(i-1)))) in
        let pPair = exp ((xPair?(i-1)) +. f*.((xPair?i)-.(xPair?(i-1)))) in
        let pPhot = exp ((xPhot?(i-1)) +. f*.((xPhot?i)-.(xPhot?(i-1)))) in
        (eIndx, (Pr pComp pPair pPhot (pComp+.pPair+.pPhot)))
 
	-- initialize the cross section tables
        -- these tables are constant, used with the energy and energy index
        -- in calculating probabilities

and rec ergs    = 
	(array 1 numlev (\[x].x)
        ([(i, v) ;; (i, v) <- combine ([1..numlev], concmap f2 erg)])
	where
	    erg =
	       [0.001; 0.0015; 0.002; 0.003; 0.004; 0.005;  
		0.006; 0.008; 0.01; 0.015; 0.02; 0.03; 0.04;
		0.05; 0.06; 0.08; 0.1; 0.15; 0.2; 0.3; 
		0.4; 0.5; 0.6; 0.8; 1.0; 1.5; 2.0; 3.0; 
		4.0; 5.0; 6.0; 8.0; 10.0; 15.0; 20.0])

and rec xComp   = 
	(array 1 numlev (\[x].x)
        ([(i, v) ;; (i, v) <- combine ([1..numlev], concmap f1 xc)])
	where
	    xc =
	       [0.015; 0.0296; 0.0451; 0.0717; 0.0913; 0.105;
		0.115; 0.128; 0.137; 0.152; 0.160; 0.165; 0.165;
		0.163; 0.160; 0.153; 0.146; 0.133; 0.122;  0.106;
		0.0953; 0.0867; 0.0802; 0.0707; 0.0637; 0.0516;
		0.0440; 0.0346; 0.0289; 0.025; 0.0221; 0.0181; 
		0.0154; 0.0114; 0.00913])

and rec xPair   = 
	(array 1 numlev (\[x].x)
        ([(i, v) ;; (i, v) <- combine ([1..numlev], concmap f1 xp)])
	where
	    xp =
	       [0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;
		0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;
		0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;
		0.0000792; 0.000316; 0.000923; 0.00153; 0.00208;
		0.00256; 0.00343; 0.00414; 0.00547; 0.00652])

and rec xPhot   = 
	(array 1 numlev (\[x].x)
        ([(i, v) ;; (i, v) <- combine ([1..numlev], concmap f1 xpe)])
	where
	    xpe =
	       [2010.0; 632.0; 280.0; 87.7; 37.3; 18.9; 10.4;
		4.01; 1.91; 0.489; 0.192; 0.0491; 0.0186; 0.00887;
		0.00481; 0.00179; 0.000862; 0.000234; 0.0000918;
		0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;
		0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0])
	
	
and f1 x = (if (x > small)
		then [log (2.2*.x)]
		else [nothing])

and f2 x = (if (x > small)
		then [log x]
		else [nothing])

and     -- print cross section tables of constant data
        -- outXsectTbl :: -> [Char]

    outXsectTbl =
          "\nEnergy table:\n" @ showArray 1 numlev ergs
        @ "\nCompton table:\n" @ showArray 1 numlev xComp
        @ "\nPair table:\n" @ showArray 1 numlev xPair
        @ "\nPhoto table:\n" @ showArray 1 numlev xPhot


and     -- print an array of floating point
        -- showArray :: Int -> Int -> (Array Float) -> [Char]
 
    showArray i max table & (max > i) =
            showFloat (table?i) @ "\n" @ showArray (i+1) max table
||  showArray i max table =
            showFloat (table?i) @ "\n"

and
    showFloat n = show_string (ftos n)


and     -- random number generator from seed
        -- genRand :: Random -> (Random, Random)
 
    genRand seed =
        let con1 = 65599.0 in
        let con2 = 71123.0 in
        let r1 = dmod (314557.0*.seed +. 2711.0) 65599 in
        let r2 = dmod (2711.0*.seed +. 314557.0) 71123 in
        (r1/.65599.0, r2/.71123.0)
 

and	-- floating point modulo
        -- dmod :: Float -> Integer -> Float

    dmod x m =
	x -. itof (((ftoi x) / m) * m)

end
