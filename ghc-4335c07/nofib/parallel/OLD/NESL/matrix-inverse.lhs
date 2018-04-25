Time-stamp: <Fri May 17 1996 20:12:28 Stardate: [-31]7519.00 hwloidl>

Matrix inversion.

Based on the NESL code presented in:
   Programming Parallel Algorithms
   by Guy E. Blelloch 
   in CACM 39(3), March 1996
   URL: http://www.cs.cmu.edu/afs/cs.cmu.edu/project/scandal/public/www/nesl/alg-numerical.html

The following inverts a dense matrix. 
It uses Gauss-Jordan elimination without pivoting.

\begin{code}
#if defined(GRAN)
import Strategies
#endif
import Random

dist a 0 = [] 
dist a n = a : dist a (n-1)
\end{code}

\begin{code}
-- gauss_jordan :: [[Int]] -> Int -> [[Int]]
-- gauss_jordan :: (Fractional a) => [[a]] -> Int -> [[a]]
gauss_jordan a i 
 | i == length a = a
 | otherwise =
    let
	(irow:ap) = a                     -- pivot row
	val = irow !! i                   -- pivot elem
	irow' = if val ==(0:%1) 
	          then error "val==0" 
		  else [ v/val | v <- irow ]     -- normalised pivot row
	ap' = [ let scale = jrow !! i
	        in [ v - scale*x | (x, v) <- zip irow' jrow ]
	      | jrow <- ap ]
         
        strategy x = parList rnf irow' `par`
                     parList rnf ap'   `par`
                     x	
    in 
#if defined(GRAN)
    strategy $
#endif
    gauss_jordan (ap'++[irow']) (i+1)

-- matrix_inverse :: (Fractional a) => [[a]] -> [[a]]
matrix_inverse a =
  let
    n = length a
    zeros = map fromIntegral (repeat 0)

    -- Pad the matrix with the identity matrix (i.e. A ++ I) 
    ap = [ row ++ ((take i zeros)++[fromIntegral 1]++(take (n-i-1) zeros))
	 | (row, i) <- zip a [0..n-1] ]

    -- Run Gauss-Jordan elimination on padded matrix 
    ap' = gauss_jordan ap 0

  -- Drop the identity matrix at the front 
  in [ drop n row | row <- ap']

showMat l = foldr (++) "" (zipWith (++) (map show l) (repeat "\n"))

main = 
#ifdef PRINT
       {-
       putStr "Input:\n" >>
       putStr (showMat a) >>
       putStr "\nInverse:\n" >>
       putStr (showMat ai) >>
       putStr "2xInverse:\n" >>
       putStr (showMat aii) >>
       putStr (if a == aii then "OK\n" else "WRONG\n") >>       
       putStr ((take 70 (repeat '-')) ++ "\n") >>
       -}       
       putStr "Input:\n" >>
       putStr (showMat a0) >>
       putStr "\nInverse:\n" >>
       putStr (showMat a0i) 
       --putStr "2xInverse:\n" >>
       --putStr (showMat a0ii) >>
       --putStr (if a0 == a0ii then "OK\n" else "WRONG\n") >>       
       {-
       putStr ((take 70 (repeat '-')) ++ "\n") >>
       putStr "Input:\n" >>
       putStr (showMat b) >>
       putStr "\nInverse:\n" >>
       putStr (showMat bi) >>
       --putStr "2xInverse:\n" >>
       --putStr (showMat bii) >>
       --putStr (if b == bii then "OK\n" else "WRONG\n") >>       
       putStr ((take 70 (repeat '-')) ++ "\n") >>
       putStr "Input:\n" >>
       putStr (showMat c) >>
       putStr "\nInverse:\n" >>
       putStr (showMat ci) >>
       putStr "2xInverse:\n" >>
       putStr (showMat cii) >>
       putStr (if c == cii then "OK\n" else "WRONG\n") >>       
       putStr ((take 70 (repeat '-')) ++ "\n")
       -}
#else
       rnf a0i `seq` putStr "Inversion of a 4x4 matrix finished\n"
#endif
     where
           a :: [[Rational]]     
	   a = [[1, 2, 1], 
		[2, 1, 1], 
		[1, 1, 2]]
	   ai = matrix_inverse a
	   aii = matrix_inverse ai
           {- Result:
             ai = [ -.2500000000   .7500000000  -.2500000000 ]
                  [                                          ]
                  [  .7500000000  -.2500000000  -.2500000000 ]
                  [                                          ]
                  [ -.2500000000  -.2500000000   .7500000000 ]
	   
           -}

           a0 :: [[Rational]]     
	   a0 = [[ 4,  0, -2, 1],
		 [ 1,  3,  1, 0],
		 [ 0, -1,  2, 1],
                 [-1,  2, -3, 0]]
	   a0i = matrix_inverse a0
	   a0ii = matrix_inverse a0i
        	   
           {- Result:
              a0i = 
                        [   11             11      13  ]
                        [  ----   5/62  - ----  - ---- ]
                        [   62             62      62  ]
                        [                              ]
                        [ -1/31   8/31   1/31    4/31  ]
                        [                              ]
                        [                          11  ]
                        [ -5/62   9/62   5/62   - ---- ]
                        [                          62  ]
                        [                              ]
                        [                 27      15   ]
                        [  4/31  -1/31   ----    ----  ]
                        [                 31      31   ]

           -}

           b :: [[Rational]]
           b = [ [ 10,  4,  2, -3,   9, -12 ],
		 [  5,  2, 21, -1,  35,  23 ],
		 [ -6, -8,  4, 11,  17,  -9 ],
		 [  5, 37, -4, 21, -16,  28 ],
		 [ 29,  4,  5, -2,  22,  16 ],
		 [ -3,  1,  8,  3,  -2,   1 ] ]
	   bi = matrix_inverse b
	   bii = matrix_inverse bi
           {- NB: b can't be inverted using gauss-jordan!!!!!!
	      Result:
              bi =
   [     25867      1523465     76535       27773     2436825     1169827  ]
   [ - --------  - --------   --------   - -------   --------    --------  ]
   [   26972084    53944168   53944168     6743021   53944168    26972084  ]
   [                                                                       ]
   [    383060     625141       489729     274473      927785       231213 ]
   [   -------    --------   - --------   --------  - --------   - ------- ]
   [   6743021    26972084     26972084   13486042    26972084     6743021 ]
   [                                                                       ]
   [   328283       94281       645991      52625     340847      3115429  ]
   [  --------    --------   - --------  - -------   --------    --------  ]
   [  26972084    53944168     53944168    6743021   53944168    26972084  ]
   [                                                                       ]
   [    706969      1511165    2825327     156227     1463413     1152725  ]
   [ - --------  - --------   --------    --------   --------    --------  ]
   [   26972084    53944168   53944168    13486042   53944168    26972084  ]
   [                                                                       ]
   [   470537      1265713     817829       45117      756729      1701745 ]
   [  --------    --------    --------    --------  - --------  - -------- ]
   [  26972084    53944168    53944168    13486042    53944168    26972084 ]
   [                                                                       ]
   [     293531    122499       115833     11221      133893        94670  ]
   [  - -------   --------   - --------   -------    --------    - ------- ]
   [    6743021   13486042     13486042   6743021    13486042      6743021 ]

           -}	   

           n = 8
           c_l = take (n*n) (r_list 99)
           f l | length l < n = []
               | otherwise    = let (a,b) = (splitAt n l) in (map fromIntegral a) : f b
           c :: [[Rational]]
           c = f c_l
           ci = matrix_inverse c	   
           cii = matrix_inverse ci	   
\end{code}

-----------------------------------------------------------------------------

This is the original NESL code:


function Gauss_Jordan(A,i) =
if (i == #A) then A
else 
    let
	(irow,Ap) = head_rest(A);
	val = irow[i];
	irow = {v/val : v in irow};
	Ap = {let scale = jrow[i]
	      in {v - scale*x : x in irow; v in jrow}
	      : jrow in Ap}
in Gauss_Jordan(Ap++[irow],i+1) $

function matrix_inverse(A) =
  let
    n = #A;

    % Pad the matrix with the identity matrix (i.e. A ++ I) %
    Ap = {row ++ rep(dist(0.,n),1.0,i):
	 row in A; i in [0:n]};

    % Run Gauss-Jordan elimination on padded matrix %
    Ap = Gauss_Jordan(Ap,0);

  % Drop the identity matrix at the front %
  in {drop(row,n) : row in Ap} $

A = [[1.0, 2.0, 1.0], [2.0, 1.0, 1.0], [1.0, 1.0, 2.0]];
AI = matrix_inverse(A);
matrix_inverse(AI);