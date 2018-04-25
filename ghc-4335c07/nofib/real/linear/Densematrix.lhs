\section{Densematrix} 

  ============================================================+
  ================== A MATRIX IMPLEMENTATION ==================
  ============================================================+


   This is an implementation of the types "matrix" and "vector"
   and their associated operations.  It is most appropriate for
   dense matrices and vectors.
   
  The operations involved are:
   
         mmult matvecmult vmmult  Multiplication
         madd  vadd               Addition
         msub  vsub               Subtraction
         vdot  vouter             Vector outer and dot products
         minverse                 the inverse of a matrix.
         msize vsize              Find size of data
                                  {(#rows,#cols) for matrices}
         norm                     Norm of a vector (vdot v v)
         swaprow swapcol          swaprow a b m ==> swap ath and bth rows
                                  of matrix m
         droprow                  drop the first row of a matrix.
         getrow getcol            getrow i m  ==>  get the ith row of a matrix
         svmult                   scalar-vector multiplication
         update vupdate           update (i,j) x m
                                  vupdate i x v  ==> update a singe element
         vhd vtl mergevectors     treat vectors like [Float]
         mkvec mkrvec mkcvec      vector creation from [Float].
         mkmat mkrmat mkcmat      matrix creation from [[Float]].  mkcmat
                                  assumes the arg is column major.

\begin{code}

module Densematrix(Matrix,Vector,
        mmult, madd, msub, vouter, vdot, norm ,mneg ,mxpose,mident, msize,
        mkmat, mkrmat, mkcmat, mkvec, mkrvec, mkcvec,
        vadd, vsub, vsize, vneg,
        swaprow, swapcol ,droprow, getrow, getcol,
	subscript, vsubscript, vecpart,
        update ,vupdate, update2,
        vhd, vtl,mergevectors,
        matvecmult, vmmult,svmult,
        showmatrix,displayvector,
        minverse,
        veclist, matlist ) where

import Data.List (transpose)
import Utils

type Matrix    =  [[Float]]
type Vector    =  [Float]



mmult   :: Matrix -> Matrix -> Matrix
madd    :: Matrix -> Matrix -> Matrix
msub    :: Matrix -> Matrix -> Matrix
vadd    :: Vector -> Vector -> Vector
vsub    :: Vector -> Vector -> Vector
vsize   :: Vector -> Int
vouter  :: Vector -> Vector -> Matrix
vdot    :: Vector -> Vector -> Float
norm    :: Vector -> Float
vneg    :: Vector -> Vector
mneg    :: Matrix -> Matrix
mxpose  :: Matrix -> Matrix
mident  :: Int -> Matrix
msize   :: Matrix -> (Int,Int)
mkmat   :: [[Float]] -> Matrix
mkrmat  :: [[Float]] -> Matrix
mkcmat  :: [[Float]] -> Matrix
mkvec   :: [Float] -> Vector
mkrvec  :: [Float] -> Vector
mkcvec  :: [Float] -> Vector
veclist :: Vector -> [Float]
matlist :: Matrix -> [[Float]]

swaprow    :: Int -> Int -> Matrix -> Matrix
swapcol    :: Int -> Int -> Matrix -> Matrix
droprow    :: Matrix -> Matrix
getrow     :: Int -> Matrix -> Vector
getcol     :: Int -> Matrix -> Vector

subscript    :: Matrix -> (Int,Int) -> Float
vsubscript   :: Vector -> Int -> Float
vecpart      :: Int -> Int -> Vector -> Vector
update       :: Matrix -> (Int,Int) -> Float -> Matrix
update2      :: Matrix -> (Int,Int,Int) -> (Float,Float) -> Matrix
vupdate      :: Vector -> Int -> Float -> Vector
vhd          :: Vector -> Float
vtl          :: Vector -> Vector
mergevectors :: [Vector] -> Vector

minverse      :: Matrix -> Matrix
lu_decomp     :: Matrix -> Lu_factor
apply_factor  :: Lu_factor -> Matrix -> Matrix
forward       :: [[Float]] -> [Int] -> Matrix -> Matrix
backward      :: [[Float]] -> [Int] -> Matrix -> Matrix

showmatrix :: Matrix -> [Char]
displayvector :: Vector -> [Char]

matvecmult :: Matrix -> Vector -> Vector
vmmult :: Vector -> Matrix -> Vector
svmult :: Float -> Vector -> Vector

\end{code}


Notes:
        vmmult implements an inner product vector matrix
        multiplication.




\begin{code}

mmult m n = if compatible then [row i | i <- [0..((length m)-1)]]
            else error errmsg
            where
              compatible = (snd (msize m)) == (fst (msize n))
              row i = [item i j | j <- [0..length(head n)-1]]
              item i j = vdot (m!!i) (map (!!j) n)
              errmsg = "mmult in densematrix: incompatible matrices " ++
                  (show (msize m)) ++ "*" ++ (show (msize n))





matvecmult m v = map (vdot v) m




vmmult v m = res
     where
        res' = mmult [v] m
        res = concat res'


svmult s v = map (*s) v


vdot u v = sum (map2 (*) u v)

norm v = vdot v v

vneg = map negate

vouter u v = [ map (k*) u | k <- v]


madd a b = map2 vadd a b

vadd u v = map2 (+) u v

msub a b = map2 vsub a b

vsub u v = map2 (-) u v

vsize = (length)

mneg rs = map (map negate) rs

mxpose = transpose

mident n
   = [ row k | k <- [1..n] ]
     where
        row i = (rep (i-1) 0) ++ [1] ++ (rep (n-i) 0)


msize m = (length m , length(head m))

mkmat = id
mkrmat = id
mkcmat = transpose
matlist = id


mkvec =  id
mkrvec = id
mkcvec = id
veclist = id



swaprow a b m = swapitems a b m
swapcol a b m = map (swapitems a b) m

swapitems :: Int -> Int -> [a] -> [a]
swapitems a b xs
   = if a< b then toa ++ [xs!!b] ++ atob ++ [xs!!a] ++ pastb
     else if a > b then swapitems b a xs else xs
     where
        toa = take a xs
        atob = take (b-a-1) (drop (a+1) xs)
        pastb = drop (b+1) xs


droprow = tail


getrow i m =  m!!i
getcol i m = map (!!i) m


subscript m (i,j) = m !! i !! j

vsubscript v n = v!!n

vecpart start intber
   = (take intber) . (drop start)

update m (i,j) val
   = (take i m) ++ [(f (m!!i))] ++ (drop (i+1) m)
     where
        f xs = (take j xs) ++ [val] ++ (drop (j+1) xs)

update2 m (i1,i2,j) (val1,val2)
   = (take i1 m) ++ [(f1 (m!!i1))] ++ [(f2 (m!!i2))] ++ (drop (i1+2) m)
     where
        f1 xs = (take j xs) ++ [val1] ++ (drop (j+1) xs)
        f2 xs = (take j xs) ++ [val2] ++ (drop (j+1) xs)


vupdate v i val
   = (take i v) ++ [val] ++ (drop (i+1) v)


vhd = head
vtl = tail

mergevectors = concat


showmatrix m = "\n" ++ showmat m ++ "\n"

showmat :: [Vector] -> [Char]
showmat m
   = concat (map show_row m)
     where
        show_row v = (displayvector v) ++ "\n"


displayvector
   =  concat                 .
      (map (rjustify 13)) .
      (map show)

\end{code}


  ============================================================
       ============== LU DECOMPOSITION ============== 
       ============== (to do inverses) ============== 
  ============================================================


   An lu_factor is a rather contorted representation of an LU
   factorization.  The first list of vectors represents the L
   matrix, the second represents the U part.  The third list
   contains information about how the matrix was permuted as
   it was decomposed.

   About the only thing one can do with an Lu_factor is to
   use it as an argument to apply_factor.



Notes:

   minverse returns the inverse of a matrix.

   apply_factor takes one of these lu_factors and applies it
   to a matrix.  This has the effect of multiplying the inverse
   of the factorized matrix by the given matrix.

   forward and backward do the forward and backward substitution
   on the pieces of an lu_factor.

   lu_decomp a
   returns ( l-vectors, u-vectors, p-vector )
   A baroque representation of an LU factorization.

   pivot m pivots the matrix m. It returns the pivoted matrix and
   the row on which the pivot was perfomed.
   pivoting preserves diagonal elements.  two rows and
   two columns are swapped.

   findpivot m finds the row/column to pivot on in (square) matrix m.
   the pivot row/col is the one with the element of greatest
   absolulte value on the diagonal.


\begin{code}

data Lu_factor = Lu_fact [[Float]] [[Float]] [Int]



minverse m
   = apply_factor (lu_decomp m) (mident (fst(msize m)))


apply_factor factor m
   = ((backward uvecs ps) . (forward lvecs ps)) m
     where
        (Lu_fact lvecs uvecs ps) = factor


  
forward  [] p b = b
forward (l:ls) (p:ps) b
   = y1 : (forward ls ps y')
     where
        y'  = msub y (vouter y1 l)
        (y1:y) = swaprow 0 p b


backward [[u]] p y = map (map (/u)) y
backward ((u1:u):us) (p:ps) (y:ys)
  = x
    where
       x = swaprow 0 p (xk:nextx)
       xk = map (/u1) (vsub y (vmmult u nextx))
       nextx = backward us ps ys




lu_decomp a
   = if (fst(msize a) == 1) then Lu_fact [] [[only_one]] [1]
      else Lu_fact (l:ls)  (u:us)  (p:ps)
     where
        Lu_fact ls us ps  = lu_decomp a11'
        (a',p)  = pivot a
        u11  = head u
        u  = getrow 0 a'
        m  = getcol 0 a'
        l  = if not(u11 == 0) then tail (map (/ u11) m)
             else error ("div by 0 in lu: block is\n  "++(show a))
        a11  = map tail (tail a')
        a11' = msub a11 (vouter (tail u) l)
        only_one  = subscript a (0,0)




pivot :: Matrix -> (Matrix,Int)
pivot m
   = (swaprow 0 p (swapcol 0 p m), p)
     where
        p = findpivot m


findpivot :: Matrix -> Int
findpivot a
   = loc_of_max 0 diag
     where
       diag = [a `subscript` (i,i) |  i <- [0..(fst(msize a))-1] ]
       absmax = maxlist (map absfloat diag)
       loc_of_max n (x:xs)
                = if (absfloat x) == absmax then n
                  else loc_of_max (n+1) xs
       

absfloat :: Float -> Float
absfloat n = if n < 0 then -n
             else n

maxlist :: [Float] -> Float
maxlist xs = foldl1 max xs

\end{code}


            






