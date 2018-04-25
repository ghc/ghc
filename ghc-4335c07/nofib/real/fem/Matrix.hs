-- Glasgow Haskell 0.403 : BLAS ( Basic Linear Algebra System )
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : matrix.hs		DATE : 5-3-1991                 *
-- *                                                                    *
-- * CONTENTS : Matrix datatype and operations implemented by using     *
-- *		vector datatype(grounded as Haskell Array type).        *
-- *		Matrices are index from 1, ie the row numbers are from  *
-- *		1 to nr and column numbers are from 1 to nc. Where nr   *
-- *		and nc are the number of rows and columns respectively. *
-- **********************************************************************

module Matrix(Mat, makemat, boundmat, matsub, incrmat, updmat,
              mmatvec, mmatmat, row, col, intchrow, intchcol, interchmat,
              displaymat) where

import Data.Ix
import Basics

import Vector

data Mat a = MAT (Int,Int) (Vec a)

makemat :: (Int, Int) -> ( (Int,Int) -> a) -> Mat a

boundmat :: Mat a -> (Int,Int)

incrmat :: (Num a) => Mat a -> [((Int,Int),a)] -> Mat a

updmat  :: Mat a -> [((Int,Int),a)] -> Mat a

matsub  :: Mat a -> (Int,Int) -> a

mmatvec :: (Num a) => Mat a -> Vec a -> Vec a

mmatmat :: (Num a) => Mat a -> Mat a -> Mat a

row     :: Mat a -> Int -> Vec a

col     :: Mat a -> Int -> Vec a

intchrow :: Int -> Int -> Mat a -> Mat a

intchcol :: Int -> Int -> Mat a -> Mat a

interchmat :: (Int,Int) -> (Int,Int) -> Mat a -> Mat a

displaymat :: (Show a) => Mat a -> [Char]

makemat (nr,nc) g =
	MAT (nr,nc) 
	(makevec (nr*nc) 
		 (\i -> (map g (range ((1,1),(nr,nc)) )) !! (i-1) )
        )

boundmat (MAT (nr,nc) elements) = (nr,nc)

updmat m s =
	MAT (nr,nc) new_elements
        where
        MAT (nr,nc) elements = m
        new_elements = updvec elements new_s
        new_s = map (\( ((i,j),x) ) -> ( ((i-1)*nc+j,x) ) ) s

incrmat m s =
	MAT (nr,nc) new_elements
	where
	MAT (nr,nc) elements = m
	new_elements = incrvec elements new_s
	new_s = map (\( ((i,j),x) ) -> ( ((i-1)*nc+j,x) ) ) s

matsub m (i,j) =
	vecsub elements ((i-1)*nc+j)
	where
	MAT (nr,nc) elements = m

mmatvec m v =
        makevec nr ( \ i -> sum [ (matsub m (i,j)) * (vecsub v j) 
                                  | j <- [1..nc] ] )
	where
	(nr,nc) = boundmat m

mmatmat m1 m2 = 
     if (t1 == t2) then 
        makemat (l,n)
                ( \ (i,j) -> sum [ (matsub m1 (i,k)) *
				   (matsub m2 (k,j)) | k <-[1..t1] ] )
     else error "Dimension error"
     where
        (l,t1) = boundmat m1
        (t2,n) = boundmat m2

row m i =
        makevec n ( \ j -> matsub m (i,j) )
        where
        (_,n) = boundmat m

col m j =
        makevec n ( \ i -> matsub m (i,j) )
        where
        (n,_) = boundmat m

intchrow i j m =
        makemat (boundmat m)
                (\ (r,c) ->      if (r==i) then matsub m (j,c)
                            else if (r==j) then matsub m (i,c)
                            else		matsub m (r,c) )

intchcol i j m =
        makemat (boundmat m)
                (\ (r,c) ->      if (c==i) then matsub m (r,j)
                            else if (c==j) then matsub m (r,i)
                            else                matsub m (r,c) )

interchmat (i1,j1) (i2,j2)  m =
        makemat (boundmat m)
                (\ (i,j) ->      if (i,j)==(i1,j1) then matsub m (i2,j2)
                            else if (i,j)==(i2,j2) then matsub m (i1,j1)
                            else                        matsub m (i,j) )

displaymat m =
        "<\n" ++
        concat [displayvec (row m i) | i<-[1..nr] ] ++
        ">\n"
        where
        (nr,_) = boundmat m

