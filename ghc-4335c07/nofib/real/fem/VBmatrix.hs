-- Glasgow Haskell 0.403 : BLAS ( Basic Linear Algebra System )
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : vbmatrix.hs		DATE : 5-3-1991                 *
-- *                                                                    *
-- * CONTENTS : Variable bandwidth matrix data type and operations      *
-- *            implemented by using one-dimension array type.          *
-- **********************************************************************

module VBmatrix(Vbm, defvbmat, makevbmat, incrvbmat, updvbmat, 
                vbmatsub, boundvbmat, addrvbmat, lengrvbmat, fstclvbmat, 
                diagadrvbm, displayvbmati, displayvbmatr) where

import Basics
import Vector

data Vbm a = VBMAT Int (Vec Int) (Vec a)

defvbmat   :: Int -> Vec Int -> Vec a -> Vbm a

makevbmat  :: Int -> Vec Int -> ( (Int,Int) -> a ) -> Vbm a
	-- make a variable bandwidth matrix, by giving the diagonal
	-- element address vector and a element value generator.

updvbmat   :: Vbm a -> [((Int,Int),a)] -> Vbm a
	-- update matrix with the given index-value association list

incrvbmat  :: (Num a) => Vbm a -> [((Int,Int),a)] -> Vbm a
        -- increase matrix by the given index-value association list

vbmatsub   :: Vbm a -> (Int,Int) -> a
	-- Access to the given matrix element value

boundvbmat :: Vbm a -> Int
	-- Return the bounds of the variable bandwidth matrix

addrvbmat  :: Vbm a -> (Int,Int) -> Int
	-- Return the address of element [i,j]

lengrvbmat :: Vbm a -> Int -> Int
        -- Return the length (the number of non-zero elements) of row i

fstclvbmat :: Vbm a -> Int -> Int
        -- Return the first non-zero element's  coulmn number on at row i

diagadrvbm :: Vbm a -> Vec Int
	-- Return the diagonal element address vector

displayvbmati  ::  Vbm Int -> [Char]

displayvbmatr  ::  Vbm Float -> [Char]

lengrvbmat (VBMAT n addiag elems) i =
	if (i==1) then 1 
	else (vecsub addiag i) - (vecsub addiag (i-1))

fstclvbmat (VBMAT n addiag elems) i =
	if (i==1) then 1
	else i - ( lengrvbmat (VBMAT n addiag elems) i ) + 1

addrvbmat vbm  (i,j) =
	vecsub addiag i + j -  i
	where
	(VBMAT n addiag elementlist) = vbm

boundvbmat (VBMAT bounds addiag elementlist) = bounds

diagadrvbm (VBMAT bounds addiag elementlist) = addiag

defvbmat bounds addiag elementlist =
	VBMAT bounds addiag elementlist

makevbmat n addiag generator =
	VBMAT n addiag (makevec (vecsub addiag n) f)
	where 
	f i    = elemts !! (i - 1)
	elemts = foldl  irow []  [1..n] 
	irow ls i = ls ++ [ generator (i,j) | j<- [(fstcl i)..i] ]
	fstcl i   = if (i==1) then 1
		    else i - vecsub addiag i + vecsub addiag (i-1) + 1

incrvbmat vbm updates =
	VBMAT n addiag new_elements
        where
	(VBMAT n addiag elements) = vbm
	new_elements = incrvec elements new_s
	new_s = map (\((i,j),x) -> (addrvbmat vbm (i,j),x) ) updates

updvbmat vbm updates =
        VBMAT n addiag new_elements
        where
        VBMAT n addiag elements = vbm
        new_elements = updvec elements new_s
        new_s = map (\((i,j),x) -> (addrvbmat vbm (i,j),x) ) updates

vbmatsub vbm (i,j) =
	vecsub elements (addrvbmat vbm (i,j))
        where
        VBMAT n addiag elements = vbm

displayvbmati vbm =
	"<  \n" ++
	concat (map displayvec rows) ++
	"> \n"
	where
	rows = [rowi vbm i | i <- [1..n]]
	n = boundvbmat vbm

displayvbmatr vbm =
        "<  \n" ++
        concat (map displayvec rows) ++
        "> \n"
        where
	rows = [rowr vbm i | i <- [1..n]]
        n = boundvbmat vbm

rowi vbm i =
	makevec n f
	where
	n = boundvbmat vbm
	f j = if ( (j >= (fstclvbmat vbm i)) && (j <= i) ) then
		vbmatsub vbm (i,j)
	      else 0

rowr vbm i =
        makevec n f
        where
        n = boundvbmat vbm
        f j = if ( (j >= (fstclvbmat vbm i)) && (j <= i) ) then
                vbmatsub vbm (i,j)
              else 0.0
