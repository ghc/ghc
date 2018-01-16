-- Glasgow Haskell 0.403 : BLAS ( Basic Linear Algebra System )
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : vblldecomp.hs		DATE : 5-3-1991                 *
-- *                                                                    *
-- * CONTENTS : LL decomposition method for variable bandwidth matrix   *
-- * 									*
-- **********************************************************************

module VBlldecomp(vblldecomp, vbllsolution, vbllsolution') where

import Basics

import Vector

import VBmatrix

vblldecomp :: Vbm Float -> Vbm Float 

	-- L[i,i] = sqrt(A[i,i] - ***)
	--          where
	--	    *** = SUM [ L[i,k]*L[i,k] | k <- [1..i-1] ]
	-- L[i,j] = (A[i,j] - ***) / L[j,j]
	--	    where
	--	    *** = SUM [ L[i,k]*L[j,k] | k <- [1..j-1] ]

vbllsolution :: Vbm Float -> Vec Float -> Vec Float
	-- Solve equation system, the variable bandwidth matrix is 
	-- undecomposed, ie it is the original matrix.

vbllsolution' :: Vbm Float -> Vec Float -> Vec Float
        -- Solve equation system, the variable bandwidth matrix is decomposed.

vblldecomp mA = m
        where 
        m = makevbmat (boundvbmat mA) (diagadrvbm mA) f 
        f (i,j) =
		if (i == j) then 
			sqrt ( vbmatsub mA (i,i)  -
			       sum ( map ( \ k -> vbmatsub m (i,k) *
                                           vbmatsub m (i,k) )
			  	        [(fstclvbmat mA i) .. (i-1)] ) )

		else 
		    (	vbmatsub mA (i,j) -
			sum ( map ( \ k -> (vbmatsub m (i,k) *
                                            vbmatsub m (j,k) ) )
				  [ (k0 i j) .. (j - 1)] )
                    ) / ( vbmatsub m (j,j) )
	k0 i j = if ( (fstclvbmat mA i) >= (fstclvbmat mA j) ) then
	   	     (fstclvbmat mA i)
	         else 
		     (fstclvbmat mA j)

vbllsolution a b =
	fst (backwarding ( forwarding (b, vblldecomp a) ) )

vbllsolution' a' b =
	fst (backwarding ( forwarding (b, a') ) )

forwarding ( b,  mVB ) =
	(b',mVB)
	where
	b' = makevec n f
	n = boundvec b
	f i = ( vecsub b i -
		sum [(vbmatsub mVB (i,j)) * (vecsub b' j) | j<-[l i..i-1] ] 
              ) / (vbmatsub mVB (i,i))
	l i = fstclvbmat mVB i

backwarding ( b, mVB ) =
	(b',mVB)
	where
	n = boundvec b
	b' = makevec n f
	f i = ( vecsub b i -
		sum [( vbmatsub mVB (j,i) ) * ( vecsub b' j )
                     | j <- ( validj i [i+1..n] ) ]
              ) / (vbmatsub mVB (i,i))
	validj i (j:js) = if ( i >= fstclvbmat mVB j ) then 
				j : (validj i js)
                          else 
				validj i js
	validj i []      = []

