-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : displacement.hs              DATE : 13-3-1991          *
-- *                                                                    *
-- * CONTENTS : Compute nodal displacement of the structure.            *
-- *                                                                    *
-- **********************************************************************

module Displacement ( uvw, getnuvw ) where

import Basics
import Vector
import Matrix
import VBmatrix
import VBlldecomp
import DB_interface
import Degrees
import Pre_assemble
import Assemble_stiffness
import Assemble_loadvec

uvw     :: (Array Int Int, Array Int Float) -> Vec Float

getnuvw :: (Array Int Int, Array Int Float) -> Int -> 
	    	Vec Float -> (Float, Float, Float)

t_Ub s = vbllsolution (kdd s) (loadvec s)

uvw s =
	incrvec initial_value index_value_assoc
	where
	initial_value = makevec ( 3 * (nnode s) ) ( \ i -> 0.0 )
	index_value_assoc = concat (map f_s [1..(nnode s)])
	f_s = f s tUb
	tUb = t_Ub s

f s tUb node =
	azip [l,l+1,l+2] (map ff dgrs)
	where
	l = 3 * (node - 1) + 1
	dgrs = getndgr s node
	ff i = if ( i == 0 ) then 0.0 else vecsub tUb i

getnuvw s node uvw =
	(u,v,theta)
	where
	u = vecsub uvw index
	v = vecsub uvw (index+1)
	theta = vecsub uvw (index+2)
	index = 3 * (node - 1) + 1 

