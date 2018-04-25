-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : pre_assemble.hs        DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Computes the diagonal element address vector of global  *
-- *		stiffness matrix.					*
-- *                                                                    *
-- * CHANGES  :  							*
-- * 	1. Mon Mar 11 10:38:35 GMT 1991					*
-- *		Add function "diagadrrlt" for debug use.		*
-- **********************************************************************

module Pre_assemble( diagadr , diagadrrlt) where

import Basics
import Vector
import DB_interface
import Degrees

diagadr :: (Array Int Int, Array Int Float) -> Vec Int

diagadr s =
	v
	where
 	v = makevec bound f
	bound = boundvec (bandvec_s)
	f i   = 
		if ( i == 1) then 1 
		else vecsub v (i-1) + vecsub bandvec_s i
	bandvec_s = bandvec s

bandvec s =
	maxupdvec initial_value 
		( concat (map pre_assemble_s [1..(nelem s)]))
	where
	initial_value = makevec (ndgrs s) (\ i -> 0) 
	pre_assemble_s = pre_assemble s

pre_assemble s element =
	azip dgrs_list (map f dgrs_list)
	where
	f x = x - (head dgrs_list) + 1
	dgrs_list = (dgrs_list_node nodel) ++ (dgrs_list_node noder)
	(nodel,noder) = getenlr s element
	dgrs_list_node node = filter (\x -> x /= 0) ( getndgr s node )


diagadrrlt :: (Array Int Int, Array Int Float) -> [Char]

diagadrrlt s =
	"DIAGONAL ADDRESS VECTOR=\n" ++ 
	displayvec (diagadr s)
