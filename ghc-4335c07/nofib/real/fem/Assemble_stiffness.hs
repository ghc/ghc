-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : assemble_stiffness.hs        DATE : 13-3-1991          *
-- *                                                                    *
-- * CONTENTS : Assemble the global stiffness matrix, stored in         *
-- *            variable bandwidth matrix .                             *
-- *                                                                    *
-- **********************************************************************

module Assemble_stiffness( kdd ) where

import Basics
import Vector
import Matrix
import VBmatrix
import DB_interface
import Degrees
import Pre_assemble
import Elemstif

kdd :: (Array Int Int, Array Int Float) -> Vbm Float

kdd s = 
	incrvbmat initial_value index_value_assoc_s
	where
	initial_value = makevbmat (ndgrs s) (diagadr s) (\ i -> 0.0)
	index_value_assoc_s = index_value_assoc s

index_value_assoc s =
	foldl assemble_s [] [ 1 .. (nelem s)]
	where
	assemble_s = assemble s

assemble s till_now_dd element =
	(till_now_dd++dd_this')
	where
	dd_this' = [ ( 	(f i, f j) ,  x i j) 
                     | i <- [0..n-1], j <- [0..i], f i > 0 , f j > 0 ]
	dgrs_list = (dgrs_list_node nodel) ++ (dgrs_list_node noder)
	(nodel,noder) = getenlr s element
	dgrs_list_node node = getndgr s node
	eindex_dgrs_list = zip [1..6] dgrs_list
	aindex_dgrs_list = filter valid_index eindex_dgrs_list
	valid_index (i,dgr) = (dgr /= 0)
	n = length aindex_dgrs_list
	x i j = matsub (beam2d s element)
		(fst (aindex_dgrs_list !! i), fst (aindex_dgrs_list !! j))
	f i   = (snd (aindex_dgrs_list !!  i))

