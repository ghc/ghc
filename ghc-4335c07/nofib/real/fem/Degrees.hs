-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : degrees.hs 		DATE : 13-3-1991		*
-- * 									*
-- * CONTENTS : Computes the degree numbers of each node.		*
-- *									*
-- * CHANGES :								*
-- * 	1. Mon Mar 11 10:28:10 GMT 1991					*
-- * 	   Add new function degreesrlt for debug use.			*
-- **********************************************************************

module Degrees( ndgrs, getndgr, degreesrlt ) where

import Data.Array
import Basics
import Vector
import DB_interface

ndgrs :: (Array Int Int, Array Int Float) -> Int
	 -- Return the total number of degrees 

getndgr :: (Array Int Int, Array Int Float) -> Int -> [Int]
	 -- Return the degree numbers of a node (U, V and THETA)

ndgrs s	=
	fst (ndgrs_and_dgrsn s)

getndgr s node =
	[u,v,theta]
	where
		u = dgrsn_s ! index
		v = dgrsn_s ! (index + 1)
		theta = dgrsn_s ! (index + 2)
 		dgrsn_s = dgrsn s
		index = (node-1) * 3 + 1

dgrsn :: (Array Int Int, Array Int Float) -> Array Int Int

dgrsn s = listArray (1, (nnode s)*3) (snd (ndgrs_and_dgrsn s))

ndgrs_and_dgrsn :: (Array Int Int, Array Int Float) -> (Int,[Int])

ndgrs_and_dgrsn s =
	foldl counting_one_node_s (0,[]) [1..(nnode s)]
	where 
	counting_one_node_s = counting_one_node s

counting_one_node s (ndgrs_till_now,dgrsn_till_now) i =
	(ndgrs_till_now + ndgrs_this_node, dgrsn_till_now ++ dgrsn_this_node)
	where
		dof = [ fod j | j <- [2,1,0]]
		fod j = if (mod (div bc (e_10 j)) 10 == 1) then 
				1 
			else    0
		e_10 j = if (j == 0) then (1::Int) else 10 * (e_10 (j-1))
		ndgrs_this_node = sum dof
		dgrsn_this_node = [g j | j <- [0,1,2]]
		g j = if ( (dof!!j) == 0 ) then 
				0 
		      else 
				sum (take (j+1) dof) + ndgrs_till_now
		bc =  getnbc s i


degreesrlt :: (Array Int Int, Array Int Float) -> [Char]

degreesrlt s =
	"DEGREE INFORMATION :\n\n" ++
	"\t Total degree numbers = " ++ showlj 4 (ndgrs s) ++ "\n\n" ++
	(concat ( map a_node_s [1..(nnode s)] )) ++ "\n\n"
	where
	a_node_s = a_node s

a_node s node =
      	"  Node.no = " ++ (showrj 2 node) ++ 
	"   u = " ++ (showrj 8 u) ++ "   v = " ++ (showrj 8 v) ++
	" theta=" ++ (showrj 8 theta) ++
	"   bc = " ++ ( showrj 3 bc) ++ "\n"
        where
	bc    = getnbc s node
	[u,v,theta] = getndgr s node


