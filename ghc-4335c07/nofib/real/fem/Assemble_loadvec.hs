-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : assemble_loadvec.hs          DATE : 13-3-1991          *
-- *                                                                    *
-- * CONTENTS : Assemble the global load vector.                        *
-- *                                                                    *
-- **********************************************************************

module Assemble_loadvec (loadvec) where

import Basics
import Vector
import DB_interface
import Degrees

loadvec :: (Array Int Int, Array Int Float) -> Vec Float

loadvec s =
	incrvec initial_value index_value_assoc_s
	where
	initial_value = makevec (ndgrs s) ( \ i -> 0.0 )
	index_value_assoc_s = index_value_assoc s 

index_value_assoc s =
	foldl assemble_s [] [1 .. (nplds s)]
	where
	assemble_s = assemble s 

assemble  s till_now_d ii =
	till_now_d ++ d_this'
	where
	d_this' = filter valid_degree (azip degrees loads)
	valid_degree (dgr , x) = (dgr /=0)
	degrees = getndgr s node
	loads   = [px,py,m]
	(node,px,py,m) = getpld s ii
