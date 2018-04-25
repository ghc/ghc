{-
	The calculation element factors and areas

	XZ, 24/10/91
-}

{-
	Modified to adopt S_arrays.

	XZ, 19/2/92
-}

module Elefac ( get_el_det_fac ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto

-----------------------------------------------------------
-- Calculating linear shape function factor and area     --
-- of each element.  The first entry of the output       --
-- tuples is the element area and the second the element --
-- factor.  Called at the data setup stage.              --
-----------------------------------------------------------

get_el_det_fac
	:: Int -> (My_Array Int (Frac_type,Frac_type))
	-> (My_Array Int [Int])
	-> (My_Array Int (Frac_type,((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type))))
get_el_det_fac e_total coord p_steer =
	s_listArray (1,e_total)
	[ 
		(coord!^n1)			`bindTo` ( \ (x1,y1) ->
		(coord!^n2)			`bindTo` ( \ (x2,y2) ->
		(coord!^n3)			`bindTo` ( \ (x3,y3) ->
		((x2-x1)*(y3-y1)-(x1-x3)*(y1-y2)) `bindTo` ( \ e_det ->
		
		(
			((abs (e_det)) / 2),
			(
				(
				(y2-y3)/e_det,
				(y3-y1)/e_det,
				(y1-y2)/e_det
				),
				(
				(x3-x2)/e_det,
				(x1-x3)/e_det,
				(x2-x1)/e_det
				)
			)
		) ))))
		| [n1,n2,n3] <- s_elems p_steer
	]
    where
    	bindTo x k = k x
