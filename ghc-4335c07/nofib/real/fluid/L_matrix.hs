{-
	L_matriices for RHS assembling

	XZ, 24/10/91
-}

{-
	Modified to adopt S_array

	XZ, 19/2/92
-}

module L_matrix ( l_mat, l_mat' ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto

-----------------------------------------------------------
-- Element matrix L.                                     --
-- Used in assembling rh1 and rh3.                       --
--   Parameters :                                        --
--     gdi : ith entry of element factor component       --
-----------------------------------------------------------

l_mat :: (My_Array Int ((Frac_type,Frac_type,Frac_type) -> [Frac_type]))
l_mat = -- element_factor
	s_listArray (1,v_nodel) [
		(\(gd1,gd2,gd3) -> [ gd1, 0.0, 0.0 ]),
		(\(gd1,gd2,gd3) -> [ 0.0, gd2, 0.0 ]),
		(\(gd1,gd2,gd3) -> [ 0.0, 0.0, gd3 ]),
		(\(gd1,gd2,gd3) -> [gd2+gd3, gd2+gd3+gd3, gd3+gd2+gd2]),
		(\(gd1,gd2,gd3) -> [gd1+gd3+gd3, gd1+gd3, gd3+gd1+gd1]),
		(\(gd1,gd2,gd3) -> [gd1+gd2+gd2, gd1+gd1+gd2, gd1+gd2])
	]

-----------------------------------------------------------
-- Transposition of the element matrix L.                --
-- Used in assembling rh2.                               --
--   Parameters :                                        --
--     gdi : ith entry of element factor component       --
-----------------------------------------------------------

l_mat' :: (My_Array Int ((Frac_type,Frac_type,Frac_type) -> [Frac_type]))
l_mat' = -- element_factor
	s_listArray (1,p_nodel) [
		(\(gd1,gd2,gd3) ->
			[ gd1, 0.0, 0.0, gd2+gd3, gd1+gd3+gd3, gd1+gd2+gd2 ]),
		(\(gd1,gd2,gd3) ->
			[ 0.0, gd2, 0.0, gd2+gd3+gd3, gd1+gd3, gd1+gd1+gd2 ]),
		(\(gd1,gd2,gd3) ->
			[ 0.0, 0.0, gd3, gd3+gd2+gd2, gd3+gd1+gd1, gd1+gd2 ])
	]
