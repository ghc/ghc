module TG_iter ( tg_iter ) where

import S_Array
import Defs
import Jcb_method
import Quad_def
import Chl_method_q

-----------------------------------------------------------
-- Iterative TG algorithm                                --
-- Functions called :                                    --
--   for equation solving: "chl_method" and "jcb_method" --
--   for RHS assembling  : "get_rh1", "get_rh2" and      --
--                         "get_rh3"                     --
-----------------------------------------------------------

tg_iter :: Bool -> Bool -> Int -> a -> Int -> Float -> Float ->
	Float -> (My_Array Int ([(([Int], (Float, ((Float, Float, Float),
	(Float, Float, Float)))), Int)], Bool),
	My_Array Int ([(([Int], (Float, ((Float, Float, Float),
	(Float, Float, Float)))), Int)],
	((Float, Float), (Bool, (Bool, Bool))))) -> TriMat Float ->
	(My_Array Int Float, My_Array Int (Float, Float)) -> [Char]

tg_iter
	mon simpl m_iter m_toler max_jcb_iter jcb_toler
	relax dlt_t n_lists@(p_node_list,v_node_list)
	tri_fac init@(p,u)
	= show_fin (last res)
	where
	res = take m_iter (tail (iterate iter_f init))
	n_bnds = s_bounds u
	p_total = snd (s_bounds p)
	get_rh1_f = get_rh1 v_node_list
	get_rh2_f = get_rh2 p_node_list
	get_rh3_f = get_rh3 v_node_list
	jcb_f = jcb_method max_jcb_iter jcb_toler relax n_bnds v_node_list
	iter_f (old_p,old_u) = (new_p,new_u)
		where
		tmp_get_rh1_f = get_rh1_f old_p

-- section start for full TG
		-- step 1a
		rh1a = tmp_get_rh1_f True old_u old_u
		u1a = add_u old_u (jcb_f simpl 1 (2/dlt_t) rh1a)
		-- step 1b
		rh1b =
			if simpl
			then tmp_get_rh1_f False old_u u1a
			else tmp_get_rh1_f True u1a u1a
		u1b = add_u old_u (jcb_f simpl 1 (1/dlt_t) rh1b)
		-- step 2
		rh2 = get_rh2_f u1b
		del_p = chl_method_q tri_fac rh2 (2/dlt_t)
		new_p = add_p old_p del_p
		-- step 3
		rh3 = get_rh3_f del_p
		new_u = add_u u1b (jcb_f False 3 (2/dlt_t) rh3)
-- section end for full TG

	show_fin (new_p,new_u) =
		"pressure:\n" ++
		(shows (s_elems new_p) "\n") ++
		"velocities:\n" ++
		(shows (s_elems new_u) "\n")
