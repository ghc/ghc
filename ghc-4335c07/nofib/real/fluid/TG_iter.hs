{-
	Taylor-Galerkin/Pressure-correction algorithm.
	Solving four increamental matrix equations iteratively:
		const1*M(U'-U)     = rh1(U,P)
		const2*M(U''-U)    = rh1(U',P)
		const3*K(P'-P)     = rh2(U')
		const4*M(U'''-U'') = rh3(P'-P)
  The 3rd equation is solved by using the Choleski
	decomposition menthod and the rest are solved by using
	the Jacobi iteration method.

	XZ, 24/10/91
-}

{-
	Modified to adopt S_arrays.

	Evaluations are forced by using normalize_obj.
	(This is currently necessary for running the whole
	program on a 200 element problem).

	XZ, 19/2/92
-}

{-
	Iteration along time-step implemented

	XZ, 25/2/92
-}

module TG_iter ( tg_iter ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Asb_routs
import Rhs_Asb_routs
import Jcb_method
import Chl_method
import Tol_cal

-----------------------------------------------------------
-- Iterative TG algorithm.                    --
-- Functions called :                                    --
--   for equation solving: "chl_method" and "jcb_method" --
--   for RHS assembling  : "get_rh1", "get_rh2" and      --
--                         "get_rh3"                     --
-----------------------------------------------------------

tg_iter
	:: Bool -> Int -> Float -> Int -> Float -> Float -> Float
	-> (S_array (Float, ((Float, Float, Float), (Float, Float, Float))))
	-> (S_array [(Int, Int)])
	-> (S_array [(Int, Int)])
	-> (S_array [Int])
	-> (S_array [Int])
	-> (S_array Bool, (S_array Bool, S_array Bool))
	-> [Int]
	-> (S_array (S_array Float, S_array (Int, [Float])), S_array Int)
	-> (S_array Float, (S_array Float, S_array Float))
	-> String

tg_iter
	mon m_iter m_toler max_jcb_iter jcb_toler
	relax dlt_t el_det_fac v_asb_table p_asb_table
	v_steer p_steer bry_nodes p_fixed chl_fac (p,u) =
	do_tg_iter m_iter (pack_obj p,pack_obj u) []
	where
	do_tg_iter n (old_p,old_u) res =
		if (n<=1) || (max_tol<m_toler)
		then new_res ++ show_final
		else
			if max_tol>large
			then error "main iteration: overflow!"
			else do_tg_iter (n-1) (new_p,new_u) new_res
		where
		new_res =
			res ++
			"at time step " ++ (shows (m_iter-n+1) ":\n\n") ++
			(if mon
			then
				"initial presure:\n" ++ (shows (retrieve_obj old_p) "\n") ++
				"inititial velocities:\n" ++ (shows (retrieve_obj old_u) "\n") ++
				"rh1a:\n" ++ (shows rh1a "\n") ++
				"velocities after the 1st half of step 1:\n" ++ (shows (retrieve_obj u1a) "\n") ++
				"rh1b:\n" ++ (shows rh1b "\n") ++
				"velocities after the 2nd half of step 1:\n" ++ (shows (retrieve_obj u1b) "\n") ++
				"rh2:\n" ++ (shows (retrieve_obj rh2) "\n") ++
				"rh3:\n" ++ (shows rh3 "\n")
			else "")
		show_final =
			"presure:\n" ++ (shows (retrieve_obj new_p) "\n") ++
			"velocities:\n" ++ (shows (retrieve_obj new_u) "\n")
		tmp_p | normalize_obj new_p = retrieve_obj new_p
		(tmp_u_x,tmp_u_y) | normalize_obj new_u = retrieve_obj new_u
		max_tol = max tol_u tol_p
		tol_u =
			tol_cal ((s_elems tmp_u_x)++(s_elems tmp_u_y))
			((subs tmp_u_x old_u_x) ++
			 (subs tmp_u_y old_u_y))
			False
			where
			(old_u_x,old_u_y) | normalize_obj old_u = retrieve_obj old_u
		tol_p | normalize_obj old_p =
			tol_cal (s_elems tmp_p)
			(subs tmp_p (retrieve_obj old_p)) False
		-- step 1a
		rh1a | normalize_obj old_p && normalize_obj old_u =
			get_rh1 el_det_fac v_asb_table v_steer p_steer
			(retrieve_obj old_p,retrieve_obj old_u)
		del_u1a =
			jcb_method 1 el_det_fac v_asb_table v_steer
			bry_nodes rh1a (2/dlt_t) max_jcb_iter jcb_toler relax
		u1a = pack_obj (add_u (retrieve_obj old_u) del_u1a)
		-- step 1b
		rh1b | normalize_obj u1a =
			get_rh1 el_det_fac v_asb_table v_steer p_steer
			(retrieve_obj old_p,retrieve_obj u1a)
		del_u1b =
			jcb_method 1 el_det_fac v_asb_table v_steer
			bry_nodes rh1b (1/dlt_t) max_jcb_iter jcb_toler relax
		u1b = pack_obj (add_u (retrieve_obj old_u) del_u1b)
		-- step 2
		rh2 | normalize_obj u1b =
			pack_obj
			(get_rh2 el_det_fac p_asb_table v_steer p_fixed
			(retrieve_obj u1b))
		del_p | normalize_obj rh2 =
			pack_obj (chl_method chl_fac (retrieve_obj rh2) (2/dlt_t))
		new_p =
			pack_obj (add_mat (retrieve_obj old_p) (retrieve_obj del_p))
		-- step 3
		rh3 | normalize_obj del_p =
			get_rh3 el_det_fac v_asb_table p_steer (retrieve_obj del_p)
		del_u3 =
			jcb_method 3 el_det_fac v_asb_table v_steer
			bry_nodes rh3 (2/dlt_t) max_jcb_iter jcb_toler relax
		new_u = pack_obj (add_u (retrieve_obj u1b) del_u3)

subs = \x' x -> zipWith (-) (s_elems x') (s_elems x)
