{-
	RHS assembling functions

	XZ, 24/10/91
-}

{-
	Modified to adopt S_array

	XZ, 19/2/92
-}

{-
	Modified

	XZ, 25/2/92
-}

module Rhs_Asb_routs ( get_rh1, get_rh2, get_rh3 ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import S_matrix
import C_matrix
import L_matrix
import Asb_routs
import Data.Ix

-----------------------------------------------------------
-- Calculating the right-hand-side for step 1.           --
-- Called in TG iteration.                               --
-----------------------------------------------------------

get_rh1
	:: (My_Array Int (Frac_type,((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type))))
	-> (My_Array Int [(Int,Int)])
	-> (My_Array Int [Int])
	-> (My_Array Int [Int])
	-> ((My_Array Int Frac_type),
			(My_Array Int Frac_type, My_Array Int Frac_type))
	-> (My_Array Int Frac_type, My_Array Int Frac_type)

get_rh1 el_det_fac asb_table v_steer p_steer (p,(u1,u2)) =
	(
		s_listArray n_bnds (map (sum.(map fst)) pair_list),
		s_listArray n_bnds (map (sum.(map snd)) pair_list)
	)
	where
	pair_list =
		[
			[
				(el_det_fac!^e)			`bindTo` ( \ el_d_f ->
				(fst el_d_f)			`bindTo` ( \ det ->
				(snd el_d_f)			`bindTo` ( \ fac ->
				(l_mat!^id)			`bindTo` ( \ l_m ->
				((s_mat!^id) fac)		`bindTo` ( \ s_m ->
				(get_val u1 (v_steer!^e),get_val u2 (v_steer!^e)) `bindTo` ( \ u ->
				(get_val p (p_steer!^e))	`bindTo` ( \ p_val ->
				((c_mat!^id) fac u)		`bindTo` ( \ c_m ->
				(
					((fst u) `bindTo` ( \ u_s ->
					(
						(list_match_prod u_s s_m) / (-3.0) +
						(list_match_prod u_s c_m) / (-1260.0) +
						(list_match_prod p_val (l_m (fst fac))) / 3.0
					) * det ))
					,
					((snd u) `bindTo` ( \ u_s ->
					(
						(list_match_prod u_s s_m) / (-3.0) +
						(list_match_prod u_s c_m) / (-1260.0) +
						(list_match_prod p_val (l_m (snd fac))) / 3.0
					) * det ))
				) ))))))))
				| (e,id)<-tab_elem
			]
			| tab_elem <- s_elems asb_table
		]
	n_bnds = s_bounds asb_table
	bindTo x k = k x

-----------------------------------------------------------
-- Calculating the right-hand-side for step 2.           --
-- Called in TG iteration.                               --
-----------------------------------------------------------

get_rh2
	:: (My_Array Int (Frac_type,((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type))))
	-> (My_Array Int [(Int,Int)])
	-> (My_Array Int [Int])
	-> [Int]
	-> (My_Array Int Frac_type, My_Array Int Frac_type)
	-> (My_Array Int Frac_type)

get_rh2 el_det_fac asb_table v_steer p_fixed (u1,u2) =
	s_def_listArray p_bnds (0::Frac_type)
	[
		if ( i `elem` p_fixed )
		-- for fixed entry
		then 0
		-- otherwise
		else
			(
				sum
				[
					(v_steer!^e) `bindTo` (\ v_s ->
					(el_det_fac!^e) `bindTo` ( \ el_d_f ->
					(fst el_d_f) `bindTo` (\ det ->
					(snd el_d_f) `bindTo` (\ fac ->
					(l_mat'!^id) `bindTo` (\ l_m ->
					(
						(list_match_prod (get_val u1 v_s) (l_m (fst fac))) +
						(list_match_prod (get_val u2 v_s) (l_m (snd fac)))
					) * det )))))
				| (e,id) <- asb_table!^i
				]
			) / (-3.0)
		| i <- range p_bnds
	]
	where
	p_bnds = s_bounds asb_table
	bindTo x k = k x

-----------------------------------------------------------
-- Calculating the right-hand-side for step 3.           --
-- Called in TG iteration.                               --
-----------------------------------------------------------

get_rh3
	:: (My_Array Int (Frac_type,((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type))))
	-> (My_Array Int [(Int,Int)])
	-> (My_Array Int [Int])
	-> (My_Array Int Frac_type)
	-> (My_Array Int Frac_type, My_Array Int Frac_type)

get_rh3 el_det_fac asb_table p_steer del_p =
	(
		s_amap (\x->x/3)
		(s_listArray n_bnds (map (sum.(map fst)) pair_list)),
		s_amap (\x->x/3)
		(s_listArray n_bnds (map (sum.(map snd)) pair_list))
	)
	where
	pair_list =
		[
			[
				(l_mat!^id) `bindTo`	( \ l_m ->
				(get_val del_p (p_steer!^e)) `bindTo` ( \ p_val ->
				(el_det_fac!^e) `bindTo` ( \ el_d_f ->
				(fst el_d_f) `bindTo`	 ( \ det ->
				(snd el_d_f) `bindTo`	 ( \ fac ->
				(
					det * (list_match_prod p_val (l_m (fst fac))),
					det * (list_match_prod p_val (l_m (snd fac)))
				) )))))
				| (e,id)<-tab_elem
			]
			| tab_elem <- s_elems asb_table
		]
	n_bnds = s_bounds asb_table
	bindTo x k = k x
