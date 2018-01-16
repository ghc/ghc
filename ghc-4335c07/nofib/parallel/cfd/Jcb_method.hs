module Jcb_method ( jcb_method, get_rh1, get_rh3 ) where

import S_Array
import Defs
import L_matrix
import C_matrix
import S_matrix

-----------------------------------------------------------
-- Jacobi iteration method:                              --
-- solves: scalor*MX = B                                 --
-- with iteration counter (r) as                         --
--  X(r+1) = X(r) + relax*(B-(scalor*M)X(r))/(scalor*D)  --
-- The system matrix M is never really assembled         --
-----------------------------------------------------------

jcb_method
	:: Int -> Frac_type -> Frac_type -> (Int,Int)
	-> My_Array Int V_Node
	-> Bool
	-> Int
	-> Frac_type
	-> My_Array Int (Frac_type,Frac_type)
	-> My_Array Int (Frac_type,Frac_type)

jcb_method
	max_iter m_iter_toler relax n_bnds node_list
	simpl f_step scalor b =
	head (drop max_iter (iterate iter_f init_u))
	where
	init_u = s_listArray n_bnds (repeat ((0,0)::(Frac_type,Frac_type)))
	fst_step = f_step == 1
	-- the recursive function of the Jacobi iteration
	iter_f old_x = add_u old_x diff
		where
		-- function which does one adaptation,
		-- ie, calculates:
		--		relax*(B - MX(r))/D
		diff = assemble fn fe (s_amap fc (arr_zip node_list b))
			where fc = \((asb,x),y) -> (asb,(x,y))
		fn l (((d1,d2),(bry,(x_fx,y_fx))),(b1,b2)) =
			(\(x,y)->
			if (not fst_step) && bry then (0,0)
			else (f' x_fx ((b1-(sum x))/d),f' y_fx ((b2-(sum y))/d)))
			(unzip l)
			where
			f' fx v = if fst_step && fx then 0 else v
			d = ((d2*scalor) + if simpl then d1 else 0) / relax
		fe e_id@((steer,(det,_)),id) =
			(list_inner_prod (map fst g_x) d_mat,
			 list_inner_prod (map snd g_x) d_mat)
			where
			g_x = get_val old_x steer
			d_mat = map ((*) det) mat_row
			mat_row =
				if simpl then zipWith (+) (s_mat_row e_id) r else r
			r = map ((*) scalor) ((m_mat ())!!(id-1))

m_mat :: () -> [[Frac_type]]
m_mat () =
	map (map (/180)) [m1 (),m2 (),m3 (),m4 (),m5 (),m6 ()]
m1 () = [6,(-1),(-1),(-4),0,0]
m2 () = [(-1),6,(-1),0,(-4),0]
m3 () = [(-1),(-1),6,0,0,(-4)]
m4 () = [(-4),0,0,32,16,16]
m5 () = [0,(-4),0,16,32,16]
m6 () = [0,0,(-4),16,16,32]

-----------------------------------------------------------
-- Calculating the right-hand-side for step 1.           --
-- Called in TG iteration.                               --
-----------------------------------------------------------

get_rh1
	:: My_Array Int V_Node
	-> (My_Array Int Frac_type)
	-> Bool
	-> (My_Array Int (Frac_type,Frac_type))
	-> (My_Array Int (Frac_type,Frac_type))
	-> My_Array Int (Frac_type,Frac_type)

get_rh1 node_list p same_u u u' =
	assemble fn fe node_list
	where
	fn l _ = (\(x,y)->(sum x,sum y)) (unzip l)
	fe e_id@((v_s,(det,fac@(fac1,fac2))),id) =
		(get_f fac1 (fst uu) (fst uu'), get_f fac2 (snd uu) (snd uu'))
		where
		p_val = get_val p (take p_nodel v_s)
		s_m = ((s_mat ())!^id) fac
		uu = unzip (get_val u v_s)
		uu' = if same_u then uu else unzip (get_val u' v_s)
		c_m = (((c_mat ())!^id) fac) uu'
		get_f fc uv uv' =
			( (list_inner_prod p_val (((l_mat ())!^id) fc)) / 3.0 +
				(list_inner_prod uv s_m) / (-3.0) +
				(list_inner_prod uv' c_m) / (-1260.0)
			) * det

-----------------------------------------------------------
-- Calculating the right-hand-side for step 3.           --
-- Called in TG iteration.                               --
-----------------------------------------------------------

get_rh3
	:: My_Array Int V_Node -> (My_Array Int Frac_type)
	-> My_Array Int (Frac_type,Frac_type)

get_rh3 node_list del_p =
	assemble fn fe node_list
	where
	fn l _ = (\(x,y)->((sum x)/3,(sum y)/3)) (unzip l)
	fe ((v_s,(det,(fac1,fac2))),id) = (get_f fac1,get_f fac2)
		where
		get_f fac =
			det *
			( list_inner_prod
				(get_val del_p (take p_nodel v_s))
				(((l_mat ())!^id) fac)
			)

s_mat_row ((_,(_,fac)),id) = map (\x->x/6) (((s_mat ())!^id) fac)
