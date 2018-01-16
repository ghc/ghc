module Gen_net (get_node_list) where

import Defs
import S_Array
import S_matrix
import Ix--1.3

get_node_list
	:: Int -> Int
	-> My_Array Int (Frac_type,Frac_type)
	-> [[Int]]
	-> ([Bool],([Bool],[Bool]))
	-> [Int]
	-> Node_Lists

get_node_list p_total v_total coord v_steer (all_bry,(x_fixed,y_fixed)) p_fixed =
	(
		arr_zip
		(s_listArray (1,p_total) (take p_total (s_elems n_list)))
		p_list,
		arr_zip n_list
		(arr_zip (s_amap f n_list)
			( s_listArray (s_bounds coord)
				(zip all_bry (zip x_fixed y_fixed))
			)
		)
	)
	where
	p_list =
		s_listArray (1,p_total) (map (`elem` p_fixed) (range (1,p_total)))
	n_list =
		s_accumArray (++) [] (s_bounds coord)
		(concat (map f1 v_steer))
		where
		f1 n_l@(n1:n2:n3:_) =
			zipWith (,) n_l (map (\z->[((n_l,e),z)]) (range (1,v_nodel)))
			where
			e =
				(
					((abs (e_det)) / 2),
					(  
						((y2-y3)/e_det,(y3-y1)/e_det,(y1-y2)/e_det),
						((x3-x2)/e_det,(x1-x3)/e_det,(x2-x1)/e_det)
					)  
				)
			v1 = coord!^n1
			v2 = coord!^n2
			v3 = coord!^n3
			x1 = fst v1
			x2 = fst v2
			x3 = fst v3
			y1 = snd v1
			y2 = snd v2
			y3 = snd v3
			e_det = (x2-x1)*(y3-y1)-(x1-x3)*(y1-y2)
	f e_list = (get_mat_diag f_s,get_mat_diag f_m)
		where
		get_mat_diag = \f22 -> sum (map f22 e_list)
		f_s = \e_id@((_,(det,fac)),id) ->
			det * (sum (map ((/ 6).abs) (((s_mat ())!^id) fac)))
		f_m ((_,(det,_)),id) = det * (get_m_r_sum id)

----------------------------------------------------------
-- Const matrix `Me' for System K                       --
----------------------------------------------------------

get_m_r_sum id = if id > p_nodel then (17/45) else (3/45)
