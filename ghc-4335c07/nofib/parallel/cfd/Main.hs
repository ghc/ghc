module Main where

import TG_iter
import Data8
import Gen_net
import S_Array
import Defs
import Quad_def

main = putStr call_tg

call_tg =
	tg_iter
	mon simpl m_iter m_toler max_jcb_iter jcb_toler relax
	dlt_t node_lists (tri_fac ()) (init_vec ())
	where
	node_lists =
		get_node_list p_total n_total (coord ()) (v_steer ()) (bry_nodes ()) p_fixed
