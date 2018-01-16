module Main where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Elefac
import Asb_routs
import Chl_routs
import Chl_decomp
import Chl_method
import Jcb_method
import TG_iter
import Min_degree

import Input_proc

main = do
    fs_cs_str <- getContents
    let (datafile,fs_cs) = read_fs_cs fs_cs_str
    data_str <- readFile datafile
    putStr (run fs_cs (read_data data_str))

run (mon,m_iter,m_toler,max_jcb_iter,jcb_toler,relax,dlt_t)
	(e_total, n_total, p_total, v_steer, p_steer, coord,
	init_vec, bry_nodes, p_fixed ) =
	tg_iter
		mon m_iter m_toler max_jcb_iter jcb_toler relax dlt_t
		el_det_fac v_asb_table p_asb_table v_steer p_steer
		bry_nodes p_fixed (chl_fac,o_to_n) init_vec
	where
	chl_fac = chl_factor init_L
	(init_L, o_to_n) =
		orded_mat p_total el_det_fac p_steer p_fixed
	p_asb_table =
 		get_asb_table p_total e_total p_nodel p_steer
	v_asb_table =
 		get_asb_table n_total e_total v_nodel v_steer
	el_det_fac =
		get_el_det_fac e_total coord p_steer
