{-
	Functions for data input.  Some data have been put
	into arrays.

	XZ, 24/10/91
-}

{-
	Modified to adopt S_Arrays.

	XA, 19/2/92
-}

module Input_proc ( read_fs_cs, read_data ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
(=:) a b = (a,b)

-----------------------------------------------------------
-- reading a Int                                         --
-----------------------------------------------------------

rd_int = \ i -> (head (reads i)) :: (Int,String)

-----------------------------------------------------------
-- reading a Floating                                    --
-----------------------------------------------------------

rd_flt = \ f -> (head (reads f)) :: (Frac_type,String)

-----------------------------------------------------------
-- Reading a pair of values ( could be structed and      --
-- different values ); results are packed into a tuple.  --
-----------------------------------------------------------

rd_pair rf1 rf2 i = ((v1,v2),rest)
	where
	(v1,rest1) = (rf1 i)
	(v2,rest) = (rf2 rest1)

-----------------------------------------------------------
-- Reading a number of values ( could be structed ).     --
-- Results are packed into a list.                       --
-----------------------------------------------------------

read_n_val rd_f n in_str =
	(takeval n in_str, dropval n in_str )
	where
	takeval n i
		| n == 0	= []
		| otherwise = x : (takeval (n-1) rest)
		where (x,rest) = (rd_f i)
	dropval n i
		| null i = []
		| n == 0	= i
		| otherwise = dropval (n-1) rest
		where (x,rest) = (rd_f i)
 
-----------------------------------------------------------
-- reading a number of Int                               --
-----------------------------------------------------------

read_n_int = read_n_val rd_int

-----------------------------------------------------------
-- reading a number of Floating                          --
-----------------------------------------------------------

read_n_flt = read_n_val rd_flt

-----------------------------------------------------------
-- reading a pair of Int and Floating                    --
-----------------------------------------------------------

read_i_f_pair = rd_pair rd_int rd_flt

-----------------------------------------------------------
-- Skip over n lines                                     --
-----------------------------------------------------------

skip_lines 0 cs = cs
skip_lines n cs =
	skip_lines (n-1) (drop 1 (dropWhile ((/=) '\n') cs))
	
-----------------------------------------------------------
-- Reading the data file of our local form.              --
-- Called at the data setup stage.                       --
-----------------------------------------------------------

read_fs_cs f =
	(data_file,(mon,m_iter,m_toler,max_jcb_iter,jcb_toler,relax,dlt_t))
	where
	hed1 = skip_lines 1 f
	(i,hed2) = rd_int hed1
	start' = skip_lines (i+2) hed2
	mon = (head (head (drop 9 (words start'))))=='t'
	start = skip_lines (i+14) hed2
	([m_iter,max_jcb_iter],rest1) = read_n_int 2 start
	([m_toler,jcb_toler,relax],rest2) = read_n_flt 3 rest1
	rest3 = skip_lines 2 rest2
	(dlt_t,rest4) = rd_flt rest3
	rest5 = skip_lines 11 rest4
	data_file = head (drop 5 (words rest5))

read_data f =
	(e_total,n_total,p_total,v_steer,p_steer,coord,
	(init_p,init_u),(all_bry,(x_fixed,y_fixed)),p_fixed)
	where
	hed1 = skip_lines 1 f
	(i,hed2) = rd_int hed1
	start = skip_lines (i+4) hed2
	((e_total:n_total:bnd_total:_:_:p_total:_),rest1) =
		read_n_int 6 start
	rest2 = skip_lines 2 rest1
	(v_vals,rest3) =
		read_n_val (read_n_int (v_nodel+2) ) e_total rest2
	rest4 = skip_lines 2 rest3
	(c_vals,rest5) =
		read_n_val (rd_pair rd_int (read_n_flt 2)) n_total rest4
	rest6 = skip_lines 2 rest5
	line_size :: Int
	line_size =
		length (words (takeWhile ((/=) '\n') rest6)) - 1
	(init_vals,rest7) =
		read_n_val (rd_pair rd_int (read_n_flt line_size)) n_total rest6
	rest8 = skip_lines 2 rest7
	(bry_vals,rest9) =
		read_n_val (rd_pair rd_int (read_n_val
		(rd_pair rd_int rd_flt) line_size)) bnd_total rest8
	-- velocity steering vector
	v_steer = 
		s_array (1,e_total) (map (\(i:_:rest)->i=:rest) v_vals)
	-- pressure steering vector
	p_steer =
		s_array (1,e_total) (map (\(i:_:rest)->i =: rest)
		(map (take (p_nodel+2)) v_vals))
	-- node coordinates
	coord =
		s_array (1,n_total) (map (\(i,(x:y:_))->i =: (x,y)) c_vals)
	v_init =
		map (\(i,(x:y:_))->(i,(x,y))) init_vals
	-- velocity initial conditions
	init_u =
		(
			s_def_array (1,n_total) (0::Frac_type)
			[ i =: 
				if x_fixed!^i
				then
					(fst.snd.head) (dropWhile (\t->(fst t)/=i) bry_xys)
				else fst v
				| (i,v) <- v_init
			],
			s_def_array (1,n_total) (0::Frac_type)
			[ i =:
				if y_fixed!^i
				then
					(snd.snd.head) (dropWhile (\t->(fst t)/=i) bry_xys)
				else snd v
				| (i,v) <- v_init
			]
		)
	-- pressure initial conditions
	init_p =
		s_def_array (1,p_total) (0::Frac_type)
		[ i =:
			(
				if i `elem` p_fixed
				then (snd.head) (dropWhile (\t->(fst t)/=i) p_cond)
				else
					if ( line_size == 5 )
					then head y
					else x
			)
			| (i,(_:_:x:y)) <- init_vals, i <= p_total
		]
	bry_xys =
		[(i,(x,y)) | (i,((_,x):(_,y):_)) <- bry_vals]
	-- velocity nodes which are fixed in the x direction
	x_fixed =
		s_def_array (1,n_total) False
		[	i=:True | (i,((1,_):_)) <- bry_vals]
	-- velocity nodes which are fixed in the y direction
	y_fixed =
		s_def_array (1,n_total) False
		[ i=:True | (i,(_:(1,_):_)) <- bry_vals]
	-- all boundary nodes
	all_bry =
		s_def_array (1,n_total) False
		[ i=:True | (i,_) <- bry_vals ]
	-- fixed pressure nodes
	p_fixed = map fst p_cond
	p_cond =
		if ( line_size == 5 )
		then [(i,p) | (i,(_:_:_:(1,p):_)) <- bry_vals]
		else [(i,p) | (i,(_:_:(1,p):_)) <- bry_vals]
