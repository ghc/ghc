{-
	The first part of Choleski decomposition.
	Contains a matrix reodering function.
	The generalized envelope method is implemented here.

	XZ, 24/10/91
-}

{-
	Modified to adopt S_arrays.

	More efficient algorithms have been adopted.
	They include:
	a) minimum degree ordering (in module Min_degree.hs);
	b) K matrix assembly.

	Also, the output format has been changed.

	XZ, 19/2/92
-}

module Chl_routs ( orded_mat ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Min_degree
import Data.Ix
infix 1 =:
(=:) a b = (a,b)
-----------------------------------------------------------
-- Liu's generalized envelope method adopted here.       --
-- Reordering the system matric by firstly applying      --
-- minimum degree ordering ( to minimize fill-ins ) and  --
-- secondly applying postordering ( to optimize matrix   --
-- structure ).  The system matrix structure is found    --
-- using the elimination tree.  Used at the data setup   --
-- stage.                                                --
-----------------------------------------------------------

orded_mat
	:: Int
	-> (My_Array Int (Frac_type,((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type))))
	-> (My_Array Int [Int])
	-> [Int]
	-> (My_Array Int (My_Array Int Frac_type,My_Array Int (Int,[Frac_type])),My_Array Int Int)

orded_mat p_total el_det_fac p_steer fixed =
	(init_L,o_to_n)
	where
	bindTo x k = k x -- old Haskell 1.0 "let", essentially
	remove x = filter ((/=) x)	-- also old Haskell 1.0 thing

	n_bnds = (1,p_total)
	n_bnds' = (0,p_total)
	-- the inverse of an 1-D Int array.
	inv_map = \a ->
		s_array n_bnds' (map (\(i,j)->j=:i) (s_assocs a))
	-- find the column indecies of nonzero entries in a row
	get_js old_i map_f =
		filter (\j->j<=i) (map ((!^) map_f) (old_rows!^old_i))
		where i = map_f!^old_i
	-- children of individual elimination tree nodes
	chldrn = \e_tree ->
		s_accumArray (++) [] n_bnds'
		(map (\(i,j)->j=:[i]) (s_assocs e_tree))
	-- the entry map from the input matrix to the output matrix
	-- ( combination of o_to_min and min_to_n )
	o_to_n :: (My_Array Int Int)
	o_to_n = s_amap ((!^) min_to_n) o_to_min
	n_to_o = inv_map o_to_n
	-- the entry map of the minimum degree ordering
	o_to_min = inv_map min_to_o
	min_to_o = s_listArray n_bnds' (0:min_degree old_rows)
	-- the entry map of postordering
	-- switch off ordering
	min_to_n :: My_Array Int Int
--	min_to_n = s_listArray n_bnds' (range n_bnds')
--	min_to_o = min_to_n
	min_to_n = 
		s_array n_bnds' ((0=:0):(fst (recur ([],1) (chn!^0))))
		where
		chn = chldrn min_e_tree
		-- recursive postordering
		recur =
			foldl
			(
				-- pattern before entering a loop
				\ res r ->
				-- current result of post-reordering
				(recur res (chn!^r)) `bindTo` ( \ (new_reord,label) ->
				((r=:label):new_reord,label+1) )
			)
	-- the elimination tree of the reordered matrix
	new_e_tree =
		s_array n_bnds
		( map (\(i,j)-> (min_to_n!^i =: min_to_n!^j))
		( s_assocs min_e_tree ))
	-- elimination tree of the matrix after minimum degree
	-- ordering
	min_e_tree =
		s_def_array n_bnds (0::Int)
		(all_rs (1::Int) init_arr [])
		where
		init_arr = s_def_array n_bnds (0::Int) []
		-- implementation of an elimination tree construction
		-- algorithm
		all_rs i ance pare =
			if ( i>p_total )
			then pare
			else all_rs (i+1) new_ance pare++rss
			where
			root old@(k,old_anc) =
				if ( (new_k==0) || (new_k==i) )
				then old
				else root (new_k,old_anc//^[k=:i])
				where new_k = old_anc!^k
			-- finding new parents and ancestors
			(rss,new_ance) =
				-- looping over connetions of current node in
				-- the matrix graph
				foldl
				(
					-- pattern before entering a loop
					\ (rs,anc) k1 ->
					-- appending a new parent
					(root (k1,anc)) `bindTo` ( \ (r,new_anc) ->
					(r=:i)		`bindTo` ( \ new_r ->
					if new_anc!^r /= 0
					then (rs, new_anc)
					else (new_r:rs, new_anc //^ [new_r]) ))
				)
				([],ance) (remove i (get_js (min_to_o!^i) o_to_min))
	-- initial L
	init_L =
		s_listArray (1,length block_ends)
		[ 
			(
				s_listArray bn [get_v i i|i<-range bn],
				(filter (\ (_,j)->j<=u)
					[ (i, find_first bn (find_non0 i))
						| i <- range (l+1,p_total)
					]) `bindTo` ( \ non_emp_set ->

				s_def_array (l+1,p_total) (u+1,[])
				[ i=:(j',[get_v i j | j<- range (j',min u (i-1))])
					| (i,j') <- non_emp_set
				] )
			)
			| bn@(l,u) <- block_bnds
		]
		where
		get_v i j =
			if ( i'<j' )
			then (old_mat!^j')!^i'
			else (old_mat!^i')!^j'
			where
			i' = n_to_o!^i
			j' = n_to_o!^j
		find_non0 i =
			foldl ( \ar j -> all_non0s j ar )
			(s_def_array (1,i) False [])
			(get_js (n_to_o!^i) o_to_n)
			where
			all_non0s j arr =
				if ( j>i || j==0 || arr!^j )
				then arr
				else all_non0s (new_e_tree!^j) (arr//^[j=:True])
		-- finding the first non-zero entry between l and u of the ith line
		find_first :: (Int,Int) -> (My_Array Int Bool) -> Int
		find_first (j1,u) non0_line = f' j1
			where
			f' j =
				if (j>u) || non0_line!^j
				then j
				else f' (j+1)
		-- reordered matrix in a new sparse form
		block_ends =
			[ i | (i,j)<-s_assocs new_e_tree, j/=(i+1) ]
		block_bnds = zip (1:(map ((+) 1) (init block_ends))) block_ends
		-- descendants of nodes of elimination tree
		decnd :: My_Array Int [Int]
		decnd =
			s_listArray n_bnds
			[ chn_n ++ concat [ decnd!^i | i <- chn_n ]
				| chn_n <- s_elems (chldrn new_e_tree)
			]
	-- rows of the K matrix (before ordering)
	old_rows =
		s_accumArray (++) [] n_bnds
		( concat
			[
				[j|(j,_)<-sparse_assocs (old_mat!^i)] `bindTo` ( \ j_set ->
				(i=:j_set):[j'=:[i]|j'<-j_set,i/=j'] )
				| i <- range n_bnds
			]
		)
	-- Value and index pairs of the original matrix.
	-- This is found by assembling system K.
	-- Fixed entries are multiplied by a large number
	old_mat :: My_Array Int (My_Array Int Frac_type)
	old_mat =
		arr //^
		[ (arr!^i) `bindTo` ( \ ar ->
		  i =: ar //^ [i=:(ar!^i)*large_scalor] )
			| i <- fixed
		]
		where
		arr =
			s_listArray n_bnds
			[
				s_accumArray (+) (0::Frac_type) (1,i) (temp!^i)
				| i<-range n_bnds
			]
		temp :: My_Array Int [(Int,Frac_type)]
		temp =
			s_accumArray (++) [] n_bnds
			( concat
				[
				  (el_det_fac!^e) `bindTo` ( \ d_f ->
				  (zip (range (1,p_nodel)) (p_steer!^e)) `bindTo` ( \ pairs ->
				  concat
					[
						(dd_mat!^ii) `bindTo` ( \ dd_m ->
						[ i =: [j =: (dd_m!^jj) d_f]
							| (jj,j) <- pairs, j<=i
						] )
						| (ii,i) <- pairs
					] ))
					| e <- s_indices el_det_fac
				]
			)
		-- element contribution matrix
		dd_mat =
			s_listArray (1,p_nodel) [
				s_listArray (1,p_nodel) [f11,f12,f13],
				s_listArray (1,p_nodel) [f12,f22,f23],
				s_listArray (1,p_nodel) [f13,f23,f33]
			]
			where
			f = \x y u v d -> (x*y+u*v)*d
			s1 = \(x,_,_) -> x
			s2 = \(_,y,_) -> y
			s3 = \(_,_,z) -> z
			f11 (det,(x,y)) = f c1 c1 c2 c2 det
				where
				c1 = s1 x
				c2 = s1 y
			f12 = \(det,(x,y)) -> f (s1 x) (s2 x) (s1 y) (s2 y) det
			f13 = \(det,(x,y)) -> f (s1 x) (s3 x) (s1 y) (s3 y) det
			f22 (det,(x,y)) = f c1 c1 c2 c2 det
				where
				c1 = s2 x
				c2 = s2 y
			f23 = \(det,(x,y)) -> f (s2 x) (s3 x) (s2 y) (s3 y) det
			f33 (det,(x,y)) = f c1 c1 c2 c2 det
				where
				c1 = s3 x
				c2 = s3 y
