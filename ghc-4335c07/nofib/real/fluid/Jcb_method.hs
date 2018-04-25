{-
	Implementation of the Jacobi iteration

	XZ, 24/10/91
-}

{-
	Modified to adopt S_array

	XZ, 19/2/92
-}

module Jcb_method ( jcb_method ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Asb_routs
import Tol_cal
import Data.Ix

-----------------------------------------------------------
-- Jacobi iteration method:                              --
-- solves: scalor*MX = B                                 --
-- with iteration counter (r) as                         --
--  X(r+1) = X(r) + relax*(B/scalor - MX(r))/D           --
-- The system matrix M is never really assembled         --
-----------------------------------------------------------

jcb_method
	:: Int
	-> My_Array Int (Frac_type,((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type)))
	-> My_Array Int [(Int,Int)]
	-> My_Array Int [Int]
	-> (My_Array Int Bool,(My_Array Int Bool, My_Array Int Bool))
	-> (My_Array Int Frac_type, My_Array Int Frac_type)
	-> Frac_type
	-> Int
	-> Frac_type
	-> Frac_type
	-> (My_Array Int Frac_type, My_Array Int Frac_type)

sparse_elems = \y -> map (\(_,x)->x) (sparse_assocs y)

jcb_method f_step el_det_fac asb_table v_steer
	(all_bry,(x_fixed,y_fixed)) (b1,b2) scalor
	max_iter m_iter_toler relax =
	sub_jacobi (init_u,init_u) max_iter
	where
	n_bnds = s_bounds asb_table
	init_u = s_array n_bnds []
	-- the recursive function of the Jacobi iteration
	sub_jacobi old_x n = -- X(r) iteration_counter
		if
			-- checking the iteration limit
			( n <= 1 ) || 
			-- checking the tolerance
			(
				(n /= max_iter) &&
				( tol_cal
					((s_elems (fst new_x)) ++ (s_elems (snd new_x)))
					((sparse_elems (fst diff)) ++ (sparse_elems (snd diff)))
					True
				) < m_iter_toler
			)
{-
				(
				(sum ( map abs ( s_elems (fst diff) ) )) +
				(sum ( map abs ( s_elems (snd diff) ) ))
				)
				< m_iter_toler
-}
		then	new_x                    -- X(r+1)
		else	sub_jacobi new_x (n-1)
		where
		-- new adaptive x
		new_x =
			if ( n == max_iter )
			-- first iteration
			then diff
			-- otherwise
			else add_u old_x diff
		diff =
			if ( f_step == 1 )
			then
				-- For step 1.  Only fixes some boundry nodes.
				(
					find_diff x_fixed (fst old_x) b1,
					find_diff y_fixed (snd old_x) b2
				)
			else
				-- For step 3.  Fixes all boundry nodes.
				(
					find_diff all_bry (fst old_x) b1,
					find_diff all_bry (snd old_x) b2
				)
		bindTo x k = k x -- old haskell 1.0 "let"

		-- function which does one adaptation,
		-- ie, calculates:
		--		relax*(B - MX(r))/D
		find_diff fixed x b = -- list_of_unfixed_nodes X(r) B
			s_def_listArray n_bnds (0::Frac_type)
			[ 
				if fixed!^i
				then 0
				else
					((b!^i) / scalor) `bindTo` ( \ b' ->
					((
						if ( n == max_iter )
						then b'
						else
							b' -
							sum [
								(list_inner_prod 
								(get_val x (v_steer!^e))
								(map (mult (fst (el_det_fac!^e))) (m_mat!^id)))
								| (e,id)<-asb_table!^i
							]
					) * relax) / (mat_diag!^i) )
				| i <- range n_bnds
			]
	-- row sums of system M
	mat_diag =
		s_listArray n_bnds
		[
			sum
			[ (fst ( el_det_fac!^e)) * (m_r_sum!^id)
				| (e,id)<-asb_table!^i
			]
			| i <- range n_bnds
		]

-----------------------------------------------------------
-- row sums of element matrix m_mat                      --
-----------------------------------------------------------

m_r_sum =
	s_def_listArray (1,v_nodel) def_v
	[ def_v,def_v,def_v,v',v',v' ]
	where
	def_v = (12 / 180)::Frac_type
	v' = (68 / 180)::Frac_type

-----------------------------------------------------------
-- element matrix                                        --
-----------------------------------------------------------

m_mat =
	s_listArray (1,v_nodel)
	(map (map (mult inv_180))
	[
		[6,(-1),(-1),(-4),0,0],
		[(-1),6,(-1),0,(-4),0],
		[(-1),(-1),6,0,0,(-4)],
		[(-4),0,0,32,16,16],
		[0,(-4),0,16,32,16],
		[0,0,(-4),16,16,32]
	])
	where
	inv_180 = 1 / 180
