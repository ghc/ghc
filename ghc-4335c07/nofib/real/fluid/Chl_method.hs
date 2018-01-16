{-
	The third part of the Choleski decomposition.
	Contains forward, backward substitution and their
	driver.

	XZ, 24/10/91
-}

{-
	Modified to employ S_array.
	(Forward and backward substitution functions have been
	recoded.)

	XZ, 7/2/92
-}

module Chl_method ( chl_method ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Asb_routs
import Data.Ix
infix 1 =:
(=:) a b = (a,b)

-----------------------------------------------------------
-- Forward substitution for the Choleski method.  Called --
-- in "chl_method".                                      --
-----------------------------------------------------------

lower_part u off_diag =
	dropWhile (\(i,_)->i<=u) (sparse_assocs off_diag)

-- forward substitution
fwd_sbs chl_fac b_old =
	s_listArray (s_bounds b_old) (gen_x (1::Int) b_old [])
	where
	b_up = snd (s_bounds chl_fac)
	-- generate solution
	gen_x blck b x =
		if blck > b_up
		then x
		else gen_x (blck+1) new_b (x++new_x)
		where
		diag = fst this_block
		off_diag = snd this_block
		this_block = chl_fac!^blck
		block_bounds = s_bounds diag
		(l,u) = block_bounds
		new_x = gen_block_x (l+1) [(b!^l)/(diag!^l)]
		-- update RHS
		new_b =
			s_accum (-) b
			[
				k =: list_inner_prod (drop (j-l) new_x) vs
				| (k,(j,vs)) <- lower_part u off_diag
			]
		-- generate solution for one block
		gen_block_x i x_res =
			if i>u
			then x_res
			else
				gen_block_x (i+1)
				( x_res ++
					[ ((b!^i)-list_inner_prod (drop (j-l) x_res) vs)/(diag!^i) ]
				)
			where (j,vs) = off_diag!^i

-----------------------------------------------------------
-- Backward substitution for the Choleski method.        --
-- It works in a column by column manner.                --
-- Called in "chl_methold".                              --
-----------------------------------------------------------

-- backward substitution
bwd_sbs chl_fac y =
	gen_x b_up ((s_array (s_bounds y) [])::(My_Array Int Frac_type))
	where
	b_up = snd (s_bounds chl_fac)
	-- generate solution
	gen_x blck x_res =
		if blck < (1::Int)
		then x_res
		else gen_x (blck-1) new_x
		where
		diag = fst this_block
		off_diag = snd this_block
		this_block = chl_fac!^blck
		block_bounds = s_bounds diag
		(l,u) = block_bounds
		new_x = gen_block_x u new_b x_res
		-- update RHS
		new_b =
			s_accum (-)
			(s_listArray block_bounds [y!^i|i<-range block_bounds])
			( concat
				[ 
					zipWith (\l v->l=:v) (range (j,u))
					(map ((*) (x_res!^k)) vs)
					| (k,(j,vs)) <- lower_part u off_diag
				]
			)
		-- generate solution for one block
		gen_block_x i b x_res1 =
			if i<l
			then x_res1
			else
				gen_block_x (i-1) new_b1 (x_res1//^[i=:new_x])
			where
			new_x = (b!^i) / (diag!^i)
			(j,vs) = off_diag!^i
			new_b1 =
				s_accum (-) b
				(zipWith (\l v->l=:v) (range (j,i-1)) (map ((*) new_x) vs))

-----------------------------------------------------------
-- The driving function for the Choleski method.         --
-- Because the used generalized envelope mathod reorders --
-- the system matrix, the right-hand-side and result are --
-- also reordered to match the internal and external     --
-- forms.                                                --
-- Called in the TG iteration.                           --
-- Calls "fwd_sbs" and "bwd_sbs"                         --
-----------------------------------------------------------

chl_method (chl_fac,o_to_n) b scalor =
	-- parameters: (Choleski_factor,ordering) right_hand_side
	-- constant_in_front_of_the_system
	s_amap ((*) scalor) (s_ixmap bnds ((!^) o_to_n) x)
	where
	x = bwd_sbs chl_fac (fwd_sbs chl_fac new_b)
	new_b = 
		s_array bnds
		(map (\(i,v)->(o_to_n!^i)=:v) (s_assocs b))
	bnds = s_bounds b
