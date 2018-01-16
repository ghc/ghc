{-
The second module of Choleski decomposition.
Contains Choleski factorization function.
   
XZ, 24/10/91
-}

{-
	Modified to employ S_array.

	The decomposition is now coded in the column-Choleski
	feshion.  It was in row-Choleski feshion.  It might be
	benifitial if numerical evaluation is forced (access
	locality).

	XZ, 7/2/92
-}
   
module Chl_decomp ( chl_factor ) where
   
import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Asb_routs
import Data.Ix
infix 1 =:
(=:) a b = (a,b)
   
-----------------------------------------------------------
-- Choleski factorization:                               --
-- it adopts the matrix struct derived from Liu's so     --
-- called generalized envelope method.  Called at the    --
-- data setup stage.                                     --
-----------------------------------------------------------

cal_one_elem (j_off,v_off) (_,(j_s,v_s)) old_v =
	case v_s of
		[] -> old_v
		_ ->
			old_v -
			list_inner_prod
			(drop (max_j-j_s) v_s)
			(drop (max_j-j_off) v_off)
	where max_j = max j_s j_off

cal_one_seg segs ((_,v_old),off@(j_off,v_off)) =
	zipWith (cal_one_elem off) segs v_old

match_pairs a@((k1,l1):as) b@(b1@(k2,l2):bs)
	| k1<k2 = match_pairs as b
	| k2<k1 = match_pairs a bs
	| otherwise = (k1=:(l1,l2)):match_pairs as bs
match_pairs _ _ = []

chl_factor :: S_array (S_array Float,S_array (Int,[Float]))
	-> S_array (S_array Float,S_array (Int,[Float]))

chl_factor init_L = foldl f init_L (range (s_bounds init_L))
  where
    f old_l j = old_l //^ [j=:step2 step1]
      where
	(l,u) = block_bnds
	block_bnds = s_bounds (fst this_block)
	this_block = (old_l!^j)
	bindTo x k = k x -- old Haskell 1.0 "let"
	step1 = foldl step1_f
		      this_block
		      -- previous_blocks...
		      [ k=: ((snd (old_l!^k)) `bindTo` ( \ b ->
			    filter (\ (i,_) -> (l<=i)&&(i<=u)) (s_assocs b) ))
			      | k <- range (1,j-1)
		      ]
	  where
	    step1_f (old_diag,old_off_diag) (k,segs_jk) = (new_diag,new_off_diag)
			where
			subs_segs ((i,pair@((j1,_),_)):rest) =
				(i=:(j1,cal_one_seg (drop (j1-l) segs_jk) pair)):
				subs_segs rest
			subs_segs _ = []
			new_diag =
				s_accum (-) old_diag 
				[ i =: list_inner_prod vs vs
					| (i,(_,vs@(_:_))) <- segs_jk
				]
			new_off_diag =
				old_off_diag //^
				(
					subs_segs
					(
						match_pairs
						(sparse_assocs old_off_diag)
						(sparse_assocs (snd (old_l!^k)))
					)
				)

	step2 (old_diag,old_off_diag) =
		(
			s_listArray block_bnds (fst ass_res),
			old_off_diag //^ (tail (snd ass_res))
		)
		where
		ass_res =
			gen_assocs (sparse_assocs old_off_diag)
			([sqrt (old_diag!^l)],[l=:(l+1,[])])
		gen_assocs (old_line@(i,(j,vs)):rest)
								(t_diag,t_off_diag) =
			if i <= u
			then
				gen_assocs rest
				(t_diag++[new_diag],t_off_diag++[i=:(j,new_off_diag)])
			else
				gen_assocs rest
				(t_diag,t_off_diag++[i=:(j,new_off_diag)])
			where
			new_diag =
				sqrt
				( (old_diag!^i) -
					list_inner_prod new_off_diag new_off_diag)
			new_off_diag =
				do_recur vs (drop (j-l) t_diag,drop (j-l) t_off_diag) []
			do_recur (o_v:o_v_r) ((d:d_r),(off:off_r)) res =
				do_recur o_v_r (d_r,off_r)
				( res ++ [ (cal_one_elem (j,res) off o_v)/d ] )
			do_recur _ _ res = res
		gen_assocs _ res = res
