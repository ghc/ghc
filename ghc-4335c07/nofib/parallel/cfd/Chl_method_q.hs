module Chl_method_q (chl_method_q, get_rh2) where

import S_Array
import Defs
import Quad_def
import L_matrix

bv_op ::
	(Frac_type -> Frac_type -> Frac_type) ->
	(Bin_Trie Frac_type) -> (Bin_Trie Frac_type) ->
	Bin_Trie Frac_type
bv_op _ a Null = a
bv_op f (Fork s a b) (Fork _ c d) =
	fork s (bv_op f a c) (bv_op f b d)
bv_op f a@Null (Fork s b c) = fork s (bv_op f a b) (bv_op f a c)
bv_op f Null (Leaf b) = leaf (f 0 b)
bv_op f (Leaf a) (Leaf b) = leaf (f a b)

addbv = bv_op (+)

subbv = bv_op (-)

sclv (Fork s a b) c = fork s (sclv a c) (sclv b c)
sclv (Leaf a) b = leaf (a*b)
sclv a _ = a

multmv (RectM a b c d) (Fork s e f) =
	fork s (addbv (multmv a e) (multmv b f))
	(addbv (multmv c e) (multmv d f))
multmv (SingM a) (Leaf b) = leaf (a*b)
multmv ZeroM _ = Null

multmtv (RectM a b c d) (Fork s e f) =
	fork s (addbv (multmtv a e) (multmtv c f))
	(addbv (multmtv b e) (multmtv d f))
multmtv (SingM a) (Leaf b) = leaf (a*b)
multmtv ZeroM _ = Null

-----------------------------------------------------------
-- Forward substitution for the Choleski method.  Called --
-- in "chl_method".                                      --
-----------------------------------------------------------

fwd_sbs (TriM a b c) (Fork s d e) =
	fork s f (fwd_sbs c (subbv e (multmv b f)))
	where f = fwd_sbs a d
fwd_sbs (SingTM v1) (Leaf v2) = leaf (v2 / v1)
fwd_sbs ZeroTM a = a

-----------------------------------------------------------
-- Backward substitution for the Choleski method.        --
-- Called in "chl_methold".                              --
-----------------------------------------------------------

bwd_sbs (TriM a b c) (Fork s d e) =
	fork s (bwd_sbs a (subbv d (multmtv b f))) f
	where
	f = bwd_sbs c e
bwd_sbs (SingTM v1) (Leaf v2) = leaf (v2 / v1)
bwd_sbs ZeroTM a = a

-----------------------------------------------------------
-- The driving function for the Choleski method.         --
-- Because the used generalized envelope mathod reorders --
-- the system matrix, the right-hand-side and result are --
-- also reordered to match the internal and external     --
-- forms.                                                --
-- Called in the TG iteration.                           --
-- Calls "fwd_sbs" and "bwd_sbs"                         --
-----------------------------------------------------------

chl_method_q chl_fac (Mk_t_Array bnds _ b) scalor =
	-- parameters: (Choleski_factor,ordering) right_hand_side
	-- constant_in_front_of_the_system
	Mk_t_Array bnds (Just 0) (sclv x scalor)
	where
	x = bwd_sbs chl_fac (fwd_sbs chl_fac b)

get_rh2 p_node_list u = assemble fn fe p_node_list
	where
	fn l p_fixed =
		if p_fixed then 0
		else (sum l) / (-3.0)
	fe ((v_s,(det,(fac1,fac2))),id) =
		((list_inner_prod g_u1 (l_m fac1)) +
			(list_inner_prod g_u2 (l_m fac2))
		) * det
		where
		l_m = (l_mat' ())!^id
		(g_u1,g_u2) = unzip (get_val u v_s)
