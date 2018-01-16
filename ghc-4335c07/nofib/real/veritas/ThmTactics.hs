module ThmTactics where

import Lookup

import Tactics

import Tacticals

import Kernel

import Tree

import Core_datatype

import DerivedRules

import Goals

import Vtslib

import Type_defs

import Parse

import X_interface	

import Edlib

import Globals

import Tags		-- partain

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Unparse

import Attributes

{-
rewrite_tac  = Tactic "Rewrite" rewrite_input rewrite_subgoal
beta_tac     = Tactic "Beta" beta_input beta_subgoal
eta_tac      = Tactic "Eta" eta_input eta_subgoal
recurse_tac  = Tactic "Recurse" recurse_input recurse_subgoal
-}


{-
create_name s i dc@(Symbol_dec t attL) 
	= case get_att Name_Style attL of
	      SOME _ -> (dc,i)
	      NONE   -> (Symbol_dec t attL' , i+1)
			where
	                nm = Name (s ++ nprimes i)
			att = set_att Name_Style (Symbol_Name nm)
			attL' = att : attL


create_name s i dc@(Axiom_dec t attL) 
	= case get_att Name_Style attL of
	      SOME _ -> (dc,i)
	      NONE   -> (Axiom_dec t attL' , i+1)
			where
	                nm = Name (s ++ nprimes i)
			att = set_att Name_Style (Symbol_Name nm)
			attL' = att : attL
-}

create_name s i (Decpair dc1 dc2 attL) 
	= (Decpair dc1' dc2' attL, i3)
	  where
	  (dc1',i2) = create_name s i dc1
	  (dc2',i3) = create_name s i2 dc2

create_name s i dc = (dc,i)



--nprimes i = take i ( repeat '\39' )

{-
(******************************************************************************)
(*    complete_thm                                                            *)
(*                                                                            *)
(*         ô                                                                  *)
(*     --------- Complete (ø : ô)                                             *)
(*                                                                            *)
(******************************************************************************)
-}

complete_tac = Tactic "Complete" complete_thm_input complete_thm



complete_thm gst sg lt (SOME [dv]) (ThmSpec itm) 
	= if not (is_valid_Thm th ) 
	      then Bad mesg
	      else if eq_trm th_itm itm 
		     then Ok ([], complete_thm_valid th) 
		     else Bad ("Derivation does not prove given goal\n" ++
				      "proves: " ++ unparse_trm sg ps th_itm )
	  where
	  ps = fetch_ps gst
	  th = parse_Thm sg ps dv
	  ( TH_Err mesg ) = th 
	  ( th_itm , _ )  = internal_Thm th

complete_thm _ _ _ _ _ = Bad "Goal is not a theorem"




complete_thm_valid th [] 
	= SOME (ThmDone th)

complete_thm_valid _ _ = NONE



complete_thm_input lt gst (ThmSpec tm) 
	= x_form True form /./
	  exp
	  where
	  exp NONE = reTurn NONE 
	  exp ( SOME [OutText s]) = reTurn ( SOME [s] )
	  form = [InComment "Complete Theorem", InMultiText "Derivation" ""]

{-
(******************************************************************************)
(*     gen_subgoal                                                            *)
(*                                                                            *)
(*                 ±ë.ô                                                       *)
(*             ------------ Gen                                               *)
(*               ë,  Ûë] ô                                                    *)
(*                                                                            *)
(******************************************************************************)
-}
	
gen_tac = OrdTactic "Gen" null_arg_fn gen_subgoal

gen_subgoal gst sg lt args (ThmSpec (Binder Forall dc tm _ _)) 
	= Ok (subgoals, [lt,lt], rwL, gen_valid sg )
	  where
	  (dc',_) = create_name "xxx" 0 dc
	  subgoals = [DecSpec dc', ThmSpec tm]
	  rwL = [True, False]
	--  ltL = [lt, extend_lookup_table true dc' lt]

gen_subgoal _ _ _ _ _
	= Bad "Cannot apply tactic to given goal"




gen_valid sg dnL rwL 
	= case (dnL, rwL) of
              ([SOME (DecDone dc), SOME (ThmDone th)],_) 
		  -> (rwL,[sg,extend dc sg],SOME (ThmDone (generalise th)))
              ([SOME (DecDone dc), NONE], [true,_]) 
		  -> ([True,True],[sg,extend dc sg],NONE)
              _   -> (rwL,[sg,sg],NONE)

{-
(******************************************************************************)
(*   disch_subgoal                                                            *)
(*                                                                            *)
(*         [ë] ¶ ô                                                            *)
(*     -------------- Dischage                                                *)
(*        ë,  Ûkİ t                                                           *)
(******************************************************************************)
-}

disch_tac = OrdTactic "Disch" null_arg_fn disch_subgoal

disch_subgoal gst sg lt args (ThmSpec (Binder Imp dc tm _ _)) 
	= Ok (subgoals, [lt,lt], rwL, disch_valid sg) 
	  where
	  (dc',_) = create_name "hhh" 0 dc
	  subgoals = [DecSpec dc', ThmSpec tm]
	  rwL = [True, False]

disch_subgoal _ _ _ _ _
	= Bad "Cannot apply 'Disch' to specified goal"

disch_valid sg dnL rwL 
	= case (dnL, rwL) of
              ([SOME (DecDone dc), SOME (ThmDone th)],_) 
		 -> (rwL,[sg,extend dc sg],SOME (ThmDone (discharge th)))
              ([SOME (DecDone dc), NONE], [true,_]) 
		 -> ([True,True],[sg,extend dc sg],NONE)
              _  -> (rwL,[sg,sg],NONE)

{-
(******************************************************************************)
(*   conj_subgoal                                                             *)
(*                                                                            *)
(*         æ ³ ù                                                              *)
(*         ----- Conjunction                                                  *)
(*         æ,  ù                                                              *)
(******************************************************************************)
-}

conj_tac = Tactic "Conjunction" null_arg_fn conj_subgoal

conj_subgoal gst sg lt args (ThmSpec (Binary' And tm1 tm2 _ _)) 
	= Ok ([ThmSpec tm1, ThmSpec tm2], conj_valid)

conj_subgoal _ _ _ _ _ 
	= Bad "cannot apply tactic to specified goal"


conj_valid [SOME (ThmDone th1), SOME (ThmDone th2)] 
	= SOME (ThmDone (conj th1 th2))

conj_valid _ = NONE

{-
(******************************************************************************)
(*   disj_subgoal                                                             *)
(*                                                                            *)
(*         c                                                                  *)
(*     --------- Disjunction (ø : a´b)             give either ø or a´b       *)
(*     a¶c,  b¶c                                                              *)
(******************************************************************************)
-}

disj_tac = Tactic "Disjunction" disj_input disj_subgoal

disj_subgoal gst sg lt (SOME args) (ThmSpec tm) 
	= case args of
	      ["Derivation", dv] 
		 -> if is_valid_Thm th 
			then case internal_Thm th of
		    		(Binary' Or tm1 tm2 _ _ , _) 
				    -> Ok ([ThmSpec tm4, ThmSpec tm5], 
							   disj_dv_valid th) 
				       where
		                       dc1 = Axiom_dec tm1 []
		                       dc2 = Axiom_dec tm2 []
		                       tm3 = shift_trm [] 1 tm
		                       tm4 = Binder Imp dc1 tm3 [] []
		                       tm5 = Binder Imp dc2 tm3 [] []
				_   -> Bad "Goal is not a disjunction"
			else Bad mesg
		    where
		    ps = fetch_ps gst
		    th = parse_Thm sg ps dv 
		    ( TH_Err mesg ) = th

	      ["Specification",tm_rep] 
		 -> parse_trm sg ps tm_rep |||
		    exp
		    where
		    exp	tm6@(Binary' Or tm1 tm2 _ _)
			  = Ok ([ThmSpec tm4, ThmSpec tm5, 
						    ThmSpec tm6], disj_tm_valid)
		    	    where
		    	    dc1 = Axiom_dec tm1 []
		    	    dc2 = Axiom_dec tm2 []
		    	    tm3 = shift_trm [] 1 tm
		    	    tm4 = Binder Imp dc1 tm3 [] []
		    	    tm5 = Binder Imp dc2 tm3 [] []
		    exp _ = Bad "Specification  is not a disjunction"
		    ps = fetch_ps gst
	      _  -> Bad "No selection"

disj_subgoal _ _ _ _ _ = Bad "Cannot apply 'disjunction'"


disj_dv_valid th [SOME (ThmDone th1), SOME (ThmDone th2)] 
	= SOME (ThmDone (disj th th1 th2))

disj_dv_valid _ _ = NONE




disj_tm_valid [SOME (ThmDone th1), SOME (ThmDone th2), SOME (ThmDone th3)] 
	= SOME (ThmDone (disj th3 th1 th2))

disj_tm_valid _ = NONE




disj_input lt gst _ 
	= x_form True form /./
	  exp
	  where
	  exp NONE = reTurn NONE 

	  exp ( SOME [OutRadio s1,OutText s2] ) 
		= reTurn ( SOME [s1,s2] )

	  form = [InComment "Disjunction",
		  InRadio "Argument Type" 0 ["Derivation","Specification"],
		  InMultiText "" ""]

{-
(******************************************************************************)
(*   reflex_subgoal                                                           *)
(*                                                                            *)
(*         ô = ô                                                              *)
(*         ----- Reflex                                                       *)
(*                                                                            *)
(******************************************************************************)
-}

reflex_tac   = Tactic "Reflex" null_arg_fn reflex_subgoal

reflex_subgoal gst sg lt args (ThmSpec (Binary' Eq' tm1 tm2 _ _)) 
	| eq_trm tm1 tm2 = Ok ([], reflex_valid (reflex (trm_to_Trm sg tm1)))
	| otherwise      = Bad "Terms not equal"

reflex_subgoal _ _ _ _ _
	= Bad "cannot apply 'reflex' to specified goal"



reflex_valid th [] = SOME (ThmDone th)

reflex_valid _ _ = NONE

{-
{-
(******************************************************************************)
(*     rewrite_subgoal                                                        *)
(*                                                                            *)
(*         P[a]i                                                              *)
(*         ----- Rewrite (ø : a = b)       supply either ø or a=b             *)
(*         P[b]i                                                              *)
(*                                                                            *)
(******************************************************************************)
-}

rewrite_subgoal gst sg lt (SOME [index,spec,spec_type]) (ThmSpec tm) 
	= case spec_type of
	      "Derivation" 
		  -> case internal_Thm th of
			 (Binary' Eq' tm2 tm3 _ _ ,_) 
			      | eq_trm tm1 tm2 
				  -> Ok ([ThmSpec tm4], rewrite_th_valid iL th)
				     where
				     tm4 = replace_trm tm tm3 iL
			      | otherwise 
				  -> Bad "LHS does not match subgoal"
			 _    -> Bad "Theorem not an equality"
		     where
		     iL = parse_index index
		     (tm1,dcL) = select_trm tm iL
		     sg1 = foldl ext_Sg sg dcL
		     ps = fetch_ps gst
		     th = parse_Thm sg ps spec 

	      "Specification" 
		  -> case parse_trm sg spec of -- add ps to parse
			 th_tm@(Binary' Eq' tm2 tm3 _ _) 
			     | eq_trm tm1 tm2
				  -> Ok ([ThmSpec tm4, ThmSpec th_tm], 
					      rewrite_tm_valid iL)
				     where
				     tm4 = replace_trm tm tm3 iL
			     | otherwise 
				  -> Bad "LHS does not match subgoal"
			 _ -> Bad "Term not an equality"
		     where
		     iL = parse_index index
		     (tm1,dcL) = select_trm tm iL
		     sg1 = foldl ext_sg (internal_Sgn sg) dcL
		     ps = fetch_ps gst

rewrite_th_valid iL th [SOME (ThmDone th1)] 
	= SOME (ThmDone (subterm_rw th1 (symmetry th) iL))

rewrite_tm_valid iL [SOME (ThmDone th1), SOME (ThmDone th2)] 
	= SOME (ThmDone (subterm_rw th1 (symmetry th2) iL))
		

rewrite_input gst lt (ThmSpec tm) 
	= error "rewrite_input not implemented"
{-
	= x_form True form /./
	  exp
	  where
	  exp NONE -> reTurn NONE 
	
	  exp ( SOME [OutSubterm s1,OutText s2,OutRadio s3] )
		= reTurn ( SOME [s1,s2,s3] )
	
	  exp _ = return_err "Unexected arguments returned" 

	  attL = get_attributes gst
	  (s,data) = unparse_trm_data ust1 tm
	  form = [InComment "Rewrite",
		  InSubterm s data,
		  InMultiText "Rewrite Theorem" "",
		  InRadio "Type of Input  " 0 ["Derivation","Specification"]]
-}


parse_index inp 
	= case next_tk inp of
	      (SOME tk, inp1) -> read tk : parse_index inp1
	      (NONE, _) -> []

ext_Sg sg dc = extend (dec_to_Dec sg dc) sg

ext_sg sg dc = Extend dc sg []

-}

induction_tac= Tactic "Induction" null_arg_fn ind_subgoal

ind_subgoal gst sg lt args (ThmSpec (Binder Forall dc tm _ attL)) 
	= Ok ( gL , ind_valid ind_thm pL )
	  where
	  tm1 = Binder Lambda dc tm [] attL
	  ( Symbol_dec st attL ) = dc
	  (i,j,spec,dcL,parmL) = get_datatype_info (internal_Sgn sg) st
	  specL = map (trm_to_Trm sg) spec
	  ind_thm = foldl specialise (induction (constructor sg i j 0)) specL
	  (Binder _ dec tm2 _ _ , _) = internal_Thm ind_thm
	  (spec_thm,pL) = find_betas (subst_trm dec tm2 tm1)
	  gL = map ThmSpec (reduce_ind spec_thm) ++ [TrmSpec tm1]

ind_subgoal _ _ _ _ _
	= Bad "Goal is not universally quantified"



reduce_ind (Binder Imp dc tm _ _) 
	= typ_of_dec dc : reduce_ind (shift_trm [] (-1) tm)

reduce_ind _ = []




ind_valid th pL dnL 
	= SOME (ThmDone (foldl mp th1 dnL1)) 
	  where
	  i = length dnL - 1
	  dnL1 = take i dnL
	  SOME (TrmDone tm) = head (drop i dnL)
	  th1 = rep_beta (specialise th tm) pL
	  mp th1 (SOME (ThmDone th2)) = modus_ponens th1 th2










axiom_tac = Tactic "Axiom" axiom_arg_fn axiom_subgoal
	    
axiom_arg_fn gst lt (ThmSpec tm) 
	= x_form True form /./
	  exp
	  where
	  exp NONE = reTurn NONE 

	  exp ( SOME [OutText s] ) = reTurn ( SOME [s] )

	  form = [InComment "Tactic: Axiom", InSingleText "Name Of Axiom" ""]



axiom_subgoal gst sg lt (SOME [name]) obj 
	= case lookup_name sg name of
	      SOME (Sym i j _ _) 
		   -> if ( is_valid_Thm app_axiom ) 
			  then Ok ([], axiom_valid app_axiom )
			  else Bad ("Axiom: " ++ show i ++ " " ++ show j)
	              where		
	              app_axiom = axiom sg i j
	      _    -> Bad ("No Such Axiom Name: " ++ name)

axiom_subgoal _ _ _ _ _
	= Bad "Hmm: it's failed, no name supplied perhaps?"



axiom_valid th [] = SOME (ThmDone th)

axiom_valid _ _ = NONE









hyp_tac = Tactic "Hyp" null_arg_fn find_hyp

find_hyp gst sg lt NONE (ThmSpec tm) 
	= case filter (find_match tm) hyps of
	      []       -> Bad "Can't find match"
	      (th : _) -> Ok ([], axiom_valid th)
	  where
	  hyps = get_hyp_from_sgn sg (internal_Sgn sg) 0

find_hyp _ _ _ _ _
	= Bad "goal is not a theorem specification"



get_hyp_from_sgn sg (Empty _) i = []

get_hyp_from_sgn sg (Extend dc isg _) i 
	= l ++ get_hyp_from_sgn sg isg (i + 1) 
	  where
	  (_, l) = get_hyp_from_dec sg dc i 0 

get_hyp_from_sgn sg _ i = []



get_hyp_from_dec sg (Axiom_dec _ _ ) i j = (j, [axiom sg i j])

get_hyp_from_dec sg (Def _ _ _) i j = (j, [axiom sg i j])

get_hyp_from_dec sg (Decpair dc1 dc2 _) i j 
	= (j'', l ++ l')
	  where
	  (j', l)  = get_hyp_from_dec sg dc1 i (j+1)
	  (j'',l') = get_hyp_from_dec sg dc2 i (j' + 1)

get_hyp_from_dec _ _ _ j = (j, [])



find_match tm th 
	= eq_trm tm tm' 
	  where
	  (tm',_) = internal_Thm th





{-
(******************************************************************************)
(*   eq_subgoal                                                               *)
(*                                                                            *)
(*       a=b                                                                  *)
(*    ---------                                                               *)
(*    a¶b   b¶a                                                               *)
(******************************************************************************)
-}

eq_tac = Tactic "Eq" null_arg_fn eq_subgoal

eq_subgoal gst sg lt args (ThmSpec (Binary' Eq' itm1 itm2 _ _)) 
	= if eq_trm ty1 ty2 && eq_trm ty1 (Constant Bool' [] [])
		 then Ok ([ThmSpec tm3, ThmSpec tm4],eq_valid gst lt sg tm1 tm2)
		 else Bad "Equality not on booleans"
	  where
	  dc1 = Axiom_dec itm1 []
	  dc2 = Axiom_dec itm2 []
	  tm3 = Binder Imp dc1 (shift_trm [] 1 itm2) [] []
	  tm4 = Binder Imp dc2 (shift_trm [] 1 itm1) [] []
	  tm1 = trm_to_Trm sg itm1
	  tm2 = trm_to_Trm sg itm2
	  (ty1,_,_) = internal_Trm (typ_of_Trm tm1)
	  (ty2,_,_) = internal_Trm (typ_of_Trm tm2)

eq_subgoal _ _ _ _ _ = Bad "Cannot apply tactic to given goal"



eq_valid gst lt sg tm1 tm2 [SOME (ThmDone th1), SOME (ThmDone th2)] 
	= SOME (ThmDone th6) 
	  where
	  inp = "\186(\177a:bool.\177b:bool.(a\182b)\182(b\182a)\182(a=b))"
	  ps = fetch_ps gst
	  eq_th = parse_Thm sg ps inp 
	  th3 = specialise eq_th tm1
	  th4 = specialise th3 tm2
	  th5 = modus_ponens th4 th1
	  th6 = modus_ponens th5 th2

eq_valid _ _ _ _ _ _ = NONE

{-
(******************************************************************************)
(*   or_subgoal                                                               *)
(*                                                                            *)
(*      a ³ b                                                                 *)
(*     -------                                                                *)
(*     µ a ¶ b                                                                *)
(******************************************************************************)
-}

or_tac = Tactic "Or" null_arg_fn or_subgoal

or_subgoal gst sg lt NONE (ThmSpec (Binary' Or tm1 tm2 _ _)) 
	= Ok ([ThmSpec tm3], or_valid gst lt sg tM1 tM2)
	  where
	  dc  = Axiom_dec (Unary Not tm1 [] []) []
	  tm3 = Binder Imp dc (shift_trm [] 1 tm2) [] []
	  tM1 = trm_to_Trm sg tm1
	  tM2 = trm_to_Trm sg tm2

or_subgoal _ _ _ _ _ = Bad "Goal is not a disjunction"



or_valid gst lt sg tm1 tm2 [SOME (ThmDone th)] 
	= SOME (ThmDone th2) 
	  where
	  inp = "\186(\177a:bool.\177b:bool.(\181a\182b)=(a\180b))"
	  ps  = fetch_ps gst
	  or_th = parse_Thm sg ps inp 
	  th1 = specialise (specialise or_th tm1) tm2
	  th2 = subterm_rw th th1 []

or_valid _ _ _ _ _ _ = NONE

{-
(******************************************************************************)
(*   not_subgoal                                                              *)
(*                                                                            *)
(*      µ µ a                                                                 *)
(*     -------                                                                *)
(*        a                                                                   *)
(******************************************************************************)
-}

not_tac = Tactic "Not" null_arg_fn not_subgoal

not_subgoal gst sg lt NONE (ThmSpec (Unary Not (Unary Not tm1 _ _) _ _)) 
	= Ok ([ThmSpec tm1], not_valid gst lt sg tM1)
	  where
	  tM1 = trm_to_Trm sg tm1

not_subgoal _ _ _ _ _ = Bad "cannot apply 'Not' to given goal"



not_valid gst lt sg tm1 [SOME (ThmDone th)] 
	= SOME (ThmDone th2) 
	  where
	  inp = "\186(\177a:bool.a=\181\181a)"
	  ps  = fetch_ps gst
	  not_th = parse_Thm sg ps inp 
	  th1 = specialise not_th tm1
	  th2 = subterm_rw th th1 []

not_valid _ _ _ _ _ = NONE


{-
(******************************************************************************)
(*     lemma-subgoal                                                          *)
(*                                                                            *)
(*          a                                                                 *)
(*     ------------ b                                                         *)
(*      b ¶ a,   b                                                            *)
(******************************************************************************)
-}

lemma_tac = Tactic "Lemma" lemma_input lemma_subgoal

lemma_subgoal gst sg lt (SOME [nm,spec]) (ThmSpec tm) 
	= parse_trm sg ps spec |||
	  exp 
	  where
	  ps = fetch_ps gst
	  exp lemma = Ok ( [g1,g2], lemma_valid ) 
		      where	
	  	      g1 = ThmSpec (Binder Imp dc tm' [] []) 
	       		   where
	       		   rnm = Name nm
	       	           dc  = Axiom_dec lemma [sym_nm rnm]
	       		   tm' = shift_trm [] 1 tm
	              g2 = ThmSpec lemma

lemma_subgoal _ _ _ _ _ = Bad "Goal is not a theorem specification"



lemma_valid [SOME (ThmDone th1), SOME (ThmDone th2)] 
	= SOME (ThmDone (modus_ponens th1 th2))

lemma_valid _ = NONE

	

lemma_input gst lt (ThmSpec tm) 
	= x_form True form /./
	  exp
	  where
	  exp NONE = reTurn NONE 

	  exp ( SOME [OutText s1,OutText s2] ) 
		= reTurn ( SOME [s1,s2] )

	  form = [InComment "Add lemma", 
		  InSingleText "Name " "",
		  InMultiText "Lemma" ""]

{-

rw_input str gst lt (ThmSpec tm) 
	= error "re_input not implemented"
{-
	= x_form True form /./
	  exp
	  where`
	  exp NONE = reTurn NONE 

	  exp ( SOME [OutSubterm s] ) = reTurn ( SOME [s] )

	  exp  _ = return_err "Unexected arguments returned" 

	  attL = get_attributes gst
	  ust = get_default_us gst
		val ust1 = U.set_defaults (U.set_lookup_table ust lt) attL
		val (s,data) = unparse_trm_data ust1 tm
	  form = [InComment str, InSubterm s data]
-}
     

beta_subgoal gst sg lt (SOME [index]) (ThmSpec tm) 
	= Ok ( [ ThmSpec tm3 ] , beta_valid th iL ) 
	  where
	  iL = parse_index index
	  (App (Binder Lambda dc tm1 _ _) tm2 _ _ ,dcL) 
		= select_trm tm iL
	  tm3 = replace_trm tm (subst_trm dc tm1 tm2) iL
	  th  = reflex (trm_to_Trm sg tm)



beta_valid th iL [SOME (ThmDone th')] 
	= SOME (ThmDone (subterm_rw th' (beta_rw th (0:iL)) []))

beta_valid _ _ _ = NONE



beta_input = rw_input "â Rewrite"

{-
    fun eta_subgoal gst sg lt (SOME [index]) (ThmSpec tm) =
	    let val iL = parse_index (strings_to_input [index])
		val (Binder(Lambda,dc,App (tm1,Sym(0,0,_,_),_,_),_,_),dcL) =
			select_trm tm iL
	    in if occurs 0 tm1
		    then fail "Not è-reducducable"
		    else
			let val tm3 = replace_trm tm (shift_trm [] ~1 tm1) iL
			    val th = reflex (trm_to_Trm sg tm)
			in ( [ ThmSpec tm3 ] , eta_valid th iL ) end
	    end

    and eta_valid th iL [SOME (ThmDone th')] =
	    SOME (ThmDone (subterm_rw th' (eta_rw th (0::iL)) []))

    val eta_input = rw_input "è Rewrite"
-}

{-
    fun recurse_subgoal gst sg lt (SOME [index]) (ThmSpec tm) =
	    let val iL = parse_index (strings_to_input [index])
		val (App (Recurse _,_,_,_),dcL) = select_trm tm iL
		val th = recurse_rw (reflex (trm_to_Trm sg tm)) (0::iL)
		val (Binary' (Eq',tm3,_,_,_), _) = internal_Thm th
	    in ( [ ThmSpec tm3 ] , recurse_valid th ) end

    and recurse_valid th [SOME (ThmDone th')] =
	    SOME (ThmDone (subterm_rw th' th []))

    val recurse_input = rw_input "Recurse Rewrite"
-}
-}

taut_tac = Tactic "Taut" null_arg_fn taut_subgoal

taut_subgoal gst sg lt _ (ThmSpec tm) 
	= if is_valid_Thm th
		then Ok ( [] , taut_valid th ) 
		else Bad mesg
	  where
	  th = taut (trm_to_Trm sg tm)
	  ( TH_Err mesg ) = th

taut_subgoal _ _ _ _ _ = Bad "goal is not a theorem specification"



taut_valid th [] = SOME (ThmDone th)

taut_valid _ _ = NONE



{-
(******************************************************************************)
(*   exists_subgoal                                                           *)
(*                                                                            *)
(*        p                                                                   *)
(*     ------ ø : ²x.q                                                        *)
(*     ±x.q¶p                                                                 *)
(******************************************************************************)
-}

exists_elim_tac = Tactic "ExistsElim" exists_elim_input exists_elim_subgoal

exists_elim_subgoal gst sg lt (SOME [s1,"Derivation"]) (ThmSpec tm) 
	= parse_Thm_M sg ps s1 |||
	  exp
	  where
	  ps = fetch_ps gst
	  exp th = Ok ( [ ThmSpec tm''' ] , exists_elim_valid th )
	  	   where
	  	   (Binder Exists dc tm' _ _ ,_) = internal_Thm th
	  	   tm'' = Binder Imp (Axiom_dec tm' [])(shift_trm [] 2 tm) [] []
	  	   tm''' = Binder Forall dc tm'' [] []

exists_elim_subgoal gst sg lt (SOME [s1,"Specification"]) (ThmSpec tm) 
	= parse_trm sg ps s1 |||
	  exp
	  where
	  ps = fetch_ps gst
	  exp tm1@(Binder Exists dc tm' _ _) 
	 	= Ok ( [ ThmSpec tm''', ThmSpec tm1 ] , exists_elim_valid' ) 
	  	  where
	          tm'' = Binder Imp (Axiom_dec tm' []) (shift_trm [] 2 tm) [] []
	  	  tm''' = Binder Forall dc tm'' [] []
	  exp _ = Bad "term is not an existential"

exists_elim_subgoal _ _ _ _ _
	= Bad "Invalid application of 'ExistsElim'"




exists_elim_valid th' [SOME (ThmDone th)] 
	= SOME (ThmDone (exists_elim th' th))

exists_elim_valid _ _ = NONE



exists_elim_valid' [SOME (ThmDone th1), SOME (ThmDone th2)] 
	= SOME (ThmDone (exists_elim th1 th2))

exists_elim_valid' _ = NONE





exists_elim_input gst lt (ThmSpec tm) 
	= x_form True form /./
	  exp
	  where
	  exp ( SOME [OutText s1,OutRadio s2] ) 
		= reTurn ( SOME [s1,s2] )

	  exp _ = reTurn NONE 

	  form = [InComment "Exists Elimination",
		  InMultiText "Existential Object" "",
		  InRadio "Type of Object" 0 ["Derivation","Specification"]]
	    
    
strip split_tac auto_tac 
	= repeat_tac (snd (lift_tactic reflex_tac)    `orelse`
                        snd (lift_tactic hyp_tac)       `orelse`
--                        snd (lift_tactic taut_tac)      `orelse`
                        snd (lift_ordtactic gen_tac)    `orelse`
                        snd (lift_ordtactic disch_tac)  `orelse`
                        snd (lift_tactic conj_tac)      `orelse`	
                        snd (lift_tactic or_tac)        `orelse`
                        snd (lift_tactic not_tac)       `orelse`	
                        snd (lift_tactic eq_tac)        `orelse`	
                        snd (lift_tactic auto_tac) ) --      `orelse`	
--                        snd (lift_ordtactic split_tac)  )

triv auto_tac 
	=  snd (lift_tactic reflex_tac)    `orelse`
	    snd (lift_tactic hyp_tac)       `orelse`
	    snd (lift_tactic auto_tac)      `orelse`
	    snd (lift_tactic taut_tac)      `orelse`
	    ( \ trst -> reTurn trst )

