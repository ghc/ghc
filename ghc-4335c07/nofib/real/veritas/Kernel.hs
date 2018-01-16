{-
 * kernel for vts90
 *
 * Tue Nov  6 14:35:39 GMT 1990
 *
 * For information about the typing rules for the construction of
 * term, signatures, declarations an signatures see the file 
 * doc/abstract_logic/rules3.tex
 *
-}


module Kernel where

import Core_database

import Dcore

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Vtslib

import Core_datatype

{- for tags -- terms and theorems now defined in core_datatype -}

--data Trm = TM ITrm ITrm ISgn | {- the term, its type, and signature	-}
--	   TM_Err String

data Sgn = SG ISgn	|
	   SG_Err String

data Dec = DC IDec ISgn	 |	{- the declaration, and its signature 	-}
	   DC_Err String

--data Thm = TH ITrm ISgn	| {- the theorem, and its signature     	-}
--	   TH_Err String

{-
	(*and Mph = MP of mapping list*)
-}

{- Symbol formation -}

symbol (SG sg) i j 
	= if share_map !! i == i 
		then TM (Sym i j [] []) (typ_of_sm sg i j) sg
	        else TM_Err "Malformed symbol" 
	  where
    	  share_map = get_share_map sg





{- Universe formation -}

universe (SG sg) i 
	| i>=0 = TM (Constant (Univ i) [] []) (Constant (Univ (i+1)) [] []) sg
	| i<0  = TM_Err "Malformed Universe" 





{- Pi formation -}

pi' (TM tm (Constant (Univ i) _ _) (Extend dc sg _)) 
	= case typ_of_trm sg (typ_of_dec dc) of
	       Constant (Univ j) _ _  
		    -> TM tm1 tm2 sg 
		       where
	       	       tm1 = Binder Pi dc tm [] []
		       tm2 = Constant (Univ (max i j)) [] []
	       _    -> TM_Err "Malformed pi expression" 

pi' _ = TM_Err "Sort of pi exression is not a universe" 





{- Pi introduction -}

lambda (TM tm1 tm2 (Extend dc sg _)) 
	= if is_sym_dec dc 
		then 
		    TM (Binder Lambda dc tm1 [] []) (Binder Pi dc tm2 [] []) sg
	        else 
		    TM_Err "lambda: invalid declaration"

lambda (TM_Err mesg ) = TM_Err mesg

lambda _ = TM_Err "Malformed lambda expression"





{- PI elimination -}

appl (TM tm1 (Binder Pi dc tm2 _ _) sg1) (TM tm3 tm4 sg2) 
	= if eq_sgn sg1 sg2 && eq_trm (typ_of_dec dc) tm4 
		then 
		    TM (App tm1 tm3 [] []) (subst_trm dc tm2 tm3) sg1
	        else 
		    TM_Err "Malformed application"
appl _ _ 
	= TM_Err "Malformed application (2)"





{- Sigma formation -}

sigma (TM tm (Constant (Univ i) _ _) (Extend dc sg _)) 
	= case typ_of_trm sg (typ_of_dec dc) of
	       Constant (Univ j) _ _ 
		   -> TM tm1 tm2 sg
		      where
	       	      tm1 = Binder Sigma dc tm [] []
		      tm2 = Constant (Univ (max i j)) [] []
	       _   -> TM_Err "Malformed sigma"

sigma _ = TM_Err "Malformed sigma (2)"





{- Sigma introduction -}

pair (TM tm1 tm2 sg1) (TM tm3 tm4 sg2) (TM tm7@(Binder Sigma dc tm5 _ _) tm6 sg3)
	= if eq_sgn sg1 sg2 && eq_sgn sg2 sg3 && 
	       eq_trm tm2 (typ_of_dec dc) && eq_trm tm4 (subst_trm dc tm5 tm1)
	    then
		TM (Pair tm1 tm3 tm7 [] []) tm7 sg1
	    else 
		TM_Err "Malformed pair"

pair _ _ _ = TM_Err "Malformed pair (2)"





{- Subtype formation -}

subtype (TM tm (Constant Bool' _ _) (Extend dc sg _)) 
	= if eq_trm (typ_of_trm sg (typ_of_dec dc)) (Constant (Univ 0) [] []) 
	      then
		  TM (Binder Subtype dc tm [] []) (Constant (Univ 0) [] []) sg
	      else 
		  TM_Err "Malformed subtype"

subtype _ = TM_Err "Malformed subtype (2)"





{- Subtype introduction -}

into (TM tm1 tm2 sg1) (TM tm3@(Binder Subtype dc tm4 _ _) _ sg2) (TH tm5 sg3) 
	= if eq_sgn sg1 sg2 && eq_sgn sg2 sg3 && 
	    eq_trm (typ_of_trm sg2 (typ_of_dec dc)) (Constant (Univ 0) [] []) &&
	    eq_trm tm2 (typ_of_dec dc) && eq_trm tm5 (subst_trm dc tm4 tm1)
	    then
	    	TM (add_type tm1 tm3) tm3 sg1
	    else
	    	TM_Err "Malformed subtype introduction "

into _ _ _ = TM_Err "Malformed subtype introduction (2)"





{- Subtype elimination -}

outof (TM tm1 (Binder Subtype dc _ _ _) sg) 
	= TM (add_type tm1 tm2) tm2 sg
	  where
    	  tm2 = typ_of_dec dc

outof _ = TM_Err " outof: argument invalid"





{- Bool formation -}

bool_sm (SG sg) 
	= TM (Constant Bool' [] []) (Constant (Univ 0) [] []) sg





{- Bool introduction -}

{- true formation -}

true_sm (SG sg) = TM (Constant T [] []) (Constant Bool' [] []) sg

{- false formation -}

false_sm (SG sg) = TM (Constant F [] []) (Constant Bool' [] []) sg





{- Universal quantification formation -}

universal (TM tm (Constant Bool' _ _) (Extend dc sg _)) 
	= if is_sym_dec dc 
	    then
	    	TM (Binder Forall dc tm [] []) (Constant Bool' [] []) sg
	    else
	    	TM_Err "Malformed universal quantification"

universal _ = TM_Err "Malformed universal quantification (2)"





{- Existential quantification formation -}

existential (TM tm (Constant Bool' _ _) (Extend dc sg _)) 
	= if is_sym_dec dc 
	    then
	    	TM (Binder Exists dc tm [] []) (Constant Bool' [] []) sg
	    else
		TM_Err "Malformed existential quantification"

existential _ = TM_Err "Malformed existential quantification (2)"





{- Implication formation -}

implication (TM tm (Constant Bool' _ _) (Extend dc sg _)) 
	= if is_axm_dec dc 
	    then
	    	TM (Binder Imp dc tm [] []) (Constant Bool' [] []) sg
	    else
		TM_Err "Malformed Implication"

implication _ =	TM_Err "Malformed Implication (2)"





{- And formation -}

conjunction (TM tm1 (Constant Bool' _ _) sg1) (TM tm2 (Constant Bool' _ _) sg2) 
	= if eq_sgn sg1 sg2 
	    then
	    	TM (Binary' And tm1 tm2 [] []) (Constant Bool' [] []) sg1
	    else
		TM_Err "Malformed conjunction"

conjunction _ _ = TM_Err "Malformed conjunction (2)"





{- Or formation -}

disjunction (TM tm1 (Constant Bool' _ _) sg1) (TM tm2 (Constant Bool' _ _) sg2) 
	= if eq_sgn sg1 sg2 
	    then
	    	TM (Binary' Or tm1 tm2 [] []) (Constant Bool' [] []) sg1
	    else
		TM_Err "Malformed disjunction "

disjunction _ _ = TM_Err "Malformed disjunction (2)"





{- Not formation -}

negation (TM tm (Constant Bool' _ _) sg) 
	= TM (Unary Not tm [] []) (Constant Bool' [] []) sg

negation _ = TM_Err "Malformed negation"





{- Eq formation -}

equal (TM tm1 _ sg1) (TM tm2 _ sg2) 
	= if eq_sgn sg1 sg2 
	    then
	    	TM (Binary' Eq' tm1 tm2 [] []) (Constant Bool' [] []) sg1
	    else
		TM_Err "Malformed equality"





{- Issustype formation -}

issubtype (TM tm1 (Constant (Univ 0) _ _) sg1) 
		(TM tm2 (Constant (Univ 0) _ _) sg2) 
	= if eq_sgn sg1 sg2 
	    then
	    	TM (Binary' Issubtype tm1 tm2 [] []) (Constant Bool' [] []) sg1
	    else
		TM_Err "Malformed subtype"

issubtype _ _ = TM_Err "Malformed subtype (2)"





{- Bool elimination (ie Conditionals) -}

conditional (TM tm1 tm2 (Extend dc1 sg1 _)) (TM tm3 tm4 (Extend dc2 sg2 _)) 
	= if eq_sgn sg1 sg2 && 
	       eq_trm tm2 tm4 &&
	       eq_trm (Unary Not (typ_of_dec dc1) [] []) (typ_of_dec dc2)
	    then
	    	TM (Cond dc1 tm1 tm3 [] []) tm2 sg1
	    else
		TM_Err "Malformed conditional"

conditional _ _ = TM_Err "Malformed conditional (2)"





{- Hilbert epsilon operator introduction -}

choose (TH (Binder Exists dc tm _ _) sg) 
	= if eq_trm (typ_of_trm sg (typ_of_dec dc)) (Constant (Univ 0) [] []) 
	    then
	    	TM (Binder Choose dc tm [] []) (Binder Subtype dc tm [] []) sg
	    else
		TM_Err "epsilon operator error"

choose _ = TM_Err "epsilon operator error (2)"





{- Datatype constructor formation and introduction -}

constructor (SG sg) i j k 
	= if share_map !! i == i 
	       then
		   TM (Const i j k [] []) (typ_of_cn sg i j k) sg
	       else
		   TM_Err "Malformed constructor"
	  where
    	  share_map = get_share_map sg





{- Datatype elimination -}

recurse tmL (TM (tm @ (Binder Pi (Symbol_dec tm1 _) _ _ _)) _ sg) 
	= if forall ok (zip tmL tyL) 
	       then
		   TM (Recurse (map fst tmL) tm [] []) tm sg
	       else
		   TM_Err "recurse error"
	  where
    	  (tyL,_) = clause_types sg tm1 tm
	  ok (TM _ tm1 sg1 , tm2) = eq_sgn sg sg1 && eq_trm tm1 tm2
	  fst (TM tm _ _) = tm

recurse _ _ = TM_Err "recurse error (2)"





{- Widen type -}

widen (TM tm1 tm2 sg1) (TH (Binary' Issubtype tm3 tm4 _ _) sg2) 
	= if eq_sgn sg1 sg2 && eq_trm tm2 tm3 
	    then
	    	TM (add_type tm1 tm4) tm4 sg1
	    else
		TM_Err "widen: error"	

widen _ _ = TM_Err "widen: error (2)"
	




{- Set the attributes of a term -}

set_Trm_att (TM tm1 tm2 sg) iL att 
	= TM (set_trm_att tm1 iL att) tm2 sg

set_Trm_att (TM_Err mesg) _ _ = TM_Err mesg





{- Get the attributes of a term -}

get_Trm_att (TM tm _ _) iL 
	= get_trm_att tm iL




{- return the internal representation of a term -}

internal_Trm (TM tm1 tm2 sg) = (tm1,tm2,sg)






{- Read a term in from the database -}
    
restore_Trm s = TM_Err "restore_Trm unimplemented"





{- The empty signature -}

empty = SG (Empty [])





{- Extend a signature with a declaration -}

extend (DC dc sg1) (SG sg2) 
	 = if eq_sgn sg1 sg2 
	    then
	    	SG (Extend dc sg2 [])
	    else
	    	SG_Err "Malformed signature extension"

extend _ _ = SG_Err "Invalid declaration in signature"





{- Combine two signatures -}

combine (SG sg1) (SG sg2) 
	= SG (Combine sg1 sg2 (len_sgn sg2) (sm2 ++ map update sm1) [])
	  where
	  sm1 = get_share_map sg1
	  sm2 = get_share_map sg2
	  offset = length sm2
	  update i = i + offset





{- Share two sub-sigantures within a signature -}

share (SG sg) i j 
	= if eq_sgn sg1 sg2 
	       then
		  SG (Share sg i j (len_sgn sg2) 
			(addequivL i j (len_sgn sg2) sm) [])
	       else
	       	  SG_Err "sg: Share"
	  where
    	  sg1 = nth_sgn i sg
	  sg2 = nth_sgn j sg
	  sm  = get_share_map sg





{- Return the attributes of a signature -}

get_Sgn_att (SG sg) = get_sgn_att sg





{- Set the attributes of a signature -}
		     
set_Sgn_att (SG sg) att = (SG (set_sgn_att sg att))





{- Return the internal representation of a signature -}
	
internal_Sgn (SG sg) = sg





{- Restore a signature from the database -}

restore_Sgn s = SG_Err "restore_Sgn unimplemented"





{- declare a new symbol -}

symbol_dec (TM tm1 tm2 sg) 
	= case typ_of_trm sg tm2 of
	      Constant (Univ _) _ _ -> DC (Symbol_dec tm1 []) sg
	      _ 		    -> DC_Err "Malformed symbol declaration"




		
{- declare a new axiom -}

axiom_dec (TM tm (Constant Bool' _ _) sg) 
	= DC (Axiom_dec tm []) sg

axiom_dec _ = DC_Err "Malformed axiom declaration"





{- Define a new symbol -}

def (TM tm1 tm2 sg) 
	= DC (Def tm1 tm2 []) sg





make_data tmLL (TH tm sg) 
	= if forall (forall (wf_param sg1)) tmLL 
		then
		   if exists (eq_trm tm) non_empty_thms 
		      then
			  DC (Data [] (map (map fst) tmLL) []) sg
		      else
			  DC_Err "Malformed datatype"
		else
		   DC_Err "Malformed datatype (2)"
	  where
	  non_empty_thms = map gen_proof (filter is_base tmLL)
	  wf_param sg1 (TM tm1 (Constant (Univ _) _ _) sg2) 
		= eq_trm tm1 (Sym 0 0 [] []) || not (occurs 0 tm1) 

	  wf_param _ _ = False

	  mk_exists tm1 tm2 
		= Binder Exists (Symbol_dec tm3 []) tm4 [] []
		  where
		  tm3 = shift_trm [] (-1) tm1
		  tm4 = shift_trm [] 1 tm2
	  fst (TM tm _ _) = tm
	  is_base tmL = not (exists (eq_trm (Sym 0 0 [] [])) (map fst tmL))
	  gen_proof tmL = foldr mk_exists (Constant T [] []) 
				( reverse (map fst tmL))
	  sg1 = Extend (Symbol_dec (Constant (Univ 0) [] []) []) sg []





polydata (DC (Data dcL tmLL _) (Extend dc sg _)) 
	= if is_sym_dec dc 
	    then
		DC (Data (dc:dcL) tmLL []) sg
	    else
		DC_Err "Malformed datatype (polydata)"

polydata _ = DC_Err "Malformed datatype (polydata 2)"





{- Declaration pair formation -}

decpair (DC dc1 (Extend dc2 sg _)) 
	= DC (Decpair dc2 dc1 []) sg

decpair _ = DC_Err "Malformed declaation pair"





{- Get the attributes of a declaration -}

get_Dec_att (DC dc _) 
	= get_dec_att dc





{- Set the attributes of a declaration -}

set_Dec_att (DC dc sg) att 
	= DC (set_dec_att dc att) sg





{- Return the internal representation of a declaration -}

internal_Dec (DC dc sg) = (dc,sg)

	



{- Restore a declaration from the database -}

restore_Dec s = error "BadDeclaration"	-- ** exn NOT IMPLEMENTED YET 





{- Axiom formation -}

axiom (SG sg) i j 
	= if share_map !! i == i 
	       then
		   TH (typ_of_axm sg i j) sg
	       else
	           TH_Err "Malformed Axiom"
	  where
	  share_map = get_share_map sg





{- Forall introduction -}

generalise (TH tm (Extend dc sg _)) 
	= if is_sym_dec dc 
	    then
	    	TH (Binder Forall dc tm [] []) sg
	    else
		TH_Err "Malformed generalisation"

generalise _ = TH_Err "Malformed generalisation (2)"





{- Forall elimination -}

specialise (TH (Binder Forall dc tm1 _ _) sg1) (TM tm2 tm3 sg2) 
	= if eq_sgn sg1 sg2 && eq_trm (typ_of_dec dc) tm3 
	    then
	    	TH (subst_trm dc tm1 tm2) sg1
	    else
		TH_Err "Malformed specialisation"

specialise _ _ 
	= TH_Err "Malformed specialisation (2)"





{- Exists introduction -}

exists_intro (TH tm1 sg1) 
	     (TM tm5@(Binder Exists dc tm2 _ _) _ sg2)
	     (TM tm3 tm4 sg3) 
	= if eq_sgn sg1 sg2 && eq_sgn sg2 sg3 &&
	       eq_trm (typ_of_dec dc) tm4 && eq_trm (subst_trm dc tm2 tm3) tm1
	    then
	    	TH tm5 sg2
	    else
		TH_Err "Malformed existential introduction"

exists_intro _ _ _ = TH_Err "Malformed existential introduction (2)"





{- Exists elimination -}

exists_elim (TH (Binder Forall dc1 (Binder Imp dc tm2 _ _) _ _) sg1)
	    (TH (Binder Exists dc2 tm3 _ _) sg2) 
	= if eq_sgn sg1 sg2 && eq_dec dc1 dc2 && 
	       eq_trm ( typ_of_dec dc ) tm3 && not (occurs 0 tm2)
	    then
	    	TH tm2 sg1
	    else
		TH_Err "Invalid existential elimination"

exists_elim _ _ = TH_Err "Invalid existential elimination"





{- => introduction -}

discharge (TH tm (Extend dc sg _)) 
	= if is_axm_dec dc 
	    then
	    	TH (Binder Imp dc tm [] [] ) sg
	    else
		TH_Err "Invalid implication introduction"

discharge _ = TH_Err "Invalid implication introduction (2)"





{- => elimination -}

modus_ponens (TH (Binder Imp dc tm2 _ _) sg1) (TH tm3 sg2) 
	= if eq_sgn sg1 sg2 && eq_trm ( typ_of_dec dc ) tm3 
	    then
	    	TH tm2 sg1
	    else
		TH_Err "Invalid implication elimination"

modus_ponens _ _ = TH_Err "Invalid implication elimination"




{- Propositional tautologies -}

taut (TM tm (Constant Bool' _ _) sg) 
	= if eval tm 
	    then
	    	TH tm sg
	    else
		TH_Err "term is not a tautology"

taut _ = TH_Err "argument must be a term of sort `bool'"





{- Reflexivity of equality -}

reflex (TM tm _ sg) 
	= TH (Binary' Eq' tm tm [] []) sg





{- Symmetry of equality -}

symmetry (TH (Binary' Eq' tm1 tm2 _ _) sg) 
	= TH (Binary' Eq' tm2 tm1 [] []) sg

symmetry _ = TH_Err "symmetry: argument must be an equality term"





{- Beta reduce a subterm of a theorem -}

beta_rw (TH tm sg) i 
	= case select_trm tm i of
	      (App (Binder Lambda dc tm1 _ _) tm2 _ _ ,_) 
		   -> TH (replace_trm tm (subst_trm dc tm1 tm2) i) sg
	      _    -> TH_Err "Invalid beta reduction"





{- Eta reduce a subterm of a theorem -}	

eta_rw (TH tm sg) i 
	= case select_trm tm i of
	      (Binder Lambda dc (App tm1 tm2 _ _) _ _ ,_) 
		  -> if not (occurs 0 tm1) && eta_match dc tm2 1 
		     then
		     	 TH (replace_trm tm (shift_trm [] (-1) tm1) i) sg
		     else
	                 TH_Err "Invalid eta reduction"
	      _   -> TH_Err "Invlaid eta reduction (2)"





{- Rewrite conditional (condition is true) -}

cond_true_rw (TH tm1 sg1) (TH tm2 sg2) i 
	= case select_trm tm2 i of
	      (Cond dc tm3 tm4 _ _ ,dcL) 
		  -> if eq_sgn sg1 sg3 && eq_trm tm1 (typ_of_dec dc) 
			then
		     	    TH (replace_trm tm2 (shift_trm [] (-1) tm3) i) sg1
			else
			    TH_Err "cond_true_rw: error"
		     where
	      	     sg3 = foldr (\ dc -> \ sg -> Extend dc sg []) sg2 dcL

	      _   -> TH_Err "cond_true_rw: error 2"





{- Rewrite conditional (condition is false) -}

cond_false_rw (TH tm1 sg1) (TH tm2 sg2) i 
	= case select_trm tm2 i of
	      (Cond dc tm3 tm4 _ _ ,dcL) 
		  -> if eq_sgn sg1 sg3 && 
		     	   eq_trm tm1 (Unary Not (typ_of_dec dc) [] [])
			then
			   TH (replace_trm tm2 (shift_trm [] (-1) tm4) i) sg1
			else
			   TH_Err "cond_false_rw: error"
		     where
	      	     sg3 = foldr (\ dc -> \ sg -> Extend dc sg []) sg2 dcL

	      _   -> TH_Err "cond_false_rw: error 2"





{- Substutution of equal terms -}

subterm_rw (TH tm1 sg1) (TH (Binary' Eq' tm2 tm3 _ _) sg2) i 
	= if eq_sgn sg3 sg3 && eq_trm tm2 tm4 
	       then
	    	   TH (replace_trm tm1 tm3 i) sg1
	       else
		   TH_Err "subterm_rw: terms or sigs unequal"
	  where
    	  (tm4,dcL) = select_trm tm1 i 
	  sg3 = foldr (\ dc -> \ sg -> Extend dc sg []) sg1 dcL

subterm_rw _ _ _ = TH_Err "subterm_rw: Invalid argument"





{- Injectivity of datatypes -}

injection (TH (Binary' Eq' tm1 tm2 _ _) sg) 
	= case (reduce_app tm1 [], reduce_app tm2 []) of
		 (c1@(Const i j k _ _):tmL1 , c2@(Const _ _ _ _ _):tmL2) 
		     -> case extract_dc j (nth_dec i sg) of
			   Data dcL _ _ 
			       -> if length dcL < length tmL1 &&
			 	    eq_trm c1 c2 && 
				    length tmL1 == length tmL2 
				  then
				      TH (foldr1 mk_and 
					   (map mk_eq (zip tmL11 tmL21))) sg

				  else
				      TH_Err "Invalid injection"
				  where
				  tmL11 = drop (length dcL) tmL1
				  tmL21 = drop (length dcL) tmL2 

			   _   -> TH_Err "Invalid injection: not a datatype"

		 _   -> TH_Err "Invalid injection (3)"
	  where
    	  reduce_app (App tm1 tm2 _ _) tmL 
		= reduce_app tm1 (tm2:tmL)

	  reduce_app tm tmL = tm:tmL

	  mk_eq (tm1,tm2) = Binary' Eq' tm1 tm2 [] []
	  mk_and tm1 tm2 = Binary' And tm1 tm2 [] []

injection _ = TH_Err "Invalid injection (4)"





{- Induction over datatypes -}

induction (TM tm@(Const i j 0 _ _) _  sg) 
	= TH ind_axm sg
	  where
	  ind_axm = induction_trm sg tm

induction _ = TH_Err "Invalid induction"





{- Issubtype introduction -}

issubstype_intro (TH (Binder Forall dc1 tm1 _ _) sg) 
	= case tm1 of 
	       Binder Exists dc2 (Binary' Eq' tm2 tm3 _ _) _ _ 
		  -> case (tm2,tm3) of
			 (Sym 1 0 _ _ , Sym 0 0 _ _) 
	 		     -> TH (Binary' Issubtype tm4 tm5 [] []) sg
		     	        where
			 	tm4 = typ_of_dec dc1
				tm5 = shift_trm [] (-1) (typ_of_dec dc2)
			 _   -> TH_Err "issubtype_intro error (1)"

	       _  -> TH_Err "issubtype_intro error (2)"

issubstype_intro _ = TH_Err "issubtype_intro error (3)"





{- Issubtype elimination -}

issubstype_elim (TH (Binary' Issubtype tm1 tm2 _ _) sg) 
	= TH (Binder Forall dc1 tm5 [] []) sg
	  where
	  dc1 = Symbol_dec tm1 []
	  dc2 = Symbol_dec (shift_trm [] 1 tm2) []
	  tm3 = Sym 1 0 [] []
	  tm4 = Sym 0 0 [] []
	  tm5 = Binder Exists dc2 (Binary' Eq' tm3 tm4 [] []) [] []

issubstype_elim _ = TH_Err "issubtype_elim error"





{- Equality of types -}

eq_of_ty (TH (Binary' Issubtype tm1 tm2 [] []) sg1)
    	    (TH (Binary' Issubtype tm3 tm4 [] []) sg2) 
	= if eq_sgn sg1 sg2 && eq_trm tm1 tm4 && eq_trm tm2 tm3 
	    then
	    	TH (Binary' Eq' tm1 tm2 [] []) sg1
	    else
	    	TH_Err "eq_of_ty error"

eq_of_ty _ _ = TH_Err "eq_of_ty error (2)"





{- project the theorem out of a terms subtype -}

from (TM tm1 (Binder Subtype dc tm2 _ _) sg) 
	= TH (subst_trm dc tm2 tm1) sg
	
from _ = TH_Err "from: argument must be term of subtype sort"





{- definition elimination -}

def_elim_thm (TH tm (Extend dc sg _)) 
	= if is_def_dec dc 
	    then
		TH (subst_trm dc1 tm tm1) sg
	    else
		TH_Err "Definition elimination error"
	  where
	  (dc1,tm1,_) = split_def dc

def_elim_thm _ = TH_Err "Definition elimination error (2)"





{- weaken a theorem -}

weaken (SG sg1) (TH tm sg2) 
	= case is_sub_sgn sg2 sg1 of
	      SOME i 
		  -> TH (shift_trm share_map i tm) sg1
		     where
	      	     share_map = get_share_map sg1

	      NONE -> TH_Err "Weaken error"



	

set_Thm_att (TH tm sg) iL att 
	= TH (set_trm_att tm iL att) sg




    
get_Thm_att (TH tm sg) iL 
	= get_trm_att tm iL




	
{- Restore a theorem from the database -}
    
restore_Thm s = error "BadTheorem" -- ** exn




    
{- return the internal representation of a theorem -}
    
internal_Thm (TH tm sg) = (tm,sg)

internal_Thm (TH_Err mesg ) = error "add feed to itm via extra itrm ctr"
    




{-
database_magic_trm_str = "VTSTRM\^H\^H\^H\^H\^H\^H\^X\^Y"
database_magic_sgn_str = "VTSSGN\^H\^H\^H\^H\^H\^H\^X\^Y"
database_magic_dec_str = "VTSDEC\^H\^H\^H\^H\^H\^H\^X\^Y"
database_magic_thm_str = "VTSTHM\^H\^H\^H\^H\^H\^H\^X\^Y"

magic_str_len = length database_magic_trm_str
dbase_str = "vts90/lib/dbase/"
trm_str   = "Term"
sgn_str   = "Signature"
dec_str   = "Declaration"
thm_str   = "Theorem"



write_obj magic_str type_str obj file 
	= if test_file full_file_name "f" 
	       then
		   False
	       else
		   let val ostr = open_out full_file_name
		   in output (ostr, magic_str);
		      output (ostr, obj);
		      close_out ostr;
	              true
		   end
	    end
	    let val home = get_env_var "HOME"
		val full_file_name = home ^ "/" ^ dbase_str ^ type_str ^ "/" ^ file


    fun read_obj exn magic_str type_str file =
	    let val home = get_env_var "HOME"
		val vtshome = get_env_var "VTS_LIB_DIR"
		val first_choice = home ^ "/" ^ dbase_str ^ type_str ^ "/" ^ file
		val second_choice = vtshome ^ "/" ^ dbase_str ^ type_str ^ "/" ^ file
		val file_name = 
			if test_file first_choice "fr" then
				first_choice
			else if test_file second_choice "fr" then
				second_choice
			else
				raise exn
		val istr = open_in file_name
		val obj_magic_str = input (istr, magic_str_len)
		val obj_str = input_to_eof istr
	    in close_in istr;
	       if magic_str = obj_magic_str then
		   obj_str
	       else
		   raise exn
	    end

    and input_to_eof istr =
	    if end_of_stream istr then 
		""
	    else
		let val str1 = input (istr,1024)
		    val str2 = input_to_eof istr
		in (str1 ^ str2) end

    val anon_sgn_name_header = "%"

    fun read file =
	    let val instr = open_in file
		val str = input_to_eof instr
	    in close_in instr;
	       str
	    end
	    handle Io _ => ""

    fun write file str =
	   let val outstr = open_out file
	   in output (outstr, str);
	      close_out outstr
	   end

    fun already_there str dir ("." :: files) =		(* skip over . and .. *)
	    already_there str dir files
      | already_there str dir (".." :: files) =
	    already_there str dir files
      | already_there str dir (file :: files) =
	    if read (dir ^ file) = str then
		SOME file
	    else
	       already_there str dir files
     | already_there str dir [] =
	    NONE

    fun save_sgn name sgn_rep =
	    let val home = get_env_var "HOME"
		val sgn_dir = home ^ "/" ^ dbase_str ^ sgn_str ^ "/"
		val sgs = System.Directory.listDir sgn_dir
	    in case already_there sgn_rep sgn_dir sgs
		 of SOME nm => nm
		  | NONE    => (write (sgn_dir ^ anon_sgn_name_header ^ name) sgn_rep;
				anon_sgn_name_header ^ name)
	    end
		   
	
    val a_chr = fromEnum "a"
    and z_chr = fromEnum "z"
    and A_chr = fromEnum "A"
    and Z_chr = fromEnum "Z"
    and zero_chr = fromEnum "0"
    and nine_chr = fromEnum "9"
    and minus_chr =  fromEnum "-"
    and underline_chr =  fromEnum "_"
    and dot_cht = fromEnum "."

    (* database names must be either:	*)
    (*		lower-case alphas a-z	*)
    (*		upper-case alphas A-Z	*)
    (*		numbers		  0-9	*)
    (*		or                _ - .	*)

    fun ok_name name =
	    let fun ok_ch ch = 
			(a_chr <= ch andalso ch <= z_chr)	orelse
			(A_chr <= ch andalso ch <= Z_chr)	orelse
			(zero_chr <= ch andalso ch <= nine_chr) orelse
			(ch = minus_chr)			orelse 
			(ch = underline_chr)			orelse 
			(ch = dot_cht)
	    in forall ok_ch (map fromEnum (explode name)) end

    in (* local *)

    (* STILL TO BE DONE *)
	(* SORT OUT SIGNATURE SHARING *)

    fun save_Trm name (TM (tm1,tm2,sg)) =
	    if ok_name name then
	    	let val tm1_str = trm_to_str tm1
		    val tm2_str = trm_to_str tm2
	            val sg_str = database_magic_sgn_str ^ sgn_to_str sg
		    val sg_nm  = save_sgn name sg_str
	        in write_obj database_magic_trm_str trm_str (tm1_str ^ tm2_str ^ sg_nm) name end
	    else
		false

    fun save_Sgn name (SG sg) =
	    if ok_name name then
	    	let val sg_str = database_magic_sgn_str ^ sgn_to_str sg
		    val sg_nm  = save_sgn name sg_str
	    	in write_obj database_magic_sgn_str sgn_str sg_nm name end
	    else
	        false

    fun save_Dec name (DC (dc,sg)) =
	    if ok_name name then
	    	let val dc_str = dec_to_str dc
		    val sg_str = database_magic_sgn_str ^ sgn_to_str sg
	            val sg_nm = save_sgn name sg_str
	    	in write_obj database_magic_dec_str dec_str (dc_str ^ sg_nm) name end
	    else
		false

    fun save_Thm name (TH (tm,sg)) =
	    if ok_name name then
	    	let val tm_str = trm_to_str tm
		    val sg_str = database_magic_sgn_str ^ sgn_to_str sg
	            val sg_nm = save_sgn name sg_str
	    	in write_obj database_magic_thm_str thm_str (tm_str ^ sg_nm) name end
	    else
		false

    fun restore_Trm file =
	    let val obj_str = read_obj BadTerm database_magic_trm_str trm_str file
		val (tm1, is1) = str_to_trm (mkistring obj_str)
		val (tm2, is2) = str_to_trm is1
		val sg_nm = rest_istring is2
		val sgn_rep = read_obj BadSignature database_magic_sgn_str sgn_str sg_nm
		val (sg,  is3) = str_to_sgn (mkistring sgn_rep)
	    in (TM (tm1,tm2,sg)) end

    fun restore_Sgn file =
	    if ok_name file then
	    	let val sg_nm = read_obj BadSignature database_magic_sgn_str sgn_str file
		    val obj_str = read_obj BadSignature database_magic_sgn_str sgn_str sg_nm
		    val (sg,  is3) = str_to_sgn (mkistring obj_str)
	    	in (SG sg) end
	    else
		raise BadSignature

    fun restore_Dec file =
	    let val obj_str = read_obj BadDeclaration database_magic_dec_str dec_str file
		val (dc, is1) = str_to_dec (mkistring obj_str)
		val sg_nm = rest_istring is1
		val sgn_rep = read_obj BadSignature database_magic_sgn_str sgn_str sg_nm
		val (sg, is2) = str_to_sgn (mkistring sgn_rep)
	    in (DC (dc,sg)) end

    fun restore_Thm file =
	    let val obj_str = read_obj BadTheorem database_magic_thm_str thm_str file
		val (tm, is1) = str_to_trm (mkistring obj_str)
		val sg_nm = rest_istring is1
		val sgn_rep = read_obj BadSignature database_magic_sgn_str sgn_str sg_nm
		val (sg, is2) = str_to_sgn (mkistring sgn_rep)
	    in (TH (tm,sg)) end

    end (* local *)

-}

eq_Trm (TM tm1 _ sg1 ) (TM tm2 _ sg2 ) 
	= eq_trm tm1 tm2 && eq_sgn sg1 sg2

eq_Sgn (SG sg1) (SG sg2) 
	= eq_sgn sg1 sg2

eq_Dec (DC dc1 sg1 ) (DC dc2 sg2 ) 
	= eq_dec dc1 dc2 && eq_sgn sg1 sg2

eq_Thm (TH tm1 sg1) (TH tm2 sg2) 
	= eq_trm tm1 tm2 && eq_sgn sg1 sg2

typ_of_Trm (TM _ tm sg) 
	= TM tm ( typ_of_trm sg tm ) sg

typ_of_Dec ( DC dc sg )
	= TM tm1 tm2 sg
	  where
	  tm1 = typ_of_dec dc 
	  tm2 = typ_of_trm sg tm1

typ_of_Thm ( TH tm sg )
	= TM tm ( Constant Bool' [] [] ) sg 






shift_Trm i (SG sg1) (TM tm1 tm2 sg2) 
	= if eq_sgn sg2 sg3 
		then TM (shift_trm sm i tm1) (shift_trm sm i tm2) sg1
	        else TM_Err "shift_Trm: signatures unequal"	
	  where
	  sg3 = nth_sgn i sg1
	  sm  = get_share_map sg2








subst_Trm (TM tm1 tm2 (Extend dc sg1 _)) (TM tm3 tm4 sg2) 
	= if eq_sgn sg1 sg2 && eq_trm (typ_of_dec dc) tm4 
		then TM (subst_trm dc tm1 tm2) (subst_trm dc tm2 tm3) sg1
	        else TM_Err "subst_Trm: signatures unequal or type of dc unequal to type of second argument"

subst_Trm _ _ = TM_Err "subst_Trm: Invalid arguments"


sgn_of_Trm (TM _ _ sg) = SG sg

sgn_of_Trm (TM_Err mesg ) = SG_Err mesg



sgn_of_Dec (DC _ sg)   = SG sg

sgn_of_Dec (DC_Err mesg ) = SG_Err mesg



sgn_of_Thm (TH _ sg)   = SG sg

sgn_of_Thm (TH_Err mesg ) = SG_Err mesg




is_valid_Thm ( TH _ _ ) = True

is_valid_Thm _ = False



is_valid_Trm ( TM _ _ _ ) = True

is_valid_Trm _ = False


is_valid_Sgn ( SG _ ) = True

is_valid_Sgn _ = False

