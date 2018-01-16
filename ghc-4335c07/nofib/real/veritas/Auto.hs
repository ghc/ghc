module Auto where	

import Core_datatype
import Edlib
import Lookup
import Tree
import X_interface

import Vtslib

import Tactics

import Parse

import Type_defs

import Goals
import Globals		-- partain
import Tags		-- partain

import Kernel


auto_tac = Tactic "Auto" null_arg_fn auto

--split_tac = OrdTactic ("Split",null_arg_fn,split)




auto _ sg lt _ (TrmSpec tm) = auto_tm sg lt tm

auto _ sg lt _ (ThmSpec  _) = Bad "Not Applicable"

auto _ sg lt _ (DecSpec dc) = auto_dc sg lt dc

auto _ sg1 lt _ (SgnSpec sg2) = auto_sg sg1 lt sg2



auto_tm sg lt tm = Ok ([], tm_valid (trm_to_Trm sg tm))


auto_dc sg lt dc = Ok ([], dc_valid (dec_to_Dec sg dc))


auto_sg sg0 lt sg = Ok ([], sg_valid (sgn_to_Sgn sg))



tm_valid tm [] = SOME (TrmDone tm)

tm_valid _ _ = NONE


dc_valid dc [] = SOME (DecDone dc)

dc_valid _ _ = NONE


sg_valid sg [] = SOME (SgnDone sg)

sg_valid _ _ = NONE

{-

split _ Sg lt _ (TrmSpec tm) = split_tm Sg lt tm

split _ Sg lt _ (DecSpec dc) = split_dc Sg lt dc

split _ Sg lt _ (SgnSpec sg) = split_sg Sg lt sg

split _ Sg lt _ (ThmSpec  _) = Bad "Not Applicable"



split_tm Sg lt tm@(Sym _ _ _ _) 
	= ([], [], [], lift_null_update Sg lt (tm_valid (trm_to_Trm Sg tm)))

split_tm Sg lt tm@(Constant _ _ _) =
	    ([], [], [], lift_null_update Sg lt (tm_valid (trm_to_Trm Sg tm)))
	
split_tm Sg lt tm@(Const _ _ _ _ _) 
	= ([], [], [], lift_null_update Sg lt (tm_valid (trm_to_Trm Sg tm)))

split_tm Sg lt tm@(App tm1 tm2 _ _) 
	= ([TrmSpec tm1,TrmSpec tm2],[lt,lt],[True,True],
		      lift_null_update Sg lt app_valid)

split_tm Sg lt tm@(Pair tm1 tm2 tm3 _ _) 
	= ([TrmSpec tm1,TrmSpec tm2,TrmSpec tm3],[lt,lt,lt],[True,True,True],
		      lift_null_update Sg lt pair_valid)

split_tm Sg lt tm@(Binder q dc tm1 _ _) 
	= ([DecSpec dc,TrmSpec tm1],[error "no lt"], [True,False],
		      binder_valid (binder_fn q) Sg)

split_tm Sg lt tm@(Cond dc tm1 tm2 _ _) 
	= ([DecSpec dc,TrmSpec tm1,TrmSpec tm2],[error "no lt"],
		      [True,False,False], cond_valid Sg)

split_tm Sg lt tm@(Unary u tm1 _ _) 
	=
	    ([TrmSpec tm1],[lt],[true],
		      lift_null_update Sg lt (unary_valid (unary_fn u)))
      | split_tm Sg lt (tm as Binary (b,tm1,tm2,_,_)) =
	    ([TrmSpec tm1,TrmSpec tm2],[lt,lt],[true,true],
		      lift_null_update Sg lt (binary_valid (binary_fn b)))
      | split_tm Sg lt (tm as Recurse (tmL,ty,_,_)) =
	    let val tmL1 = tmL @ [ty]
	    in  (map TrmSpec tmL1, 
		 map (fn x => lt) tmL1,
		 map (fn x => true) tmL1,
		 lift_null_update Sg lt recurse_valid)
	    end

    and split_dc Sg lt (dc as Symbol_dec (tm,_)) =
	    ([TrmSpec tm],[lt],[true],
		      lift_null_update Sg lt (symbol_dec_valid symbol_dec))
      | split_dc Sg lt (dc as Axiom_dec (tm,_)) =
	    ([TrmSpec tm],[lt],[true],
		      lift_null_update Sg lt (symbol_dec_valid axiom_dec))
      | split_dc Sg lt (dc as Def (tm,_,_)) =
	    ([TrmSpec tm],[lt],[true],
		      lift_null_update Sg lt (symbol_dec_valid def))
      | split_dc Sg lt (dc as Decpair (dc1,dc2,_)) =
	    ([DecSpec dc1, DecSpec dc2],[lt,extend_lookup_table true dc1 lt],
		      [true,false], decpair_valid Sg)
      | split_dc Sg lt (dc as Data (dcL, tmLL,_)) =
	    fail "Can't split datatypes yet!"

    and split_sg Sg lt (Empty _) = ( [], [],[],
					 lift_null_update Sg lt (empty_valid) )
      | split_sg Sg lt (Extend (dc,sg,_)) =
	    ([SgnSpec sg, DecSpec dc],[lt,extend_lookup_table true dc lt],
		      [true,false], extend_valid Sg)
      | split_sg Sg lt (Combine _) =
	    fail "Can't split combine yet!"
      | split_sg Sg lt (Share _) =
	    fail "Can't split combine yet!"

    and empty_valid [] = SOME (SgnDone empty)

    and extend_valid Sg dnL rwL =
	    case (dnL, rwL)
	      of ([SOME (SgnDone Sg), SOME (DecDone dc)],_) =>
		     (rwL,[Sg,Sg],SOME (SgnDone (extend dc Sg)))
	       | ([SOME (SgnDone Sg), NONE], [true,false]) =>
		     ([true,true],[Sg,Sg],NONE)
	   | _ => (rwL,[Sg,Sg],NONE)

    and symbol_dec_valid f [SOME (TrmDone tm)] =
	    SOME (DecDone (f tm))

    and decpair_valid Sg dnL rwL =
	    case (dnL, rwL)
	      of ([SOME (DecDone dc1), SOME (DecDone dc2)],_) =>
		     (rwL,[Sg,extend dc1 Sg],SOME (DecDone (decpair dc2)))
	       | ([SOME (DecDone dc1), NONE], [true,false]) =>
		     ([true,true],[Sg,extend dc1 Sg],NONE)
	       | _ => (rwL,[Sg,Sg],NONE)

    and app_valid [SOME (TrmDone tm1), SOME (TrmDone tm2)] =
	    SOME (TrmDone (appl tm1 tm2))

    and pair_valid [SOME (TrmDone tm1),SOME (TrmDone tm2),SOME (TrmDone tm3)] =
	    SOME (TrmDone (pair tm1 tm2 tm3))

    and binder_valid binder Sg dnL rwL =
	    case (dnL, rwL)
	      of ([SOME (DecDone dc), SOME (TrmDone tm)],_) =>
		     (rwL,[Sg,extend dc Sg],SOME (TrmDone (binder tm)))
	       | ([SOME (DecDone dc), NONE], [true,false]) =>
		     ([true,true],[Sg,extend dc Sg],NONE)
	       | _ => (rwL,[Sg,Sg],NONE)

    and cond_valid Sg dnL rwL =
	    case (dnL, rwL)
	      of ([SOME (DecDone dc), 
		   SOME (TrmDone tm1), 
		   SOME (TrmDone tm2)],_) => 
			(rwL,
			 [Sg,extend dc Sg],
			 SOME (TrmDone (conditional tm1 tm2)))
	       | ([SOME (DecDone dc), NONE,NONE],[true,false,false]) =>
		    ([true,true,true],[Sg,extend dc Sg,extend dc Sg],NONE)
	       | _ => (rwL,[Sg,Sg,Sg],NONE)

    and unary_valid unary [SOME (TrmDone tm1)] =
	    SOME (TrmDone (unary tm1))

    and binary_valid binary [SOME (TrmDone tm1), SOME (TrmDone tm2)] =
	    SOME (TrmDone (binary tm1 tm2))

    and recurse_valid tmL =
	    if forall (fn SOME _ => true | NONE => false) tmL
		then 	let val (rec_tmL, rec_ty) = split_list tmL
			in SOME (TrmDone (recurse rec_tmL rec_ty)) end
		else 	NONE

    and split_list [SOME (TrmDone tm)] = ([],tm)
      | split_list (SOME (TrmDone x)::l) = 
	    let val (l1,l2) = split_list l in (x::l1,l2) end

    and lift_null_update Sg lt vf dnL rwL =
	    let val dn = vf dnL
	    in (rwL,for (length rwL) (rep Sg) [], dn) end

    and rep Sg l = Sg :: l

    and binder_fn Forall    = universal
      | binder_fn Exists    = existential
      | binder_fn Lambda    = lambda
      | binder_fn Imp       = implication
      | binder_fn Subtype   = subtype
      | binder_fn Pi        = pi
      | binder_fn Sigma     = sigma

    and unary_fn  Not	    = negation

    and binary_fn And       = conjunction
      | binary_fn Or        = disjunction
      | binary_fn Issubtype = issubtype
      | binary_fn Eq'       = equal

    in


    end
    end
end
-}
