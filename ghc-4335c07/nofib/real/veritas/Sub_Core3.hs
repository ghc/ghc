
{-
 * Mon Nov  5 09:54:24 GMT 1990
 *
 * Implementation of untyped terms, signatures and declarations
 *
 * Each constructors last argument (of the tuple) is a list of
 * information attributes that the parser, unparsers, tactics etc use.
 *	
 * Each terms' next to last argument is a list of alternative types the the
 * term can have to its natutal type.
 *
-}

module Sub_Core3 where

import Vtslib

import Core_datatype

import Sub_Core2

import Sub_Core1




select_sm_ty f sg i j 
	= case extract_dc j dc of
	         Symbol_dec tm _ | f     -> uncurry_trm dc j tm
	         Axiom_dec tm _  | not f -> uncurry_trm dc j tm
	         Def _ tm _      | f     -> uncurry_trm dc j tm
		 Def tm _ _   	 | not f -> Binary' Eq' (Sym 0 j [] []) 
						(uncurry_trm dc j tm) [] []
	         _ -> error ("select: " ++ show f ++ show i ++ show j ++ "|\n")
	  where
	  dc = nth_dec i sg




select_cn_ty :: ISgn -> Int -> Int -> Int -> ITrm

select_cn_ty sg i j k 
	= Sym 0 0 [] []
{-
	= case extract_dc j dc of
		Data dcL tmL _ 
			-> if k == 0 then uncurry_trm dc j ty 
				     else remake_ty ty4 dcL 
					 (foldr make_app sms_base (reverse sms))
			   where
			   ty  = foldr mk_pi (Constant (Univ 0) [] []) 
						( reverse dcL )
	    		   sms = mk_smsl dcL 0 []
			   ty1 = foldr make_app (Sym 0 0 [] []) ( reverse sms )
			   ty2 = foldr mk_fnspace (Sym 0 0 [] []) (tmL!!k-1)
			   d1  = Symbol_dec (Constant (Univ 0) [] [] ) []
		 	   ty3 = foldr mk_pi (Binder Pi d1 ty2 [] []) 
						( reverse dcL )
			   ty4 = uncurry_trm dc j ty3
			   sms_base = Const (length dcL) j 0 [] []
--	        _ 	-> error "BadIndex" -- ** exn
	  where
	  dc = nth_dec i sg
-}


make_app tm1 tm2 = App tm1 tm2 [] []


{- return the type of a symbol -}

typ_of_sm sg i j 
	= shift_trm (get_share_map sg) i ty1 
	  where
	  ty1 = select_sm_ty True sg i j -- partain: was true





{- return the type of a constructor -}

typ_of_cn sg i j k 
	= shift_trm (get_share_map sg) i ty1 
	  where
	  ty1 = select_cn_ty sg i j k 






{- return the type of an axiom -}

typ_of_axm sg i j 
	= shift_trm (get_share_map sg) i ty1 
	  where
	  ty1 = select_sm_ty False sg i j -- partain: was false





{- extract the alternative types for a term -}

other_typ (Sym _ _ tmL _) = tmL

other_typ (App _ _ tmL _) = tmL

other_typ (Pair _ _ _ tmL _) = tmL

other_typ (Const _ _ _ tmL _) = tmL

other_typ (Binder _ _ _ tmL _) = tmL

other_typ (Unary _ _ tmL _) = tmL

other_typ (Binary' _ _ _ tmL _) = tmL

other_typ (Cond _ _ _ tmL _) = tmL

other_typ (Constant _ tmL _) = tmL

other_typ (Recurse _ _ tmL _) = tmL





{-
 * return the type of a term. If a term has alternative type other
 * than its natural type return that that.
 *
-}

-- typ_of_trm :: ISgn -> ITrm -> ITrm

typ_of_trm sg tm 
	= case other_typ tm of
	      []     -> typ_of_trm' sg tm
	      (tm:_) -> tm



typ_of_trm' sg (Sym i j _ _) 
	= typ_of_sm sg i j

typ_of_trm' sg (Const i j k _ _) 
	= typ_of_cn sg i j k 

typ_of_trm' sg (App tm1 tm2 _ _) 
	= case typ_of_trm sg tm1 of
	       Binder Pi dc tm3 _ _
			-> subst_trm dc tm3 tm2
	       _        -> case typ_of_trm' sg tm1 of
		                Binder Pi dc tm3 _ _ 
				     -> subst_trm dc tm3 tm2
--		                _    -> error "TypeOfTerm" -- ** exn

typ_of_trm' sg (Pair tm1 tm2 tm3 _ _) 
	= tm3

typ_of_trm' sg (Binder q dc tm _ _) 
	= typ_of_bnd sg q dc tm

typ_of_trm' sg (Constant c _ _) 
	= typ_of_cnt c

typ_of_trm' sg (Recurse _ tm _ _) 
	= tm

typ_of_trm' _ _ 
	= Constant Bool' [] []
    




typ_of_bnd sg Forall dc tm 
	= Constant Bool' [] []

typ_of_bnd sg Exists dc tm 
	= Constant Bool' [] []

typ_of_bnd sg Imp dc tm 
	= Constant Bool' [] []

typ_of_bnd sg Pi dc tm 
	= Constant (Univ (max i j)) [] [] 
	  where
	  sg1 = Extend dc sg []
	  (Constant (Univ i) _ _) = typ_of_trm sg1 tm
	  (Constant (Univ j) _ _) = typ_of_trm sg (typ_of_dec dc)

typ_of_bnd sg Sigma dc tm 
	= Constant (Univ (max i j)) [] [] 
	  where
	  sg1 = Extend dc sg []
	  (Constant (Univ i) _ _) = typ_of_trm sg1 tm
	  (Constant (Univ j) _ _) = typ_of_trm sg (typ_of_dec dc)

typ_of_bnd sg Subtype dc tm 
	= Constant (Univ 0) [] []

typ_of_bnd sg Lambda dc tm 
	= Binder Pi dc (typ_of_trm (Extend dc sg []) tm) [] []

typ_of_bnd sg Choose dc tm 
	= Binder Subtype dc tm [] []

--typ_of_bnd _ _ _ _ = error "System_Error" -- ** exn







typ_of_cnt T = Constant Bool' [] []

typ_of_cnt F = Constant Bool' [] []

typ_of_cnt Bool' = Constant (Univ 0) [] []

typ_of_cnt (Univ i) = Constant (Univ (i+1)) [] []






{-
 * evaluate a propositional term.
 *   A propositional term consists of:
 *      T, F, #fo d.t, #ex d.t, t1 #an t1, t1 #or t2, t1 #im t2, t1 = t2, #no t
 *	and locally bound symbols of type bool.
 *
-}

-- eval :: ITrm -> Bool

eval (Constant T _ _) = True

eval (Constant F _ _) = False

--eval (Constant _ _ _) = error "EvalError" -- ** exn
	
eval (Binder Forall dc tm _ _) 
	= eval_quant forall dc tm

eval (Binder Exists dc tm _ _) 
	= eval_quant exists dc tm

eval (Binder Imp dc tm _ _) 
	= not (eval ( typ_of_dec dc)) || eval tm

--eval (Binder _ _ _ _ _) 
--	= error "EvalError"

eval (Binary' And tm1 tm2 _ _) 
	= eval tm1 && eval tm2

eval (Binary' Or tm1 tm2 _ _) 
	= eval tm1 || eval tm2

eval (Binary' Eq' tm1 tm2 _ _) 
	= eval tm1 == eval tm2

eval (Unary Not tm _ _) 
	= not (eval tm)

eval (Cond dc tm1 tm2 _ _) 
	= eval (subst_trm dc tm1 (Constant T [] [])) &&
	    eval (subst_trm dc tm1 (Constant F [] []))

--eval _ = error "EvalError" -- ** exn





eval_quant f dc tm 
	= f (eval . subst_trm dc tm) (truth_table dc)





truth_table (Symbol_dec (Constant Bool' _ _ ) _) 
	= [ Constant T [] [] , Constant F [] [] ]

truth_table (Decpair dc1 dc2 _) 
	= make_pair (truth_table dc1) (truth_table dc2)

--truth_table _ = error "EvalError" -- ** exn





make_pair [] _ = []

make_pair (tm:tmL) l 
	= map (\ x -> Pair tm x (Constant Bool' [] []) [] []) l 
		++ make_pair tmL l





{-
 * check to see if any symbols are defined at particular level
-}

occurs n (Sym i _ _ _) 
	= n == i

occurs n (App tm1 tm2 _ _) 
	= occurs n tm1 || occurs n tm2 

occurs n (Pair tm1 tm2 tm3 _ _) 
	= occurs n tm1 || occurs n tm2 || occurs n tm3

occurs n (Binder _ dc tm _ _) 
	= occurs' n dc || occurs (n+1) tm 

occurs n (Unary _ tm _ _) 
	= occurs n tm 

occurs n (Binary' _ tm1 tm2 _ _) 
	= occurs n tm1 || occurs n tm2 

occurs n (Cond dc tm1 tm2 _ _) 
	= occurs' n dc || occurs (n+1) tm1 || occurs (n+1) tm2

occurs n (Recurse tmL tm _ _) 
	= exists (occurs n) tmL || occurs n tm

occurs _ _ = False





occurs' n (Symbol_dec tm _) 
	= occurs n tm

occurs' n (Axiom_dec tm _) 
	= occurs n tm

occurs' n (Decpair dc1 dc2 _) 
	= occurs' n dc1 || occurs' (n+1) dc2

--occurs' _ _ = error "VTS_ERROR" -- ** exn





{-
 * functions for retrieving and setting the information fields of
 * term, decs and sigs.
-}

get_trm_att tm iL 
	= case subtm of
		(Sym _ _ _ att)       -> att
      		(App _ _ _ att)       -> att
		(Pair _ _ _ _ att)    -> att
		(Constant _ _ att)    -> att
		(Binder _ _ _ _ att)  -> att
		(Unary _ _ _ att)     -> att
		(Binary' _ _ _ _ att) -> att
		(Cond _ _ _ _ att)    -> att
		(Const _ _ _ _ att)   -> att
		(Recurse _ _ _ att)   -> att
	  where
	  (subtm,_) = select_trm tm iL




set_trm_att tm iL att 
	= replace_trm tm (set subtm) iL 
	  where
    	  set (Sym i j tmL _)          = Sym i j tmL att
	  set (App tm1 tm2 tmL _)      = App tm1 tm2 tmL att
	  set (Pair tm1 tm2 tm3 tmL _) = Pair tm1 tm2 tm3 tmL att
	  set (Constant c tmL _)       = Constant c tmL att
	  set (Binder c tm1 tm2 tmL _) = Binder c tm1 tm2 tmL att
	  set (Unary c tm tmL _)       = Unary c tm tmL att
	  set (Binary' c dc tm tmL _)  = Binary' c dc tm tmL att
	  set (Cond dc tm1 tm2 tmL _)  = Cond dc tm1 tm2 tmL att
	  set (Const i j k tmL _)      = Const i j k tmL att
	  set (Recurse tmL tm tyL _)   = Recurse tmL tm tyL att

	  (subtm,_) = select_trm tm iL




