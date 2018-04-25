
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

module Sub_Core4 where

import Vtslib

import Core_datatype

import Sub_Core1

import Sub_Core2

import Sub_Core3




get_dec_att (Symbol_dec _ att) = att

get_dec_att (Axiom_dec _ att) = att

get_dec_att (Def _ _ att) = att

get_dec_att (Data _ _ att) = att

get_dec_att (Decpair _ _ att) = att





set_dec_att (Symbol_dec tm _) att 
	= Symbol_dec tm att

set_dec_att (Axiom_dec tm _) att 
	= Axiom_dec tm att

set_dec_att (Def tm1 tm2 _) att 
	= Def tm1 tm2 att

set_dec_att (Data dcL tl _) att 
	= Data dcL tl att

set_dec_att (Decpair dc1 dc2 _) att 
	= Decpair dc1 dc2 att





get_sgn_att (Empty att) = att

get_sgn_att (Extend _ _ att) = att

get_sgn_att (Combine _ _ _ _ att) = att

get_sgn_att (Share _ _ _ _ _ att) = att





set_sgn_att (Empty _) att = Empty att

set_sgn_att (Extend dc sg _) att = Extend dc sg att

set_sgn_att (Combine sg1 sg2 l sm _) att = Combine sg1 sg2 l sm att

set_sgn_att (Share sg i j k sm _) att = Share sg i j k sm att




{-
    (* given a signature and a datasum return the type of of each      *)
    (* clause in each prong of a corrsponding recurse expression of    *)
    (* which the datasum would be the type			       *)
-}

add_type (Sym i j tmL att) ty 
	= Sym i j (ty:tmL) att

add_type (App tm1 tm2 tmL att) ty 
	= App tm1 tm2 (ty:tmL) att

add_type (Pair tm1 tm2 tm3 tmL att) ty 
	= Pair tm1 tm2 tm3 (ty:tmL) att

add_type (Constant c tmL att) ty 
	= Constant c (ty:tmL) att

add_type (Binder c tm1 tm2 tmL att) ty 
	= Binder c tm1 tm2 (ty:tmL) att

add_type (Unary c tm tmL att) ty 
	= Unary c tm (ty:tmL) att

add_type (Binary' c dc tm tmL att) ty 
	= Binary' c dc tm (ty:tmL) att

add_type (Cond dc tm1 tm2 tmL att) ty 
	= Cond dc tm1 tm2 (ty:tmL) att

add_type (Const i j k tmL att) ty 
	= Const i j k (ty:tmL) att

add_type (Recurse tmL tm tyL att) ty 
	= Recurse tmL tm (ty:tyL) att





is_sub_sgn sg1 sg2 
	= sub_sgn 0 sg2 
	  where
	  sub_sgn i sg2 
		= if eq_sgn sg1 sg2 
			then SOME i
			else
			    case sg2 of
			      Empty _ 
				  -> NONE
			      Extend _ sg3 _ 
				  -> sub_sgn (i+1) sg3
			      Combine sg3 sg4 k _ _ 
				  -> case sub_sgn i sg4 of
				         SOME i -> SOME i
					 NONE   -> sub_sgn (i+k) sg3
			      Share sg3 _ _ _ _ _ 
				  -> sub_sgn i sg3


eta_match dc tm i = error "VTS_ERROR" -- ** exn





make_rec fntype clause_ty [] 
	= clause_ty

make_rec (fntype @ ( Binder Pi dc tm _ _)) clause_ty (ty:tyL) 
	= Binder Pi (Symbol_dec ty2 []) ty1 [] [] 
	  where
	  ty1 = make_rec (shift_trm [] 1 fntype) (shift_trm [] 1 clause_ty) tyL
	  ty2 = subst_trm dc tm ty






gen_type i (fntype @(Binder Pi dc tm _ _)) rectypeL const [] 
	= make_rec fntype  (subst_trm dc tm const) rectypeL

gen_type i (fntype @(Binder Pi dc tm _ _)) rectypeL const (ty : tyL) 
	= Binder Pi (Symbol_dec (shift_trm [] i ty) [])	ty1 [] [] 
	  where
	  const1    = App (shift_trm [] 1 const) (Sym 0 0 [] []) [] []
	  fntype1   = shift_trm [] 1 fntype
	  rectypeL1 = map (shift_trm [] 1) rectypeL ++
			   if eq_trm ty (Sym 0 0 [] []) 
				then [Sym i 1 [] []] 
				else []
	  ty1 = gen_type (i+1) fntype1 rectypeL1 const1 tyL

--gen_type _ _ _ _ _ = error "VTS_ERROR" -- ** exn






gen_typeL _ _ _ [] _ _ _ = []

gen_typeL tyL tm fntype (tmL : tmLL) i j k 
	= substL (length tyL - 1) (subst_trm dc ty tm) tyL :
	       gen_typeL tyL tm fntype tmLL i j (k+1)
	  where
	  const = foldr make_app (Const i j k [] []) ( reverse tyL )
	  ty = gen_type 0 fntype [] const tmL
	  dc = Symbol_dec tm []

	  substL i tm [] = tm
	  substL i tm (tm1:tmL1) 
		= substL (i-1) (subst_trm dc tm (shift_trm [] i tm1)) tmL1


	


get_datatype_info sg (App tm1 tm2 _ _) 
	= (i,j,tm2:tmL,dcL,tmLLL) 
	  where
	  (i,j,tmL,dcL,tmLLL) = get_datatype_info sg tm1

get_datatype_info sg (Const i j k _ _) 
	= case shift_dec (get_share_map sg) i dc of
		 Data dcL tmLLL _ -> (i,j,[],dcL,tmLLL)
--		 _ 		  -> error "VTS_ERROR" -- ** exn
	  where
	  dc = extract_dc j (nth_dec i sg)

--get_datatype_info _ _ = error "VTS_ERROR" -- ** exn






no_params :: [ITrm] -> Int

no_params ((Sym _ _ _ _) : tmL) = 2 + no_params tmL

no_params (_ : tmL) = no_params tmL

no_params [] = 0






shift :: Int -> Int -> ITrm -> ITrm

shift i j tm 
	= tm3 
	  where
	  bind tm = Binder Pi (Symbol_dec (Sym 0 0 [] []) []) tm [] []
	  body (Binder _ _ tm _ _) = tm 
--	  body _ = error "System_Error" -- ** exn	
	  tm1 = for i bind tm
	  tm2 = shift_trm [] j tm1
	  tm3 = for i body tm2





clause_types :: ISgn -> ITrm -> ITrm -> ([ITrm], [Int])

clause_types sg dtype fntype 
	= (clauses,params)
	  where
	  (i,j,spec,dcL,tmLL) = get_datatype_info sg dtype
	  fntype1 = shift_trm [] (length dcL + 1) fntype
	  dtype1  = shift_trm [] (length dcL) dtype
	  tmLL1   = map (map (shift (length dcL + 1) j)) tmLL
	  clauses = gen_typeL spec dtype1 fntype1 tmLL1 i j 0 
	  params  = map no_params tmLL





make_ind tm rectype [] = rectype

make_ind tm rectype (ty : tyL) 
	= Binder Imp ty2 ty1 [] [] 
	  where
	  ty1 = make_ind tm rectype tyL
	  ty2 = Symbol_dec ( make_app tm ty ) []





ind_type i tm rectypeL const [] 
	= make_ind tm (make_app tm const) rectypeL

ind_type i tm rectypeL const (ty : tyL) 
	= Binder Forall (Symbol_dec (shift_trm [] i ty) []) ty1 [] []
	  where
	  const1 = make_app (shift_trm [] 1 const) (Sym 0 0 [] [])
	  tm1 = shift_trm [] 1 tm
	  rectypeL1 = map (shift_trm [] 1) rectypeL ++
			if eq_trm ty (Sym 0 0 [] []) 
				then [ty] 
				else []
	  ty1 = ind_type (i+1) tm1 rectypeL1 const1 tyL


	

ind_typeL _ _ _ [] _ _ _ = []

ind_typeL rectype sms tm (tyL:tyLL) i j k 
	= (ty2 : ind_typeL rectype sms tm tyLL i j (k+1)) 
	  where
	  const = foldr make_app (Const i j k [] []) ( reverse sms )
	  ty1 = ind_type 0 tm [] const tyL
	  ty2 = subst_trm (Symbol_dec (Const i j 0 [] []) []) ty1 rectype 
	    




induction_trm sg tm 
	= foldr make_forall tm2 ( reverse (dc : dcL)) 
	  where	
	  make_imp tm1 tm2 = Binder Imp dc_imp tm2 [] []
	  make_forall dc tm = Binder Forall dc tm [] []
	  (i,j,_,dcL,tmLL) = get_datatype_info sg tm
	  tmLL1 = map (map (shift 1 1)) tmLL
	  sms = mk_smsl dcL 0 []
	  dtype = foldr make_app (shift_trm [] (length dcL) tm) ( reverse sms )
	  ty = Binder Pi (Symbol_dec dtype []) (Constant Bool' [] []) [] []
	  dc = Symbol_dec ty []
	  dtype1 = shift_trm [] 1 dtype
	  sms1 = map (shift_trm [] 2) sms
	  tmL = ind_typeL dtype1 sms1 (Sym 1 0 [] []) 
			tmLL1 (2+i+length dcL) j 1
	  dc_imp = Symbol_dec tm1 []
          tm1 = Binder Forall (Symbol_dec dtype1 []) tm1 [] [] 
	        where
	        tm1 = make_app (Sym 1 0 [] []) (Sym 0 0 [] [])
	  tm2 = foldr1 make_imp (tmL ++ [tm1])
	




equiv :: (Eq b) => b -> [b] -> [Int]

equiv i m 
	= equivf 0 m 
	  where
	  equivf j [] = []

	  equivf j (k:m) 
		= (if k==i then [j] else []) ++ equivf (j+1) m





union (i:l) m 
	= (if i `elem` m then [] else [i]) ++ union l m

union [] m = m




update :: b -> [Int] -> [b] -> [b]

update i eq m 
	= updatef 0 m 
	  where
	  updatef j [] = []
	  updatef j (k:m) 
		= (if  j `elem` eq then [i] else [k]) ++ updatef (j+1) m




canonical :: Int -> [b] -> b

canonical i m = m !! i




addequiv :: (Ord a) => (Int, Int) -> [a] -> [a]

addequiv (i,j) m 
	= update (min i' j') eqij m 
	  where
	  i' = canonical i m
	  j' = canonical j m
	  eqi = equiv i' m
	  eqj = equiv j' m
	  eqij = union eqi eqj





addequivL i j k m 
	= foldr addequiv m (list i j k) 
	  where
	  list i' j' 0 = []
	  list i' j' k' = (i',j') : list (i'+1) (j'+1) (k'-1)






split_def (Def tm1 tm2 _) 
	= (Symbol_dec tm2 [],tm1,tm2)

split_def (Decpair dc1 dc2 _) 
	= (Decpair dc3 dc4 [] , Pair tm1 tm6 tm5 [] [] , tm5) 
	  where	
	  (dc3,tm1,tm2) = split_def dc1
	  (dc4,tm3,tm4) = split_def dc2
	  tm5 = shift_trm [] 1 (subst_trm dc1 tm4 tm1)
	  tm6 = subst_trm dc1 tm3 tm1
	  tm7 = Binder Sigma dc3 tm5 [] []

--split_def _ = error "VTS_ERROR" -- ** exn

