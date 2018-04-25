> module Build_Tm where

> import Unparse

> import Kernel

> import Sub_Core1

> import Sub_Core2

> import Sub_Core3

> import Sub_Core4

> import Type_defs

> import Core_datatype

> import Vtslib

> build_trm :: Sgn -> Flagged_ITrm -> Trm

> build_trm sg ( Opnd ( Itrm itm ))
>	= build_trm' sg itm

> build_trm ( SG sg ) oth
>	= error ("Error: " ++ unparse sg oth ++ "|\n")



> build_trm' sg ( Sym i j tyL attL )
> 	= add_l ( symbol sg i j ) tyL attL

> build_trm' sg ( App itm1 itm2 tyL attL )
>	= add_l ( appl tm1 tm2 ) tyL attL
>	  where
>	  tm1 = build_trm' sg itm1 
>	  tm2 = build_trm' sg itm2

have to deduce term if not explicitely given (type is U-1)

> build_trm' sg ( Pair itm1 itm2 itm3 tyL attL )
>	= add_l ( pair tm1 tm2 tm3 ) tyL attL
>	  where
>	  tm1 = build_trm' sg itm1
>	  tm2 = build_trm' sg itm2
>	  tm3 = case itm3 of
>			Constant ( Univ (-1)) _ _ 
>				-> sigma btm2 
>				   where
>				   btm2 = shift_Trm 1 sg2 ( typ_of_Trm tm2 )
>				   sg2  = extend bdc1 sg
>				   bdc1 = symbol_dec ( typ_of_Trm tm1 )
>			_	-> build_trm' sg itm3

> build_trm' sg ( Binder bdr idc itm tyL attL )
>	= add_l ( bdr_fn tm ) tyL attL
>	  where
>	  tm = build_trm' ( extend dc sg ) itm
>	  dc = gen_dc sg idc
>	  bdr_fn = case bdr of
>	  		Lambda  -> lambda
>	  	 	Forall  -> universal
>			Imp     -> implication
>	  		Exists  -> existential
>	  		Pi      -> pi'
>	  		Sigma   -> sigma
>	  		Subtype -> subtype
>			Delta   -> \ _ -> TM_Err "Invalid occurance of \196"
>	  		Choose  -> \ _ -> TM_Err "Invalid occurance of \229"	

> build_trm' sg ( Constant cst tyL attL )
>	= add_l ( cst_fn sg ) tyL attL
>	  where
>	  cst_fn = case cst of
>			Bool'  -> bool_sm
>			T      -> true_sm
>			F      -> false_sm
>			Univ i -> \sg -> universe sg i

> build_trm' sg ( Binary' b_op itm1 itm2 tyL attL )
>	= add_l ( bin_fn tm1 tm2 ) tyL attL
>	  where
>	  tm1 = build_trm' sg itm1
>	  tm2 = build_trm' sg itm2
>	  bin_fn = case b_op of
>			And       -> conjunction
>			Or        -> disjunction
>			Eq'       -> equal
>			Issubtype -> issubtype

> build_trm' sg ( Unary Not itm tyL attL )
>	= add_l ( negation tm ) tyL attL 
>	  where
>	  tm = build_trm' sg itm

> build_trm' sg ( Cond idc itm1 itm2 tyL attL )
>	= add_l ( conditional tm1 tm2 ) tyL attL
>	  where
>	  tm1 = build_trm' sg2 itm1
>	  tm2 = build_trm' sg3 itm2
>	  sg2 = extend dc1 sg
>	  sg3 = extend dc2 sg 
>	  dc2 = symbol_dec ( negation ( typ_of_Dec dc1 ))
>	  dc1 = gen_dc sg idc

> build_trm' sg ( Const i j k tyL attL )
> 	= add_l ( constructor sg i j k ) tyL attL

> build_trm' sg ( Recurse itmL itm tyL attL )
>	= add_l ( recurse tmL tm ) tyL attL
>	  where
>	  tmL = map ( build_trm' sg ) itmL
>	  tm  = build_trm' sg itm

> build_trm' sg ( Tagid ( str , _ , cnv_fnL ) argL )
>       = case fetch_fn cnv_fnL of
>               Ok cnv_fn -> cnv_fn argL
>               Bad mesg  -> TM_Err mesg
>         where
>--	  fetch_fn :: [Cnv_Fn] -> MayBe ( [Tag_Arg] -> Trm )
>         fetch_fn ( Trm_Fn fn : _ ) = Ok fn
>         fetch_fn ( _ : oth )       = fetch_fn oth
>         fetch_fn []                = Bad ( "cannot convert tag " ++ str ++ " to term" )

> build_trm' ( SG isg ) oth
>	= error ("Unimplemented construction: " ++ unparse' isg oth ++ "|\n")




> build_dc = gen_dc

Check type of itm and build axiom_dec if bool

> gen_dc sg ( Symbol_dec itm attL )
>	= case typ_of_trm isg itm of
>		Constant Bool' _ _ 
>		       -> set_Dec_att ( axiom_dec ( build_trm' sg itm )) attL
>		otherwise 
>		       -> set_Dec_att ( symbol_dec ( build_trm' sg itm )) attL
>	  where
>	  isg = internal_Sgn sg

> gen_dc sg ( Axiom_dec itm attL )
>	= set_Dec_att ( axiom_dec ( build_trm' sg itm )) attL

> gen_dc sg ( Decpair idc1 idc2 attL )
>	= set_Dec_att dc_pair attL
>	  where
>	  dc_pair = decpair dc2
>	  dc2     = gen_dc sg2 idc2 
>	  sg2     = extend dc1 sg 
>	  dc1     = gen_dc sg idc1

> gen_dc _ _ = error "Only symbol_dec so far implemented"




pass on nested errors unchanged

> add_l ( TM_Err mesg ) _ _ = TM_Err mesg

check validity of sort at head of sort list if present

> add_l tm tyL@( srt : _ ) attL
>	= if eq_trm srt_tm srt 
>		then set_Trm_att tm [] attL	
>	        else TM_Err "Invalid type specification"
>	  where
>	  ( _ , srt_tm , _ ) = internal_Trm tm

> add_l tm [] attL 
>	= set_Trm_att tm [] attL




this function ignores attributes

> build_sg :: ISgn -> Sgn

> build_sg ( Empty _ ) = empty

> build_sg ( Extend idc isg _ ) 
>	= extend ( build_dc sg idc ) sg
>	  where
>	  sg = build_sg isg

> build_sg _ = error "unimplemented signature constructor"

