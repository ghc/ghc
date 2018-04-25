
> module Attributes where

> import Core_datatype

attributes

> sym_nm nm = ( Name_Style , Symbol_Name nm )

> dat_nm nmL = ( Name_Style , Datatype_Name nmL )


> sym_nmd = ( Symbol_Style , Named )

> sym_ind = ( Symbol_Style , Indexed )


> pr_typed = ( Pair_Style , Typed )

> pr_untyped = ( Pair_Style , Untyped )


> let_stl = ( Let_Style , Let )


> op_stl Pre = ( Opr_Style , Prefixed )

> op_stl BinL = ( Opr_Style , Linfixed )

> op_stl BinR = ( Opr_Style , Rinfixed )

> op_stl Post = ( Opr_Style , Postfixed )


> ifx_bdr = ( Binder_Style , Infix_Binder )

> pre_bdr = ( Binder_Style , Prefix_Binder )	


> rcrs_stl = ( Recurse_Style , Recursive )


> hyp_ndpnd = ( Hyp_Style , NonDependent )



> grp = ( Dec_Style , Grouped )

> un_grp = ( Dec_Style , Ungrouped )

> dec_tpe = ( Dec_Style , Typed )

> dec_untpe = ( Dec_Style , Untyped )


> def_par = ( Def_Style , Parameter )

> def_unpar = ( Def_Style , NonParameter )



retrieve value associated with attribute tag

> attval tag1 (( tag2 , val ) : attL )
>	| tag1 == tag2 = val
>	| otherwise    = attval tag1 attL

> attval _ [] = Symbol_Name ( Name "_" ) -- hack for temporary unparser -- should be Undefined
