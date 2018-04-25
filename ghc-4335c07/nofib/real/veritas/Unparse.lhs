> module Unparse(unparse_tm , unparse_th , unparse , unparse' , disp_tk , unparse_sg , unparse_sg' , unparse_nm , unparse_trm , unparse_Trm , unparse_sgn , unparse_Sgn , unparse_Dec , unparse_dec , unparse_Thm ) where

temorary unparser - Flagged_Itrms to strings

> import Kernel

> import Type_defs

> import Core_datatype

> import Attributes


unparse functions ro allow editor to type check

> unparse_trm ( SG isg ) _ = unparse' isg

> unparse_Trm _ _ ( TM itm _ isg ) = unparse' isg itm

> unparse_sgn _ _ isg  = unparse_sg' isg

> unparse_Sgn _ _ ( SG isg )  = unparse_sg' isg


> unparse_dec ( SG isg ) _ idc = unparse_dc isg idc

> unparse_Dec _ _ ( DC idc isg ) = unparse_dc isg idc


> unparse_Thm _ _ ( TH itm isg ) = unparse' isg itm


> unparse_th ( TH tm sg )
>	= "Theorem: " ++ unparse' sg tm ++ "\n"

> unparse_th ( TH_Err mesg )
>	= "Invalid theorem: " ++ mesg ++ "\n"






> unparse_tm ( TM tm1 tm2 sg )
>	= "Term: " ++ unparse' sg tm1 ++ "\nType: " ++ unparse' sg tm2 ++ "\nSig: " ++ unparse_sg' sg ++ "\n"

> unparse_tm ( TM_Err mesg ) = "\nTerm formation error\n" ++ mesg ++ "\n"







> unparse sg ( Opnd ( Itrm tm ))
>	= unparse' sg tm 

> unparse sg ( Opnd ( Idec dc ))
>	= unparse_dc sg dc 

> unparse _ ( Prs_Err mess ) = mess ++ "\n"

other (debugging) unparses

> unparse sg ( Opr ( OpItrm tm ) tpe prc )
>	= unparse' sg tm ++ "{tpe" ++ show_tpe tpe ++ "prc" ++ show prc ++ "}\n"

> unparse _ ( Opr (Spl "") _ _ ) = "unparse operator"

> unparse sg ( Opnd ( TypApp tm )) = "Unparse TypApp: " ++ unparse sg tm 

> unparse sg ( Opr ( Spl "typed" ) _ _ ) = "Unparse 'typed'" 

> unparse sg oth = "unparse unknown"



> unparse' sg ( Sym n1 n2 _ att )
>	= case attval Symbol_Style att  of
>		Named   -> lookUp sg n1 n2 (-1)
>		Indexed -> "\168" ++ show n1 ++ "," ++ show n2 ++ "\169"

> unparse' sg ( Const i j k _ _ )
> 	= lookUp sg i j k

> unparse' sg ( App tm1 tm2 _ _ )
>	= "(" ++ unparse' sg tm1 ++ " " ++ unparse' sg tm2 ++ ")"

> unparse' sg ( Pair tm1 tm2 tm3 _ _ )
>	= "((" ++ unparse' sg tm1 ++ " , " ++ unparse' sg tm2 ++ ") : " ++ unparse' sg tm3 ++ ")"

> unparse' sg ( Binder Subtype dc tm _ _ )
>	= "{" ++ unparse_dc sg dc ++ " | " ++ unparse' sg2 tm ++ "}"
>	  where
>	  sg2 = Extend dc sg []

N.B. Implies needs extended sg 

> unparse' sg ( Binder Imp dc tm _ _ )
>	= " [" ++ unparse_dc sg dc ++ "] " ++ "\182" ++ " " ++ unparse' sg2 tm
>	  where
>	  sg2 = Extend dc sg []

> unparse' sg ( Binder con dc tm _ _ )
>	= "(" ++ unparse_bdr con ++ unparse_dc sg dc ++ ". " ++ unparse' sg2 tm ++ ")" 
>	  where
>	  sg2 = Extend dc sg []

> unparse' sg ( Constant con _ _ )
> 	= unparse_constant con 

> unparse' sg ( Unary con tm _ _ )
>	= " \181" ++ unparse' sg tm


> unparse' sg ( Binary' con tm1 tm2 _ _ )
>	= "(" ++ unparse' sg tm1 ++ " " ++ unparse_bcon con 
>			++ " " ++ unparse' sg tm2 ++ ")"

> unparse' sg ( Cond dc tm1 tm2 _ _ )
>	= " if [" ++ unparse_dc sg dc ++ "] then " ++ unparse' sg2 tm1 ++
>			" else " ++ unparse' sg2 tm2
>	  where
>	  sg2 = Extend dc sg []


> unparse' sg ( Recurse tml srt _ _ )
>	= "\nrecurse\n\t" ++ concat ( map unparse_cls tml ) ++
>			"\nend typed " ++ unparse' sg srt
>	  where
>	  unparse_cls tm = unparse' sg tm ++ "\n\t"


> unparse' sg ( Tagid ( nm , _ , _ ) argL )
>	= "( " ++ nm ++ concat' " " ( map argl argL ) ++ " )"
>	  where
>	  concat' sep ( a:x ) = sep ++ a ++ concat' sep x
>	  concat' _ []        = ""
>	  argl ( Tg_Trm tm ) = unparse_tm tm
>	  argl ( Tg_Thm th ) = unparse_th th
>	  argl ( Tg_Int iL ) = "\168" ++ concat' "," ( map show iL ) ++ "\169"

> unparse' sg ( ITrm_Err mesg ) = mesg





> unparse_bdr Lambda = "\236"

> unparse_bdr Forall = "\177"

> unparse_bdr Exists = "\178"

> unparse_bdr Pi = "\208"

> unparse_bdr Sigma = "\211"

> unparse_bdr Choose = "\229"

> unparse_bdr Delta = "\196"



> unparse_dc sg ( Symbol_dec tm attL ) 
>	= unparse_nm nm ++ " : " ++ unparse' sg tm
>	  where
>	  nm = case attval Name_Style attL of
>		   Symbol_Name nm'   -> nm'
>		   Datatype_Name nmL -> Name " Unexpected Datatype "

> unparse_dc sg ( Axiom_dec tm attL ) 
>	= unparse_nm nm ++ " :a: " ++ unparse' sg tm
>	  where
>	  nm = case attval Name_Style attL of
>		   Symbol_Name nm'   -> nm'
>		   Datatype_Name nmL -> Name " Unexpected Datatype "

> unparse_dc sg ( Decpair dc1 dc2 _ )
>	= " ( " ++ unparse_dc sg dc1 ++ " ; " ++ unparse_dc sg2 dc2 ++ " ) "
>	  where
>	  sg2 = Extend dc1 sg [] 

> unparse_dc sg ( Data dcl ctrs attL )
>	= "datatype " ++ unparse_nm nm ++ " " ++ unparse_fmls sg dcl ++ " = " ++ unparse_ctrs sg2 nmL ctrs
>	  where
>	  sg2 = Extend tp_dcl ( extend_sg sg ( reverse dcl )) []
>	  tp_dcl = Symbol_dec u0 [ sym_nm nm ] 
>	  u0 = Constant ( Univ 0 ) [] []
>	  extend_sg sg' ( dc : dcl ) = extend_sg ( Extend dc sg' []) dcl
>	  extend_sg sg' [] = sg'
>	  nm : nmL = case attval Name_Style attL of
>		  	 Symbol_Name nm'    -> [ Name " Unexpected Symbol " ]
>		   	 Datatype_Name nmL' -> nmL'

> unparse_dc sg ( Def tm srt attL )
>	= unparse_nm nm ++ " \189 " ++ unparse' sg tm
>	  where
>	  nm = case attval Name_Style attL of
>		   Symbol_Name nm'   -> nm'
>		   Datatype_Name nmL -> Name " Unexpected Datatype "

> unparse_dc sg _
>	= " dc not implemented "



> unparse_fmls sg ( dc : dcl )
>	= "(" ++ unparse_dc sg dc ++ ") " ++ unparse_fmls sg dcl 

> unparse_fmls _ [] = ""


> unparse_ctrs sg ( nm : nml ) ( ctr : ctrl )
>	= unparse_nm nm ++ " " ++ unparse_ctr sg ctr ++ " | " ++ unparse_ctrs sg nml ctrl

> unparse_ctrs _ [] [] = ""


> unparse_ctr sg ( arg : argl )
>	= unparse' sg arg ++ " " ++ unparse_ctr sg argl

> unparse_ctr _ [] = ""







> unparse_nm ( Name nm )
>	= nm

> unparse_nm ( Operator' op prc tpe )
>	= "{" ++ op ++ " " ++ show_tpe tpe ++ " " ++ show prc ++ "}"




> show_tpe Pre = "Pre"

> show_tpe BinL = "BinL"

> show_tpe BinR = "BinR"

> show_tpe Post = "Post"





> unparse_bcon :: Binary_conn -> String

> unparse_bcon Or = "\180"

> unparse_bcon And = "\179"

>-- unparse_bcon Imp = "\182"

> unparse_bcon Eq' = "="

> unparse_bcon Issubtype = "\172"



> unparse_constant T = "True"

> unparse_constant F = "False"

> unparse_constant Bool' = "Bool"

> unparse_constant ( Univ i ) = "U" ++ show i





> lookUp ( Extend dc sg _ ) 0 i2 i3
>	= lookup_dc dc [] i2 i3

> lookUp ( Extend dc sg _ ) i1 i2 i3 | i1 > 0
>	= lookUp sg (i1-1) i2 i3

> lookUp _ _ _ _ = "symbol not found "




> lookup_dc ( Symbol_dec tm attL ) _ 0 _
>	= case nm of
>	 	Name inm         -> inm
>		Operator' op _ _ -> op
>	  where
>	  nm = case attval Name_Style attL of
>		   Symbol_Name nm'   -> nm'
>		   Datatype_Name nmL -> Name " Unexpected Datatype "

> lookup_dc ( Axiom_dec tm attL ) _ 0 _
>	= case nm of
>	 	Name inm         -> inm
>		Operator' op _ _ -> op
>	  where
>	  nm = case attval Name_Style attL of
>		   Symbol_Name nm'   -> nm'
>		   Datatype_Name nmL -> Name " Unexpected Datatype "


> lookup_dc ( Decpair dc1 dc2 attL ) dcl i k | i > 0
>	= case attval Dec_Style attL of
>--		Grouped   -> --HERE
>		Grouped   ->  lookup_dc dc1 ( dc2 : dcl ) (i-1) k
>		Ungrouped -> lookup_dc dc1 ( dc2 : dcl ) (i-1) k

> lookup_dc ( Data _ _ attL ) _ 0 k
>	= lookup_ctr nmL k
>	  where
>	  nmL = case attval Name_Style attL of
>		   Symbol_Name nm'    -> [ nm' ] -- error cond (change?)
>		   Datatype_Name nmL' -> nmL'

> lookup_dc ( Def _ _ attL ) _ 0 _ 
>	= case nm of
>	 	Name inm         -> inm
>		Operator' op _ _ -> op
>	  where
>	  nm = case attval Name_Style attL of
>		   Symbol_Name nm'   -> nm'
>		   Datatype_Name nmL -> Name " Unexpected Datatype "

> lookup_dc _ ( dc : dcl ) i k | i > 0
>	= lookup_dc dc dcl (i-1) k

> lookup_dc _ _ _ _ = " symbol not found "







> lookup_ctr ( nm : nml ) k | k > 0
>	= lookup_ctr nml (k-1)

> lookup_ctr ( nm : nml ) 0
>	= case nm of
>	 	Name inm         -> inm
>		Operator' op _ _ -> op

> lookup_ctr [] _
>	= " Constructor not found "







token display function used in debugging messages

> disp_tk :: Token -> String

> disp_tk ( Rvd str ) = str

> disp_tk ( Clr str ) = str

> disp_tk ( Bdr bdr ) = unparse_bdr bdr

> disp_tk ( IfxBdr str ) = str

> disp_tk ( IfxOp str ) = str

> disp_tk ( Scan_Err str ) = "Invalid token: " ++ str -- have to change this?



unparse signature

> unparse_sg :: Flagged_ITrm -> String

> unparse_sg ( Opnd ( Isgn sg )) = unparse_sg' sg

> unparse_sg ( Prs_Err mesg ) = mesg





> unparse_sg' ( Empty _ ) = ""

> unparse_sg' ( Extend dc sg _ )
>	= unparse_dc ( Empty [] ) dc ++ " ;\n" ++ unparse_sg' sg

