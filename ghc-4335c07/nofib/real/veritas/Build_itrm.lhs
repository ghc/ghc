itrm building functions used by parser

> module Build_itrm where

> import Data.Char(isDigit)--1.3
> import Core_datatype

> import Type_defs

> import Attributes

> import Unparse  -- for debugging purposes only

> import Kernel

> import Sub_Core1

> import Sub_Core2

> import Sub_Core3

> import Sub_Core4

> import Vtslib

rewrite for continuation based version of read?

> sym' :: String -> String -> Flagged_ITrm

> sym' str1 str2
>	= if isint str1 && isint str2
>		then Opnd ( Itrm  ( Sym no1 no2 [] [sym_ind] ))
>		else Prs_Err ( errstr ++ " is not an integer " )
>	  where
>	  no1 = read str1
>	  no2 = read str2
>	  isint = and . ( map isDigit )
>	  errstr = if isint str1 then str2 else str1




const' needs to be similar to above

> const' :: String -> String -> String -> Flagged_ITrm

> const' str1 str2 str3
>	= Opnd ( Itrm ( Const no1 no2 no3 [] [sym_ind]))
>	  where
>	  no1 = read str1
>	  no2 = read str2
>	  no3 = read str3





> binder' bnd_con ( Opnd ( Idec dc )) ( Opnd ( Itrm tm ))
>	= ( Opnd . Itrm ) ( Binder bnd_con dc tm [] [pre_bdr] )

improve err mess

> binder' _ ( Prs_Err x ) _ = Prs_Err x

> binder' _ _ ( Prs_Err x ) = Prs_Err x





> cond' :: Flagged_ITrm -> Flagged_ITrm -> Flagged_ITrm -> Flagged_ITrm

> cond' ( Prs_Err x ) _ _ = Prs_Err x

> cond' _ ( Prs_Err x ) _ = Prs_Err x

> cond' _ _ ( Prs_Err x ) = Prs_Err x

> cond' ( Opnd ( Idec dc ) ) ( Opnd ( Itrm tm1 ) ) ( Opnd ( Itrm tm2 )) 
>	= Opnd ( Itrm ( Cond dc tm1 tm2 [] [] ))




> let' ( Prs_Err x ) _ _ = Prs_Err x

> let' _ ( Prs_Err x ) _ = Prs_Err x

> let' _ _ ( Prs_Err x ) = Prs_Err x

> let' ( Opnd ( Idec dc)) ( Opnd ( Itrm tm1)) ( Opnd ( Itrm tm2 )) 
>	= opnd ( App ( Binder Lambda dc tm2 [] [] ) tm1 [] [let_stl] )

> let' ( Opr _ _ _ ) _ _ = error "1"
> let' _ ( Opr _ _ _ ) _ = error "2"
> let' _ _ ( Opr _ _ _ ) = error "3"
> let' _ ( Opnd ( PApp _ _ _ )) _ = error "4.1"
> let' _ ( Opnd ( PairApp _ ) ) _ = error "4.2"
> let' _ ( Opnd ( ParIfx _ _) ) _ = error "4.3"
> let' _ ( Opnd ( TypApp _ ) ) _ = error "4.4"
> let' _ ( Opnd ( Itrm _ ) ) _ = error "4.5"
> let' _ ( Opnd ( Idec _ ) ) _ = error "4.6"
> let' _ ( Opnd _ ) _ = error "5"
> let' _ _ ( Opnd _ ) = error "6"







remove dummy application operators

> app' ( Opr ( Spl "" ) _ _ ) x = x

> app' x ( Opr ( Spl "" ) _ _ ) = x

case analysis on special operators
First case is for infix binders X -> etc ( prefix binders are dealt with
by term' in parser )

infix binders x , -> etc

> app' ( Opr ( OpBdr bdr ) _ _ )  ( Opnd ( Idec dc ))  
> 	= Opnd ( PApp bdr dc False )

> app' ( Opr ( OpBdr bdr ) _ _ )  ( Opnd ( Itrm srt ))  
> 	= Opnd ( PApp bdr dc True )
>	  where
>	  dc = Symbol_dec srt [ sym_nm ( Name "_" ) ]

> app' (Opnd ( PApp bdr dc anon )) ( Opnd ( Itrm tm ))
> 	= opnd ( Binder bdr dc shft_tm [] [ifx_bdr] ) 
>	  where
>	  shft_tm | anon     = shift_trm [] 1 tm
>		  | not anon = tm 

infix operators , /\. \/ etc

> app' ( Opr ( OpIfx op ) _ _ ) ( Opnd ( Itrm tm ))
>	= Opnd ( ParIfx op tm )

> app' ( Opnd ( ParIfx op tm1 ) ) ( Opnd ( Itrm tm2 )) 
> 	= opnd ( Binary' op tm1 tm2 [] [] )


PairApp is , applied to first argument only

> app' ( Opr ( Spl "," ) _ _ )  ( Opnd ( Itrm tm1 )) 
>	= Opnd ( PairApp tm1 )

form pair when second argument found
type set to U-1 to indicate it has not yet been defined. Will be defined
explicitely by "typed" (see following clause) or will be deduced by
`build_trm'' when proper term is constructed.

> app' ( Opnd ( PairApp tm1 )) ( Opnd ( Itrm tm2 )) 
>	= Opnd ( Itrm pairtm ) 
> 	  where
>	  pairtm = Pair tm1 tm2 pdt [] [pr_untyped]
>	  pdt = Constant ( Univ (-1) ) [] [] 

TypApp is 'typed' applied to its first argument
(does it need flagged_itrm as an argument in general?)

> app' ( Opr ( Spl "typed" ) _ _ ) opnd1
>	= Opnd ( TypApp opnd1 )

add other TypApp cases for other types (recurse )
(Note that pr_typed will replaced pr_untyped in attribute list)

> app' ( Opnd ( TypApp ( Opnd ( Itrm ( Pair tm1 tm2 _ a b )))))
>							( Opnd ( Itrm srt ))
>	= Opnd ( Itrm ( Pair tm1 tm2 srt a [pr_typed] ))

prefix not

> app' ( Opr ( Spl "Not" ) _ _ ) ( Opnd ( Itrm tm ))
>	= opnd ( Unary Not tm [] [] )

":" operator

> app' ( Opr ( Spl ":" ) _ _ ) ( Opnd ( Itrm tm ))
>	= Opnd ( ParColon tm )

> app' ( Opnd ( ParColon tm1 )) ( Opnd ( Itrm tm2 ))
>	= opnd ( add_type tm1 tm2 )

four permutations for operators and operands

> app' ( Opnd ( Itrm tm1 )) ( Opnd ( Itrm tm2 )) 
>	= opnd ( App tm1 tm2 [] [] ) 

> app' ( Opr ( OpItrm op ) stl _ ) ( Opnd ( Itrm tm )) 
>	= opnd ( App op tm [] [ op_stl stl ] )

> app' ( Opnd ( Itrm tm )) ( Opr ( OpItrm op ) _ _ ) -- happen?
>	= opnd ( App tm op [] [] ) 

?what happens to style for second operator here?

> app' ( Opr ( OpItrm op1 ) stl _ ) ( Opr ( OpItrm op2 ) _ _ ) 
>	= opnd ( App op1 op2 [] [ op_stl stl ] )

> app' ( Prs_Err mess ) _ = Prs_Err mess

> app' _ ( Prs_Err mess ) = Prs_Err mess

> app' x y = error ( "app' y: " ++ unparse ( Empty []) y ++ "\nx: " ++ unparse (Empty [] ) x )






> opnd :: ITrm -> Flagged_ITrm

> opnd = Opnd . Itrm


include following as optimisation?

> fetch_arg :: Flagged_ITrm -> ITrm

> fetch_arg ( Opnd ( Itrm tm )) = tm

> fetch_arg ( Opr ( OpItrm op ) _ _ ) = op



> decpair' _ ( Prs_Err x ) _ = Prs_Err x

> decpair' _ _ ( Prs_Err x ) = Prs_Err x

> decpair' att ( Opnd ( Idec dc1 )) ( Opnd ( Idec dc2 ))
>	= ( Opnd . Idec ) ( Decpair dc1 dc2 att )



> symbol_dec' ( Opnd ( Itrm tm )) nm attL
>	= ( Opnd . Idec ) ( Symbol_dec tm ( sym_nm nm : attL ) )

> symbol_dec' ( Prs_Err mess ) nm _ = Prs_Err mess





> data' ( _ , [ Prs_Err mesg ] ) _
>	= Prs_Err mesg

> data' ( type_nm , dcl ) ctr_defs
>	= case ctrs of
>		Ok ( ctr_nml , ctr_srtl ) 
>			 -> ( Opnd . Idec ) ( Data ( clear dcl ) ctr_srtl att )
>			    where
>         		    att = [ dat_nm ( type_nm : ctr_nml ) ]	
>		Bad mesg -> Prs_Err mesg
>	  where
>	  ctrs = ctr' [] [] ctr_defs
>	  clear ( Opnd ( Idec dc ) : dcl ) = dc : clear dcl
>	  clear [] = []




> ctr' nml argll (( nm , argl ) : ctrl )
>	= case checked_args of
>		Ok iargl -> ctr' ( nm : nml ) ( iargl : argll ) ctrl 
>	 	Bad mesg -> Bad mesg
>	  where
>	  checked_args = check_arg [] argl

> ctr' nml argll []
>	= Ok ( reverse nml , reverse argll )



> check_arg tml ( Opnd ( Itrm tm ) : argl )
>	= check_arg ( tm : tml ) argl

> check_arg tml []
>	= Ok ( reverse tml )

> check_arg _ ( Prs_Err mesg : argl )
>	= Bad mesg



> extend' ( Opnd ( Idec dc )) ( Opnd ( Isgn sg ))
>	= Opnd ( Isgn ( Extend dc sg [] ))	

> extend' _ ( Prs_Err mesg )
>	= Prs_Err mesg

> extend' ( Prs_Err mesg ) _
>	= Prs_Err mesg




> def' ( _ , [ Prs_Err mesg ] ) _ _
>	= Prs_Err mesg

> def' _ ( Prs_Err mesg ) _
>	= Prs_Err mesg

> def' ( nm , fmls ) ( Opnd ( Itrm rhs )) att
>	= ( Opnd . Idec ) ( Def tm srt [ sym_nm nm , att ] )
>	  where
>	  tm = make_tm fmls rhs
>	  srt = Constant ( Univ 0 ) [] [] -- temporary - need sort fn


> make_tm ( Opnd ( Idec dc ) : dcl ) rhs
>	= Binder Lambda dc ( make_tm dcl rhs ) [] []

> make_tm [] rhs
>	= rhs


> add_sg_att ( Prs_Err mesg ) _ = Prs_Err mesg

> add_sg_att ( Opnd ( Isgn ( Empty attl ))) att 
>	= Opnd ( Isgn ( Empty ( att : attl )))

> add_sg_att ( Opnd ( Isgn ( Extend  dc sg attl ))) att 
>	= Opnd ( Isgn ( Extend dc sg ( att : attl )))






> recurse' _ ( Prs_Err mesg ) = Prs_Err mesg

> recurse' tml ( Opnd ( Itrm srt ))
>	= case clr [] tml of
>		Ok itml  -> Opnd ( Itrm ( Recurse itml srt [] [rcrs_stl] ))
>	        Bad mesg -> Prs_Err mesg 
>	  where
>	  clr itml (( Opnd ( Itrm tm )) : tml ) = clr ( tm : itml ) tml
>	  clr itml [] = Ok itml 
>	  clr _ ( Prs_Err mesg : _ ) = Bad mesg





> tag' tg tgL
>	= case check tgL of
>		Ok _     -> Opnd ( Itrm ( Tagid tg tgL ))
>	 	Bad mesg -> Prs_Err mesg
>	  where
>	  check ( Tg_Trm ( TM _ _ _ ) : oth )    = check oth
>	  check ( Tg_Trm ( TM_Err mesg ) :oth )  = Bad mesg
>	  check ( Tg_Thm ( TH _ _ ) : oth )      = check oth
>	  check ( Tg_Thm ( TH_Err mesg ) : oth ) = Bad mesg
>	  check ( Tg_Int iL : oth )              = check oth
>	  check []                               = Ok [] -- list is given in tgL


