{
#include "HsVersions.h"
module ParseUnfolding ( parseUnfolding ) where

IMP_Ubiq(){-uitous-}

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import HsTypes		( mkHsForAllTy )
import HsCore
import Literal
import PrimRep          ( decodePrimRep )
import HsPragmas	( noGenPragmas, noDataPragmas, noClassPragmas, noClassOpPragmas, noInstancePragmas )
import IdInfo		( exactArity, mkStrictnessInfo, mkBottomStrictnessInfo,
			  ArgUsageInfo, FBTypeInfo, ArityInfo, StrictnessInfo
			)
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import Lex		

import RnMonad		( SYN_IE(ImportVersion), SYN_IE(LocalVersion), ParsedIface(..),
			  SYN_IE(RdrNamePragma), SYN_IE(ExportItem), GenAvailInfo
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName(..), isTCOcc, Provenance, SYN_IE(Module) )
import SrcLoc		( mkIfaceSrcLoc )
import Util		( panic{-, pprPanic ToDo:rm-} )
import Pretty           ( Doc )
import Outputable	( PprStyle(..) )
import Maybes           ( MaybeErr(..) )

------------------------------------------------------------------

parseUnfolding ls =
  let
   res =
    case parseUnfold ls of
      v@(Succeeded _) -> v
        -- ill-formed unfolding, crash and burn.
      Failed err      -> panic (show (err PprDebug))
  in
  res
}

%name parseUnfold
%tokentype { IfaceToken }
%monad	    { IfM }{ thenIf }{ returnIf }

%token
	PRAGMAS_PART	    { ITpragmas }
	DATA		    { ITdata }
	TYPE		    { ITtype }
	NEWTYPE		    { ITnewtype }
	DERIVING	    { ITderiving }
	CLASS		    { ITclass }
	WHERE		    { ITwhere }
	INSTANCE	    { ITinstance }
	FORALL		    { ITforall }
	BANG		    { ITbang }
	VBAR		    { ITvbar }
	DCOLON		    { ITdcolon }
	COMMA		    { ITcomma }
	DARROW		    { ITdarrow }
	DOTDOT		    { ITdotdot }
	EQUAL		    { ITequal }
	OCURLY		    { ITocurly }
	OBRACK		    { ITobrack }
	OPAREN		    { IToparen }
	RARROW		    { ITrarrow }
	CCURLY		    { ITccurly }
	CBRACK		    { ITcbrack }
	CPAREN		    { ITcparen }
	SEMI		    { ITsemi }

	VARID		    { ITvarid  	 $$ }
	CONID		    { ITconid  	 $$ }
	VARSYM		    { ITvarsym 	 $$ }
	CONSYM		    { ITconsym 	 $$ }
	QVARID		    { ITqvarid   $$ }
	QCONID		    { ITqconid   $$ }
	QVARSYM		    { ITqvarsym  $$ }
	QCONSYM		    { ITqconsym  $$ }

	ARITY_PART	{ ITarity }
	STRICT_PART	{ ITstrict }
	UNFOLD_PART	{ ITunfold $$ }
	DEMAND		{ ITdemand $$ }
	BOTTOM		{ ITbottom }
	LAM		{ ITlam }
	BIGLAM		{ ITbiglam }
	CASE		{ ITcase }
	PRIM_CASE	{ ITprim_case }
	LET		{ ITlet }
	LETREC		{ ITletrec }
	IN		{ ITin }
	OF		{ ITof }
	COERCE_IN	{ ITcoerce_in }
	COERCE_OUT	{ ITcoerce_out }
	ATSIGN		{ ITatsign }
	CCALL		{ ITccall $$ }
	SCC		{ ITscc $$ }

	CHAR		{ ITchar $$ }
	STRING		{ ITstring $$ }	
	INTEGER		{ ITinteger  $$ }
	DOUBLE		{ ITdouble $$ }

	INTEGER_LIT	{ ITinteger_lit }
	FLOAT_LIT	{ ITfloat_lit }
	RATIONAL_LIT	{ ITrational_lit }
	ADDR_LIT	{ ITaddr_lit }
	LIT_LIT		{ ITlit_lit }
	STRING_LIT	{ ITstring_lit }

	UNKNOWN         { ITunknown $$ }
%%

id_info		:: { [HsIdInfo RdrName] }
id_info		: 	 					{ [] }
		| id_info_item id_info				{ $1 : $2 }

id_info_item	:: { HsIdInfo RdrName }
id_info_item	: ARITY_PART arity_info			{ HsArity $2 }
		| STRICT_PART strict_info		{ HsStrictness $2 }
		| BOTTOM 				{ HsStrictness HsBottom }
		| UNFOLD_PART core_expr			{ HsUnfold $1 $2 }

arity_info	:: { ArityInfo }
arity_info	: INTEGER					{ exactArity (fromInteger $1) }

strict_info	:: { HsStrictnessInfo RdrName }
strict_info	: DEMAND any_var_name OCURLY data_names CCURLY 	{ HsStrictnessInfo $1 (Just ($2,$4)) }
		| DEMAND any_var_name 			 	{ HsStrictnessInfo $1 (Just ($2,[])) }
		| DEMAND 					{ HsStrictnessInfo $1 Nothing }

core_expr	:: { UfExpr RdrName }
core_expr	: any_var_name					{ UfVar $1 }
		| data_name					{ UfVar $1 }
		| core_lit					{ UfLit $1 }
		| OPAREN core_expr CPAREN			{ $2 }
		| data_name OCURLY data_args CCURLY		{ UfCon $1 $3 }

		| core_expr ATSIGN atype			{ UfApp $1 (UfTyArg $3) }
		| core_expr core_arg				{ UfApp $1 $2 }
 		| LAM core_val_bndrs RARROW core_expr		{ foldr UfLam $4 $2 }
		| BIGLAM core_tv_bndrs RARROW core_expr		{ foldr UfLam $4 $2 }

		| CASE core_expr OF 
		  OCURLY alg_alts core_default CCURLY		{ UfCase $2 (UfAlgAlts  $5 $6) }
		| PRIM_CASE core_expr OF 
		  OCURLY prim_alts core_default CCURLY		{ UfCase $2 (UfPrimAlts $5 $6) }


		| LET OCURLY core_val_bndr EQUAL core_expr CCURLY
		  IN core_expr					{ UfLet (UfNonRec $3 $5) $8 }
		| LETREC OCURLY rec_binds CCURLY		
		  IN core_expr					{ UfLet (UfRec $3) $6 }

		| coerce atype core_expr			{ UfCoerce $1 $2 $3 }

		| CCALL ccall_string 
			OBRACK atype atypes CBRACK core_args	{ let
									(is_casm, may_gc) = $1
								  in
								  UfPrim (UfCCallOp $2 is_casm may_gc $5 $4)
									 $7
								}
		| SCC core_expr 	                        {  UfSCC $1 $2	}

rec_binds	:: { [(UfBinder RdrName, UfExpr RdrName)] }
		:						{ [] }
		| core_val_bndr EQUAL core_expr SEMI rec_binds	{ ($1,$3) : $5 }

coerce		:: { UfCoercion RdrName }
coerce		: COERCE_IN  data_name				{ UfIn  $2 }
		| COERCE_OUT data_name				{ UfOut $2 }
		
prim_alts	:: { [(Literal,UfExpr RdrName)] }
		:						{ [] }
		| core_lit RARROW core_expr SEMI prim_alts	{ ($1,$3) : $5 }

alg_alts	:: { [(RdrName, [RdrName], UfExpr RdrName)] }
		: 						{ [] }
		| data_name var_names RARROW 
			core_expr SEMI alg_alts			{ ($1,$2,$4) : $6 }

core_default	:: { UfDefault RdrName }
		: 						{ UfNoDefault }
		| var_name RARROW core_expr SEMI		{ UfBindDefault $1 $3 }

core_arg	:: { UfArg RdrName }
		: any_var_name					{ UfVarArg $1 }
		| data_name					{ UfVarArg $1 }
		| core_lit					{ UfLitArg $1 }

core_args	:: { [UfArg RdrName] }
		:						{ [] }
		| core_arg core_args				{ $1 : $2 }

data_args	:: { [UfArg RdrName] }
		: 						{ [] }
		| ATSIGN atype data_args			{ UfTyArg $2 : $3 }
		| core_arg data_args				{ $1 : $2 }

core_lit	:: { Literal }
core_lit	: INTEGER			{ MachInt $1 True }
		| CHAR				{ MachChar $1 }
		| STRING			{ MachStr $1 }
		| STRING_LIT STRING		{ NoRepStr $2 }
		| DOUBLE			{ MachDouble (toRational $1) }
		| FLOAT_LIT DOUBLE		{ MachFloat (toRational $2) }

		| INTEGER_LIT INTEGER		{ NoRepInteger  $2 (panic "NoRepInteger type") 
							-- The type checker will add the types
						}

		| RATIONAL_LIT INTEGER INTEGER	{ NoRepRational ($2 % $3) 
								(panic "NoRepRational type")
									-- The type checker will add the type
						}

		| ADDR_LIT INTEGER		{ MachAddr $2 }
		| LIT_LIT prim_rep STRING	{ MachLitLit $3 (decodePrimRep $2) }

core_val_bndr 	:: { UfBinder RdrName }
core_val_bndr	: var_name DCOLON atype				{ UfValBinder $1 $3 }

core_val_bndrs 	:: { [UfBinder RdrName] }
core_val_bndrs	: 						{ [] }
		| core_val_bndr core_val_bndrs			{ $1 : $2 }

core_tv_bndr	:: { UfBinder RdrName }
core_tv_bndr	:  tv_name DCOLON akind				{ UfTyBinder $1 $3 }
		|  tv_name					{ UfTyBinder $1 mkBoxedTypeKind }

core_tv_bndrs	:: { [UfBinder RdrName] }
core_tv_bndrs	: 						{ [] }
		| core_tv_bndr core_tv_bndrs			{ $1 : $2 }

ccall_string	:: { FAST_STRING }
		: STRING					{ $1 }
		| VARID						{ $1 }
		| CONID						{ $1 }

prim_rep  :: { Char }
	  : VARID						{ head (_UNPK_ $1) }
	  | CONID						{ head (_UNPK_ $1)

---variable names-----------------------------------------------------
								     }
var_occ		:: { OccName }
var_occ		: VARID			{ VarOcc $1 }
		| VARSYM		{ VarOcc $1 }
		| BANG  		{ VarOcc SLIT("!") {-sigh, double-sigh-} }

data_name	:: { RdrName }
data_name	:  QCONID		{ lexVarQual $1 }
		|  QCONSYM		{ lexVarQual $1 }
		|  CONID		{ Unqual (VarOcc $1) }
		|  CONSYM		{ Unqual (VarOcc $1) }

qvar_name	:: { RdrName }
		:  QVARID		{ lexVarQual $1 }
		|  QVARSYM		{ lexVarQual $1 }

var_name	:: { RdrName }
var_name	:  var_occ		{ Unqual $1 }

any_var_name	:: {RdrName}
any_var_name	:  var_name		{ $1 }
		|  qvar_name		{ $1 }

var_names	:: { [RdrName] }
var_names	:			{ [] }
		| var_name var_names	{ $1 : $2 }

data_names	:: { [RdrName] }
data_names	:			{ [] }
		| data_name data_names	{ $1 : $2

--productions-for-types--------------------------------
					     }
forall		: OBRACK tv_bndrs CBRACK		{ $2 }

context		:: { RdrNameContext }
context		:  					{ [] }
		| OCURLY context_list1 CCURLY		{ $2 }

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class COMMA context_list1 		{ $1 : $3 }

class		:: { (RdrName, RdrNameHsType) }
class		:  tc_name atype			{ ($1, $2) }

type		:: { RdrNameHsType }
type		: FORALL forall context DARROW type	{ mkHsForAllTy $2 $3 $5 }
		|  btype RARROW type			{ MonoFunTy $1 $3 }
		|  btype				{ $1 }

types2		:: { [RdrNameHsType] 			{- Two or more -}  }	
types2		:  type COMMA type			{ [$1,$3] }
		|  type COMMA types2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  btype atype				{ MonoTyApp $1 $2 }

atype		:: { RdrNameHsType }
atype		:  tc_name 			  	{ MonoTyVar $1 }
		|  tv_name			  	{ MonoTyVar $1 }
		|  OPAREN types2 CPAREN	  		{ MonoTupleTy dummyRdrTcName $2 }
		|  OBRACK type CBRACK		  	{ MonoListTy  dummyRdrTcName $2 }
		|  OCURLY tc_name atype CCURLY		{ MonoDictTy $2 $3 }
		|  OPAREN type CPAREN		  	{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2
---------------------------------------------------------------------
					 		}

tv_bndr		:: { HsTyVar RdrName }
tv_bndr		:  tv_name DCOLON akind	{ IfaceTyVar $1 $3 }
		|  tv_name		{ UserTyVar $1 }

tv_bndrs	:: { [HsTyVar RdrName] }
		:  			{ [] }
		| tv_bndr tv_bndrs	{ $1 : $2 }

kind		:: { Kind }
		: akind			{ $1 }
		| akind RARROW kind	{ mkArrowKind $1 $3 }

akind		:: { Kind }
		: VARSYM		{ mkBoxedTypeKind {- ToDo: check that it's "*" -} }
		| OPAREN kind CPAREN	{ $2 }

tv_name		:: { RdrName }
tv_name		:  VARID 		{ Unqual (TvOcc $1) }
		|  VARSYM		{ Unqual (TvOcc $1) {- Allow $t2 as a tyvar -} }

tv_names	:: { [RdrName] }
		:  			{ [] }
		| tv_name tv_names	{ $1 : $2 }

tc_name		:: { RdrName }
tc_name		:  QCONID		{ lexTcQual $1 }
		|  CONID		{ Unqual (TCOcc $1) }
		|  CONSYM		{ Unqual (TCOcc $1) }
		|  OPAREN RARROW CPAREN	{ Unqual (TCOcc SLIT("->")) }
