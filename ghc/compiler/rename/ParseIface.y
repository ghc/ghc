{
#include "HsVersions.h"

module ParseIface ( parseIface ) where

IMP_Ubiq(){-uitous-}

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..) )
import HsTypes		( mkHsForAllTy )
import HsCore
import Literal
import HsPragmas	( noGenPragmas, noDataPragmas, noClassPragmas, noClassOpPragmas, noInstancePragmas )
import IdInfo		( exactArity, mkStrictnessInfo, mkBottomStrictnessInfo,
			  ArgUsageInfo, FBTypeInfo
			)
import Kind		( Kind, mkArrowKind, mkTypeKind )
import Lex		

import RnMonad		( SYN_IE(ImportVersion), SYN_IE(LocalVersion), ParsedIface(..),
			  SYN_IE(RdrNamePragma), SYN_IE(ExportItem)
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName(..), Provenance )
import SrcLoc		( mkIfaceSrcLoc )
import Util		( panic{-, pprPanic ToDo:rm-} )


-----------------------------------------------------------------

parseIface = parseIToks . lexIface

-----------------------------------------------------------------
}

%name	    parseIToks
%tokentype  { IfaceToken }
%monad	    { IfM }{ thenIf }{ returnIf }

%token
	INTERFACE	    { ITinterface }
	USAGES_PART	    { ITusages }
	VERSIONS_PART	    { ITversions }
	EXPORTS_PART	    { ITexports }
	INSTANCE_MODULES_PART { ITinstance_modules }
	INSTANCES_PART	    { ITinstances }
	FIXITIES_PART	    { ITfixities }
	DECLARATIONS_PART   { ITdeclarations }
	PRAGMAS_PART	    { ITpragmas }
	BANG		    { ITbang }
	CBRACK		    { ITcbrack }
	CCURLY		    { ITccurly }
	CLASS		    { ITclass }
	COMMA		    { ITcomma }
	CPAREN		    { ITcparen }
	DARROW		    { ITdarrow }
	DATA		    { ITdata }
	DCOLON		    { ITdcolon }
	DERIVING	    { ITderiving }
	DOTDOT		    { ITdotdot }
	EQUAL		    { ITequal }
	FORALL		    { ITforall }
	INFIX		    { ITinfix }
	INFIXL		    { ITinfixl }
	INFIXR		    { ITinfixr }
	INSTANCE	    { ITinstance }
	NEWTYPE		    { ITnewtype }
	OBRACK		    { ITobrack }
	OCURLY		    { ITocurly }
	OPAREN		    { IToparen }
	RARROW		    { ITrarrow }
	SEMI		    { ITsemi }
	TYPE		    { ITtype }
	VBAR		    { ITvbar }
	WHERE		    { ITwhere }
	INTEGER		    { ITinteger  $$ }
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
	UNFOLD_PART	{ ITunfold }
	DEMAND		{ ITdemand $$ }
	BOTTOM		{ ITbottom }
	LAM		{ ITlam }
	BIGLAM		{ ITbiglam }
	CASE		{ ITcase }
	PRIM_CASE	{ ITprim_case }
	OF		{ ITof }
	LET		{ ITlet }
	LETREC		{ ITletrec }
	IN		{ ITin }
	ATSIGN		{ ITatsign }
	COERCE_IN	{ ITcoerce_in }
	COERCE_OUT	{ ITcoerce_out }
	CHAR		{ ITchar $$ }
	STRING		{ ITstring $$ }	
	DOUBLE		{ ITdouble $$ }
	INTEGER_LIT	{ ITinteger_lit }
	STRING_LIT	{ ITstring_lit }
	FLOAT_LIT	{ ITfloat_lit }
	RATIONAL_LIT	{ ITrational_lit }
	ADDR_LIT	{ ITaddr_lit }
	LIT_LIT		{ ITlit_lit }
	CCALL		{ ITccall $$ }
%%

iface		:: { ParsedIface }
iface		: INTERFACE CONID INTEGER
		  inst_modules_part 
		  usages_part
		  exports_part fixities_part
		  instances_part
		  decls_part
		  { ParsedIface 
			$2 			-- Module name
			(fromInteger $3) 	-- Module version
			$5  		        -- Usages
			$6  		        -- Exports
			$4  		        -- Instance modules
			$7  		        -- Fixities
			$9  		        -- Decls
			$8 			-- Local instances
		    }


usages_part	    :: { [ImportVersion OccName] }
usages_part	    :  USAGES_PART module_stuff_pairs		{ $2 }
		    |						{ [] }

module_stuff_pairs  :: { [ImportVersion OccName] }
module_stuff_pairs  :  						{ [] }
		    |  module_stuff_pair module_stuff_pairs	{ $1 : $2 }

module_stuff_pair   ::  { ImportVersion OccName }
module_stuff_pair   :  mod_name INTEGER DCOLON name_version_pairs SEMI
			{ ($1, fromInteger $2, $4) }

versions_part	    :: { [LocalVersion OccName] }
versions_part	    :  VERSIONS_PART name_version_pairs		{ $2 }
		    |						{ [] }

name_version_pairs  ::	{ [LocalVersion OccName] }
name_version_pairs  :  						{ [] }
		    |  name_version_pair name_version_pairs	{ $1 : $2 }

name_version_pair   ::	{ LocalVersion OccName }
name_version_pair   :  entity_occ INTEGER			{ ($1, fromInteger $2)
--------------------------------------------------------------------------
								}

exports_part	:: { [ExportItem] }
exports_part	:  EXPORTS_PART export_items			{ $2 }
		|			     			{ [] }

export_items	:: { [ExportItem] }
export_items	:  			    			{ [] }
		|  mod_name entities SEMI export_items 		{ ($1,$2) : $4 }

entities	:: { [(OccName, [OccName])] }
entities	: 						{ [] }
		|  entity entities				{ $1 : $2 }

entity		:: { (OccName, [OccName]) }
entity		:  entity_occ maybe_inside			{ ($1, $2) }

maybe_inside	:: { [OccName] }
maybe_inside	:	     					{ [] }
		|  OPAREN val_occs CPAREN			{ $2
--------------------------------------------------------------------------
								}

inst_modules_part :: { [Module] }
inst_modules_part :				    		{ [] }
		  |  INSTANCE_MODULES_PART mod_list 		{ $2 }

mod_list	:: { [Module] }
mod_list	:  						{ [] }
		|  mod_name mod_list				{ $1 : $2
--------------------------------------------------------------------------
								  }

fixities_part	:: { [(OccName,Fixity)] }
fixities_part	:						{ [] }
		|  FIXITIES_PART fixes				{ $2 }

fixes		:: { [(OccName,Fixity)] }
fixes		:  				  		{ []  }
		|  fix fixes					{ $1 : $2 }

fix		:: { (OccName, Fixity) }
fix		:  INFIXL INTEGER val_occ SEMI { ($3, Fixity (fromInteger $2) InfixL) }
		|  INFIXR INTEGER val_occ SEMI { ($3, Fixity (fromInteger $2) InfixR) }
		|  INFIX  INTEGER val_occ SEMI { ($3, Fixity (fromInteger $2) InfixN)
--------------------------------------------------------------------------
										      }

decls_part	:: { [(Version, RdrNameHsDecl)] }
decls_part	: 		    		 	{ [] }
		|	DECLARATIONS_PART topdecls	{ $2 }

topdecls	:: { [(Version, RdrNameHsDecl)] }
topdecls	:  		    			{ [] }
		|  version topdecl topdecls		{ ($1,$2) : $3 }

version		:: { Version }
version		:  INTEGER				{ fromInteger $1 }

topdecl		:: { RdrNameHsDecl }
topdecl		:  TYPE  tc_name tv_bndrs EQUAL type SEMI
			{ TyD (TySynonym $2 $3 $5 mkIfaceSrcLoc) }
		|  DATA decl_context tc_name tv_bndrs EQUAL constrs deriving SEMI
			{ TyD (TyData $2 $3 $4 $6 $7 noDataPragmas mkIfaceSrcLoc) }
		|  NEWTYPE decl_context tc_name tv_bndrs EQUAL constr1 deriving SEMI
			{ TyD (TyNew $2 $3 $4 $6 $7 noDataPragmas mkIfaceSrcLoc) }
		|  CLASS decl_context tc_name tv_bndr csigs SEMI
			{ ClD (ClassDecl $2 $3 $4 $5 EmptyMonoBinds noClassPragmas mkIfaceSrcLoc) }
		|  var_name DCOLON type id_info SEMI
			{ SigD (IfaceSig $1 $3 $4 mkIfaceSrcLoc) }

decl_context	:: { RdrNameContext }
decl_context	:  					{ [] }
		| OCURLY context_list1 CCURLY DARROW	{ $2 }

csigs		:: { [RdrNameSig] }
csigs		:  				{ [] }
		| WHERE OCURLY csigs1 CCURLY	{ $3 }

csigs1		:: { [RdrNameSig] }
csigs1		: csig				{ [$1] }
		| csig SEMI csigs1		{ $1 : $3 }

csig		:: { RdrNameSig }
csig		:  var_name DCOLON type 	{ ClassOpSig $1 $3 noClassOpPragmas mkIfaceSrcLoc
----------------------------------------------------------------
			 			 }

constrs		:: { [RdrNameConDecl] }
constrs		:  constr		{ [$1] }
		|  constr VBAR constrs	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  data_name batypes			{ ConDecl $1 $2 mkIfaceSrcLoc }
		|  data_name OCURLY fields1 CCURLY	{ RecConDecl $1 $3 mkIfaceSrcLoc }

constr1		:: { RdrNameConDecl 	{- For a newtype -} }
constr1		:  data_name atype			{ NewConDecl $1 $2 mkIfaceSrcLoc }

deriving	:: { Maybe [RdrName] }
		: 					{ Nothing }
		| DERIVING OPAREN qtc_names1 CPAREN	{ Just $3 }

batypes		:: { [RdrNameBangType] }
batypes		:  					{ [] }
		|  batype batypes			{ $1 : $2 }

batype		:: { RdrNameBangType }
batype		:  atype				{ Unbanged $1 }
		|  BANG atype				{ Banged   $2 }

fields1		:: { [([RdrName], RdrNameBangType)] }
fields1		: field					{ [$1] }
		| field COMMA fields1			{ $1 : $3 }

field		:: { ([RdrName], RdrNameBangType) }
field		:  var_name DCOLON type	   	{ ([$1], Unbanged $3) }
		|  var_name DCOLON BANG type    	{ ([$1], Banged   $4)
--------------------------------------------------------------------------
						    	}

forall		:: { [HsTyVar RdrName] }
forall		: OBRACK tv_bndrs CBRACK		{ $2 }

context		:: { RdrNameContext }
context		:  					{ [] }
		| OCURLY context_list1 CCURLY		{ $2 }

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class COMMA context_list1 		{ $1 : $3 }

class		:: { (RdrName, RdrNameHsType) }
class		:  qtc_name atype			{ ($1, $2) }

type		:: { RdrNameHsType }
type		: FORALL forall context DARROW tautype	{ mkHsForAllTy $2 $3 $5 }
		| tautype				{ $1 }

tautype		:: { RdrNameHsType }
tautype		:  btype				{ $1 }
		|  btype RARROW tautype			{ MonoFunTy $1 $3 }

types2		:: { [RdrNameHsType] 			{- Two or more -}  }	
types2		:  type COMMA type			{ [$1,$3] }
		|  type COMMA types2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  qtc_name atype atypes		{ MonoTyApp $1 ($2:$3) }
		|  tv_name  atype atypes		{ MonoTyApp $1 ($2:$3) }

atype		:: { RdrNameHsType }
atype		:  qtc_name 			  	{ MonoTyApp $1 [] }
		|  tv_name			  	{ MonoTyVar $1 }
		|  OPAREN types2 CPAREN	  		{ MonoTupleTy dummyRdrTcName $2 }
		|  OBRACK type CBRACK		  	{ MonoListTy  dummyRdrTcName $2 }
		|  OCURLY qtc_name atype CCURLY		{ MonoDictTy $2 $3 }
		|  OPAREN type CPAREN		  	{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2
---------------------------------------------------------------------
					 		}

mod_name	:: { Module }
		:  CONID		{ $1 }

var_occ		:: { OccName }
var_occ		: VARID			{ VarOcc $1 }
		| VARSYM		{ VarOcc $1 }
		| BANG  		{ VarOcc SLIT("!") {-sigh, double-sigh-} }

entity_occ	:: { OccName }
entity_occ	:  var_occ		{ $1 }
		|  CONID		{ TCOcc $1 }
		|  CONSYM		{ TCOcc $1 }

val_occ		:: { OccName }
val_occ		:  var_occ 		{ $1 }
		|  CONID		{ VarOcc $1 }
		|  CONSYM		{ VarOcc $1 }

val_occs	:: { [OccName] }
		:  	    		{ [] }
		|  val_occ val_occs	{ $1 : $2 }


qvar_name	:: { RdrName }
		:  QVARID		{ varQual $1 }
		|  QVARSYM		{ varQual $1 }

var_name	:: { RdrName }
var_name	:  var_occ		{ Unqual $1 }

any_var_name	:: {RdrName}
any_var_name	:  var_name		{ $1 }
		|  qvar_name		{ $1 }

qdata_name	:: { RdrName }
qdata_name	:  QCONID		{ varQual $1 }
		|  QCONSYM		{ varQual $1 }

data_name	:: { RdrName }
data_name	:  CONID		{ Unqual (VarOcc $1) }
		|  CONSYM		{ Unqual (VarOcc $1) }


qtc_name	:: { RdrName }
qtc_name	:  QCONID		{ tcQual $1 }

qtc_names1	:: { [RdrName] }
		: qtc_name			{ [$1] }
		| qtc_name COMMA qtc_names1	{ $1 : $3 }

tc_name		:: { RdrName }
tc_name		: CONID			{ Unqual (TCOcc $1) }		


tv_name		:: { RdrName }
tv_name		:  VARID 		{ Unqual (TvOcc $1) }

tv_names	:: { [RdrName] }
		:  			{ [] }
		| tv_name tv_names	{ $1 : $2 }

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
		: VARSYM		{ mkTypeKind {- ToDo: check that it's "*" -} }
		| OPAREN kind CPAREN	{ $2
--------------------------------------------------------------------------
					}


instances_part	:: { [RdrNameInstDecl] }
instances_part	:  INSTANCES_PART instdecls { $2 }
		|			    { [] }

instdecls	:: { [RdrNameInstDecl] }
instdecls	:  			    { [] }
		|  instd instdecls	    { $1 : $2 }

instd		:: { RdrNameInstDecl }
instd		:  INSTANCE type EQUAL var_name SEMI 
			{ InstDecl $2
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   (Just $4)		{- Dfun id -}
				   mkIfaceSrcLoc 
--------------------------------------------------------------------------
		    }

id_info		:: { [HsIdInfo RdrName] }
id_info		: 	 					{ [] }
		| id_info_item id_info				{ $1 : $2 }

id_info_item	:: { HsIdInfo RdrName }
id_info_item	: ARITY_PART arity_info			{ HsArity $2 }
		| STRICT_PART strict_info		{ HsStrictness $2 }
		| BOTTOM 				{ HsStrictness mkBottomStrictnessInfo }
		| UNFOLD_PART core_expr			{ HsUnfold $2 }

arity_info	:: { ArityInfo }
arity_info	: INTEGER					{ exactArity (fromInteger $1) }

strict_info	:: { StrictnessInfo RdrName }
strict_info	: DEMAND any_var_name				{ mkStrictnessInfo $1 (Just $2) }
		| DEMAND 					{ mkStrictnessInfo $1 Nothing }

core_expr	:: { UfExpr RdrName }
core_expr	: any_var_name					{ UfVar $1 }
		| qdata_name					{ UfVar $1 }
		| core_lit					{ UfLit $1 }
		| OPAREN core_expr CPAREN			{ $2 }

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

rec_binds	:: { [(UfBinder RdrName, UfExpr RdrName)] }
		:						{ [] }
		| core_val_bndr EQUAL core_expr SEMI rec_binds	{ ($1,$3) : $5 }

coerce		:: { UfCoercion RdrName }
coerce		: COERCE_IN  qdata_name				{ UfIn  $2 }
		| COERCE_OUT qdata_name				{ UfOut $2 }
		
prim_alts	:: { [(Literal,UfExpr RdrName)] }
		:						{ [] }
		| core_lit RARROW core_expr SEMI prim_alts	{ ($1,$3) : $5 }

alg_alts	:: { [(RdrName, [UfBinder RdrName], UfExpr RdrName)] }
		: 						{ [] }
		| qdata_name core_val_bndrs RARROW 
			core_expr SEMI alg_alts			{ ($1,$2,$4) : $6 }

core_default	:: { UfDefault RdrName }
		: 						{ UfNoDefault }
		| core_val_bndr RARROW core_expr SEMI		{ UfBindDefault $1 $3 }

core_arg	:: { UfArg RdrName }
		: var_name					{ UfVarArg $1 }
		| qvar_name					{ UfVarArg $1 }
		| qdata_name					{ UfVarArg $1 }
		| core_lit					{ UfLitArg $1 }

core_args	:: { [UfArg RdrName] }
		:						{ [] }
		| core_arg core_args				{ $1 : $2 }

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
		| LIT_LIT STRING		{ MachLitLit $2 (panic "ParseIface.y: ToDo: need PrimRep on LitLits in ifaces") }

core_val_bndr 	:: { UfBinder RdrName }
core_val_bndr	: var_name DCOLON atype				{ UfValBinder $1 $3 }

core_val_bndrs 	:: { [UfBinder RdrName] }
core_val_bndrs	: 						{ [] }
		| core_val_bndr core_val_bndrs			{ $1 : $2 }

core_tv_bndr	:: { UfBinder RdrName }
core_tv_bndr	:  tv_name DCOLON akind				{ UfTyBinder $1 $3 }
		|  tv_name					{ UfTyBinder $1 mkTypeKind }

core_tv_bndrs	:: { [UfBinder RdrName] }
core_tv_bndrs	: 						{ [] }
		| core_tv_bndr core_tv_bndrs			{ $1 : $2 }

ccall_string	:: { FAST_STRING }
		: STRING					{ $1 }
		| VARID						{ $1 }
		| CONID						{ $1 }
