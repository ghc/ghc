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
	OF		{ ITof }
	LET		{ ITlet }
	LETREC		{ ITletrec }
	IN		{ ITin }
	COERCE_IN	{ ITcoerce_in }
	COERCE_OUT	{ ITcoerce_out }
	CHAR		{ ITchar $$ }
	STRING		{ ITstring $$ }	
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
		|  export_item export_items 			{ $1 : $2 }

export_item	:: { ExportItem }
export_item	:  mod_name entity_occ maybe_dotdot		{ ($1, $2, $3) }

maybe_dotdot	:: { [OccName] }
maybe_dotdot	:	     					{ [] }
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
		|  var_name DCOLON ctype id_info SEMI
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
csig		:  var_name DCOLON ctype 	{ ClassOpSig $1 $3 noClassOpPragmas mkIfaceSrcLoc
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
field		:  var_name DCOLON ctype	   	{ ([$1], Unbanged $3) }
		|  var_name DCOLON BANG ctype    	{ ([$1], Banged   $4)
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

ctype		:: { RdrNameHsType }
ctype		: FORALL forall context DARROW type	{ mkHsForAllTy $2 $3 $5 }
		| type					{ $1 }

type		:: { RdrNameHsType }
type		:  btype				{ $1 }
		|  btype RARROW type			{ MonoFunTy $1 $3 }

ctypes2		:: { [RdrNameHsType] 			{- Two or more -}  }	
ctypes2		:  ctype COMMA ctype			{ [$1,$3] }
		|  ctype COMMA ctypes2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  qtc_name atypes1			{ MonoTyApp $1 $2 }
		|  tv_name  atypes1			{ MonoTyApp $1 $2 }

atype		:: { RdrNameHsType }
atype		:  qtc_name 			  	{ MonoTyApp $1 [] }
		|  tv_name			  	{ MonoTyVar $1 }
		|  OPAREN ctypes2 CPAREN	  	{ MonoTupleTy dummyRdrTcName $2 }
		|  OBRACK type CBRACK		  	{ MonoListTy  dummyRdrTcName $2 }
		|  OCURLY qtc_name atype CCURLY		{ MonoDictTy $2 $3 }
		|  OPAREN ctype CPAREN		  	{ $2 }

atypes1		:: { [RdrNameHsType] 	{-  One or more -} }
atypes1		:  atype				{ [$1] }
		|  atype atypes1			{ $1 : $2
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
instd		:  INSTANCE ctype EQUAL var_name SEMI 
			{ InstDecl $2
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   (Just $4)		{- Dfun id -}
				   mkIfaceSrcLoc 
--------------------------------------------------------------------------
		    }

id_info		:: { [HsIdInfo RdrName] }
id_info		: 	 					{ [] }
		| ARITY_PART arity_info id_info			{ HsArity $2 :  $3 }
		| STRICT_PART strict_info id_info		{ HsStrictness $2 : $3 }
		| UNFOLD_PART core_expr id_info			{ HsUnfold $2 : $3 }

arity_info	:: { ArityInfo }
arity_info	: INTEGER					{ exactArity (fromInteger $1) }

strict_info	:: { StrictnessInfo RdrName }
strict_info	: DEMAND qvar_name				{ mkStrictnessInfo $1 (Just $2) }
		| DEMAND 					{ mkStrictnessInfo $1 Nothing }
		| BOTTOM					{ mkBottomStrictnessInfo }

core_expr	:: { UfExpr RdrName }
core_expr	: var_name					{ UfVar $1 }
		| qvar_name					{ UfVar $1 }
		| qdata_name					{ UfVar $1 }
		| core_lit					{ UfLit $1 }
		| core_expr core_arg				{ UfApp $1 $2 }
		| LAM core_val_bndr RARROW core_expr		{ UfLam $2 $4 }
		| BIGLAM core_tv_bndrs RARROW core_expr		{ foldr UfLam $4 $2 }

		| CASE core_expr OF 
		  OCURLY alg_alts core_default CCURLY		{ UfCase $2 (UfAlgAlts  $5 $6) }
		| CASE BANG core_expr OF 
		  OCURLY prim_alts core_default CCURLY		{ UfCase $3 (UfPrimAlts $6 $7) }

		| LET OCURLY core_val_bndr EQUAL core_expr CCURLY
		  IN core_expr					{ UfLet (UfNonRec $3 $5) $8 }
		| LETREC OCURLY rec_binds CCURLY		
		  IN core_expr					{ UfLet (UfRec $3) $6 }

		| qdata_name BANG core_args			{ UfCon $1 $3 }
		| qvar_name  BANG core_args			{ UfPrim (UfOtherOp $1) $3 }
		| coerce atype core_expr			{ UfCoerce $1 $2 $3 }

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
		| core_val_bndr RARROW core_expr		{ UfBindDefault $1 $3 }

core_arg	:: { UfArg RdrName }
		: var_name					{ UfVarArg $1 }
		| qvar_name					{ UfVarArg $1 }
		| qdata_name					{ UfVarArg $1 }
		| core_lit					{ UfLitArg $1 }
		| OBRACK atype CBRACK				{ UfTyArg  $2 }

core_args	:: { [UfArg RdrName] }
		:						{ [] }
		| core_arg core_args				{ $1 : $2 }

core_lit	:: { Literal }
core_lit	: INTEGER					{ MachInt $1 True }
		| CHAR						{ MachChar $1 }
		| STRING					{ MachStr $1 }

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

