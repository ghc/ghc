{
#include "HsVersions.h"
module ParseIface ( parseIface ) where

IMP_Ubiq(){-uitous-}

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..), HsStrictnessInfo )
import HsTypes		( mkHsForAllTy )
import HsCore
import Literal
import BasicTypes	( IfaceFlavour(..), Fixity(..), FixityDirection(..), NewOrData(..), Version(..) )
import HsPragmas	( noDataPragmas, noClassPragmas )
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import IdInfo           ( ArgUsageInfo, FBTypeInfo )
import Lex		

import RnMonad		( SYN_IE(ImportVersion), SYN_IE(LocalVersion), ParsedIface(..),
			  SYN_IE(RdrNamePragma), SYN_IE(ExportItem), SYN_IE(RdrAvailInfo), GenAvailInfo(..)
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName(..), isTCOcc, Provenance, SYN_IE(Module) )
import SrcLoc		( mkIfaceSrcLoc )
--import Util		( panic{-, pprPanic ToDo:rm-} )
import ParseType        ( parseType )
import ParseUnfolding   ( parseUnfolding )
import Maybes

-----------------------------------------------------------------

parseIface ls = parseIToks (lexIface ls)

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
	DATA		    { ITdata }
	TYPE		    { ITtype }
	NEWTYPE		    { ITnewtype }
	DERIVING	    { ITderiving }
	CLASS		    { ITclass }
	WHERE		    { ITwhere }
	INSTANCE	    { ITinstance }
	INFIXL		    { ITinfixl }
	INFIXR		    { ITinfixr }
	INFIX		    { ITinfix }
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

	IDINFO_PART     { ITidinfo $$ }
	TYPE_PART       { ITtysig $$ }
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
module_stuff_pair   :  mod_name opt_bang INTEGER DCOLON name_version_pairs SEMI
			{ ($1, $2, fromInteger $3, $5) }

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
		|  opt_bang mod_name entities SEMI export_items { ($2,$1,$3) : $5 }

opt_bang	:: { IfaceFlavour }
opt_bang	:						{ HiFile }
		| BANG						{ HiBootFile }

entities	:: { [RdrAvailInfo] }
entities	: 						{ [] }
		|  entity entities				{ $1 : $2 }

entity		:: { RdrAvailInfo }
entity		:  entity_occ 				{ if isTCOcc $1 
							  then AvailTC $1 [$1]
							  else Avail $1 }
		|  entity_occ stuff_inside		{ AvailTC $1 ($1:$2) }
		|  entity_occ VBAR stuff_inside		{ AvailTC $1 $3 }

stuff_inside	:: { [OccName] }
stuff_inside	:  OPAREN val_occs1 CPAREN		{ $2
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
		|  DATA decl_context tc_name tv_bndrs constrs deriving SEMI
			{ TyD (TyData DataType $2 $3 $4 $5 $6 noDataPragmas mkIfaceSrcLoc) }
		|  NEWTYPE decl_context tc_name tv_bndrs newtype_constr deriving SEMI
			{ TyD (TyData NewType $2 $3 $4 $5 $6 noDataPragmas mkIfaceSrcLoc) }
		|  CLASS decl_context tc_name tv_bndr csigs SEMI
			{ ClD (ClassDecl $2 $3 $4 $5 EmptyMonoBinds noClassPragmas mkIfaceSrcLoc) }
		|  var_name TYPE_PART id_info
			{
			 let
			  (Succeeded tp) = parseType $2
			 in
  		 	 SigD (IfaceSig $1 tp $3 mkIfaceSrcLoc) }

id_info 	:: { [HsIdInfo RdrName] }
id_info		:				{ [] }
		| IDINFO_PART   { let { (Succeeded id_info) = parseUnfolding $1 } in id_info}

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
csig		:  var_name DCOLON type 	{ ClassOpSig $1 Nothing $3 mkIfaceSrcLoc }
	        |  var_name EQUAL DCOLON type	{ ClassOpSig $1 (Just (error "Un-filled-in default method"))
								$4 mkIfaceSrcLoc
----------------------------------------------------------------
			 			 }

constrs		:: { [RdrNameConDecl] {- empty for handwritten abstract -} }
		: 				{ [] }
		| EQUAL constrs1		{ $2 }

constrs1	:: { [RdrNameConDecl] }
constrs1	:  constr		{ [$1] }
		|  constr VBAR constrs1	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  data_name batypes			{ ConDecl $1 [] (VanillaCon $2) mkIfaceSrcLoc }
		|  data_name OCURLY fields1 CCURLY	{ ConDecl $1 [] (RecCon $3)     mkIfaceSrcLoc }

newtype_constr	:: { [RdrNameConDecl] {- Empty if handwritten abstract -} }
newtype_constr	:  				{ [] }
		| EQUAL data_name atype		{ [ConDecl $2 [] (NewCon $3) mkIfaceSrcLoc] }

deriving	:: { Maybe [RdrName] }
		: 					{ Nothing }
		| DERIVING OPAREN tc_names1 CPAREN	{ Just $3 }

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
field		:  var_names1 DCOLON type		{ ($1, Unbanged $3) }
		|  var_names1 DCOLON BANG type    	{ ($1, Banged   $4)
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

mod_name	:: { Module }
		:  CONID		{ $1 }

var_occ		:: { OccName }
var_occ		: VARID			{ VarOcc $1 }
		| VARSYM		{ VarOcc $1 }
		| BANG  		{ VarOcc SLIT("!") {-sigh, double-sigh-} }

tc_occ		:: { OccName }
tc_occ		:  CONID		{ TCOcc $1 }
		|  CONSYM		{ TCOcc $1 }
		|  OPAREN RARROW CPAREN	{ TCOcc SLIT("->") }

entity_occ	:: { OccName }
entity_occ	:  var_occ		{ $1 }
		|  tc_occ 		{ $1 }
		|  RARROW		{ TCOcc SLIT("->") {- Allow un-paren'd arrow -} }

val_occ		:: { OccName }
val_occ		:  var_occ 		{ $1 }
		|  CONID		{ VarOcc $1 }
		|  CONSYM		{ VarOcc $1 }

val_occs1	:: { [OccName] }
		:  val_occ    		{ [$1] }
		|  val_occ val_occs1	{ $1 : $2 }


qvar_name	:: { RdrName }
		:  QVARID		{ lexVarQual $1 }
		|  QVARSYM		{ lexVarQual $1 }

var_name	:: { RdrName }
var_name	:  var_occ		{ Unqual $1 }

var_names1	:: { [RdrName] }
var_names1	: var_name		{ [$1] }
		| var_name var_names1	{ $1 : $2 }

any_var_name	:: {RdrName}
any_var_name	:  var_name		{ $1 }
		|  qvar_name		{ $1 }

qdata_name	:: { RdrName }
qdata_name	:  QCONID		{ lexVarQual $1 }
		|  QCONSYM		{ lexVarQual $1 }

data_name	:: { RdrName }
data_name	:  CONID		{ Unqual (VarOcc $1) }
		|  CONSYM		{ Unqual (VarOcc $1) }


tc_names1	:: { [RdrName] }
		: tc_name			{ [$1] }
		| tc_name COMMA tc_names1	{ $1 : $3 }

tc_name		:: { RdrName }
tc_name		: tc_occ			{ Unqual $1 }
		| QCONID			{ lexTcQual $1 }

tv_name		:: { RdrName }
tv_name		:  VARID 		{ Unqual (TvOcc $1) }
		|  VARSYM		{ Unqual (TvOcc $1) {- Allow $t2 as a tyvar -} }

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
		: VARSYM		{ mkBoxedTypeKind {- ToDo: check that it's "*" -} }
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
