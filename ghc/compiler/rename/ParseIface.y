{
module ParseIface ( parseIface, IfaceStuff(..) ) where

#include "HsVersions.h"

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import HsTypes		( mkHsForAllTy )
import HsCore
import Literal
import BasicTypes	( IfaceFlavour(..), Fixity(..), FixityDirection(..), NewOrData(..), Version(..) )
import HsPragmas	( noDataPragmas, noClassPragmas )
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind, mkTypeKind )
import IdInfo           ( ArgUsageInfo, FBTypeInfo, ArityInfo, exactArity )
import PrimRep		( decodePrimRep )
import Lex		

import RnMonad		( ImportVersion, LocalVersion, ParsedIface(..), WhatsImported(..),
			  RdrNamePragma, ExportItem, RdrAvailInfo, GenAvailInfo(..)
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName(..), isTCOcc, Provenance, SYN_IE(Module) )
import SrcLoc		( SrcLoc )
import Maybes
import Outputable

}

%name	    parseIface
%tokentype  { IfaceToken }
%monad	    { IfM }{ thenIf }{ returnIf }
%lexer      { lexIface } { ITeof }

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

	STRICT_PART	{ ITstrict $$ }
	TYPE_PART       { ITtysig _ _ }
	ARITY_PART	{ ITarity }
	UNFOLD_PART	{ ITunfold $$ }
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

-- iface_stuff is the main production.
-- It recognises (a) a whole interface file
--		 (b) a type (so that type sigs can be parsed lazily)
--		 (c) the IdInfo part of a signature (same reason)

iface_stuff :: { IfaceStuff }
iface_stuff : iface		{ PIface  $1 }
      	    | type		{ PType   $1 }
      	    | id_info		{ PIdInfo $1 }


iface		:: { ParsedIface }
iface		: INTERFACE CONID INTEGER checkVersion
		  inst_modules_part 
		  usages_part
		  exports_part fixities_part
		  instances_part
		  decls_part
		  { ParsedIface 
			$2 			-- Module name
			(fromInteger $3) 	-- Module version
			$6  		        -- Usages
			$7  		        -- Exports
			$5  		        -- Instance modules
			$8  		        -- Fixities
			$10  		        -- Decls
			$9 			-- Local instances
		    }


usages_part	    :: { [ImportVersion OccName] }
usages_part	    :  USAGES_PART module_stuff_pairs		{ $2 }
		    |						{ [] }

module_stuff_pairs  :: { [ImportVersion OccName] }
module_stuff_pairs  :  						{ [] }
		    |  module_stuff_pair module_stuff_pairs	{ $1 : $2 }

module_stuff_pair   ::  { ImportVersion OccName }
module_stuff_pair   :  mod_name opt_bang INTEGER DCOLON whats_imported SEMI
			{ ($1, $2, fromInteger $3, $5) }

whats_imported      :: { WhatsImported OccName }
whats_imported      :                                           { Everything }
                    | name_version_pair name_version_pairs      { Specifically ($1:$2) }

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
topdecl		:  src_loc TYPE  tc_name tv_bndrs EQUAL type SEMI
			{ TyD (TySynonym $3 $4 $6 $1) }
		|  src_loc DATA decl_context tc_name tv_bndrs constrs deriving SEMI
			{ TyD (TyData DataType $3 $4 $5 $6 $7 noDataPragmas $1) }
		|  src_loc NEWTYPE decl_context tc_name tv_bndrs newtype_constr deriving SEMI
			{ TyD (TyData NewType $3 $4 $5 $6 $7 noDataPragmas $1) }
		|  src_loc CLASS decl_context tc_name tv_bndrs csigs SEMI
			{ ClD (mkClassDecl $3 $4 $5 $6 EmptyMonoBinds noClassPragmas $1) }
		|  src_loc var_name TYPE_PART
			{
			 case $3 of
			    ITtysig sig idinfo_part ->	-- Parse type and idinfo lazily
				let info = 
				      case idinfo_part of
					Nothing -> []
					Just s  -> case parseIface s $1 of 
						     Succeeded (PIdInfo id_info) -> id_info
						     other ->  pprPanic "IdInfo parse failed"
							      	        (ppr $2)

				    tp = case parseIface sig $1 of
					    Succeeded (PType tp) -> tp
					    other -> pprPanic "Id type parse failed"
							      (ppr $2)
				 in
  			 	 SigD (IfaceSig $2 tp info $1) }

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
csig		:  src_loc var_name DCOLON type { ClassOpSig $2 Nothing $4 $1 }
	        |  src_loc var_name EQUAL DCOLON type	{ ClassOpSig $2 
								(Just (error "Un-filled-in default method"))
								$5 $1 }
----------------------------------------------------------------


constrs		:: { [RdrNameConDecl] {- empty for handwritten abstract -} }
		: 				{ [] }
		| EQUAL constrs1		{ $2 }

constrs1	:: { [RdrNameConDecl] }
constrs1	:  constr		{ [$1] }
		|  constr VBAR constrs1	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  src_loc data_name batypes			{ ConDecl $2 [] (VanillaCon $3) $1 }
		|  src_loc data_name OCURLY fields1 CCURLY	{ ConDecl $2 [] (RecCon $4)     $1 }

newtype_constr	:: { [RdrNameConDecl] {- Empty if handwritten abstract -} }
newtype_constr	:  					{ [] }
		| src_loc EQUAL data_name atype		{ [ConDecl $3 [] (NewCon $4) $1] }

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
		|  var_names1 DCOLON BANG type    	{ ($1, Banged   $4) }
--------------------------------------------------------------------------

type		:: { RdrNameHsType }
type		: FORALL forall context DARROW type	{ mkHsForAllTy $2 $3 $5 }
		|  btype RARROW type			{ MonoFunTy $1 $3 }
		|  btype				{ $1 }

forall		:: { [HsTyVar RdrName] }
forall		: OBRACK tv_bndrs CBRACK		{ $2 }

context		:: { RdrNameContext }
context		:  					{ [] }
		| OCURLY context_list1 CCURLY		{ $2 }

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class COMMA context_list1 		{ $1 : $3 }

class		:: { (RdrName, [RdrNameHsType]) }
class		:  tc_name atypes			{ ($1, $2) }

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
		|  OCURLY tc_name atypes CCURLY		{ MonoDictTy $2 $3 }
		|  OPAREN type CPAREN		  	{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2 }
---------------------------------------------------------------------

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


var_name	:: { RdrName }
var_name	:  var_occ		{ Unqual $1 }

qvar_name	:: { RdrName }
qvar_name	:  var_name		{ $1 }
		|  QVARID		{ lexVarQual $1 }
		|  QVARSYM		{ lexVarQual $1 }

var_names	:: { [RdrName] }
var_names	: 			{ [] }
		| var_name var_names	{ $1 : $2 }

var_names1	:: { [RdrName] }
var_names1	: var_name var_names	{ $1 : $2 }

data_name	:: { RdrName }
data_name	:  CONID		{ Unqual (VarOcc $1) }
		|  CONSYM		{ Unqual (VarOcc $1) }

qdata_name	:: { RdrName }
qdata_name	: data_name		{ $1 }
		|  QCONID		{ lexVarQual $1 }
		|  QCONSYM		{ lexVarQual $1 }
				
qdata_names	:: { [RdrName] }
qdata_names	:				{ [] }
		| qdata_name qdata_names	{ $1 : $2 }

tc_name		:: { RdrName }
tc_name		: tc_occ			{ Unqual $1 }
		| QCONID			{ lexTcQual $1 }
		| QCONSYM			{ lexTcQual $1 }

tc_names1	:: { [RdrName] }
		: tc_name			{ [$1] }
		| tc_name COMMA tc_names1	{ $1 : $3 }

tv_name		:: { RdrName }
tv_name		:  VARID 		{ Unqual (TvOcc $1) }
		|  VARSYM		{ Unqual (TvOcc $1) {- Allow t2 as a tyvar -} }

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
		: VARSYM		{ if $1 == SLIT("*") then
						mkBoxedTypeKind
					  else if $1 == SLIT("**") then
						mkTypeKind
					  else panic "ParseInterface: akind"
					}
		| OPAREN kind CPAREN	{ $2 }
--------------------------------------------------------------------------


instances_part	:: { [RdrNameInstDecl] }
instances_part	:  INSTANCES_PART instdecls { $2 }
		|			    { [] }

instdecls	:: { [RdrNameInstDecl] }
instdecls	:  			    { [] }
		|  instd instdecls	    { $1 : $2 }

instd		:: { RdrNameInstDecl }
instd		:  src_loc INSTANCE type EQUAL var_name SEMI 
			{ InstDecl $3
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   (Just $5)		{- Dfun id -}
				   $1
		    }
--------------------------------------------------------------------------

id_info		:: { [HsIdInfo RdrName] }
id_info		: 	 					{ [] }
		| id_info_item id_info				{ $1 : $2 }

id_info_item	:: { HsIdInfo RdrName }
id_info_item	: ARITY_PART arity_info			{ HsArity $2 }
		| strict_info				{ HsStrictness $1 }
		| BOTTOM 				{ HsStrictness HsBottom }
		| UNFOLD_PART core_expr			{ HsUnfold $1 $2 }

arity_info	:: { ArityInfo }
arity_info	: INTEGER					{ exactArity (fromInteger $1) }

strict_info	:: { HsStrictnessInfo RdrName }
strict_info	: STRICT_PART qvar_name OCURLY qdata_names CCURLY 	{ HsStrictnessInfo $1 (Just ($2,$4)) }
		| STRICT_PART qvar_name 			 	{ HsStrictnessInfo $1 (Just ($2,[])) }
		| STRICT_PART 						{ HsStrictnessInfo $1 Nothing }

core_expr	:: { UfExpr RdrName }
core_expr	: qvar_name					{ UfVar $1 }
		| qdata_name					{ UfVar $1 }
		| core_lit					{ UfLit $1 }
		| OPAREN core_expr CPAREN			{ $2 }
		| qdata_name OCURLY data_args CCURLY		{ UfCon $1 $3 }

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
coerce		: COERCE_IN  qdata_name				{ UfIn  $2 }
		| COERCE_OUT qdata_name				{ UfOut $2 }
		
prim_alts	:: { [(Literal,UfExpr RdrName)] }
		:						{ [] }
		| core_lit RARROW core_expr SEMI prim_alts	{ ($1,$3) : $5 }

alg_alts	:: { [(RdrName, [RdrName], UfExpr RdrName)] }
		: 						{ [] }
		| qdata_name var_names RARROW 
			core_expr SEMI alg_alts			{ ($1,$2,$4) : $6 }

core_default	:: { UfDefault RdrName }
		: 						{ UfNoDefault }
		| var_name RARROW core_expr SEMI		{ UfBindDefault $1 $3 }

core_arg	:: { UfArg RdrName }
		: qvar_name					{ UfVarArg $1 }
		| qdata_name					{ UfVarArg $1 }
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
	  | CONID						{ head (_UNPK_ $1) }

-------------------------------------------------------------------

src_loc :: { SrcLoc }
src_loc : 				{% getSrcLocIf }

checkVersion :: { () }
	   : {-empty-}			{% checkVersion Nothing }
	   | INTEGER			{% checkVersion (Just (fromInteger $1)) }

------------------------------------------------------------------- 

--			Haskell code 
{

data IfaceStuff = PIface 	ParsedIface
		| PIdInfo	[HsIdInfo RdrName]
		| PType		RdrNameHsType

}
