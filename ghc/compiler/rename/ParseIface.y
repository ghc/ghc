{
module ParseIface ( parseIface, IfaceStuff(..) ) where

#include "HsVersions.h"

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import HsTypes		( mkHsForAllTy )
import HsCore
import Const		( Literal(..), mkMachInt_safe )
import BasicTypes	( IfaceFlavour(..), Fixity(..), FixityDirection(..), 
			  NewOrData(..), Version
			)
import HsPragmas	( noDataPragmas, noClassPragmas )
import Type		( Kind, mkArrowKind, boxedTypeKind, openTypeKind )
import IdInfo           ( ArityInfo, exactArity )
import Lex		

import RnEnv            ( ifaceUnqualTC, ifaceUnqualVar, ifaceUnqualTv, ifaceQualVar, ifaceQualTC )
import RnMonad		( ImportVersion, LocalVersion, ParsedIface(..), WhatsImported(..),
			  RdrNamePragma, ExportItem, RdrAvailInfo, GenAvailInfo(..)
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName, isTCOcc, Provenance, Module,
			  varOcc, tcOcc, mkModuleFS
			)
import PrelMods         ( mkTupNameStr, mkUbxTupNameStr )
import PrelInfo         ( mkTupConRdrName, mkUbxTupConRdrName )
import SrcLoc		( SrcLoc )
import Maybes
import Outputable

import GlaExts
}

%name	    parseIface
%tokentype  { IfaceToken }
%monad	    { IfM }{ thenIf }{ returnIf }
%lexer      { lexIface } { ITeof }

%token
 'case' 	{ ITcase }  			-- Haskell keywords
 'class' 	{ ITclass } 
 'data' 	{ ITdata } 
 'default' 	{ ITdefault }
 'deriving' 	{ ITderiving }
 'do' 		{ ITdo }
 'else' 	{ ITelse }
 'if' 		{ ITif }
 'import' 	{ ITimport }
 'in' 		{ ITin }
 'infix' 	{ ITinfix }
 'infixl' 	{ ITinfixl }
 'infixr' 	{ ITinfixr }
 'instance' 	{ ITinstance }
 'let' 		{ ITlet }
 'module' 	{ ITmodule }
 'newtype' 	{ ITnewtype }
 'of' 		{ ITof }
 'then' 	{ ITthen }
 'type' 	{ ITtype }
 'where' 	{ ITwhere }
 'as' 		{ ITas }
 'qualified' 	{ ITqualified }
 'hiding' 	{ IThiding }

 '__interface'	{ ITinterface }			-- GHC-extension keywords
 '__export'	{ ITexport }
 '__instimport'	{ ITinstimport }
 '__forall'	{ ITforall }
 '__letrec'	{ ITletrec }
 '__coerce'	{ ITcoerce }
 '__inline'	{ ITinline }
 '__DEFAULT'	{ ITdefaultbranch }
 '__bot'	{ ITbottom }
 '__integer'	{ ITinteger_lit }
 '__float'	{ ITfloat_lit }
 '__rational'	{ ITrational_lit }
 '__addr'	{ ITaddr_lit }
 '__litlit'	{ ITlit_lit }
 '__string'	{ ITstring_lit }
 '__ccall'	{ ITccall $$ }
 '__scc' 	{ ITscc $$ }
 '__a'		{ ITtypeapp }

 '__A'		{ ITarity }
 '__P'		{ ITspecialise }
 '__C'		{ ITnocaf }
 '__U'		{ ITunfold $$ }
 '__S'		{ ITstrict $$ }

 '..'		{ ITdotdot }  			-- reserved symbols
 '::'		{ ITdcolon }
 '='		{ ITequal }
 '\\'		{ ITlam }
 '|'		{ ITvbar }
 '<-'		{ ITlarrow }
 '->'		{ ITrarrow }
 '@'		{ ITat }
 '~'		{ ITtilde }
 '=>'		{ ITdarrow }
 '-'		{ ITminus }
 '!'		{ ITbang }

 '/\\'		{ ITbiglam }			-- GHC-extension symbols

 '{'		{ ITocurly } 			-- special symbols
 '}'		{ ITccurly }
 '['		{ ITobrack }
 ']'		{ ITcbrack }
 '('		{ IToparen }
 ')'		{ ITcparen }
 '(#'		{ IToubxparen }
 '#)'		{ ITcubxparen }
 ';'		{ ITsemi }
 ','		{ ITcomma }

 VARID   	{ ITvarid    $$ }		-- identifiers
 CONID   	{ ITconid    $$ }
 VARSYM  	{ ITvarsym   $$ }
 CONSYM  	{ ITconsym   $$ }
 QVARID  	{ ITqvarid   $$ }
 QCONID  	{ ITqconid   $$ }
 QVARSYM 	{ ITqvarsym  $$ }
 QCONSYM 	{ ITqconsym  $$ }

 PRAGMA		{ ITpragma   $$ }

 CHAR		{ ITchar     $$ }
 STRING		{ ITstring   $$ }
 INTEGER	{ ITinteger  $$ }
 RATIONAL	{ ITrational $$ }

 UNKNOWN	{ ITunknown  $$ }
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
iface		: '__interface' mod_name INTEGER checkVersion 'where'
                  import_part
		  instance_import_part
		  exports_part
		  instance_decl_part
		  decls_part
		  { ParsedIface 
		        $2  		        -- Module name
			(fromInteger $3) 	-- Module version
			(reverse $6)	        -- Usages
			(reverse $8)	        -- Exports
			(reverse $7)	        -- Instance import modules
			(reverse $10)		-- Decls
			(reverse $9)		-- Local instances
		  }

--------------------------------------------------------------------------

import_part :: { [ImportVersion OccName] }
import_part :				    		  { [] }
	    |  import_part import_decl			  { $2 : $1 }
	    
import_decl :: { ImportVersion OccName }
import_decl : 'import' mod_name opt_bang INTEGER '::' whats_imported ';'
			{ ($2, $3, fromInteger $4, $6) }

whats_imported      :: { WhatsImported OccName }
whats_imported      :                                           { Everything }
                    | name_version_pair name_version_pairs      { Specifically ($1:$2) }

name_version_pairs  ::	{ [LocalVersion OccName] }
name_version_pairs  :  						{ [] }
		    |  name_version_pair name_version_pairs	{ $1 : $2 }

name_version_pair   ::	{ LocalVersion OccName }
name_version_pair   :  entity_occ INTEGER			{ ($1, fromInteger $2) }

instance_import_part :: { [Module] }
instance_import_part : 						{   []    }
                     | instance_import_part '__instimport' mod_name ';'
						 		{ $3 : $1 }

--------------------------------------------------------------------------

exports_part	:: { [ExportItem] }
exports_part	:  					{ [] }
		| exports_part '__export' opt_bang mod_name entities ';'
						{ ($4,$3,$5) : $1 }

opt_bang	:: { IfaceFlavour }
opt_bang	:						{ HiFile }
		| '!'						{ HiBootFile }

entities	:: { [RdrAvailInfo] }
entities	: 						{ [] }
		|  entity entities				{ $1 : $2 }

entity		:: { RdrAvailInfo }
entity		:  entity_occ 				{ if isTCOcc $1 
							  then AvailTC $1 [$1]
							  else Avail $1 }
		|  entity_occ stuff_inside		{ AvailTC $1 ($1:$2) }
		|  entity_occ '|' stuff_inside		{ AvailTC $1 $3 }

stuff_inside	:: { [OccName] }
stuff_inside	:  '{' val_occs '}'			{ $2 }

--------------------------------------------------------------------------

fixity      :: { FixityDirection }
fixity      : 'infixl'                                  { InfixL }
            | 'infixr'                                  { InfixR }
            | 'infix'                                   { InfixN }
   
mb_fix      :: { Int }
mb_fix	    : {-nothing-}				{ 9 }
	    | INTEGER					{ (fromInteger $1) }

-----------------------------------------------------------------------------

csigs		:: { [RdrNameSig] }
csigs		:  				{ [] }
		| 'where' '{' csigs1 '}'	{ $3 }

csigs1		:: { [RdrNameSig] }
csigs1		: csig				{ [$1] }
		| csig ';' csigs1		{ $1 : $3 }

csig		:: { RdrNameSig }
csig		:  src_loc var_name '::' type { ClassOpSig $2 Nothing $4 $1 }
	        |  src_loc var_name '=' '::' type	
			{ ClassOpSig $2 
			    (Just (error "Un-filled-in default method"))
			    $5 $1 }

--------------------------------------------------------------------------

instance_decl_part :: { [RdrNameInstDecl] }
instance_decl_part : {- empty -}		       { [] }
		   | instance_decl_part inst_decl      { $2 : $1 }

inst_decl	:: { RdrNameInstDecl }
inst_decl	:  src_loc 'instance' type '=' var_name ';'
			{ InstDecl $3
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   (Just $5)		{- Dfun id -}
				   $1
			}

--------------------------------------------------------------------------

decls_part :: { [(Version, RdrNameHsDecl)] }
decls_part 
	:  {- empty -}				{ [] }
	|  decls_part version decl ';'		{ ($2,$3):$1 }

decl	:: { RdrNameHsDecl }
decl    : src_loc var_name '::' type maybe_idinfo
  		 	 { SigD (IfaceSig $2 $4 ($5 $2) $1) }
	| src_loc 'type' tc_name tv_bndrs '=' type 		       
			{ TyClD (TySynonym $3 $4 $6 $1) }
	| src_loc 'data' decl_context data_fs tv_bndrs constrs 	       
	       		{ TyClD (TyData DataType $3 (ifaceUnqualTC $4) $5 $6 Nothing noDataPragmas $1) }
	| src_loc 'newtype' decl_context tc_name tv_bndrs newtype_constr
			{ TyClD (TyData NewType $3 $4 $5 $6 Nothing noDataPragmas $1) }
	| src_loc 'class' decl_context tc_name tv_bndrs csigs
			{ TyClD (mkClassDecl $3 $4 $5 $6 EmptyMonoBinds 
					noClassPragmas $1) }
        | src_loc fixity mb_fix val_occ
                        { FixD (FixitySig (Unqual $4) (Fixity $3 $2) $1) }

maybe_idinfo  :: { RdrName -> [HsIdInfo RdrName] }
maybe_idinfo  : {- empty -} 	{ \_ -> [] }
	      | src_loc PRAGMA	{ \x -> 
				   case parseIface $2 $1 of
				     Succeeded (PIdInfo id_info) -> id_info
				     Failed err -> pprPanic "IdInfo parse failed" 
				                            (vcat [ppr x, err])
				}

-----------------------------------------------------------------------------

version		:: { Version }
version		:  INTEGER				{ fromInteger $1 }

decl_context	:: { RdrNameContext }
decl_context	:  					{ [] }
		| '{' context_list1 '}' '=>'	{ $2 }

----------------------------------------------------------------

constrs		:: { [RdrNameConDecl] {- empty for handwritten abstract -} }
		: 			{ [] }
		| '=' constrs1		{ $2 }

constrs1	:: { [RdrNameConDecl] }
constrs1	:  constr		{ [$1] }
		|  constr '|' constrs1	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  src_loc ex_stuff data_fs batypes		{ mkConDecl (ifaceUnqualVar $3) $2 (VanillaCon $4) $1 }
		|  src_loc ex_stuff data_fs '{' fields1 '}'	{ mkConDecl (ifaceUnqualVar $3) $2 (RecCon $5)     $1 }
                -- We use "data_fs" so as to include ()

newtype_constr	:: { [RdrNameConDecl] {- Empty if handwritten abstract -} }
newtype_constr	:  					{ [] }
		| src_loc '=' ex_stuff data_name atype	{ [mkConDecl $4 $3 (NewCon $5 Nothing) $1] }
		| src_loc '=' ex_stuff data_name '{' var_name '::' atype '}'
							{ [mkConDecl $4 $3 (NewCon $8 (Just $6)) $1] }

ex_stuff :: { ([HsTyVar RdrName], RdrNameContext) }
ex_stuff	:                                       { ([],[]) }
                | '__forall' forall context '=>'            { ($2,$3) }

batypes		:: { [RdrNameBangType] }
batypes		:  					{ [] }
		|  batype batypes			{ $1 : $2 }

batype		:: { RdrNameBangType }
batype		:  atype				{ Unbanged $1 }
		|  '!' atype				{ Banged   $2 }

fields1		:: { [([RdrName], RdrNameBangType)] }
fields1		: field					{ [$1] }
		| field ',' fields1			{ $1 : $3 }

field		:: { ([RdrName], RdrNameBangType) }
field		:  var_names1 '::' type		{ ($1, Unbanged $3) }
		|  var_names1 '::' '!' type    	{ ($1, Banged   $4) }
--------------------------------------------------------------------------

type		:: { RdrNameHsType }
type		: '__forall' forall context '=>' type	
						{ mkHsForAllTy $2 $3 $5 }
		| btype '->' type		{ MonoFunTy $1 $3 }
		| btype				{ $1 }

forall		:: { [HsTyVar RdrName] }
forall		: '[' tv_bndrs ']'		        { $2 }

context		:: { RdrNameContext }
context		:  					{ [] }
		| '{' context_list1 '}'		        { $2 }

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class ',' context_list1 		{ $1 : $3 }

class		:: { (RdrName, [RdrNameHsType]) }
class		:  qtc_name atypes			{ ($1, $2) }

types2		:: { [RdrNameHsType] 			{- Two or more -}  }	
types2		:  type ',' type			{ [$1,$3] }
		|  type ',' types2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  btype atype				{ MonoTyApp $1 $2 }

atype		:: { RdrNameHsType }
atype		:  qtc_name 			  	{ MonoTyVar $1 }
		|  tv_name			  	{ MonoTyVar $1 }
	  	|  '(' ')' 				{ MonoTupleTy [] True }
		|  '(' types2 ')'	  		{ MonoTupleTy $2 True{-boxed-} }
		|  '(#' type '#)'  			{ MonoTupleTy [$2] False{-unboxed-} }
		|  '(#' types2 '#)'			{ MonoTupleTy $2 False{-unboxed-} }
		|  '[' type ']'		  		{ MonoListTy  $2 }
		|  '{' qtc_name atypes '}'		{ MonoDictTy $2 $3 }
		|  '(' type ')'		  		{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2 }
---------------------------------------------------------------------

mod_name	:: { Module }
		:  CONID		{ mkModuleFS $1 }

var_fs		:: { FAST_STRING }
		: VARID			{ $1 }
		| VARSYM		{ $1 }
		| '-' 			{ SLIT("-") }
		| '!'	  		{ SLIT("!") }

data_fs         :: { FAST_STRING }
		:  CONID		{ $1 }
		|  CONSYM		{ $1 }
		|  '->'			{ SLIT("->") }
                |  '(' ')'        	{ SLIT("()") }
		|  '(' commas ')'	{ snd (mkTupNameStr $2) }
		|  '[' ']'              { SLIT("[]") }

commas		:: { Int }
		: ','			{ 2 }
		| commas ','		{ $1 + 1 }

val_occ		:: { OccName }
		:  var_fs 		{ varOcc $1 }
                |  data_fs              { varOcc $1 }

val_occs	:: { [OccName] }
		:  val_occ    		{ [$1] }
		|  val_occ val_occs	{ $1 : $2 }

entity_occ	:: { OccName }
		:  var_fs		{ varOcc $1 }
		|  data_fs 		{ tcOcc $1 }

var_name	:: { RdrName }
var_name	:  var_fs		{ ifaceUnqualVar $1 }

qvar_name	:: { RdrName }
qvar_name	:  var_name		{ $1 }
		|  QVARID		{ ifaceQualVar $1 }
		|  QVARSYM		{ ifaceQualVar $1 }

var_names	:: { [RdrName] }
var_names	: 			{ [] }
		| var_name var_names	{ $1 : $2 }

var_names1	:: { [RdrName] }
var_names1	: var_name var_names	{ $1 : $2 }

data_name	:: { RdrName }
		:  CONID		{ ifaceUnqualVar $1 }
		|  CONSYM		{ ifaceUnqualVar $1 }
		|  '(' commas ')'	{ ifaceUnqualVar (snd (mkTupNameStr $2)) }
		|  '[' ']'              { ifaceUnqualVar SLIT("[]") }

qdata_name	:: { RdrName }
qdata_name	:  data_name		{ $1 }
		|  QCONID		{ ifaceQualVar $1 }
		|  QCONSYM		{ ifaceQualVar $1 }
				
qdata_names	:: { [RdrName] }
qdata_names	:				{ [] }
		| qdata_name qdata_names	{ $1 : $2 }

tc_name		:: { RdrName }
tc_name		:  CONID		{ ifaceUnqualTC $1 }
		|  CONSYM		{ ifaceUnqualTC $1 }
		|  '(' '->' ')'		{ ifaceUnqualTC SLIT("->") }
		|  '(' commas ')'	{ ifaceUnqualTC (snd (mkTupNameStr $2)) }
		|  '[' ']'		{ ifaceUnqualTC SLIT("[]") }

qtc_name	:: { RdrName }
qtc_name	: tc_name		{ $1 }
		| QCONID		{ ifaceQualTC $1 }
		| QCONSYM		{ ifaceQualTC $1 }

tv_name		:: { RdrName }
tv_name		:  VARID 		{ ifaceUnqualTv $1 }
		|  VARSYM		{ ifaceUnqualTv $1 {- Allow t2 as a tyvar -} }

tv_bndr		:: { HsTyVar RdrName }
tv_bndr		:  tv_name '::' akind	{ IfaceTyVar $1 $3 }
		|  tv_name		{ IfaceTyVar $1 boxedTypeKind }

tv_bndrs	:: { [HsTyVar RdrName] }
		:  			{ [] }
		| tv_bndr tv_bndrs	{ $1 : $2 }

kind		:: { Kind }
		: akind			{ $1 }
		| akind '->' kind	{ mkArrowKind $1 $3 }

akind		:: { Kind }
		: VARSYM		{ if $1 == SLIT("*") then
						boxedTypeKind
					  else if $1 == SLIT("?") then
						openTypeKind
					  else panic "ParseInterface: akind"
					}
		| '(' kind ')'	{ $2 }

--------------------------------------------------------------------------

id_info		:: { [HsIdInfo RdrName] }
id_info		: 	 			{ [] }
		| id_info_item id_info		{ $1 : $2 }

id_info_item	:: { HsIdInfo RdrName }
id_info_item	: '__A' arity_info		{ HsArity $2 }
		| strict_info			{ HsStrictness $1 }
		| '__U' core_expr		{ HsUnfold $1 (Just $2) }
                | '__U' 		 	{ HsUnfold $1 Nothing }
                | '__P' spec_tvs
                     atypes '=' core_expr       { HsSpecialise $2 $3 $5 }
		| '__C'                         { HsNoCafRefs }


spec_tvs	:: { [HsTyVar RdrName] }
spec_tvs	: '[' tv_bndrs ']' 		{ $2 }
	

arity_info	:: { ArityInfo }
arity_info	: INTEGER			{ exactArity (fromInteger $1) }

strict_info	:: { HsStrictnessInfo RdrName }
strict_info	: '__S' qvar_name '{' qdata_names '}' 	
					{ HsStrictnessInfo $1 (Just ($2,$4)) }
		| '__S' qvar_name 	{ HsStrictnessInfo $1 (Just ($2,[])) }
		| '__S'			{ HsStrictnessInfo $1 Nothing }

-------------------------------------------------------
core_expr	:: { UfExpr RdrName }
core_expr	: '\\' core_bndrs '->' core_expr	{ foldr UfLam $4 $2 }
		| 'case' core_expr 'of' var_name
		  '{' core_alts '}'		        { UfCase $2 $4 $6 }

		| 'let' '{' core_val_bndr '=' core_expr
		      '}' 'in' core_expr		{ UfLet (UfNonRec $3 $5) $8 }
		| '__letrec' '{' rec_binds '}'		
		  'in' core_expr			{ UfLet (UfRec $3) $6 }

		| con_or_primop '{' core_args '}'	{ UfCon $1 $3 }
                | '__litlit' STRING atype               { UfCon (UfLitLitCon $2 $3) [] }

                | '__inline' core_expr               { UfNote UfInlineCall $2 }
                | '__coerce' atype core_expr         { UfNote (UfCoerce $2) $3 }
		| '__scc' core_expr                  { UfNote (UfSCC $1) $2  }
		| fexpr				     { $1 }

fexpr   :: { UfExpr RdrName }
fexpr   : fexpr core_arg				{ UfApp $1 $2 }
        | core_aexpr					{ $1 }

core_arg	:: { UfExpr RdrName }
		: '__a' atype                                  { UfType $2 }
                | core_aexpr                                    { $1 }

core_args	:: { [UfExpr RdrName] }
		:						{ [] }
		| core_arg core_args				{ $1 : $2 }

core_aexpr      :: { UfExpr RdrName }              -- Atomic expressions
core_aexpr      : qvar_name				        { UfVar $1 }

                | qdata_name                                    { UfVar $1 }
			-- This one means that e.g. "True" will parse as 
			-- (UfVar True_Id) rather than (UfCon True_Con []).
			-- No big deal; it'll be inlined in a jiffy.  I tried 
			-- parsing it to (Con con []) directly, but got bitten 
			-- when a real constructor Id showed up in an interface
			-- file.  As usual, a hack bites you in the end.
			-- If you want to get a UfCon, then use the
			-- curly-bracket notation (True {}).

		| core_lit		 { UfCon (UfLitCon $1) [] }
		| '(' core_expr ')'	 { $2 }
		| '('  ')'	 	 { UfTuple (mkTupConRdrName 0) [] }
		| '(' comma_exprs2 ')'	 { UfTuple (mkTupConRdrName (length $2)) $2 }
		| '(#' core_expr '#)'	 { UfTuple (mkUbxTupConRdrName 1) [$2] }
		| '(#' comma_exprs2 '#)' { UfTuple (mkUbxTupConRdrName (length $2)) $2 }

comma_exprs2	:: { [UfExpr RdrName] }	-- Two or more
comma_exprs2	: core_expr ',' core_expr			{ [$1,$3] }
		| core_expr ',' comma_exprs2			{ $1 : $3 }

con_or_primop   :: { UfCon RdrName }
con_or_primop   : qdata_name                    { UfDataCon $1 }
                | qvar_name			{ UfPrimOp $1 }
                | '__ccall' ccall_string      { let
						(is_casm, may_gc) = $1
					        in
						UfCCallOp $2 is_casm may_gc
						}

rec_binds	:: { [(UfBinder RdrName, UfExpr RdrName)] }
		:						{ [] }
		| core_val_bndr '=' core_expr ';' rec_binds	{ ($1,$3) : $5 }

core_alts	:: { [UfAlt RdrName] }
		: core_alt					{ [$1] }
		| core_alt ';' core_alts	                { $1 : $3 }

core_alt        :: { UfAlt RdrName }
core_alt	: core_pat '->' core_expr	{ (fst $1, snd $1, $3) }

core_pat	:: { (UfCon RdrName, [RdrName]) }
core_pat	: core_lit			{ (UfLitCon  $1, []) }
		| '__litlit' STRING atype	{ (UfLitLitCon $2 $3, []) }
		| qdata_name var_names		{ (UfDataCon $1, $2) }
		| '(' comma_var_names ')' 	{ (UfDataCon (mkTupConRdrName (length $2)), $2) }
		| '(#' comma_var_names1 '#)'	{ (UfDataCon (mkUbxTupConRdrName (length $2)), $2) }
		| '__DEFAULT'			{ (UfDefault, []) }
		| '(' core_pat ')'		{ $2 }


comma_var_names :: { [RdrName] }	-- Zero, or two or more
comma_var_names : 						{ [] }
		| var_name ',' comma_var_names1		{ $1 : $3 }

comma_var_names1 :: { [RdrName] }	-- One or more
comma_var_names1 : var_name					{ [$1] }
		 | var_name ',' comma_var_names1		{ $1 : $3 }

core_lit	:: { Literal }
core_lit	: INTEGER			{ mkMachInt_safe $1 }
		| CHAR				{ MachChar $1 }
		| STRING			{ MachStr $1 }
		| '__string' STRING		{ NoRepStr $2 (panic "NoRepStr type") }
		| RATIONAL			{ MachDouble $1 }
		| '__float' RATIONAL		{ MachFloat $2 }

		| '__integer' INTEGER		{ NoRepInteger  $2 (panic "NoRepInteger type") 
							-- The type checker will add the types
						}

		| '__rational' INTEGER INTEGER	{ NoRepRational ($2 % $3) 
				  		   (panic "NoRepRational type")
							-- The type checker will add the type
						}

		| '__addr' INTEGER		{ MachAddr $2 }

core_bndr       :: { UfBinder RdrName }
core_bndr       : core_val_bndr                                 { $1 }
                | core_tv_bndr                                  { $1 }

core_bndrs	:: { [UfBinder RdrName] }
core_bndrs	: 						{ [] }
		| core_bndr core_bndrs			        { $1 : $2 }

core_val_bndr 	:: { UfBinder RdrName }
core_val_bndr	: var_name '::' atype				{ UfValBinder $1 $3 }

core_tv_bndr	:: { UfBinder RdrName }
core_tv_bndr	:  '__a' tv_name '::' akind		{ UfTyBinder $2 $4 }
		|  '__a' tv_name			{ UfTyBinder $2 boxedTypeKind }

ccall_string	:: { FAST_STRING }
		: STRING					{ $1 }
		| VARID						{ $1 }
		| CONID						{ $1 }

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

mkConDecl name (ex_tvs, ex_ctxt) details loc = ConDecl name ex_tvs ex_ctxt details loc
}
