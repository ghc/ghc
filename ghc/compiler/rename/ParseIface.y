{
module ParseIface ( parseIface, IfaceStuff(..) ) where

#include "HsVersions.h"

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsTypes		( mkHsForAllTy, mkHsUsForAllTy )
import HsCore
import Const		( Literal(..), mkMachInt_safe )
import BasicTypes	( Fixity(..), FixityDirection(..), 
			  NewOrData(..), Version
			)
import CostCentre       ( CostCentre(..), IsCafCC(..), IsDupdCC(..) )
import HsPragmas	( noDataPragmas, noClassPragmas )
import Type		( Kind, mkArrowKind, boxedTypeKind, openTypeKind, UsageAnn(..) )
import IdInfo           ( ArityInfo, exactArity, CprInfo(..), InlinePragInfo(..) )
import Lex		

import RnMonad		( ImportVersion, LocalVersion, ParsedIface(..), WhatsImported(..),
			  RdrNamePragma, ExportItem, RdrAvailInfo, GenAvailInfo(..), WhetherHasOrphans
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import RdrName          ( RdrName, mkRdrUnqual, mkSysQual, mkSysUnqual )
import Name		( OccName, Provenance )
import OccName          ( mkSysOccFS,
			  tcName, varName, ipName, dataName, clsName, tvName, uvName,
			  EncodedFS 
			)
import Module           ( ModuleName, mkSysModuleFS )			
import PrelMods         ( mkTupNameStr, mkUbxTupNameStr )
import PrelInfo         ( mkTupConRdrName, mkUbxTupConRdrName )
import SrcLoc		( SrcLoc )
import Maybes
import Outputable

import GlaExts

#if __HASKELL1__ > 4
import Ratio ( (%) )
#endif
}

%name	    parseIface
%tokentype  { Token }
%monad	    { P }{ thenP }{ returnP }
%lexer      { lexer } { ITeof }

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

 'forall'	{ ITforall }			-- GHC extension keywords
 'foreign'	{ ITforeign }
 'export'	{ ITexport }
 'label'	{ ITlabel } 
 'dynamic'	{ ITdynamic }
 'unsafe'	{ ITunsafe }
 'with'		{ ITwith }

 '__interface'	{ ITinterface }			-- interface keywords
 '__export'	{ IT__export }
 '__forall'	{ IT__forall }
 '__depends'	{ ITdepends }
 '__letrec'	{ ITletrec }
 '__coerce'	{ ITcoerce }
 '__inline_call'{ ITinlineCall }
 '__inline_me'  { ITinlineMe }
 '__DEFAULT'	{ ITdefaultbranch }
 '__bot'	{ ITbottom }
 '__integer'	{ ITinteger_lit }
 '__float'	{ ITfloat_lit }
 '__rational'	{ ITrational_lit }
 '__addr'	{ ITaddr_lit }
 '__litlit'	{ ITlit_lit }
 '__string'	{ ITstring_lit }
 '__ccall'	{ ITccall $$ }
 '__scc' 	{ ITscc }
 '__sccC'       { ITsccAllCafs }

 '__u'		{ ITusage }
 '__fuall'	{ ITfuall }

 '__A'		{ ITarity }
 '__P'		{ ITspecialise }
 '__C'		{ ITnocaf }
 '__U'		{ ITunfold $$ }
 '__S'		{ ITstrict $$ }
 '__R'		{ ITrules }
 '__D'		{ ITdeprecated }
 '__M'		{ ITcprinfo $$ }

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

 IPVARID   	{ ITipvarid  $$ }		-- GHC extension

 PRAGMA		{ ITpragma   $$ }

 CHAR		{ ITchar     $$ }
 STRING		{ ITstring   $$ }
 INTEGER	{ ITinteger  $$ }
 RATIONAL	{ ITrational $$ }
 CLITLIT	{ ITlitlit   $$ }

 UNKNOWN	{ ITunknown  $$ }
%%

-- iface_stuff is the main production.
-- It recognises (a) a whole interface file
--		 (b) a type (so that type sigs can be parsed lazily)
--		 (c) the IdInfo part of a signature (same reason)

iface_stuff :: { IfaceStuff }
iface_stuff : iface		{ let (nm, iff) = $1 in PIface nm iff }
      	    | type		{ PType    $1 }
      	    | id_info		{ PIdInfo  $1 }
	    | '__R' rules	{ PRules   $2 }
	    | '__D' deprecs	{ PDeprecs $2 }


iface		:: { (ModuleName, ParsedIface) }
iface		: '__interface' mod_fs INTEGER orphans checkVersion 'where'
		  exports_part
                  import_part
		  instance_decl_part
		  decls_part
		  rules_and_deprecs
		  { ( $2                        -- Module name
		    , ParsedIface {
			pi_mod = fromInteger $3, 	-- Module version
			pi_orphan  = $4,
			pi_exports = $7,        -- Exports
			pi_usages  = $8,	-- Usages
			pi_insts   = $9,	-- Local instances
			pi_decls   = $10,	-- Decls
		 	pi_rules   = fst $11,	-- Rules 
		 	pi_deprecs = snd $11	-- Deprecations 
		      } ) }

--------------------------------------------------------------------------

import_part :: { [ImportVersion OccName] }
import_part :				    		  { [] }
	    |  import_part import_decl			  { $2 : $1 }
	    
import_decl :: { ImportVersion OccName }
import_decl : 'import' mod_fs INTEGER orphans whats_imported ';'
			{ (mkSysModuleFS $2, fromInteger $3, $4, $5) }
	-- import Foo 3 :: a 1 b 3 c 7 ;	means import a,b,c from Foo
	-- import Foo 3	;			means import all of Foo
	-- import Foo 3 ! :: ...stuff... ;	the ! means that Foo contains orphans

orphans		    :: { WhetherHasOrphans }
orphans		    : 						{ False }
		    | '!'					{ True }

whats_imported      :: { WhatsImported OccName }
whats_imported      :                                           { Everything }
                    | '::' name_version_pairs      		{ Specifically $2 }

name_version_pairs  ::	{ [LocalVersion OccName] }
name_version_pairs  :  						{ [] }
		    |  name_version_pair name_version_pairs	{ $1 : $2 }

name_version_pair   ::	{ LocalVersion OccName }
name_version_pair   :  var_occ INTEGER			        { ($1, fromInteger $2) }
                    |  tc_occ  INTEGER                          { ($1, fromInteger $2) }


--------------------------------------------------------------------------

exports_part	:: { [ExportItem] }
exports_part	:  					{ [] }
		| exports_part '__export' 
		  mod_fs entities ';'			{ (mkSysModuleFS $3, $4) : $1 }

entities	:: { [RdrAvailInfo] }
entities	: 					{ [] }
		|  entity entities			{ $1 : $2 }

entity		:: { RdrAvailInfo }
entity		:  tc_occ 				{ AvailTC $1 [$1] }
		|  var_occ				{ Avail $1 }
		|  tc_occ stuff_inside		        { AvailTC $1 ($1:$2) }
		|  tc_occ '|' stuff_inside		{ AvailTC $1 $3 }

stuff_inside	:: { [OccName] }
stuff_inside	:  '{' val_occs '}'			{ $2 }

val_occ		:: { OccName }
		:  var_occ 		{ $1 }
                |  data_occ             { $1 }

val_occs	:: { [OccName] }
		:  val_occ    		{ [$1] }
		|  val_occ val_occs	{ $1 : $2 }


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
csig		:  src_loc var_name '::' type		{ mkClassOpSig False $2 $4 $1 }
	        |  src_loc var_name '=' '::' type	{ mkClassOpSig True  $2 $5 $1 }

--------------------------------------------------------------------------

instance_decl_part :: { [RdrNameInstDecl] }
instance_decl_part : {- empty -}		       { [] }
		   | instance_decl_part inst_decl      { $2 : $1 }

inst_decl	:: { RdrNameInstDecl }
inst_decl	:  src_loc 'instance' type '=' var_name ';'
			{ InstDecl $3
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   $5			{- Dfun id -}
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
	| src_loc 'data' decl_context tc_name tv_bndrs constrs 	       
	       		{ TyClD (TyData DataType $3 $4 $5 $6 Nothing noDataPragmas $1) }
	| src_loc 'newtype' decl_context tc_name tv_bndrs newtype_constr
			{ TyClD (TyData NewType $3 $4 $5 $6 Nothing noDataPragmas $1) }
	| src_loc 'class' decl_context tc_name tv_bndrs fds csigs
			{ TyClD (mkClassDecl $3 $4 $5 $6 $7 EmptyMonoBinds 
					noClassPragmas $1) }
        | src_loc fixity mb_fix var_or_data_name
                        { FixD (FixitySig $4 (Fixity $3 $2) $1) }

maybe_idinfo  :: { RdrName -> [HsIdInfo RdrName] }
maybe_idinfo  : {- empty -} 	{ \_ -> [] }
	      | pragma		{ \x -> case $1 of
				     POk _ (PIdInfo id_info) -> id_info
				     PFailed err -> 
				        pprPanic "IdInfo parse failed" 
				            (vcat [ppr x, err])
				}

pragma	:: { ParseResult IfaceStuff }
pragma	: src_loc PRAGMA	{ parseIface $2 PState{ bol = 0#, atbol = 1#,
							context = [],
							glasgow_exts = 1#,
							loc = $1 }
				}

-----------------------------------------------------------------------------

rules_and_deprecs :: { ([RdrNameRuleDecl], [RdrNameDeprecation]) }
rules_and_deprecs : {- empty -}	{ ([], []) }
		  | rules_and_deprecs rule_or_deprec
				{ let
				     append2 (xs1,ys1) (xs2,ys2) =
					(xs1 `app` xs2, ys1 `app` ys2)
				     xs `app` [] = xs -- performance paranoia
				     xs `app` ys = xs ++ ys
				  in append2 $1 $2
				}

rule_or_deprec :: { ([RdrNameRuleDecl], [RdrNameDeprecation]) }
rule_or_deprec : pragma	{ case $1 of
			     POk _ (PRules   rules)   -> (rules,[])
			     POk _ (PDeprecs deprecs) -> ([],deprecs)
			     PFailed err -> pprPanic "Rules/Deprecations parse failed" err
			}

-----------------------------------------------------------------------------

rules	   :: { [RdrNameRuleDecl] }
	   : {- empty -}	{ [] }
	   | rule ';' rules	{ $1:$3 }

rule	   :: { RdrNameRuleDecl }
rule	   : src_loc STRING rule_forall qvar_name 
	     core_args '=' core_expr	{ IfaceRuleDecl $4 (UfRuleBody $2 $3 $5 $7) $1 } 

rule_forall	:: { [UfBinder RdrName] }
rule_forall	: '__forall' '{' core_bndrs '}'	{ $3 }
		  
-----------------------------------------------------------------------------

deprecs 	:: { [RdrNameDeprecation] }
deprecs		: {- empty -}		{ [] }
		| deprecs deprec ';'	{ $2 : $1 }

deprec		:: { RdrNameDeprecation }
deprec		: STRING		{ Deprecation (IEModuleContents undefined) $1 }
		| deprec_name STRING	{ Deprecation $1 $2 }

-- SUP: TEMPORARY HACK
deprec_name	:: { RdrNameIE }
		: var_name		{ IEVar      $1 }
		| data_name		{ IEThingAbs $1 }

-----------------------------------------------------------------------------

version		:: { Version }
version		:  INTEGER				{ fromInteger $1 }

decl_context	:: { RdrNameContext }
decl_context	:  					{ [] }
		| '{' context_list1 '}' '=>'	{ $2 }

----------------------------------------------------------------------------

constrs		:: { [RdrNameConDecl] {- empty for handwritten abstract -} }
		: 			{ [] }
		| '=' constrs1		{ $2 }

constrs1	:: { [RdrNameConDecl] }
constrs1	:  constr		{ [$1] }
		|  constr '|' constrs1	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  src_loc ex_stuff data_name batypes		{ mkConDecl $3 $2 (VanillaCon $4) $1 }
		|  src_loc ex_stuff data_name '{' fields1 '}'	{ mkConDecl $3 $2 (RecCon $5)     $1 }
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
		|  '!' '!' atype			{ Unpacked $3 }

fields1		:: { [([RdrName], RdrNameBangType)] }
fields1		: field					{ [$1] }
		| field ',' fields1			{ $1 : $3 }

field		:: { ([RdrName], RdrNameBangType) }
field		:  var_names1 '::' type		{ ($1, Unbanged $3) }
		|  var_names1 '::' '!' type    	{ ($1, Banged   $4) }
		|  var_names1 '::' '!' '!' type	{ ($1, Unpacked $5) }
--------------------------------------------------------------------------

type		:: { RdrNameHsType }
type		: '__fuall'  fuall '=>' type    { mkHsUsForAllTy $2 $4 }
                | '__forall' forall context '=>' type	
						{ mkHsForAllTy (Just $2) $3 $5 }
		| btype '->' type		{ MonoFunTy $1 $3 }
		| btype				{ $1 }

fuall		:: { [RdrName] }
fuall		: '[' uv_bndrs ']'		        { $2 }

forall		:: { [HsTyVar RdrName] }
forall		: '[' tv_bndrs ']'		        { $2 }

context		:: { RdrNameContext }
context		:  					{ [] }
		| '{' context_list1 '}'		        { $2 }

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class ',' context_list1 		{ $1 : $3 }

class		:: { HsPred RdrName }
class		:  qcls_name atypes			{ (HsPClass $1 $2) }
		|  IPVARID '::' type			{ (HsPIParam (mkSysUnqual ipName $1) $3) }

types0		:: { [RdrNameHsType] 			{- Zero or more -}  }	
types0		:  {- empty -}				{ [ ] }
		|  type					{ [ $1 ] }
		|  types2				{ $1 }

types2		:: { [RdrNameHsType] 			{- Two or more -}  }	
types2		:  type ',' type			{ [$1,$3] }
		|  type ',' types2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  btype atype				{ MonoTyApp $1 $2 }
                |  '__u' usage atype			{ MonoUsgTy $2 $3 }

usage		:: { MonoUsageAnn RdrName }
usage		: '-' 					{ MonoUsOnce }
		| '!' 					{ MonoUsMany }
		| uv_name 				{ MonoUsVar $1 }

atype		:: { RdrNameHsType }
atype		:  qtc_name 			  	{ MonoTyVar $1 }
		|  tv_name			  	{ MonoTyVar $1 }
		|  '(' types2 ')'	  		{ MonoTupleTy $2 True{-boxed-} }
		|  '(#' types0 '#)'			{ MonoTupleTy $2 False{-unboxed-} }
		|  '[' type ']'		  		{ MonoListTy  $2 }
		|  '{' qcls_name atypes '}'		{ MonoDictTy $2 $3 }
		|  '{' IPVARID '::' type '}'		{ MonoIParamTy (mkSysUnqual ipName $2) $4 }
		|  '(' type ')'		  		{ $2 }

-- This one is dealt with via qtc_name
--	  	|  '(' ')' 				{ MonoTupleTy [] True }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2 }
---------------------------------------------------------------------
mod_fs	        :: { EncodedFS }
		:  CONID		{ $1 }

mod_name	:: { ModuleName }
		:  mod_fs		{ mkSysModuleFS $1 }


---------------------------------------------------
var_fs		:: { EncodedFS }
		: VARID			{ $1 }
		| '!'	  		{ SLIT("!") }
		| 'as'			{ SLIT("as") }
		| 'qualified'		{ SLIT("qualified") }
		| 'hiding'		{ SLIT("hiding") }
		| 'forall'		{ SLIT("forall") }
		| 'foreign'		{ SLIT("foreign") }
		| 'export'		{ SLIT("export") }
		| 'label'		{ SLIT("label") }
		| 'dynamic'		{ SLIT("dynamic") }
		| 'unsafe'		{ SLIT("unsafe") }
		| 'with'		{ SLIT("with") }

qvar_fs		:: { (EncodedFS, EncodedFS) }
		:  QVARID		{ $1 }
		|  QVARSYM		{ $1 }

var_occ	        :: { OccName }
		:  var_fs		{ mkSysOccFS varName $1 }

var_name	:: { RdrName }
var_name	:  var_occ		{ mkRdrUnqual $1 }

qvar_name	:: { RdrName }
qvar_name	:  var_name		{ $1 }
		|  qvar_fs      	{ mkSysQual varName $1 }

var_names	:: { [RdrName] }
var_names	: 			{ [] }
		| var_name var_names	{ $1 : $2 }

var_names1	:: { [RdrName] }
var_names1	: var_name var_names	{ $1 : $2 }

---------------------------------------------------
-- For some bizarre reason, 
--      (,,,)      is dealt with by the parser
--      Foo.(,,,)  is dealt with by the lexer
-- Sigh

data_fs	        :: { EncodedFS }
		:  CONID		{ $1 }
		|  CONSYM		{ $1 }

qdata_fs	:: { (EncodedFS, EncodedFS) }
                :  QCONID		{ $1 }
                |  QCONSYM		{ $1 }

data_occ	:: { OccName }
		:  data_fs		{ mkSysOccFS dataName $1 }

data_name	:: { RdrName }
                :  data_occ             { mkRdrUnqual $1 }

qdata_name	:: { RdrName }
qdata_name	:  data_name		{ $1 }
		|  qdata_fs 	        { mkSysQual dataName $1 }
				
qdata_names	:: { [RdrName] }
qdata_names	:				{ [] }
		| qdata_name qdata_names	{ $1 : $2 }

var_or_data_name :: { RdrName }
                  : var_name                    { $1 }
                  | data_name                   { $1 }

---------------------------------------------------
tc_fs           :: { EncodedFS }
                :  data_fs              { $1 }

tc_occ	        :: { OccName }
		:  tc_fs 		{ mkSysOccFS tcName $1 }

tc_name		:: { RdrName }
                :  tc_occ		{ mkRdrUnqual $1 }

qtc_name	:: { RdrName }
                : tc_name		{ $1 }
		| qdata_fs		{ mkSysQual tcName $1 }

---------------------------------------------------
cls_name	:: { RdrName }
        	:  data_fs		{ mkSysUnqual clsName $1 }

qcls_name	:: { RdrName }
         	: cls_name		{ $1 }
		| qdata_fs		{ mkSysQual clsName $1 }

---------------------------------------------------
uv_name		:: { RdrName }
		:  VARID 		{ mkSysUnqual uvName $1 }

uv_bndr		:: { RdrName }
		:  uv_name		{ $1 }

uv_bndrs	:: { [RdrName] }
		:  			{ [] }
		| uv_bndr uv_bndrs	{ $1 : $2 }

---------------------------------------------------
tv_name		:: { RdrName }
		:  VARID 		{ mkSysUnqual tvName $1 }
		|  VARSYM		{ mkSysUnqual tvName $1 {- Allow t2 as a tyvar -} }

tv_bndr		:: { HsTyVar RdrName }
		:  tv_name '::' akind	{ IfaceTyVar $1 $3 }
		|  tv_name		{ IfaceTyVar $1 boxedTypeKind }

tv_bndrs	:: { [HsTyVar RdrName] }
		:  			{ [] }
		| tv_bndr tv_bndrs	{ $1 : $2 }

---------------------------------------------------
fds :: { [([RdrName], [RdrName])] }
	: {- empty -}			{ [] }
	| '|' fds1			{ reverse $2 }

fds1 :: { [([RdrName], [RdrName])] }
	: fds1 ',' fd			{ $3 : $1 }
	| fd				{ [$1] }

fd :: { ([RdrName], [RdrName]) }
	: varids0 '->' varids0		{ (reverse $1, reverse $3) }

varids0	:: { [RdrName] }
	: {- empty -}			{ [] }
	| varids0 tv_name		{ $2 : $1 }

---------------------------------------------------
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
		: 	 			{ [] }
		| id_info_item id_info		{ $1 : $2 }

id_info_item	:: { HsIdInfo RdrName }
		: '__A' INTEGER			{ HsArity (exactArity (fromInteger $2)) }
		| '__U' inline_prag core_expr	{ HsUnfold $2 $3 }
		| '__M'				{ HsCprInfo $1 }
		| '__S'				{ HsStrictness (HsStrictnessInfo $1) }
		| '__C'                         { HsNoCafRefs }
		| '__P' qvar_name 		{ HsWorker $2 }

inline_prag     :: { InlinePragInfo }
                :  {- empty -}                  { NoInlinePragInfo }
                | '[' INTEGER ']'               { IMustNotBeINLINEd True  (Just (fromInteger $2)) } -- INLINE n
                | '[' '!' INTEGER ']'           { IMustNotBeINLINEd False (Just (fromInteger $3)) } -- NOINLINE n

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

                | '__inline_me' core_expr               { UfNote UfInlineMe $2 }
                | '__inline_call' core_expr             { UfNote UfInlineCall $2 }
                | '__coerce' atype core_expr            { UfNote (UfCoerce $2) $3 }
		| scc core_expr                         { UfNote (UfSCC $1) $2  }
		| fexpr				        { $1 }

fexpr   :: { UfExpr RdrName }
fexpr   : fexpr core_arg				{ UfApp $1 $2 }
        | core_aexpr					{ $1 }

core_arg	:: { UfExpr RdrName }
		: '@' atype                                     { UfType $2 }
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
		| '(' comma_exprs2 ')'	 { UfTuple (mkTupConRdrName (length $2)) $2 }
		| '(#' comma_exprs0 '#)' { UfTuple (mkUbxTupConRdrName (length $2)) $2 }

-- This one is dealt with by qdata_name: see above comments
--		| '('  ')'	 	 { UfTuple (mkTupConRdrName 0) [] }

comma_exprs0    :: { [UfExpr RdrName] }	-- Zero or more
comma_exprs0	: {- empty -}			{ [ ] }
		| core_expr			{ [ $1 ] }
		| comma_exprs2			{ $1 }

comma_exprs2	:: { [UfExpr RdrName] }	-- Two or more
comma_exprs2	: core_expr ',' core_expr			{ [$1,$3] }
		| core_expr ',' comma_exprs2			{ $1 : $3 }

con_or_primop   :: { UfCon RdrName }
con_or_primop   : qdata_name                    { UfDataCon $1 }
                | qvar_name			{ UfPrimOp $1 }
                | '__ccall' ccall_string      { let
						(is_dyn, is_casm, may_gc) = $1
					        in
						UfCCallOp $2 is_dyn is_casm may_gc
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
		| qdata_name core_pat_names	{ (UfDataCon $1, $2) }
		| '(' comma_var_names1 ')' 	{ (UfDataCon (mkTupConRdrName (length $2)), $2) }
		| '(#' comma_var_names1 '#)'	{ (UfDataCon (mkUbxTupConRdrName (length $2)), $2) }
		| '__DEFAULT'			{ (UfDefault, []) }
		| '(' core_pat ')'		{ $2 }

core_pat_names :: { [RdrName] }
core_pat_names : 				{ [] }
		| core_pat_name core_pat_names	{ $1 : $2 }

-- Tyvar names and variable names live in different name spaces
-- so they need to be signalled separately.  But we don't record 
-- types or kinds in a pattern; we work that out from the type 
-- of the case scrutinee
core_pat_name	:: { RdrName }
core_pat_name	: var_name			{ $1 }
		| '@' tv_name			{ $2 }
	
comma_var_names1 :: { [RdrName] }	-- One or more
comma_var_names1 : var_name					{ [$1] }
		 | var_name ',' comma_var_names1		{ $1 : $3 }

core_lit	:: { Literal }
core_lit	: integer			{ mkMachInt_safe $1 }
		| CHAR				{ MachChar $1 }
		| STRING			{ MachStr $1 }
		| '__string' STRING		{ NoRepStr $2 (panic "NoRepStr type") }
		| rational			{ MachDouble $1 }
		| '__float' rational		{ MachFloat $2 }

		| '__integer' integer		{ NoRepInteger  $2 (panic "NoRepInteger type") 
							-- The type checker will add the types
						}

		| '__rational' integer integer	{ NoRepRational ($2 % $3) 
				  		   (panic "NoRepRational type")
							-- The type checker will add the type
						}

		| '__addr' integer		{ MachAddr $2 }

integer		:: { Integer }
		: INTEGER                       { $1 }
		| '-' INTEGER		        { (-$2) }

rational	:: { Rational }
		: RATIONAL                      { $1 }
		| '-' RATIONAL			{ (-$2) }

core_bndr       :: { UfBinder RdrName }
core_bndr       : core_val_bndr                                 { $1 }
                | core_tv_bndr                                  { $1 }

core_bndrs	:: { [UfBinder RdrName] }
core_bndrs	: 						{ [] }
		| core_bndr core_bndrs			        { $1 : $2 }

core_val_bndr 	:: { UfBinder RdrName }
core_val_bndr	: var_name '::' atype				{ UfValBinder $1 $3 }

core_tv_bndr	:: { UfBinder RdrName }
core_tv_bndr	:  '@' tv_name '::' akind		{ UfTyBinder $2 $4 }
		|  '@' tv_name			        { UfTyBinder $2 boxedTypeKind }

ccall_string	:: { FAST_STRING }
		: STRING					{ $1 }
		| CLITLIT					{ $1 }
		| VARID						{ $1 }
		| CONID						{ $1 }

------------------------------------------------------------------------
scc     :: { CostCentre }
        :  '__sccC' '{' mod_name STRING '}'                      { AllCafsCC $3 $4 }
        |  '__scc' '{' cc_name mod_name STRING cc_dup cc_caf '}'
                             { NormalCC { cc_name = $3, cc_mod = $4, cc_grp = $5,
                                          cc_is_dupd = $6, cc_is_caf = $7 } }

cc_name :: { EncodedFS }
        : CONID                 { $1 }
        | var_fs                { $1 }
  
cc_dup  :: { IsDupdCC }
cc_dup  :                       { OriginalCC }
        | '!'                   { DupdCC }

cc_caf  :: { IsCafCC }
        :                       { NotCafCC }
        | '__C'                 { CafCC }

-------------------------------------------------------------------

src_loc :: { SrcLoc }
src_loc : 				{% getSrcLocP }

checkVersion :: { () }
	   : {-empty-}			{% checkVersion Nothing }
	   | INTEGER			{% checkVersion (Just (fromInteger $1)) }

------------------------------------------------------------------- 

--			Haskell code 
{
happyError :: P a
happyError buf PState{ loc = loc } = PFailed (ifaceParseErr buf loc)

data IfaceStuff = PIface 	EncodedFS{-.hi module name-} ParsedIface
		| PIdInfo	[HsIdInfo RdrName]
		| PType		RdrNameHsType
		| PRules	[RdrNameRuleDecl]
		| PDeprecs	[RdrNameDeprecation]

mkConDecl name (ex_tvs, ex_ctxt) details loc = ConDecl name ex_tvs ex_ctxt details loc
}
