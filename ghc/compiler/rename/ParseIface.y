{-	Notes about the syntax of interface files
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The header
~~~~~~~~~~
  interface "edison" M 4 6 2 ! 406 	Module M, version 4, from package 'edison',
					Fixities version 6, rules version 2
					Interface syntax version 406
					! means M contains orphans

Import declarations
~~~~~~~~~~~~~~~~~~~
  import Foo ;				To compile M I used nothing from Foo, but it's 
					below me in the hierarchy

  import Foo ! @ ;			Ditto, but the ! means that Foo contains orphans
					and        the @ means that Foo is a boot interface

  import Foo :: 3 ;			To compile M I used everything from Foo, which has 
					module version 3

  import Foo :: 3 2 6 a 1 b 3 c 7 ;	To compile M I used Foo.  It had 
						module version 3
						fixity version 2
						rules  version 6
					and some specific things besides.

-}


{
module ParseIface ( parseIface, IfaceStuff(..) ) where

#include "HsVersions.h"

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsTypes		( mkHsForAllTy, mkHsUsForAllTy, mkHsTupCon )
import HsCore
import Demand		( mkStrictnessInfo )
import Literal		( Literal(..), mkMachInt, mkMachInt64, mkMachWord, mkMachWord64 )
import BasicTypes	( Fixity(..), FixityDirection(..), 
			  NewOrData(..), Version, initialVersion, Boxity(..)
			)
import CostCentre       ( CostCentre(..), IsCafCC(..), IsDupdCC(..) )
import CallConv         ( cCallConv )
import HsPragmas	( noDataPragmas, noClassPragmas )
import Type		( Kind, mkArrowKind, boxedTypeKind, openTypeKind, UsageAnn(..) )
import IdInfo           ( ArityInfo, exactArity, CprInfo(..), InlinePragInfo(..) )
import PrimOp           ( CCall(..), CCallTarget(..) )
import Lex		

import RnMonad		( ImportVersion, ParsedIface(..), WhatsImported(..),
			  RdrNamePragma, ExportItem, RdrAvailInfo, GenAvailInfo(..), 
                          WhetherHasOrphans, IsBootInterface
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import RdrName          ( RdrName, mkRdrUnqual, mkSysQual, mkSysUnqual, mkRdrNameWkr )
import Name		( OccName, Provenance )
import OccName          ( mkSysOccFS,
			  tcName, varName, ipName, dataName, clsName, tvName, uvName,
			  EncodedFS 
			)
import Module           ( ModuleName, PackageName, mkSysModuleFS, mkModule )			
import SrcLoc		( SrcLoc )
import CmdLineOpts	( opt_InPackage )
import Maybes
import Outputable
import List		( insert )

import GlaExts
import FastString	( tailFS )

#if __HASKELL1__ > 4
import Ratio ( (%) )
#endif
}

%name	    parseIface
%tokentype  { Token }
%monad	    { P }{ thenP }{ returnP }
%lexer      { lexer } { ITeof }

%token
 'as' 		{ ITas }
 'case' 	{ ITcase }  			-- Haskell keywords
 'class' 	{ ITclass } 
 'data' 	{ ITdata } 
 'default' 	{ ITdefault }
 'deriving' 	{ ITderiving }
 'do' 		{ ITdo }
 'else' 	{ ITelse }
 'hiding' 	{ IThiding }
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
 'qualified' 	{ ITqualified }
 'then' 	{ ITthen }
 'type' 	{ ITtype }
 'where' 	{ ITwhere }

 'forall'	{ ITforall }			-- GHC extension keywords
 'foreign'	{ ITforeign }
 'export'	{ ITexport }
 'label'	{ ITlabel } 
 'dynamic'	{ ITdynamic }
 'unsafe'	{ ITunsafe }
 'with'		{ ITwith }
 'stdcall'      { ITstdcallconv }
 'ccall'        { ITccallconv }

 '__interface'	{ ITinterface }			-- interface keywords
 '__export'	{ IT__export }
 '__depends'	{ ITdepends }
 '__forall'	{ IT__forall }
 '__letrec'	{ ITletrec }
 '__coerce'	{ ITcoerce }
 '__inline_me'  { ITinlineMe }
 '__inline_call'{ ITinlineCall }
 '__DEFAULT'	{ ITdefaultbranch }
 '__bot'	{ ITbottom }
 '__integer'	{ ITinteger_lit }
 '__float'	{ ITfloat_lit }
 '__word'	{ ITword_lit }
 '__int64'	{ ITint64_lit }
 '__word64'	{ ITword64_lit }
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
 '__M'		{ ITcprinfo }
 '__D'		{ ITdeprecated }

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
iface_stuff : iface		{ PIface   $1 }
      	    | type		{ PType    $1 }
      	    | id_info		{ PIdInfo  $1 }
	    | '__R' rules	{ PRules   $2 }
	    | '__D' deprecs	{ PDeprecs $2 }


iface		:: { ParsedIface }
iface		: '__interface' package mod_name 
			version sub_versions
			orphans checkVersion 'where'
		  exports_part
                  import_part
		  fix_decl_part
		  instance_decl_part
		  decls_part
		  rules_and_deprecs
		  { ParsedIface {
			pi_mod  = mkModule $3 $2,	-- Module itself
			pi_vers = $4, 			-- Module version
			pi_orphan  = $6,
			pi_exports = $9,    	   	-- Exports
			pi_usages  = $10,		-- Usages
			pi_fixity  = (fst $5,$11),	-- Fixies
			pi_insts   = $12,		-- Local instances
			pi_decls   = $13,		-- Decls
		 	pi_rules   = (snd $5,fst $14),	-- Rules 
		 	pi_deprecs = snd $14		-- Deprecations 
		   } }

-- Versions for fixities and rules (optional)
sub_versions :: { (Version,Version) }
	: '[' version version ']'		{ ($2,$3) }
	| {- empty -}				{ (initialVersion, initialVersion) }

--------------------------------------------------------------------------

import_part :: { [ImportVersion OccName] }
import_part :				    		  { [] }
	    |  import_decl import_part 			  { $1 : $2 }
	    
import_decl :: { ImportVersion OccName }
import_decl : 'import' mod_name orphans is_boot whats_imported ';'
			{ (mkSysModuleFS $2, $3, $4, $5) }

orphans		    :: { WhetherHasOrphans }
orphans		    : 						{ False }
		    | '!'					{ True }

is_boot		    :: { IsBootInterface }
is_boot		    : 						{ False }
		    | '@'					{ True }

whats_imported      :: { WhatsImported OccName }
whats_imported      :                                           	{ NothingAtAll }
		    | '::' version					{ Everything $2 }
                    | '::' version version version name_version_pairs   { Specifically $2 $3 $4 $5 }

name_version_pairs  ::	{ [(OccName, Version)] }
name_version_pairs  :  						{ [] }
		    |  name_version_pair name_version_pairs	{ $1 : $2 }

name_version_pair   ::	{ (OccName, Version) }
name_version_pair   :  var_occ version			        { ($1, $2) }
                    |  tc_occ  version                          { ($1, $2) }


--------------------------------------------------------------------------

exports_part	:: { [ExportItem] }
exports_part	:  					{ [] }
		| '__export' mod_name entities ';'
			exports_part 			{ (mkSysModuleFS $2, $3) : $5 }

entities	:: { [RdrAvailInfo] }
entities	: 					{ [] }
		|  entity entities			{ $1 : $2 }

entity		:: { RdrAvailInfo }
entity		:  var_occ				{ Avail $1 }
		|  tc_occ 				{ AvailTC $1 [$1] }
		|  tc_occ '|' stuff_inside		{ AvailTC $1 $3 }
		|  tc_occ stuff_inside		        { AvailTC $1 (insert $1 $2) }
		-- The 'insert' is important.  The stuff_inside is sorted, and
		-- insert keeps it that way.  This is important when comparing 
		-- against the new interface file, which has the stuff in sorted order
		-- If they differ, we'll bump the module number when it's unnecessary

stuff_inside	:: { [OccName] }
stuff_inside	:  '{' val_occs '}'			{ $2 }

val_occ		:: { OccName }
		:  var_occ 		{ $1 }
                |  data_occ             { $1 }

val_occs	:: { [OccName] }
		:  val_occ    		{ [$1] }
		|  val_occ val_occs	{ $1 : $2 }


--------------------------------------------------------------------------

fix_decl_part :: { [RdrNameFixitySig] }
fix_decl_part : {- empty -}				{ [] }
	      | fix_decls ';'				{ $1 }

fix_decls     :: { [RdrNameFixitySig] }
fix_decls     : 					{ [] }
	      | fix_decl fix_decls			{ $1 : $2 }

fix_decl :: { RdrNameFixitySig }
fix_decl : src_loc fixity prec var_or_data_name		{ FixitySig $4 (Fixity $3 $2) $1 }

fixity      :: { FixityDirection }
fixity      : 'infixl'                                  { InfixL }
            | 'infixr'                                  { InfixR }
            | 'infix'                                   { InfixN }
   
prec        :: { Int }
prec	    : INTEGER					{ fromInteger $1 }

-----------------------------------------------------------------------------

csigs		:: { [RdrNameSig] }
csigs		:  				{ [] }
		| 'where' '{' csigs1 '}'	{ $3 }

csigs1		:: { [RdrNameSig] }
csigs1		: 				{ [] }
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
	|  opt_version decl ';' decls_part 		{ ($1,$2):$4 }

decl	:: { RdrNameHsDecl }
decl    : src_loc var_name '::' type maybe_idinfo
  		 	 { SigD (IfaceSig $2 $4 ($5 $2) $1) }
	| src_loc 'type' tc_name tv_bndrs '=' type 		       
			{ TyClD (TySynonym $3 $4 $6 $1) }
	| src_loc 'data' opt_decl_context tc_name tv_bndrs constrs 	       
	       		{ TyClD (TyData DataType $3 $4 $5 $6 (length $6) Nothing noDataPragmas $1) }
	| src_loc 'newtype' opt_decl_context tc_name tv_bndrs newtype_constr
			{ TyClD (TyData NewType $3 $4 $5 $6 1 Nothing noDataPragmas $1) }
	| src_loc 'class' opt_decl_context tc_name tv_bndrs fds csigs
			{ TyClD (mkClassDecl $3 $4 $5 $6 $7 EmptyMonoBinds 
					noClassPragmas $1) }

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
	     core_args '=' core_expr	{ IfaceRule $2 $3 $4 $5 $7 $1 } 

rule_forall	:: { [UfBinder RdrName] }
rule_forall	: '__forall' '{' core_bndrs '}'	{ $3 }
		  
-----------------------------------------------------------------------------

deprecs 	:: { [RdrNameDeprecation] }
deprecs		: {- empty -}		{ [] }
		| deprec ';' deprecs	{ $1 : $3 }

deprec		:: { RdrNameDeprecation }
deprec		: src_loc STRING		{ Deprecation (IEModuleContents undefined) $2 $1 }
		| src_loc deprec_name STRING	{ Deprecation $2 $3 $1 }

-- SUP: TEMPORARY HACK
deprec_name	:: { RdrNameIE }
		: var_name		{ IEVar      $1 }
		| data_name		{ IEThingAbs $1 }

-----------------------------------------------------------------------------

version		:: { Version }
version		:  INTEGER			{ fromInteger $1 }

opt_version	:: { Version }
opt_version	: version			{ $1 }
		| {- empty -}			{ initialVersion }
	
opt_decl_context  :: { RdrNameContext }
opt_decl_context  :  				{ [] }
		  | context '=>'		{ $1 }

----------------------------------------------------------------------------

constrs		:: { [RdrNameConDecl] {- empty for handwritten abstract -} }
		: 			{ [] }
		| '=' constrs1		{ $2 }

constrs1	:: { [RdrNameConDecl] }
constrs1	:  constr		{ [$1] }
		|  constr '|' constrs1	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  src_loc ex_stuff data_name batypes		{ mk_con_decl $3 $2 (VanillaCon $4) $1 }
		|  src_loc ex_stuff data_name '{' fields1 '}'	{ mk_con_decl $3 $2 (RecCon $5)     $1 }
                -- We use "data_fs" so as to include ()

newtype_constr	:: { [RdrNameConDecl] {- Empty if handwritten abstract -} }
newtype_constr	:  					{ [] }
		| src_loc '=' ex_stuff data_name atype	{ [mk_con_decl $4 $3 (NewCon $5 Nothing) $1] }
		| src_loc '=' ex_stuff data_name '{' var_name '::' atype '}'
							{ [mk_con_decl $4 $3 (NewCon $8 (Just $6)) $1] }

ex_stuff :: { ([HsTyVarBndr RdrName], RdrNameContext) }
ex_stuff	:                                       { ([],[]) }
                | '__forall' tv_bndrs opt_context '=>'  { ($2,$3) }

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
                | '__forall' tv_bndrs 
			opt_context '=>' type	{ mkHsForAllTy (Just $2) $3 $5 }
		| btype '->' type		{ HsFunTy $1 $3 }
		| btype				{ $1 }

fuall		:: { [RdrName] }
fuall		: '[' uv_bndrs ']'		        { $2 }

opt_context	:: { RdrNameContext }
opt_context	:  					{ [] }
		| context 			        { $1 }

context		:: { RdrNameContext }
context		: '(' context_list1 ')'		        { $2 }
		| '{' context_list1 '}'		        { $2 }	-- Backward compatibility

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class ',' context_list1 		{ $1 : $3 }

class		:: { HsPred RdrName }
class		:  qcls_name atypes			{ (HsPClass $1 $2) }
		|  ipvar_name '::' type			{ (HsPIParam $1 $3) }

types0		:: { [RdrNameHsType] 			{- Zero or more -}  }	
types0		:  {- empty -}				{ [ ] }
		|  type					{ [ $1 ] }
		|  types2				{ $1 }

types2		:: { [RdrNameHsType] 			{- Two or more -}  }	
types2		:  type ',' type			{ [$1,$3] }
		|  type ',' types2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  btype atype				{ HsAppTy $1 $2 }
                |  '__u' usage atype			{ HsUsgTy $2 $3 }

usage		:: { HsUsageAnn RdrName }
usage		: '-' 					{ HsUsOnce }
		| '!' 					{ HsUsMany }
		| uv_name 				{ HsUsVar $1 }

atype		:: { RdrNameHsType }
atype		:  qtc_name 			  	{ HsTyVar $1 }
		|  tv_name			  	{ HsTyVar $1 }
	  	|  '(' ')' 				{ HsTupleTy (mkHsTupCon tcName Boxed   []) [] }
		|  '(' types2 ')'	  		{ HsTupleTy (mkHsTupCon tcName Boxed   $2) $2 }
		|  '(#' types0 '#)'			{ HsTupleTy (mkHsTupCon tcName Unboxed $2) $2 }
		|  '[' type ']'		  		{ HsListTy  $2 }
		|  '{' qcls_name atypes '}'		{ mkHsDictTy $2 $3 }
		|  '{' ipvar_name '::' type '}'		{ mkHsIParamTy $2 $4 }
		|  '(' type ')'		  		{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2 }
---------------------------------------------------------------------
package		:: { PackageName }
		:  STRING		{ $1 }
		| {- empty -}		{ opt_InPackage }	-- Useful for .hi-boot files,
								-- which can omit the package Id
								-- Module loops are always within a package

mod_name	:: { ModuleName }
		:  CONID		{ mkSysModuleFS $1 }


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
		| 'ccall' 		{ SLIT("ccall") }
		| 'stdcall' 		{ SLIT("stdcall") }

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

ipvar_name	:: { RdrName }
		:  IPVARID		{ mkSysUnqual ipName (tailFS $1) }

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

tv_bndr		:: { HsTyVarBndr RdrName }
		:  tv_name '::' akind	{ IfaceTyVar $1 $3 }
		|  tv_name		{ IfaceTyVar $1 boxedTypeKind }

tv_bndrs	:: { [HsTyVarBndr RdrName] }
tv_bndrs	: tv_bndrs1		{ $1 }
		| '[' tv_bndrs1 ']'	{ $2 } 	-- Backward compatibility

tv_bndrs1	:: { [HsTyVarBndr RdrName] }
		:  			{ [] }
		| tv_bndr tv_bndrs1	{ $1 : $2 }

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
		| '__M'				{ HsCprInfo }
		| '__S'				{ HsStrictness (mkStrictnessInfo $1) }
		| '__C'                         { HsNoCafRefs }
		| '__P' qvar_name 		{ HsWorker $2 }

inline_prag     :: { InlinePragInfo }
                :  {- empty -}                  { NoInlinePragInfo }
		| '[' from_prag phase ']'	{ IMustNotBeINLINEd $2 $3 }

from_prag	:: { Bool }
		: {- empty -}			{ True }
		| '!'				{ False }

phase		:: { Maybe Int }
		: {- empty -}			{ Nothing }
		| INTEGER 			{ Just (fromInteger $1) }

-------------------------------------------------------
core_expr	:: { UfExpr RdrName }
core_expr	: '\\' core_bndrs '->' core_expr	{ foldr UfLam $4 $2 }
		| 'case' core_expr 'of' var_name
		  '{' core_alts '}'		        { UfCase $2 $4 $6 }

		| 'let' '{' core_val_bndr '=' core_expr
		      '}' 'in' core_expr		{ UfLet (UfNonRec $3 $5) $8 }
		| '__letrec' '{' rec_binds '}'		
		  'in' core_expr			{ UfLet (UfRec $3) $6 }

                | '__litlit' STRING atype               { UfLitLit $2 $3 }

		| fexpr				        { $1 }

fexpr   :: { UfExpr RdrName }
fexpr   : fexpr core_arg				{ UfApp $1 $2 }
	| scc core_aexpr         	                { UfNote (UfSCC $1) $2  }
        | '__inline_me' core_aexpr 			{ UfNote UfInlineMe $2 }
        | '__inline_call' core_aexpr			{ UfNote UfInlineCall $2 }
        | '__coerce' atype core_aexpr			{ UfNote (UfCoerce $2) $3 }
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

		| core_lit		 { UfLit $1 }
		| '(' core_expr ')'	 { $2 }

		| '('  ')'	 	 { UfTuple (mkHsTupCon dataName Boxed [])   [] }
		| '(' comma_exprs2 ')'	 { UfTuple (mkHsTupCon dataName Boxed $2)   $2 }
		| '(#' comma_exprs0 '#)' { UfTuple (mkHsTupCon dataName Unboxed $2) $2 }

                | '{' '__ccall' ccall_string type '}'       
                           { let
                                 (is_dyn, is_casm, may_gc) = $2

			         target | is_dyn    = DynamicTarget (error "CCall dyn target bogus unique")
					| otherwise = StaticTarget $3

	                         ccall = CCall target is_casm may_gc cCallConv
 	                     in
			     UfCCall ccall $4
			   }


comma_exprs0    :: { [UfExpr RdrName] }	-- Zero or more
comma_exprs0	: {- empty -}			{ [ ] }
		| core_expr			{ [ $1 ] }
		| comma_exprs2			{ $1 }

comma_exprs2	:: { [UfExpr RdrName] }	-- Two or more
comma_exprs2	: core_expr ',' core_expr			{ [$1,$3] }
		| core_expr ',' comma_exprs2			{ $1 : $3 }

rec_binds	:: { [(UfBinder RdrName, UfExpr RdrName)] }
		:						{ [] }
		| core_val_bndr '=' core_expr ';' rec_binds	{ ($1,$3) : $5 }

core_alts	:: { [UfAlt RdrName] }
		: 						{ [] }
		| core_alt ';' core_alts	                { $1 : $3 }

core_alt        :: { UfAlt RdrName }
core_alt	: core_pat '->' core_expr	{ (fst $1, snd $1, $3) }

core_pat	:: { (UfConAlt RdrName, [RdrName]) }
core_pat	: core_lit			{ (UfLitAlt  $1, []) }
		| '__litlit' STRING atype	{ (UfLitLitAlt $2 $3, []) }
		| qdata_name core_pat_names	{ (UfDataAlt $1, $2) }
		| '('  ')'	 	 	{ (UfTupleAlt (mkHsTupCon dataName Boxed []),   []) }
		| '(' comma_var_names1 ')' 	{ (UfTupleAlt (mkHsTupCon dataName Boxed $2),   $2) }
		| '(#' comma_var_names1 '#)'	{ (UfTupleAlt (mkHsTupCon dataName Unboxed $2), $2) }
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
core_lit	: integer			{ mkMachInt $1 }
		| CHAR				{ MachChar $1 }
		| STRING			{ MachStr $1 }
		| rational			{ MachDouble $1 }
		| '__word' integer		{ mkMachWord $2 }
		| '__word64' integer		{ mkMachWord64 $2 }
		| '__int64' integer		{ mkMachInt64 $2 }
		| '__float' rational		{ MachFloat $2 }
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
        :  '__sccC' '{' mod_name '}'                      { AllCafsCC $3 }
        |  '__scc' '{' cc_name mod_name cc_dup cc_caf '}'
                             { NormalCC { cc_name = $3, cc_mod = $4,
                                          cc_is_dupd = $5, cc_is_caf = $6 } }

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

-- Check the project version: this makes sure
-- that the project version (e.g. 407) in the interface
-- file is the same as that for the compiler that's reading it
checkVersion :: { () }
	   : {-empty-}			{% checkVersion Nothing }
	   | INTEGER			{% checkVersion (Just (fromInteger $1)) }

------------------------------------------------------------------- 

--			Haskell code 
{
happyError :: P a
happyError buf PState{ loc = loc } = PFailed (ifaceParseErr buf loc)

data IfaceStuff = PIface 	ParsedIface
		| PIdInfo	[HsIdInfo RdrName]
		| PType		RdrNameHsType
		| PRules	[RdrNameRuleDecl]
		| PDeprecs	[RdrNameDeprecation]

mk_con_decl name (ex_tvs, ex_ctxt) details loc = mkConDecl name ex_tvs ex_ctxt details loc
}
