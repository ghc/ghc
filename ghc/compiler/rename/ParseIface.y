{-	Notes about the syntax of interface files		  -*-haskell-*-
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
module ParseIface ( parseIface, parseType, parseRules, parseIdInfo ) where

#include "HsVersions.h"

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsTypes		( mkHsForAllTy, mkHsTupCon )
import HsCore
import Literal		( Literal(..), mkMachInt, mkMachInt64, mkMachWord, mkMachWord64 )
import BasicTypes	( Fixity(..), FixityDirection(..), StrictnessMark(..),
			  NewOrData(..), Version, initialVersion, Boxity(..),
                          Activation(..), IPName(..)
			)
import CostCentre       ( CostCentre(..), IsCafCC(..), IsDupdCC(..) )
import Type		( Kind, mkArrowKind, liftedTypeKind, openTypeKind, usageTypeKind )
import ForeignCall	( ForeignCall(..), CCallConv(..), CCallSpec(..), CCallTarget(..) )
import Lex		

import RnMonad		( ParsedIface(..), ExportItem, IfaceDeprecs ) 
import HscTypes         ( WhetherHasOrphans, IsBootInterface, GenAvailInfo(..), 
                          ImportVersion, WhatsImported(..),
                          RdrAvailInfo )

import RdrName          ( RdrName, mkRdrUnqual, mkIfaceOrig )
import TyCon		( DataConDetails(..) )
import Name		( OccName )
import OccName          ( mkSysOccFS,
			  tcName, varName, dataName, clsName, tvName,
			  EncodedFS 
			)
import Module           ( ModuleName, PackageName, mkSysModuleNameFS )
import SrcLoc		( SrcLoc )
import CmdLineOpts	( opt_InPackage, opt_IgnoreIfacePragmas )
import Outputable
import Class            ( DefMeth (..) )

import GlaExts
import FastString	( tailFS )
}

%name	    parseIface      iface
%name	    parseType       type
%name	    parseIdInfo     id_info
%name	    parseRules      rules_and_deprecs

%tokentype  { Token }
%monad	    { P }{ thenP }{ returnP }
%lexer      { lexer } { ITeof }

%token
 'as' 		{ ITas }
 'case' 	{ ITcase }  			-- Haskell keywords
 'class' 	{ ITclass } 
 'data' 	{ ITdata } 
 'hiding' 	{ IThiding }
 'import' 	{ ITimport }
 'in' 		{ ITin }
 'infix' 	{ ITinfix }
 'infixl' 	{ ITinfixl }
 'infixr' 	{ ITinfixr }
 'instance' 	{ ITinstance }
 'let' 		{ ITlet }
 'newtype' 	{ ITnewtype }
 'of' 		{ ITof }
 'qualified' 	{ ITqualified }
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
 '__forall'	{ IT__forall }
 '__letrec'	{ ITletrec }
 '__coerce'	{ ITcoerce }
 '__inline_me'  { ITinlineMe }
 '__inline_call'{ ITinlineCall }
 '__DEFAULT'	{ ITdefaultbranch }
 '__float'	{ ITfloat_lit }
 '__word'	{ ITword_lit }
 '__int64'	{ ITint64_lit }
 '__word64'	{ ITword64_lit }
 '__addr'	{ ITaddr_lit }
 '__label'	{ ITlabel_lit }
 '__litlit'	{ ITlit_lit }
 '__ccall'	{ ITccall $$ }
 '__scc' 	{ ITscc }
 '__sccC'       { ITsccAllCafs }

 '__u'		{ ITusage }

 '__A'		{ ITarity }
 '__P'		{ ITspecialise }
 '__C'		{ ITnocaf }
 '__U'		{ ITunfold }
 '__S'		{ ITstrict $$ }
 '__R'		{ ITrules }
 '__D'		{ ITdeprecated }

 '::'		{ ITdcolon }
 '='		{ ITequal }
 '\\'		{ ITlam }
 '|'		{ ITvbar }
 '->'		{ ITrarrow }
 '@'		{ ITat }
 '~'		{ ITtilde }
 '=>'		{ ITdarrow }
 '-'		{ ITminus }
 '!'		{ ITbang }
 '*'		{ ITstar }

 '{'		{ ITocurly } 			-- special symbols
 '}'		{ ITccurly }
 '['		{ ITobrack }
 ']'		{ ITcbrack }
 '[:'		{ ITopabrack }
 ':]'		{ ITcpabrack }
 '('		{ IToparen }
 ')'		{ ITcparen }
 '(#'		{ IToubxparen }
 '#)'		{ ITcubxparen }
 ';'		{ ITsemi }
 ','		{ ITcomma }
 '.'		{ ITdot }

 VARID   	{ ITvarid    $$ }		-- identifiers
 CONID   	{ ITconid    $$ }
 VARSYM  	{ ITvarsym   $$ }
 QVARID  	{ ITqvarid   $$ }
 QCONID  	{ ITqconid   $$ }

 IPDUPVARID   	{ ITdupipvarid   $$ }		-- GHC extension
 IPSPLITVARID  	{ ITsplitipvarid $$ }		-- GHC extension

 PRAGMA		{ ITpragma   $$ }

 CHAR		{ ITchar     $$ }
 STRING		{ ITstring   $$ }
 INTEGER	{ ITinteger  $$ }
 RATIONAL	{ ITrational $$ }
 CLITLIT	{ ITlitlit   $$ }
%%

iface		:: { ParsedIface }
iface		: '__interface' package mod_name 
			version sub_versions
			orphans checkVersion 'where'
		  exports_part
                  import_part
		  fix_decl_part
		  instance_decl_part
		  decls_part
		  rules_and_deprecs_part
		  { let (rules,deprecs) = $14 () in
		    ParsedIface {
			pi_mod  = $3,			-- Module name
			pi_pkg = $2,			-- Package name
			pi_vers = $4, 			-- Module version
			pi_orphan  = $6,
			pi_exports = (fst $5, $9),    	-- Exports
			pi_usages  = $10,		-- Usages
			pi_fixity  = $11,		-- Fixies
			pi_insts   = $12,		-- Local instances
			pi_decls   = $13,		-- Decls
		 	pi_rules   = (snd $5,rules),	-- Rules 
		 	pi_deprecs = deprecs		-- Deprecations 
		   } }

-- Versions for exports and rules (optional)
sub_versions :: { (Version,Version) }
	: '[' version version ']'		{ ($2,$3) }
	| {- empty -}				{ (initialVersion, initialVersion) }

--------------------------------------------------------------------------

import_part :: { [ImportVersion OccName] }
import_part :				    		  { [] }
	    |  import_decl import_part 			  { $1 : $2 }
	    
import_decl :: { ImportVersion OccName }
import_decl : 'import' mod_name orphans is_boot whats_imported ';'
			{ ({-mkSysModuleNameFS-} $2, $3, $4, $5) }

orphans		    :: { WhetherHasOrphans }
orphans		    : 						{ False }
		    | '!'					{ True }

is_boot		    :: { IsBootInterface }
is_boot		    : 						{ False }
		    | '@'					{ True }

whats_imported      :: { WhatsImported OccName }
whats_imported      :                                           	{ NothingAtAll }
		    | '::' version					{ Everything $2 }
                    | '::' version version version name_version_pairs   { Specifically $2 (Just $3) $5 $4 }
                    | '::' version version name_version_pairs		{ Specifically $2 Nothing $4 $3 }

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
			exports_part 			{ ({-mkSysModuleNameFS-} $2, $3) : $5 }

entities	:: { [RdrAvailInfo] }
entities	: 					{ [] }
		|  entity entities			{ $1 : $2 }

entity		:: { RdrAvailInfo }
entity		:  var_occ				{ Avail $1 }
		|  tc_occ 				{ AvailTC $1 [$1] }
		|  tc_occ '|' stuff_inside		{ AvailTC $1 $3 }
		|  tc_occ stuff_inside		        { AvailTC $1 ($1:$2) }
		-- Note that the "main name" comes at the beginning

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
csig		:  src_loc qvar_name '::' type		{ ClassOpSig $2 NoDefMeth $4 $1 }
	        |  src_loc qvar_name ';' '::' type	{ ClassOpSig $2 GenDefMeth $5 $1 }		
	        |  src_loc qvar_name '=' '::' type	{ mkClassOpSigDM $2 $5 $1 }

--------------------------------------------------------------------------

instance_decl_part :: { [RdrNameInstDecl] }
instance_decl_part : {- empty -}		       { [] }
		   | instance_decl_part inst_decl      { $2 : $1 }

inst_decl	:: { RdrNameInstDecl }
inst_decl	:  src_loc 'instance' type '=' qvar_name ';'
			{ InstDecl $3
				   EmptyMonoBinds	{- No bindings -}
				   []    		{- No user pragmas -}
				   (Just $5)		{- Dfun id -}
				   $1
			}

--------------------------------------------------------------------------

decls_part :: { [(Version, RdrNameTyClDecl)] }
decls_part 
	:  {- empty -}				{ [] }
	|  opt_version decl ';' decls_part 		{ ($1,$2):$4 }

decl	:: { RdrNameTyClDecl }
decl    : src_loc qvar_name '::' type maybe_idinfo
  		 	{ IfaceSig $2 $4 ($5 $2) $1 }
	| src_loc 'type' qtc_name tv_bndrs '=' type 		       
			{ TySynonym $3 $4 $6 $1 }
	| src_loc 'foreign' 'type' qtc_name  		       
			{ ForeignType $4 Nothing DNType $1 }
	| src_loc 'data' tycl_hdr constrs 	       
	       		{ mkTyData DataType $3 $4 Nothing $1 }
	| src_loc 'newtype' tycl_hdr newtype_constr
			{ mkTyData NewType $3 (DataCons [$4]) Nothing $1 }
	| src_loc 'class' tycl_hdr fds csigs
			{ mkClassDecl $3 $4 $5 Nothing $1 }

tycl_hdr :: { (RdrNameContext, RdrName, [RdrNameHsTyVar]) }
	: context '=>' qtc_name tv_bndrs	{ ($1, $3, $4) }
	| qtc_name tv_bndrs			{ ([], $1, $2) }

maybe_idinfo  :: { RdrName -> [HsIdInfo RdrName] }
maybe_idinfo  : {- empty -} 	{ \_ -> [] }
	      | pragma		{ \x -> if opt_IgnoreIfacePragmas then [] 
					else case $1 of
						Just (POk _ id_info) -> id_info
						Just (PFailed err) -> pprPanic "IdInfo parse failed" 
								        (vcat [ppr x, err])
				}
    {-
      If a signature decl is being loaded, and opt_IgnoreIfacePragmas is on,
      we toss away unfolding information.

      Also, if the signature is loaded from a module we're importing from source,
      we do the same. This is to avoid situations when compiling a pair of mutually
      recursive modules, peering at unfolding info in the interface file of the other, 
      e.g., you compile A, it looks at B's interface file and may as a result change
      its interface file. Hence, B is recompiled, maybe changing its interface file,
      which will the unfolding info used in A to become invalid. Simple way out is to
      just ignore unfolding info.

      [Jan 99: I junked the second test above.  If we're importing from an hi-boot
       file there isn't going to *be* any pragma info.  The above comment
       dates from a time where we picked up a .hi file first if it existed.]
    -}

pragma	:: { Maybe (ParseResult [HsIdInfo RdrName]) }
pragma	: src_loc PRAGMA	{ let exts = ExtFlags {glasgowExtsEF = True,
						       parrEF	     = True}
				  in
				  Just (parseIdInfo $2 (mkPState $1 exts))
				}

-----------------------------------------------------------------------------

-- This production is lifted so that it doesn't get eagerly parsed when we
-- use happy --strict.
rules_and_deprecs_part :: { () -> ([RdrNameRuleDecl], IfaceDeprecs) }
rules_and_deprecs_part
  : {- empty -}		{ \_ -> ([], Nothing) }
  | src_loc PRAGMA	{ \_ -> let exts = ExtFlags {glasgowExtsEF = True,
						     parrEF	   = True}
				in case parseRules $2 (mkPState $1 exts) of
					POk _ rds   -> rds
					PFailed err -> pprPanic "Rules/Deprecations parse failed" err
			}

rules_and_deprecs :: { ([RdrNameRuleDecl], IfaceDeprecs) }
rules_and_deprecs : rule_prag deprec_prag	{ ($1, $2) }


-----------------------------------------------------------------------------

rule_prag :: { [RdrNameRuleDecl] }
rule_prag : {- empty -}			{ [] }
	  | '__R' rules			{ $2 }

rules	   :: { [RdrNameRuleDecl] }
	   : {- empty -}	{ [] }
	   | rule ';' rules	{ $1:$3 }

rule	   :: { RdrNameRuleDecl }
rule	   : src_loc STRING activation rule_forall qvar_name 
	     core_args '=' core_expr	{ IfaceRule $2 $3 $4 $5 $6 $8 $1 } 

activation :: { Activation }
activation : {- empty -}                { AlwaysActive }
           | '[' INTEGER ']'            { ActiveAfter (fromInteger $2) }
           | '[' '~' INTEGER ']'        { ActiveBefore (fromInteger $3) }

rule_forall	:: { [UfBinder RdrName] }
rule_forall	: '__forall' '{' core_bndrs '}'	{ $3 }
		  
-----------------------------------------------------------------------------

deprec_prag	:: { IfaceDeprecs }
deprec_prag	: {- empty -}		{ Nothing }
		| '__D' deprecs		{ Just $2 } 

deprecs 	:: { Either DeprecTxt [(RdrName,DeprecTxt)] }
deprecs		: STRING		{ Left $1 }
		| deprec_list		{ Right $1 }

deprec_list	:: { [(RdrName,DeprecTxt)] }
deprec_list	: deprec			{ [$1] }
		| deprec ';' deprec_list	{ $1 : $3 }

deprec		:: { (RdrName,DeprecTxt) }
deprec		: deprec_name STRING	{ ($1, $2) }

deprec_name	:: { RdrName }
		: qvar_name		{ $1 }
		| qtc_name		{ $1 }

-----------------------------------------------------------------------------

version		:: { Version }
version		:  INTEGER			{ fromInteger $1 }

opt_version	:: { Version }
opt_version	: version			{ $1 }
		| {- empty -}			{ initialVersion }
	

----------------------------------------------------------------------------

constrs		:: { DataConDetails RdrNameConDecl }
		: 			{ Unknown }
		| '=' 			{ DataCons [] }
		| '=' constrs1		{ DataCons $2 }

constrs1	:: { [RdrNameConDecl] }
constrs1	:  constr		{ [$1] }
		|  constr '|' constrs1	{ $1 : $3 }

constr		:: { RdrNameConDecl }
constr		:  src_loc ex_stuff qdata_name batypes		{ mk_con_decl $3 $2 (VanillaCon $4) $1 }
		|  src_loc ex_stuff qdata_name '{' fields1 '}'	{ mk_con_decl $3 $2 (RecCon $5)     $1 }
                -- We use "data_fs" so as to include ()

newtype_constr	:: { RdrNameConDecl }
newtype_constr	: src_loc '=' ex_stuff qdata_name atype	{ mk_con_decl $4 $3 (VanillaCon [unbangedType $5]) $1 }
		| src_loc '=' ex_stuff qdata_name '{' qvar_name '::' atype '}'
							{ mk_con_decl $4 $3 (RecCon [([$6], unbangedType $8)]) $1 }

ex_stuff :: { ([HsTyVarBndr RdrName], RdrNameContext) }
ex_stuff	:                                       { ([],[]) }
                | '__forall' tv_bndrs opt_context '=>'  { ($2,$3) }

batypes		:: { [RdrNameBangType] }
batypes		:  					{ [] }
		|  batype batypes			{ $1 : $2 }

batype		:: { RdrNameBangType }
batype		:  tatype				{ unbangedType $1 }
		|  '!' tatype				{ BangType MarkedStrict    $2 }
		|  '!' '!' tatype			{ BangType MarkedUnboxed   $3 }

fields1		:: { [([RdrName], RdrNameBangType)] }
fields1		: field					{ [$1] }
		| field ',' fields1			{ $1 : $3 }

field		:: { ([RdrName], RdrNameBangType) }
field		:  qvar_names1 '::' ttype		{ ($1, unbangedType $3) }
		|  qvar_names1 '::' '!' ttype    	{ ($1, BangType MarkedStrict    $4) }
		|  qvar_names1 '::' '!' '!' ttype	{ ($1, BangType MarkedUnboxed   $5) }

--------------------------------------------------------------------------

type		:: { RdrNameHsType }
type 		: '__forall' tv_bndrs 
			opt_context '=>' type	{ mkHsForAllTy (Just $2) $3 $5 }
		| btype '->' type		{ HsFunTy $1 $3 }
		| btype				{ $1 }

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
class		:  qcls_name atypes			{ (HsClassP $1 $2) }
		|  ipvar_name '::' type			{ (HsIParam $1 $3) }

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

atype		:: { RdrNameHsType }
atype		:  qtc_name 			  	{ HsTyVar $1 }
		|  tv_name			  	{ HsTyVar $1 }
		|  '.'					{ hsUsOnce }
		|  '!'					{ hsUsMany }
	  	|  '(' ')' 				{ HsTupleTy (mkHsTupCon tcName Boxed   []) [] }
		|  '(' types2 ')'	  		{ HsTupleTy (mkHsTupCon tcName Boxed   $2) $2 }
		|  '(#' types0 '#)'			{ HsTupleTy (mkHsTupCon tcName Unboxed $2) $2 }
		|  '[' type ']'		  		{ HsListTy  $2 }
		|  '[:' type ':]'			{ HsPArrTy $2 }
		|  '{' qcls_name atypes '}'		{ mkHsDictTy $2 $3 }
		|  '{' ipvar_name '::' type '}'		{ mkHsIParamTy $2 $4 }
		|  '(' type ')'		  		{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2 }
--------------------------------------------------------------------------

-- versions of type/btype/atype that cant begin with '!' (or '.')
-- for use where the kind is definitely known NOT to be '$'

ttype		:: { RdrNameHsType }
ttype 		: '__forall' tv_bndrs 
			opt_context '=>' type		{ mkHsForAllTy (Just $2) $3 $5 }
		| tbtype '->' type			{ HsFunTy $1 $3 }
		| tbtype				{ $1 }

tbtype		:: { RdrNameHsType }
tbtype		:  tatype				{ $1 }
		|  tbtype atype				{ HsAppTy $1 $2 }

tatype		:: { RdrNameHsType }
tatype		:  qtc_name 			  	{ HsTyVar $1 }
		|  tv_name			  	{ HsTyVar $1 }
	  	|  '(' ')' 				{ HsTupleTy (mkHsTupCon tcName Boxed   []) [] }
		|  '(' types2 ')'	  		{ HsTupleTy (mkHsTupCon tcName Boxed   $2) $2 }
		|  '(#' types0 '#)'			{ HsTupleTy (mkHsTupCon tcName Unboxed $2) $2 }
		|  '[' type ']'		  		{ HsListTy  $2 }
		|  '[:' type ':]'			{ HsPArrTy $2 }
		|  '{' qcls_name atypes '}'		{ mkHsDictTy $2 $3 }
		|  '{' ipvar_name '::' type '}'		{ mkHsIParamTy $2 $4 }
		|  '(' type ')'		  		{ $2 }
---------------------------------------------------------------------

package		:: { PackageName }
		:  STRING		{ $1 }
		| {- empty -}		{ opt_InPackage }
				-- Useful for .hi-boot files,
				-- which can omit the package Id
				-- Module loops are always within a package

mod_name	:: { ModuleName }
		:  CONID		{ mkSysModuleNameFS $1 }


---------------------------------------------------
var_fs          :: { EncodedFS }
		: VARID			{ $1 }
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

var_occ	        :: { OccName }
		:  var_fs		{ mkSysOccFS varName $1 }

var_name	:: { RdrName }
var_name	:  var_occ		{ mkRdrUnqual $1 }

qvar_name	:: { RdrName }
qvar_name	:  var_name		{ $1 }
		|  QVARID	      	{ mkIfaceOrig varName $1 }

ipvar_name	:: { IPName RdrName }
	        : IPDUPVARID		{ Dupable (mkRdrUnqual (mkSysOccFS varName $1)) }
	        | IPSPLITVARID		{ Linear  (mkRdrUnqual (mkSysOccFS varName $1)) }

qvar_names1	:: { [RdrName] }
qvar_names1	: qvar_name		{ [$1] }
		| qvar_name qvar_names1	{ $1 : $2 }

---------------------------------------------------

data_occ	:: { OccName }
		:  CONID		{ mkSysOccFS dataName $1 }

qdata_name	:: { RdrName }
		:  data_occ             { mkRdrUnqual $1 }
		|  QCONID 	        { mkIfaceOrig dataName $1 }
				
var_or_data_name :: { RdrName }
                  : qvar_name		{ $1 }
                  | qdata_name		{ $1 }

---------------------------------------------------
tc_occ	        :: { OccName }
		:  CONID 		{ mkSysOccFS tcName $1 }

qtc_name	:: { RdrName }
                : tc_occ		{ mkRdrUnqual $1 }
		| QCONID		{ mkIfaceOrig tcName $1 }

---------------------------------------------------
qcls_name	:: { RdrName }
         	: CONID			{ mkRdrUnqual (mkSysOccFS clsName $1) }
		| QCONID		{ mkIfaceOrig clsName $1 }

---------------------------------------------------
tv_name		:: { RdrName }
		:  var_fs 		{ mkRdrUnqual (mkSysOccFS tvName $1) }

tv_bndr		:: { HsTyVarBndr RdrName }
		:  tv_name '::' akind	{ IfaceTyVar $1 $3 }
		|  tv_name		{ IfaceTyVar $1 liftedTypeKind }

tv_bndrs	:: { [HsTyVarBndr RdrName] }
		: tv_bndrs1		{ $1 }
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
                : '*'                   { liftedTypeKind }
		| VARSYM		{ if $1 == SLIT("?") then
						openTypeKind
					  else if $1 == SLIT("\36") then
                                                usageTypeKind  -- dollar
                                          else panic "ParseInterface: akind"
					}
		| '(' kind ')'	{ $2 }

--------------------------------------------------------------------------

id_info		:: { [HsIdInfo RdrName] }
		: id_info_item 			{ [$1] }
		| id_info_item id_info		{ $1 : $2 }

id_info_item	:: { HsIdInfo RdrName }
		: '__A' INTEGER			{ HsArity (fromInteger $2) }
		| '__U' activation core_expr	{ HsUnfold $2 $3 }
		| '__S'				{ HsStrictness $1 }
		| '__C'                         { HsNoCafRefs }
		| '__P' qvar_name INTEGER	{ HsWorker $2 (fromInteger $3) }

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

			         target | is_dyn    = DynamicTarget
					| is_casm   = CasmTarget $3
					| otherwise = StaticTarget $3

	                         ccall = CCallSpec target CCallConv may_gc
 	                     in
			     UfFCall (CCall ccall) $4
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
		| '__label' STRING		{ MachLabel $2 }

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
		|  '@' tv_name			        { UfTyBinder $2 liftedTypeKind }

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

mk_con_decl name (ex_tvs, ex_ctxt) details loc = mkConDecl name ex_tvs ex_ctxt details loc
}
