{
#include "HsVersions.h"

module ParseIface ( parseIface ) where

IMP_Ubiq(){-uitous-}

import ParseUtils

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsPragmas	( noGenPragmas )

import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( ExportFlag(..), mkTupNameStr, preludeQual,
			  RdrName(..){-instance Outputable:ToDo:rm-}
			)
import Outputable	-- ToDo:rm
import PprStyle		( PprStyle(..) ) -- ToDo: rm debugging
import SrcLoc		( mkIfaceSrcLoc )
import Util		( panic, pprPanic{-ToDo:rm-} )

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
	DCCURLY		    { ITdccurly }
	CLASS		    { ITclass }
	COMMA		    { ITcomma }
	CPAREN		    { ITcparen }
	DARROW		    { ITdarrow }
	DATA		    { ITdata }
	DCOLON		    { ITdcolon }
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
	DOCURLY		    { ITdocurly }
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
%%

iface		:: { ParsedIface }
iface		: INTERFACE CONID INTEGER
		  usages_part versions_part
		  exports_part inst_modules_part
		  fixities_part decls_part instances_part pragmas_part
		  { case $9 of { (tm, vm) ->
		    ParsedIface $2 (panic "merge modules") (fromInteger $3) Nothing{-src version-}
			$4  -- usages
			$5  -- local versions
			$6  -- exports map
			$7  -- instance modules
			$8  -- fixities map
			tm  -- decls maps
			vm
			$10  -- local instances
			$11 -- pragmas map
		    }
--------------------------------------------------------------------------
		  }

usages_part	    :: { UsagesMap }
usages_part	    :  USAGES_PART module_stuff_pairs	{ bagToFM $2 }
		    |					{ emptyFM }

versions_part	    :: { VersionsMap }
versions_part	    :  VERSIONS_PART name_version_pairs { bagToFM $2 }
		    |					{ emptyFM }

module_stuff_pairs  :: { Bag (Module, (Version, FiniteMap FAST_STRING Version)) }
module_stuff_pairs  :  module_stuff_pair
			{ unitBag $1 }
		    |  module_stuff_pairs module_stuff_pair
			{ $1 `snocBag` $2 }

module_stuff_pair   ::  { (Module, (Version, FiniteMap FAST_STRING Version)) }
module_stuff_pair   :  CONID INTEGER DCOLON name_version_pairs SEMI
			{ ($1, (fromInteger $2, bagToFM $4)) }

name_version_pairs  ::	{ Bag (FAST_STRING, Int) }
name_version_pairs  :  name_version_pair
			{ unitBag $1 }
		    |  name_version_pairs name_version_pair
			{ $1 `snocBag` $2 }

name_version_pair   ::	{ (FAST_STRING, Int) }
name_version_pair   :  name INTEGER
			{ ($1, fromInteger $2)
--------------------------------------------------------------------------
			}

exports_part	:: { ExportsMap }
exports_part	:  EXPORTS_PART export_items { bagToFM $2 }
		|			     { emptyFM }

export_items	:: { Bag (FAST_STRING, (OrigName, ExportFlag)) }
export_items	:  export_item		    { unitBag $1 }
		|  export_items export_item { $1 `snocBag` $2 }

export_item	:: { (FAST_STRING, (OrigName, ExportFlag)) }
export_item	:  CONID name maybe_dotdot { ($2, (OrigName $1 $2, $3)) }

maybe_dotdot	:: { ExportFlag }
maybe_dotdot	:  DOTDOT { ExportAll }
		|	  { ExportAbs
--------------------------------------------------------------------------
			  }

inst_modules_part :: { Bag Module }
inst_modules_part :  INSTANCE_MODULES_PART mod_list { $2 }
		  |				    { emptyBag }

mod_list	:: { Bag Module }
mod_list	:  CONID	  { unitBag $1 }
		|  mod_list CONID { $1 `snocBag` $2
--------------------------------------------------------------------------
				  }

fixities_part	:: { FixitiesMap }
fixities_part	:  FIXITIES_PART fixes	{ $2 }
		|			{ emptyFM }

fixes		:: { FixitiesMap }
fixes		:  fix  	{ case $1 of (k,v) -> unitFM k v }
		|  fixes fix	{ case $2 of (k,v) -> addToFM $1 k v }

fix		:: { (FAST_STRING, RdrNameFixityDecl) }
fix		:  INFIXL INTEGER qname SEMI { (de_qual $3, InfixL $3 (fromInteger $2)) }
		|  INFIXR INTEGER qname SEMI { (de_qual $3, InfixR $3 (fromInteger $2)) }
		|  INFIX  INTEGER qname SEMI { (de_qual $3, InfixN $3 (fromInteger $2))
--------------------------------------------------------------------------
				      }

decls_part	:: { (LocalTyDefsMap, LocalValDefsMap) }
decls_part	: DECLARATIONS_PART topdecls { $2 }
		|			     { (emptyFM, emptyFM) }

topdecls	:: { (LocalTyDefsMap, LocalValDefsMap) }
topdecls	:  topdecl	    { $1 }
		|  topdecls topdecl { case $1 of { (ts1, vs1) ->
				      case $2 of { (ts2, vs2) ->
				      (plusFM ts1 ts2, plusFM vs1 vs2)}}
				     }

topdecl		:: { (LocalTyDefsMap, LocalValDefsMap) }
topdecl		:  typed  SEMI	{ ($1, emptyFM) }
		|  datad  SEMI	{ $1 }
		|  newtd  SEMI	{ $1 }
		|  classd SEMI	{ $1 }
		|  decl		{ case $1 of { (n, Sig qn ty _ loc) ->
				  (emptyFM, unitFM n (ValSig qn loc ty)) }
				}

typed		:: { LocalTyDefsMap }
typed		:  TYPE simple EQUAL type	{ mk_type $2 $4 }

datad		:: { (LocalTyDefsMap, LocalValDefsMap) }
datad		:  DATA		       simple EQUAL constrs { mk_data [] $2 $4 }
		|  DATA context DARROW simple EQUAL constrs { mk_data $2 $4 $6 }

newtd		:: { (LocalTyDefsMap, LocalValDefsMap) }
newtd		:  NEWTYPE		  simple EQUAL constr1 { mk_new [] $2 $4 }
		|  NEWTYPE context DARROW simple EQUAL constr1 { mk_new $2 $4 $6 }

classd		:: { (LocalTyDefsMap, LocalValDefsMap) }
classd		:  CLASS		class cbody { mk_class [] $2 $3 }
		|  CLASS context DARROW class cbody { mk_class $2 $4 $5 }

cbody		:: { [(FAST_STRING, RdrNameSig)] }
cbody		:  WHERE OCURLY decls CCURLY { $3 }
		|			     { [] }

decls		:: { [(FAST_STRING, RdrNameSig)] }
decls		: decl		{ [$1] }
		| decls decl	{ $1 ++ [$2] }

decl		:: { (FAST_STRING, RdrNameSig) }
decl		:  var DCOLON ctype SEMI { (de_qual $1, Sig $1 $3 noGenPragmas mkIfaceSrcLoc) }

context		:: { RdrNameContext }
context		:  DOCURLY context_list DCCURLY	{ reverse $2 }

context_list	:: { RdrNameContext{-reversed-} }
context_list	:  class			{ [$1] }
		|  context_list COMMA class	{ $3 : $1 }

class		:: { (RdrName, RdrName) }
class		:  gtycon VARID			{ ($1, Unqual $2) }

ctype		:: { RdrNamePolyType }
ctype		: FORALL OBRACK tyvars CBRACK context DARROW type  { HsForAllTy (map Unqual $3) $5 $7 }
		| FORALL OBRACK tyvars CBRACK type		       { HsForAllTy (map Unqual $3) [] $5 }
		| context DARROW type	{{-ToDo:rm-} HsPreForAllTy $1 $3 }
		| type		        {{-ToDo:change-} HsPreForAllTy [] $1 }

type		:: { RdrNameMonoType }
type		:  btype		{ $1 }
		|  btype RARROW type	{ MonoFunTy $1 $3 }

types		:: { [RdrNameMonoType] }
types		:  type			{ [$1] }
		|  types COMMA type	{ $1 ++ [$3] }

btype		:: { RdrNameMonoType }
btype		:  gtyconapp		{ case $1 of (tc, tys) -> MonoTyApp tc tys }
		|  ntyconapp		{ case $1 of { (ty1, tys) ->
					  if null tys
					  then ty1
					  else
					  case ty1 of {
					    MonoTyVar tv    -> MonoTyApp tv tys;
					    MonoTyApp tc ts -> MonoTyApp tc (ts++tys);
					    MonoFunTy t1 t2 -> MonoTyApp (preludeQual SLIT("->")) (t1:t2:tys);
					    MonoListTy ty   -> MonoTyApp (preludeQual SLIT("[]")) (ty:tys);
					    MonoTupleTy ts  -> MonoTyApp (preludeQual (mkTupNameStr (length ts)))
									 (ts++tys);
					    _		    -> pprPanic "test:" (ppr PprDebug $1)
					  }}
					}

ntyconapp	:: { (RdrNameMonoType, [RdrNameMonoType]) }
ntyconapp	: ntycon		{ ($1, []) }
		| ntyconapp atype	{ case $1 of (t1,tys) -> (t1, tys ++ [$2]) }

gtyconapp	:: { (RdrName, [RdrNameMonoType]) }
gtyconapp	: gtycon		{ ($1, []) }
		| gtyconapp atype	{ case $1 of (tc,tys) -> (tc, tys ++ [$2]) }

atype		:: { RdrNameMonoType }
atype		:  gtycon		{ MonoTyApp $1 [] }
		|  ntycon		{ $1 }

atypes		:: { [RdrNameMonoType] }
atypes		:  atype		{ [$1] }
		|  atypes atype		{ $1 ++ [$2] }

ntycon		:: { RdrNameMonoType }
ntycon		:  VARID			  { MonoTyVar (Unqual $1) }
		|  OPAREN type COMMA types CPAREN { MonoTupleTy ($2 : $4) }
		|  OBRACK type CBRACK		  { MonoListTy $2 }
		|  OPAREN type CPAREN		  { $2 }

gtycon		:: { RdrName }
gtycon		:  QCONID		{ $1 }
		|  OPAREN RARROW CPAREN	{ preludeQual SLIT("->") }
		|  OBRACK CBRACK	{ preludeQual SLIT("[]") }
		|  OPAREN CPAREN	{ preludeQual SLIT("()") }
		|  OPAREN commas CPAREN	{ preludeQual (mkTupNameStr $2) }

commas		:: { Int }
commas		:  COMMA		{ 2{-1 comma => arity 2-} }
		|  commas COMMA		{ $1 + 1 }

simple		:: { (RdrName, [FAST_STRING]) }
simple		:  gtycon	{ ($1, []) }
		|  gtyconvars   { case $1 of (tc,tvs) -> (tc, reverse tvs) }

gtyconvars	:: { (RdrName, [FAST_STRING] {-reversed-}) }
gtyconvars	:  gtycon     VARID { ($1, [$2]) }
		|  gtyconvars VARID { case $1 of (tc,tvs) -> (tc, $2 : tvs) }

constrs		:: { [(RdrName, RdrNameConDecl)] }
constrs		:  constr		{ [$1] }
		|  constrs VBAR constr	{ $1 ++ [$3] }

constr		:: { (RdrName, RdrNameConDecl) }
constr		:  btyconapp
		   { case $1 of (con, tys) -> (con, ConDecl con tys mkIfaceSrcLoc) }
		|  QCONSYM	   { ($1, ConDecl $1 [] mkIfaceSrcLoc) }
		|  QCONSYM batypes { ($1, ConDecl $1 $2 mkIfaceSrcLoc) }
		|  gtycon OCURLY fields CCURLY
		   { ($1, RecConDecl $1 $3 mkIfaceSrcLoc) }

btyconapp	:: { (RdrName, [RdrNameBangType]) }
btyconapp	:  gtycon			{ ($1, []) }
		|  btyconapp batype		{ case $1 of (tc,tys) -> (tc, tys ++ [$2]) }

batype		:: { RdrNameBangType }
batype		:  atype			{ Unbanged (HsForAllTy [{-ToDo:tvs-}] [] $1) }
		|  BANG atype			{ Banged   (HsForAllTy [{-ToDo:tvs-}] [] $2) }

batypes		:: { [RdrNameBangType] }
batypes		:  batype			{ [$1] }
		|  batypes batype		{ $1 ++ [$2] }

fields		:: { [([RdrName], RdrNameBangType)] }
fields		: field				{ [$1] }
		| fields COMMA field		{ $1 ++ [$3] }

field		:: { ([RdrName], RdrNameBangType) }
field		:  var DCOLON type	    { ([$1], Unbanged (HsForAllTy [{-ToDo:tvs-}] [] $3)) }
		|  var DCOLON BANG atype    { ([$1], Banged   (HsForAllTy [{-ToDo:tvs-}] [] $4)) }

constr1		:: { (RdrName, RdrNameMonoType) }
constr1		:  gtycon atype	{ ($1, $2) }

var		:: { RdrName }
var		:  QVARID		{ $1 }
		|  QVARSYM		{ $1 }

qname		:: { RdrName }
qname		:  QVARID		{ $1 }
		|  QCONID		{ $1 }
		|  QVARSYM		{ $1 }
		|  QCONSYM		{ $1 }

name		:: { FAST_STRING }
name		:  VARID		{ $1 }
		|  CONID		{ $1 }
		|  VARSYM		{ $1 }
		|  BANG  		{ SLIT("!"){-sigh, double-sigh-} }
		|  CONSYM		{ $1 }  
		|  OBRACK CBRACK	{ SLIT("[]") }
		|  OPAREN CPAREN	{ SLIT("()") }
		|  OPAREN commas CPAREN	{ mkTupNameStr $2 }

instances_part	:: { Bag RdrIfaceInst }
instances_part	:  INSTANCES_PART instdecls { $2 }
		|			    { emptyBag }

instdecls	:: { Bag RdrIfaceInst }
instdecls	:  instd		    { unitBag $1 }
		|  instdecls instd	    { $1 `snocBag` $2 }

instd		:: { RdrIfaceInst }
instd		:  INSTANCE FORALL OBRACK tyvars CBRACK context DARROW gtycon restrict_inst SEMI { mk_inst (Just (map Unqual $4)) $6 $8 $9 }
		|  INSTANCE FORALL OBRACK tyvars CBRACK		       gtycon general_inst  SEMI { mk_inst (Just (map Unqual $4)) [] $6 $7 }
		|  INSTANCE context DARROW gtycon restrict_inst	SEMI {{-ToDo:rm-} mk_inst Nothing $2 $4 $5 }
		|  INSTANCE		   gtycon general_inst	SEMI {{-ToDo:rm-} mk_inst Nothing [] $2 $3 }

restrict_inst	:: { RdrNameMonoType }
restrict_inst	:  gtycon				{ MonoTyApp $1 [] }
		|  OPAREN gtyconvars CPAREN		{ case $2 of (tc,tvs) -> MonoTyApp tc (map en_mono (reverse tvs)) }
		|  OPAREN VARID COMMA tyvars CPAREN	{ MonoTupleTy (map en_mono ($2:$4)) }
		|  OBRACK VARID CBRACK			{ MonoListTy (en_mono $2) }
		|  OPAREN VARID RARROW VARID CPAREN	{ MonoFunTy (en_mono $2) (en_mono $4) }

general_inst	:: { RdrNameMonoType }
general_inst	:  gtycon				{ MonoTyApp $1 [] }
		|  OPAREN gtyconapp CPAREN		{ case $2 of (tc,tys) -> MonoTyApp tc tys }
		|  OPAREN type COMMA types CPAREN	{ MonoTupleTy ($2:$4) }
		|  OBRACK type CBRACK			{ MonoListTy $2 }
		|  OPAREN btype RARROW type CPAREN	{ MonoFunTy $2 $4 }

tyvars		:: { [FAST_STRING] }
tyvars		:  VARID		    { [$1] }
		|  tyvars COMMA VARID   { $1 ++ [$3]
--------------------------------------------------------------------------
					    }

pragmas_part	:: { LocalPragmasMap }
pragmas_part	:  PRAGMAS_PART
		   { emptyFM }
		|  { emptyFM }
{
}
