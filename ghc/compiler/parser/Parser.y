{-								-*-haskell-*-
-----------------------------------------------------------------------------
$Id: Parser.y,v 1.90 2002/02/15 22:13:33 sof Exp $

Haskell grammar.

Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-----------------------------------------------------------------------------
-}

{
module Parser ( parseModule, parseStmt, parseIdentifier ) where

import HsSyn
import HsTypes		( mkHsTupCon )

import RdrHsSyn
import Lex
import ParseUtil
import RdrName
import PrelNames	( mAIN_Name, unitTyCon_RDR, funTyCon_RDR, 
			  listTyCon_RDR, parrTyCon_RDR, tupleTyCon_RDR, 
			  unitCon_RDR, nilCon_RDR, tupleCon_RDR )
import ForeignCall	( Safety(..), CExportSpec(..), 
			  CCallConv(..), CCallTarget(..), defaultCCallConv,
			)
import OccName		( UserFS, varName, tcName, dataName, tcClsName, tvName )
import TyCon		( DataConDetails(..) )
import SrcLoc		( SrcLoc )
import Module
import CmdLineOpts	( opt_SccProfilingOn )
import Type		( Kind, mkArrowKind, liftedTypeKind )
import BasicTypes	( Boxity(..), Fixity(..), FixityDirection(..), IPName(..),
			  NewOrData(..), StrictnessMark(..), Activation(..) )
import Panic

import GlaExts
import CStrings		( CLabelString )
import FastString
import Maybes		( orElse )
import Outputable

#include "HsVersions.h"
}

{-
-----------------------------------------------------------------------------
Conflicts: 21 shift/reduce, -=chak[4Feb2]

9 for abiguity in 'if x then y else z + 1'
	(shift parses as 'if x then y else (z + 1)', as per longest-parse rule)
	8 because op might be: - ! * . `x` VARSYM CONSYM QVARSYM QCONSYM
1 for ambiguity in 'if x then y else z :: T'
	(shift parses as 'if x then y else (z :: T)', as per longest-parse rule)
1 for ambiguity in 'if x then y else z with ?x=3'
	(shift parses as 'if x then y else (z with ?x=3)'

3 for ambiguity in 'case x of y :: a -> b'
	(don't know whether to reduce 'a' as a btype or shift the '->'.
	 conclusion:  bogus expression anyway, doesn't matter)

1 for ambiguity in '{-# RULES "name" forall = ... #-}' 
	since 'forall' is a valid variable name, we don't know whether
	to treat a forall on the input as the beginning of a quantifier
	or the beginning of the rule itself.  Resolving to shift means
	it's always treated as a quantifier, hence the above is disallowed.
	This saves explicitly defining a grammar for the rule lhs that
	doesn't include 'forall'.

1 for ambiguity in 'x @ Rec{..}'.  
	Only sensible parse is 'x @ (Rec{..})', which is what resolving
	to shift gives us.

6 for conflicts between `fdecl' and `fdeclDEPRECATED', which are resolved
  correctly, and moreover, should go away when `fdeclDEPRECATED' is removed.

-----------------------------------------------------------------------------
-}

%token
 '_'            { ITunderscore }		-- Haskell keywords
 'as' 		{ ITas }
 'case' 	{ ITcase }  	
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
 '_scc_'	{ ITscc }	      -- ToDo: remove

 'forall'	{ ITforall }			-- GHC extension keywords
 'foreign'	{ ITforeign }
 'export'	{ ITexport }
 'label'	{ ITlabel } 
 'dynamic'	{ ITdynamic }
 'safe'		{ ITsafe }
 'threadsafe'	{ ITthreadsafe }
 'unsafe'	{ ITunsafe }
 'with' 	{ ITwith }
 'stdcall'      { ITstdcallconv }
 'ccall'        { ITccallconv }
 'dotnet'       { ITdotnet }
 '_ccall_'	{ ITccall (False, False, PlayRisky) }
 '_ccall_GC_'	{ ITccall (False, False, PlaySafe False) }
 '_casm_'	{ ITccall (False, True,  PlayRisky) }
 '_casm_GC_'	{ ITccall (False, True,  PlaySafe False) }

 '{-# SPECIALISE'  { ITspecialise_prag }
 '{-# SOURCE'	   { ITsource_prag }
 '{-# INLINE'      { ITinline_prag }
 '{-# NOINLINE'    { ITnoinline_prag }
 '{-# RULES'	   { ITrules_prag }
 '{-# SCC'	   { ITscc_prag }
 '{-# DEPRECATED'  { ITdeprecated_prag }
 '#-}'		   { ITclose_prag }

{-
 '__interface'	{ ITinterface }			-- interface keywords
 '__export'	{ IT__export }
 '__instimport'	{ ITinstimport }
 '__forall'	{ IT__forall }
 '__letrec'	{ ITletrec }
 '__coerce'	{ ITcoerce }
 '__depends'	{ ITdepends }
 '__inline'	{ ITinline }
 '__DEFAULT'	{ ITdefaultbranch }
 '__bot'	{ ITbottom }
 '__integer'	{ ITinteger_lit }
 '__float'	{ ITfloat_lit }
 '__rational'	{ ITrational_lit }
 '__addr'	{ ITaddr_lit }
 '__label'	{ ITlabel_lit }
 '__litlit'	{ ITlit_lit }
 '__string'	{ ITstring_lit }
 '__ccall'	{ ITccall $$ }
 '__scc' 	{ IT__scc }
 '__sccC'       { ITsccAllCafs }

 '__A'		{ ITarity }
 '__P'		{ ITspecialise }
 '__C'		{ ITnocaf }
 '__U'		{ ITunfold }
 '__S'		{ ITstrict $$ }
 '__M'		{ ITcprinfo $$ }
-}

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
 '*'		{ ITstar }
 '.'		{ ITdot }

 '{'		{ ITocurly } 			-- special symbols
 '}'		{ ITccurly }
 '{|'           { ITocurlybar }
 '|}'           { ITccurlybar }
 vccurly	{ ITvccurly } -- virtual close curly (from layout)
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
 '`'		{ ITbackquote }

 VARID   	{ ITvarid    $$ }		-- identifiers
 CONID   	{ ITconid    $$ }
 VARSYM  	{ ITvarsym   $$ }
 CONSYM  	{ ITconsym   $$ }
 QVARID  	{ ITqvarid   $$ }
 QCONID  	{ ITqconid   $$ }
 QVARSYM 	{ ITqvarsym  $$ }
 QCONSYM 	{ ITqconsym  $$ }

 IPDUPVARID   	{ ITdupipvarid   $$ }		-- GHC extension
 IPSPLITVARID  	{ ITsplitipvarid $$ }		-- GHC extension

 CHAR		{ ITchar     $$ }
 STRING		{ ITstring   $$ }
 INTEGER	{ ITinteger  $$ }
 RATIONAL	{ ITrational $$ }

 PRIMCHAR	{ ITprimchar   $$ }
 PRIMSTRING	{ ITprimstring $$ }
 PRIMINTEGER	{ ITprimint    $$ }
 PRIMFLOAT	{ ITprimfloat  $$ }
 PRIMDOUBLE	{ ITprimdouble $$ }
 CLITLIT	{ ITlitlit     $$ }

%monad { P } { thenP } { returnP }
%lexer { lexer } { ITeof }
%name parseModule module
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%tokentype { Token }
%%

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

module 	:: { RdrNameHsModule }
 	: srcloc 'module' modid maybemoddeprec maybeexports 'where' body 
		{ HsModule $3 Nothing $5 (fst $7) (snd $7) $4 $1 }
	| srcloc body
		{ HsModule mAIN_Name Nothing Nothing (fst $2) (snd $2) Nothing $1 }

maybemoddeprec :: { Maybe DeprecTxt }
	: '{-# DEPRECATED' STRING '#-}' 	{ Just $2 }
	|  {- empty -}				{ Nothing }

body 	:: { ([RdrNameImportDecl], [RdrNameHsDecl]) }
	:  '{'            top '}'		{ $2 }
 	|      layout_on  top close		{ $2 }

top 	:: { ([RdrNameImportDecl], [RdrNameHsDecl]) }
	: importdecls				{ (reverse $1,[]) }
	| importdecls ';' cvtopdecls		{ (reverse $1,$3) }
	| cvtopdecls				{ ([],$1) }

cvtopdecls :: { [RdrNameHsDecl] }
	: topdecls				{ cvTopDecls (groupBindings $1)}

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { Maybe [RdrNameIE] }
	:  '(' exportlist ')'			{ Just $2 }
	|  {- empty -}				{ Nothing }

exportlist :: { [RdrNameIE] }
 	:  exportlist ',' export		{ $3 : $1 }
	|  exportlist ','			{ $1 }
 	|  export				{ [$1]  }
	|  {- empty -}				{ [] }

   -- GHC extension: we allow things like [] and (,,,) to be exported
export 	:: { RdrNameIE }
	:  qvar					{ IEVar $1 }
	|  gtycon				{ IEThingAbs $1 }
	|  gtycon '(' '..' ')'			{ IEThingAll $1 }
	|  gtycon '(' ')'		        { IEThingWith $1 [] }
	|  gtycon '(' qcnames ')'		{ IEThingWith $1 (reverse $3) }
	|  'module' modid			{ IEModuleContents $2 }

qcnames :: { [RdrName] }
	:  qcnames ',' qcname			{ $3 : $1 }
	|  qcname				{ [$1]  }

qcname 	:: { RdrName }
	:  qvar					{ $1 }
	|  gcon					{ $1 }

-----------------------------------------------------------------------------
-- Import Declarations

-- import decls can be *empty*, or even just a string of semicolons
-- whereas topdecls must contain at least one topdecl.

importdecls :: { [RdrNameImportDecl] }
	: importdecls ';' importdecl		{ $3 : $1 }
	| importdecls ';'			{ $1 }
	| importdecl				{ [ $1 ] }
	| {- empty -}				{ [] }

importdecl :: { RdrNameImportDecl }
	: 'import' srcloc maybe_src optqualified modid maybeas maybeimpspec 
		{ ImportDecl $5 $3 $4 $6 $7 $2 }

maybe_src :: { WhereFrom }
	: '{-# SOURCE' '#-}'			{ ImportByUserSource }
	| {- empty -}				{ ImportByUser }

optqualified :: { Bool }
      	: 'qualified'                           { True  }
      	| {- empty -}				{ False }

maybeas :: { Maybe ModuleName }
      	: 'as' modid                            { Just $2 }
      	| {- empty -}				{ Nothing }

maybeimpspec :: { Maybe (Bool, [RdrNameIE]) }
	: impspec				{ Just $1 }
	| {- empty -}				{ Nothing }

impspec :: { (Bool, [RdrNameIE]) }
	:  '(' exportlist ')'  			{ (False, reverse $2) }
	|  'hiding' '(' exportlist ')' 		{ (True,  reverse $3) }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec 	:: { Int }
	: {- empty -}				{ 9 }
	| INTEGER				{%  checkPrec $1 `thenP_`
						    returnP (fromInteger $1) }

infix 	:: { FixityDirection }
	: 'infix'				{ InfixN  }
	| 'infixl'				{ InfixL  }
	| 'infixr'				{ InfixR }

ops   	:: { [RdrName] }
	: ops ',' op				{ $3 : $1 }
	| op					{ [$1] }

-----------------------------------------------------------------------------
-- Top-Level Declarations

topdecls :: { [RdrBinding] }
	: topdecls ';' topdecl		{ ($3 : $1) }
	| topdecls ';'			{ $1 }
	| topdecl			{ [$1] }

topdecl :: { RdrBinding }
 	: srcloc 'type' tycon tv_bndrs '=' ctype	
		-- Note ctype, not sigtype.
		-- We allow an explicit for-all but we don't insert one
		-- in 	type Foo a = (b,b)
		-- Instead we just say b is out of scope
 		{ RdrHsDecl (TyClD (TySynonym $3 $4 $6 $1)) }


	| srcloc 'data' tycl_hdr constrs deriving
		{% returnP (RdrHsDecl (TyClD
		      (mkTyData DataType $3 (DataCons (reverse $4)) $5 $1))) }

	| srcloc 'newtype' tycl_hdr '=' newconstr deriving
		{% returnP (RdrHsDecl (TyClD
		      (mkTyData NewType $3 (DataCons [$5]) $6 $1))) }

	| srcloc 'class' tycl_hdr fds where
		{% let 
			(binds,sigs) = cvMonoBindsAndSigs cvClassOpSig (groupBindings $5) 
		   in
	 	   returnP (RdrHsDecl (TyClD
		      (mkClassDecl $3 $4 sigs (Just binds) $1))) }

	| srcloc 'instance' inst_type where
		{ let (binds,sigs) 
			= cvMonoBindsAndSigs cvInstDeclSig 
				(groupBindings $4)
		  in RdrHsDecl (InstD (InstDecl $3 binds sigs Nothing $1)) }

	| srcloc 'default' '(' comma_types0 ')'		{ RdrHsDecl (DefD (DefaultDecl $4 $1)) }
	| 'foreign' fdecl				{ RdrHsDecl $2 }
	| '{-# DEPRECATED' deprecations '#-}'	 	{ $2 }
	| '{-# RULES' rules '#-}'		 	{ $2 }
      	| decl						{ $1 }

-- tycl_hdr parses the header of a type or class decl,
-- which takes the form
--	T a b
-- 	Eq a => T a
--	(Eq a, Ord b) => T a b
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { (RdrNameContext, RdrName, [RdrNameHsTyVar]) }
	: '(' comma_types1 ')' '=>' gtycon tv_bndrs	{% mapP checkPred $2	`thenP` \ cxt ->
							   returnP (cxt, $5, $6) }
          -- qtycon for the class below name would lead to many s/r conflicts
	  --   FIXME: does the renamer pick up all wrong forms and raise an
	  --	      error 
	| gtycon atypes1 '=>' gtycon atypes0		{% checkTyVars $5	`thenP` \ tvs ->
						  	   returnP ([HsClassP $1 $2], $4, tvs) }
	| gtycon  atypes0				{% checkTyVars $2	`thenP` \ tvs ->
						  	   returnP ([], $1, tvs) }
		-- We have to have qtycon in this production to avoid s/r
		-- conflicts with the previous one.  The renamer will complain
		-- if we use a qualified tycon.
		--
		-- Using a `gtycon' throughout.  This enables special syntax,
		-- such as "[]" for tycons as well as tycon ops in
		-- parentheses.  This is beyond H98, but used repeatedly in
		-- the Prelude modules.  (So, it would be a good idea to raise
		-- an error in the renamer if some non-H98 form is used and
		-- -fglasgow-exts is not given.)  -=chak 

decls 	:: { [RdrBinding] }
	: decls ';' decl		{ $3 : $1 }
	| decls ';'			{ $1 }
	| decl				{ [$1] }
	| {- empty -}			{ [] }

decl 	:: { RdrBinding }
	: fixdecl			{ $1 }
	| valdef			{ $1 }
	| '{-# INLINE'   srcloc activation qvar '#-}'	      { RdrSig (InlineSig True  $4 $3 $2) }
	| '{-# NOINLINE' srcloc inverse_activation qvar '#-}' { RdrSig (InlineSig False $4 $3 $2) }
	| '{-# SPECIALISE' srcloc qvar '::' sigtypes '#-}'
	 	{ foldr1 RdrAndBindings 
		    (map (\t -> RdrSig (SpecSig $3 t $2)) $5) }
	| '{-# SPECIALISE' srcloc 'instance' inst_type '#-}'
		{ RdrSig (SpecInstSig $4 $2) }

wherebinds :: { RdrNameHsBinds }
	: where			{ cvBinds cvValSig (groupBindings $1) }

where 	:: { [RdrBinding] }
	: 'where' decllist		{ $2 }
	| {- empty -}			{ [] }

declbinds :: { RdrNameHsBinds }
	: decllist			{ cvBinds cvValSig (groupBindings $1) }

decllist :: { [RdrBinding] }
	: '{'            decls '}'	{ $2 }
	|     layout_on  decls close	{ $2 }

fixdecl :: { RdrBinding }
	: srcloc infix prec ops	    	{ foldr1 RdrAndBindings
					    [ RdrSig (FixSig (FixitySig n 
							    (Fixity $3 $2) $1))
					    | n <- $4 ] }

-----------------------------------------------------------------------------
-- Transformation Rules

rules	:: { RdrBinding }
	:  rules ';' rule			{ $1 `RdrAndBindings` $3 }
        |  rules ';'				{ $1 }
        |  rule					{ $1 }
	|  {- empty -}				{ RdrNullBind }

rule  	:: { RdrBinding }
	: STRING activation rule_forall infixexp '=' srcloc exp
	     { RdrHsDecl (RuleD (HsRule $1 $2 $3 $4 $7 $6)) }

activation :: { Activation }           -- Omitted means AlwaysActive
        : {- empty -}                           { AlwaysActive }
        | explicit_activation                   { $1 }

inverse_activation :: { Activation }   -- Omitted means NeverActive
        : {- empty -}                           { NeverActive }
        | explicit_activation                   { $1 }

explicit_activation :: { Activation }  -- In brackets
        : '[' INTEGER ']'                       { ActiveAfter  (fromInteger $2) }
        | '[' '~' INTEGER ']'                   { ActiveBefore (fromInteger $3) }

rule_forall :: { [RdrNameRuleBndr] }
	: 'forall' rule_var_list '.'            { $2 }
        | {- empty -}				{ [] }

rule_var_list :: { [RdrNameRuleBndr] }
        : rule_var				{ [$1] }
        | rule_var rule_var_list		{ $1 : $2 }

rule_var :: { RdrNameRuleBndr }
	: varid                              	{ RuleBndr $1 }
       	| '(' varid '::' ctype ')'             	{ RuleBndrSig $2 $4 }

-----------------------------------------------------------------------------
-- Deprecations

deprecations :: { RdrBinding }
	: deprecations ';' deprecation		{ $1 `RdrAndBindings` $3 }
	| deprecations ';'			{ $1 }
	| deprecation				{ $1 }
	| {- empty -}				{ RdrNullBind }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { RdrBinding }
	: srcloc depreclist STRING
		{ foldr RdrAndBindings RdrNullBind 
			[ RdrHsDecl (DeprecD (Deprecation n $3 $1)) | n <- $2 ] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

-- for the time being, the following accepts foreign declarations conforming
-- to the FFI Addendum, Version 1.0 as well as pre-standard declarations
--
-- * a flag indicates whether pre-standard declarations have been used and
--   triggers a deprecation warning further down the road
--
-- NB: The first two rules could be combined into one by replacing `safety1'
--     with `safety'.  However, the combined rule conflicts with the
--     DEPRECATED rules.
--
fdecl :: { RdrNameHsDecl }
fdecl : srcloc 'import' callconv safety1 fspec	{% mkImport $3 $4       $5 $1 }
      | srcloc 'import' callconv         fspec	{% mkImport $3 (PlaySafe False) $4 $1 }
      | srcloc 'export'	callconv         fspec  {% mkExport $3          $4 $1 }
        -- the following syntax is DEPRECATED
      | srcloc fdecl1DEPRECATED			{ ForD ($2 True $1) }
      | srcloc fdecl2DEPRECATED			{ $2 $1 }

fdecl1DEPRECATED :: { Bool -> SrcLoc -> ForeignDecl RdrName }
fdecl1DEPRECATED 
  ----------- DEPRECATED label decls ------------
  : 'label' ext_name varid '::' sigtype
    { ForeignImport $3 $5 (CImport defaultCCallConv (PlaySafe False) _NIL_ _NIL_ 
				   (CLabel ($2 `orElse` mkExtName $3))) }

  ----------- DEPRECATED ccall/stdcall decls ------------
  --
  -- NB: This business with the case expression below may seem overly
  --	 complicated, but it is necessary to avoid some conflicts.

    -- DEPRECATED variant #1: lack of a calling convention specification
    --			      (import) 
  | 'import' {-no callconv-} ext_name safety varid_no_unsafe '::' sigtype
    { let
	target = StaticTarget ($2 `orElse` mkExtName $4)
      in
      ForeignImport $4 $6 (CImport defaultCCallConv $3 _NIL_ _NIL_ 
				   (CFunction target)) }

    -- DEPRECATED variant #2: external name consists of two separate strings
    --			      (module name and function name) (import)
  | 'import' callconv STRING STRING safety varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> returnP $
           let
	     imp = CFunction (StaticTarget $4)
	   in
	   ForeignImport $6 $8 (CImport cconv $5 _NIL_ _NIL_ imp) }

    -- DEPRECATED variant #3: `unsafe' after entity
  | 'import' callconv STRING 'unsafe' varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> returnP $
           let
	     imp = CFunction (StaticTarget $3)
	   in
	   ForeignImport $5 $7 (CImport cconv PlayRisky _NIL_ _NIL_ imp) }

    -- DEPRECATED variant #4: use of the special identifier `dynamic' without
    --			      an explicit calling convention (import)
  | 'import' {-no callconv-} 'dynamic' safety varid_no_unsafe '::' sigtype
    { ForeignImport $4 $6 (CImport defaultCCallConv $3 _NIL_ _NIL_ 
				   (CFunction DynamicTarget)) }

    -- DEPRECATED variant #5: use of the special identifier `dynamic' (import)
  | 'import' callconv 'dynamic' safety varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> returnP $
	   ForeignImport $5 $7 (CImport cconv $4 _NIL_ _NIL_ 
					(CFunction DynamicTarget)) }

    -- DEPRECATED variant #6: lack of a calling convention specification
    --			      (export) 
  | 'export' {-no callconv-} ext_name varid '::' sigtype
    { ForeignExport $3 $5 (CExport (CExportStatic ($2 `orElse` mkExtName $3) 
				   defaultCCallConv)) }

    -- DEPRECATED variant #7: external name consists of two separate strings
    --			      (module name and function name) (export)
  | 'export' callconv STRING STRING varid '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> returnP $
           ForeignExport $5 $7 
			 (CExport (CExportStatic $4 cconv)) }

    -- DEPRECATED variant #8: use of the special identifier `dynamic' without
    --			      an explicit calling convention (export)
  | 'export' {-no callconv-} 'dynamic' varid '::' sigtype
    { ForeignImport $3 $5 (CImport defaultCCallConv (PlaySafe False) _NIL_ _NIL_ 
				   CWrapper) }

    -- DEPRECATED variant #9: use of the special identifier `dynamic' (export)
  | 'export' callconv 'dynamic' varid '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> returnP $
	   ForeignImport $4 $6 (CImport cconv (PlaySafe False) _NIL_ _NIL_ CWrapper) }

  ----------- DEPRECATED .NET decls ------------
  -- NB: removed the .NET call declaration, as it is entirely subsumed
  --     by the new standard FFI declarations

fdecl2DEPRECATED :: { SrcLoc -> RdrNameHsDecl }
fdecl2DEPRECATED 
  : 'import' 'dotnet' 'type' ext_name tycon
	  { \loc -> TyClD (ForeignType $5 $4 DNType loc) }
    -- left this one unchanged for the moment as type imports are not
    -- covered currently by the FFI standard -=chak


callconv :: { CallConv }
	  : 'stdcall'			{ CCall  StdCallConv }
	  | 'ccall'			{ CCall  CCallConv   }
	  | 'dotnet'			{ DNCall	     }

safety :: { Safety }
	: 'unsafe'			{ PlayRisky }
	| 'safe'			{ PlaySafe False }
	| 'threadsafe'			{ PlaySafe True  }
	| {- empty -}			{ PlaySafe False }

safety1 :: { Safety }
	: 'unsafe'			{ PlayRisky }
	| 'safe'			{ PlaySafe  False }
	| 'threadsafe'			{ PlaySafe  True }
	  -- only needed to avoid conflicts with the DEPRECATED rules

fspec :: { (FAST_STRING, RdrName, RdrNameHsType) }
       : STRING varid '::' sigtype      { ($1      , $2, $4) }
       |        varid '::' sigtype      { (SLIT(""), $1, $3) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-- DEPRECATED syntax
ext_name :: { Maybe CLabelString }
	: STRING		{ Just $1 }
	| STRING STRING		{ Just $2 }	-- Ignore "module name" for now
	| {- empty -}           { Nothing }


-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { Maybe RdrNameHsType }
	: {- empty -}			{ Nothing }
	| '::' sigtype			{ Just $2 }

opt_asig :: { Maybe RdrNameHsType }
	: {- empty -}			{ Nothing }
	| '::' atype			{ Just $2 }

sigtypes :: { [RdrNameHsType] }
	: sigtype			{ [ $1 ] }
	| sigtypes ',' sigtype		{ $3 : $1 }

sigtype :: { RdrNameHsType }
	: ctype				{ mkHsForAllTy Nothing [] $1 }

sig_vars :: { [RdrName] }
	 : sig_vars ',' var		{ $3 : $1 }
	 | var				{ [ $1 ] }

-----------------------------------------------------------------------------
-- Types

-- A ctype is a for-all type
ctype	:: { RdrNameHsType }
	: 'forall' tv_bndrs '.' ctype	{ mkHsForAllTy (Just $2) [] $4 }
	| context '=>' type		{ mkHsForAllTy Nothing   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| type				{ $1 }

-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--	(Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>
context :: { RdrNameContext }
	: btype 			{% checkContext $1 }

type :: { RdrNameHsType }
	: gentype '->' type		{ HsFunTy $1 $3 }
	| ipvar '::' type		{ mkHsIParamTy $1 $3 }
	| gentype			{ $1 }

gentype :: { RdrNameHsType }
        : btype                         { $1 }
-- Generics
        | atype tyconop atype           { HsOpTy $1 $2 $3 }

btype :: { RdrNameHsType }
	: btype atype			{ HsAppTy $1 $2 }
	| atype				{ $1 }

atype :: { RdrNameHsType }
	: gtycon			{ HsTyVar $1 }
	| tyvar				{ HsTyVar $1 }
	| '(' type ',' comma_types1 ')'	{ HsTupleTy (mkHsTupCon tcName Boxed  ($2:$4)) ($2:$4) }
	| '(#' comma_types1 '#)'	{ HsTupleTy (mkHsTupCon tcName Unboxed     $2) $2      }
	| '[' type ']'			{ HsListTy $2 }
	| '[:' type ':]'		{ HsPArrTy $2 }
	| '(' ctype ')'		        { $2 }
	| '(' ctype '::' kind ')'	{ HsKindSig $2 $4 }
-- Generics
        | INTEGER                       { HsNumTy $1 }

-- An inst_type is what occurs in the head of an instance decl
--	e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type, with a MonoDictTy at the right
-- hand corner, for convenience.
inst_type :: { RdrNameHsType }
	: ctype				{% checkInstType $1 }

comma_types0  :: { [RdrNameHsType] }
	: comma_types1			{ $1 }
	| {- empty -}			{ [] }

comma_types1	:: { [RdrNameHsType] }
	: type				{ [$1] }
	| type  ',' comma_types1	{ $1 : $3 }

atypes0	:: { [RdrNameHsType] }
	: atypes1			{ $1 }
	| {- empty -}			{ [] }

atypes1	:: { [RdrNameHsType] }
	: atype				{ [$1] }
	| atype atypes1			{ $1 : $2 }

tv_bndrs :: { [RdrNameHsTyVar] }
	 : tv_bndr tv_bndrs		{ $1 : $2 }
	 | {- empty -}			{ [] }

tv_bndr :: { RdrNameHsTyVar }
	: tyvar				{ UserTyVar $1 }
	| '(' tyvar '::' kind ')'	{ IfaceTyVar $2 $4 }

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
	| varids0 tyvar			{ $2 : $1 }

-----------------------------------------------------------------------------
-- Kinds

kind	:: { Kind }
	: akind			{ $1 }
	| akind '->' kind	{ mkArrowKind $1 $3 }

akind	:: { Kind }
	: '*'			{ liftedTypeKind }
	| '(' kind ')'		{ $2 }


-----------------------------------------------------------------------------
-- Datatype declarations

newconstr :: { RdrNameConDecl }
	: srcloc conid atype	{ mkConDecl $2 [] [] (VanillaCon [unbangedType $3]) $1 }
	| srcloc conid '{' var '::' ctype '}'
				{ mkConDecl $2 [] [] (RecCon [([$4], unbangedType $6)]) $1 }

constrs :: { [RdrNameConDecl] }
        : {- empty; a GHC extension -}  { [] }
        | '=' constrs1                  { $2 }

constrs1 :: { [RdrNameConDecl] }
	: constrs1 '|' constr		{ $3 : $1 }
	| constr			{ [$1] }

constr :: { RdrNameConDecl }
	: srcloc forall context '=>' constr_stuff
		{ mkConDecl (fst $5) $2 $3 (snd $5) $1 }
	| srcloc forall constr_stuff
		{ mkConDecl (fst $3) $2 [] (snd $3) $1 }

forall :: { [RdrNameHsTyVar] }
	: 'forall' tv_bndrs '.'		{ $2 }
	| {- empty -}			{ [] }

constr_stuff :: { (RdrName, RdrNameConDetails) }
	: btype				{% mkVanillaCon $1 []		    }
	| btype '!' atype satypes	{% mkVanillaCon $1 (BangType MarkedUserStrict $3 : $4) }
	| gtycon '{' '}' 		{% mkRecCon $1 [] }
	| gtycon '{' fielddecls '}' 	{% mkRecCon $1 $3 }
	| sbtype conop sbtype		{ ($2, InfixCon $1 $3) }

satypes	:: { [RdrNameBangType] }
	: atype satypes			{ unbangedType $1 : $2 }
	| '!' atype satypes		{ BangType MarkedUserStrict $2 : $3 }
	| {- empty -}			{ [] }

sbtype :: { RdrNameBangType }
	: btype				{ unbangedType $1 }
	| '!' atype			{ BangType MarkedUserStrict $2 }

fielddecls :: { [([RdrName],RdrNameBangType)] }
	: fielddecl ',' fielddecls	{ $1 : $3 }
	| fielddecl			{ [$1] }

fielddecl :: { ([RdrName],RdrNameBangType) }
	: sig_vars '::' stype		{ (reverse $1, $3) }

stype :: { RdrNameBangType }
	: ctype				{ unbangedType $1 }
	| '!' atype			{ BangType MarkedUserStrict $2 }

deriving :: { Maybe RdrNameContext }
	: {- empty -}			{ Nothing }
	| 'deriving' context		{ Just $2 }
             -- Glasgow extension: allow partial 
             -- applications in derivings

-----------------------------------------------------------------------------
-- Value definitions

{- There's an awkward overlap with a type signature.  Consider
	f :: Int -> Int = ...rhs...
   Then we can't tell whether it's a type signature or a value
   definition with a result signature until we see the '='.
   So we have to inline enough to postpone reductions until we know.
-}

{-
  ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
  instead of qvar, we get another shift/reduce-conflict. Consider the
  following programs:
  
     { (^^) :: Int->Int ; }          Type signature; only var allowed

     { (^^) :: Int->Int = ... ; }    Value defn with result signature;
				     qvar allowed (because of instance decls)
  
  We can't tell whether to reduce var to qvar until after we've read the signatures.
-}

valdef :: { RdrBinding }
	: infixexp srcloc opt_sig rhs		{% (checkValDef $1 $3 $4 $2) }
	| infixexp srcloc '::' sigtype		{% (checkValSig $1 $4 $2) }
	| var ',' sig_vars srcloc '::' sigtype	{ foldr1 RdrAndBindings 
							 [ RdrSig (Sig n $6 $4) | n <- $1:$3 ]
                                                }


rhs	:: { RdrNameGRHSs }
	: '=' srcloc exp wherebinds	{ (GRHSs (unguardedRHS $3 $2) $4 placeHolderType)}
	| gdrhs	wherebinds		{ GRHSs (reverse $1) $2 placeHolderType }

gdrhs :: { [RdrNameGRHS] }
	: gdrhs gdrh			{ $2 : $1 }
	| gdrh				{ [$1] }

gdrh :: { RdrNameGRHS }
	: '|' srcloc quals '=' exp  	{ GRHS (reverse (ResultStmt $5 $2 : $3)) $2 }

-----------------------------------------------------------------------------
-- Expressions

exp   :: { RdrNameHsExpr }
	: infixexp '::' sigtype		{ (ExprWithTySig $1 $3) }
	| infixexp 'with' dbinding	{ HsWith $1 $3 }
	| infixexp			{ $1 }

infixexp :: { RdrNameHsExpr }
	: exp10				{ $1 }
	| infixexp qop exp10		{ (OpApp $1 (HsVar $2) 
						(panic "fixity") $3 )}

exp10 :: { RdrNameHsExpr }
	: '\\' srcloc aexp aexps opt_asig '->' srcloc exp	
			{% checkPatterns $2 ($3 : reverse $4) `thenP` \ ps -> 
			   returnP (HsLam (Match ps $5 
					    (GRHSs (unguardedRHS $8 $7) 
						   EmptyBinds placeHolderType))) }
  	| 'let' declbinds 'in' exp		{ HsLet $2 $4 }
	| 'if' srcloc exp 'then' exp 'else' exp { HsIf $3 $5 $7 $2 }
   	| 'case' srcloc exp 'of' altslist	{ HsCase $3 $5 $2 }
	| '-' fexp				{ mkHsNegApp $2 }
  	| srcloc 'do' stmtlist			{% checkDo $3  `thenP` \ stmts ->
						   returnP (HsDo DoExpr stmts $1) }

	| '_ccall_'    ccallid aexps0		{ HsCCall $2 $3 PlayRisky False placeHolderType }
	| '_ccall_GC_' ccallid aexps0		{ HsCCall $2 $3 (PlaySafe False) False placeHolderType }
	| '_casm_'     CLITLIT aexps0		{ HsCCall $2 $3 PlayRisky True  placeHolderType }
	| '_casm_GC_'  CLITLIT aexps0		{ HsCCall $2 $3 (PlaySafe False) True  placeHolderType }

        | scc_annot exp		    		{ if opt_SccProfilingOn
							then HsSCC $1 $2
							else HsPar $2 }

	| fexp					{ $1 }

scc_annot :: { FAST_STRING }
	: '_scc_' STRING			{ $2 }
	| '{-# SCC' STRING '#-}'		{ $2 }

ccallid :: { FAST_STRING }
	:  VARID				{ $1 }
	|  CONID				{ $1 }

fexp 	:: { RdrNameHsExpr }
	: fexp aexp				{ (HsApp $1 $2) }
  	| aexp					{ $1 }

aexps0 	:: { [RdrNameHsExpr] }
	: aexps					{ reverse $1 }

aexps 	:: { [RdrNameHsExpr] }
	: aexps aexp				{ $2 : $1 }
  	| {- empty -}				{ [] }

aexp	:: { RdrNameHsExpr }
        : var_or_con '{|' gentype '|}'          { (HsApp $1 (HsType $3)) }
  	| aexp '{' fbinds '}' 			{% (mkRecConstrOrUpdate $1 
							(reverse $3)) }
  	| aexp1					{ $1 }

var_or_con :: { RdrNameHsExpr }
        : qvar                          { HsVar $1 }
        | gcon                          { HsVar $1 }

aexp1	:: { RdrNameHsExpr }
	: ipvar				{ HsIPVar $1 }
	| var_or_con			{ $1 }
	| literal			{ HsLit $1 }
	| INTEGER			{ HsOverLit (mkHsIntegral   $1) }
	| RATIONAL			{ HsOverLit (mkHsFractional $1) }
	| '(' exp ')'			{ HsPar $2 }
	| '(' exp ',' texps ')'		{ ExplicitTuple ($2 : reverse $4) Boxed}
	| '(#' texps '#)'		{ ExplicitTuple (reverse $2)      Unboxed }
	| '[' list ']'                  { $2 }
	| '[:' parr ':]'                { $2 }
	| '(' infixexp qop ')'		{ (SectionL $2 (HsVar $3))  }
	| '(' qopm infixexp ')'		{ (SectionR $2 $3) }
	| qvar '@' aexp			{ EAsPat $1 $3 }
	| '_'				{ EWildPat }
	| '~' aexp1			{ ELazyPat $2 }

texps :: { [RdrNameHsExpr] }
	: texps ',' exp			{ $3 : $1 }
	| exp				{ [$1] }


-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.

list :: { RdrNameHsExpr }
	: exp				{ ExplicitList placeHolderType [$1] }
	| lexps 			{ ExplicitList placeHolderType (reverse $1) }
	| exp '..'			{ ArithSeqIn (From $1) }
	| exp ',' exp '..' 		{ ArithSeqIn (FromThen $1 $3) }
	| exp '..' exp	 		{ ArithSeqIn (FromTo $1 $3) }
	| exp ',' exp '..' exp		{ ArithSeqIn (FromThenTo $1 $3 $5) }
	| exp srcloc pquals		{% let { body [qs] = qs;
					         body  qss = [ParStmt (map reverse qss)] }
					   in
					   returnP ( HsDo ListComp
							   (reverse (ResultStmt $1 $2 : body $3))
							   $2
						  )
					}

lexps :: { [RdrNameHsExpr] }
	: lexps ',' exp 		{ $3 : $1 }
	| exp ',' exp			{ [$3,$1] }

-----------------------------------------------------------------------------
-- List Comprehensions

pquals :: { [[RdrNameStmt]] }
	: pquals '|' quals		{ $3 : $1 }
	| '|' quals			{ [$2] }

quals :: { [RdrNameStmt] }
	: quals ',' stmt		{ $3 : $1 }
	| stmt				{ [$1] }

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { RdrNameHsExpr }
	: 				{ ExplicitPArr placeHolderType [] }
	| exp				{ ExplicitPArr placeHolderType [$1] }
	| lexps 			{ ExplicitPArr placeHolderType 
						       (reverse $1) }
	| exp '..' exp	 		{ PArrSeqIn (FromTo $1 $3) }
	| exp ',' exp '..' exp		{ PArrSeqIn (FromThenTo $1 $3 $5) }
	| exp srcloc pquals		{% let {
					     body [qs] = qs;
					     body  qss = [ParStmt 
							   (map reverse qss)]}
					   in
					   returnP $ 
					     HsDo PArrComp 
						  (reverse (ResultStmt $1 $2 
							    : body $3))
						  $2
					}

-- We are reusing `lexps' and `pquals' from the list case.

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { [RdrNameMatch] }
	: '{'            alts '}'	{ reverse $2 }
	|     layout_on  alts  close	{ reverse $2 }

alts    :: { [RdrNameMatch] }
        : alts1				{ $1 }
	| ';' alts			{ $2 }

alts1 	:: { [RdrNameMatch] }
	: alts1 ';' alt			{ $3 : $1 }
	| alts1 ';'			{ $1 }
	| alt				{ [$1] }

alt 	:: { RdrNameMatch }
	: srcloc infixexp opt_sig ralt wherebinds
					{% (checkPattern $1 $2 `thenP` \p ->
				   	   returnP (Match [p] $3
					             (GRHSs $4 $5 placeHolderType))  )}

ralt :: { [RdrNameGRHS] }
	: '->' srcloc exp		{ [GRHS [ResultStmt $3 $2] $2] }
	| gdpats			{ reverse $1 }

gdpats :: { [RdrNameGRHS] }
	: gdpats gdpat			{ $2 : $1 }
	| gdpat				{ [$1] }

gdpat	:: { RdrNameGRHS }
	: srcloc '|' quals '->' exp 	{ GRHS (reverse (ResultStmt $5 $1:$3)) $1}

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { [RdrNameStmt] }
	: '{'            	stmts '}'	{ $2 }
	|     layout_on_for_do  stmts close	{ $2 }

--	do { ;; s ; s ; ; s ;; }
-- The last Stmt should be a ResultStmt, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use ExprStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
stmts :: { [RdrNameStmt] }
	: stmt stmts_help		{ $1 : $2 }
	| ';' stmts			{ $2 }
	| {- empty -}			{ [] }

stmts_help :: { [RdrNameStmt] }
	: ';' stmts			{ $2 }
	| {- empty -}			{ [] }

-- For typing stmts at the GHCi prompt, where 
-- the input may consist of just comments.
maybe_stmt :: { Maybe RdrNameStmt }
	: stmt				{ Just $1 }
	| {- nothing -}			{ Nothing }

stmt  :: { RdrNameStmt }
	: srcloc infixexp '<-' exp	{% checkPattern $1 $2 `thenP` \p ->
					   returnP (BindStmt p $4 $1) }
	| srcloc exp			{ ExprStmt $2 placeHolderType $1 }
  	| srcloc 'let' declbinds	{ LetStmt $3 }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds 	:: { RdrNameHsRecordBinds }
	: fbinds ',' fbind		{ $3 : $1 }
	| fbinds ','			{ $1 }
	| fbind				{ [$1] }
	| {- empty -}			{ [] }

fbind	:: { (RdrName, RdrNameHsExpr, Bool) }
	: qvar '=' exp			{ ($1,$3,False) }

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinding :: { [(IPName RdrName, RdrNameHsExpr)] }
	: '{' dbinds '}'		{ $2 }
	| layout_on dbinds close	{ $2 }

dbinds 	:: { [(IPName RdrName, RdrNameHsExpr)] }
	: dbinds ';' dbind		{ $3 : $1 }
	| dbinds ';'			{ $1 }
	| dbind				{ [$1] }
	| {- empty -}			{ [] }

dbind	:: { (IPName RdrName, RdrNameHsExpr) }
dbind	: ipvar '=' exp			{ ($1, $3) }

-----------------------------------------------------------------------------
-- Variables, Constructors and Operators.

identifier :: { RdrName }
	: qvar				{ $1 }
	| gcon				{ $1 }
	| qop				{ $1 }

depreclist :: { [RdrName] }
depreclist : deprec_var			{ [$1] }
	   | deprec_var ',' depreclist	{ $1 : $3 }

deprec_var :: { RdrName }
deprec_var : var			{ $1 }
	   | tycon			{ $1 }

gtycon 	:: { RdrName }
	: qtycon			{ $1 }
 	| '(' qtyconop ')'		{ $2 }
	| '(' ')'			{ unitTyCon_RDR }
	| '(' '->' ')'			{ funTyCon_RDR }
	| '[' ']'			{ listTyCon_RDR }
	| '[:' ':]'			{ parrTyCon_RDR }
	| '(' commas ')'		{ tupleTyCon_RDR $2 }

gcon 	:: { RdrName }	-- Data constructor namespace
	: '(' ')'		{ unitCon_RDR }
	| '[' ']'		{ nilCon_RDR }
	| '(' commas ')'	{ tupleCon_RDR $2 }
 	| qcon			{ $1 }
-- the case of '[:' ':]' is part of the production `parr'

var 	:: { RdrName }
	: varid			{ $1 }
	| '(' varsym ')'	{ $2 }

qvar 	:: { RdrName }
	: qvarid		{ $1 }
	| '(' varsym ')'	{ $2 }
	| '(' qvarsym1 ')'	{ $2 }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

ipvar	:: { IPName RdrName }
	: IPDUPVARID		{ Dupable (mkUnqual varName $1) }
	| IPSPLITVARID		{ Linear  (mkUnqual varName $1) }

qcon	:: { RdrName }
	: qconid		{ $1 }
	| '(' qconsym ')'	{ $2 }

varop	:: { RdrName }
	: varsym		{ $1 }
	| '`' varid '`'		{ $2 }

qvarop :: { RdrName }
	: qvarsym		{ $1 }
	| '`' qvarid '`'	{ $2 }

qvaropm :: { RdrName }
	: qvarsym_no_minus	{ $1 }
	| '`' qvarid '`'	{ $2 }

conop :: { RdrName }
	: consym		{ $1 }	
	| '`' conid '`'		{ $2 }

qconop :: { RdrName }
	: qconsym		{ $1 }
	| '`' qconid '`'	{ $2 }

-----------------------------------------------------------------------------
-- Any operator

op	:: { RdrName }   -- used in infix decls
	: varop			{ $1 }
	| conop 		{ $1 }

qop	:: { RdrName {-HsExpr-} }   -- used in sections
	: qvarop		{ $1 }
	| qconop		{ $1 }

qopm	:: { RdrNameHsExpr }   -- used in sections
	: qvaropm		{ HsVar $1 }
	| qconop		{ HsVar $1 }

-----------------------------------------------------------------------------
-- VarIds

qvarid :: { RdrName }
	: varid			{ $1 }
	| QVARID		{ mkQual varName $1 }

varid :: { RdrName }
	: varid_no_unsafe 	{ $1 }
	| 'unsafe'		{ mkUnqual varName SLIT("unsafe") }

varid_no_unsafe :: { RdrName }
	: VARID			{ mkUnqual varName $1 }
	| special_id		{ mkUnqual varName $1 }
	| 'forall'		{ mkUnqual varName SLIT("forall") }

tyvar 	:: { RdrName }
	: VARID			{ mkUnqual tvName $1 }
	| special_id		{ mkUnqual tvName $1 }
	| 'unsafe' 		{ mkUnqual tvName SLIT("unsafe") }

-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   A special_id collects all thsee
-- except 'unsafe' and 'forall' whose treatment differs depending on context
special_id :: { UserFS }
special_id
	: 'as'			{ SLIT("as") }
	| 'qualified'		{ SLIT("qualified") }
	| 'hiding'		{ SLIT("hiding") }
	| 'export'		{ SLIT("export") }
	| 'label'		{ SLIT("label")  }
	| 'dynamic'		{ SLIT("dynamic") }
	| 'stdcall'             { SLIT("stdcall") }
	| 'ccall'               { SLIT("ccall") }

-----------------------------------------------------------------------------
-- ConIds

qconid :: { RdrName }	-- Qualified or unqualifiedb
	: conid			{ $1 }
	| QCONID		{ mkQual dataName $1 }

conid 	:: { RdrName }
	: CONID			{ mkUnqual dataName $1 }

-----------------------------------------------------------------------------
-- ConSyms

qconsym :: { RdrName }	-- Qualified or unqualifiedb
	: consym		{ $1 }
	| QCONSYM		{ mkQual dataName $1 }

consym :: { RdrName }
	: CONSYM		{ mkUnqual dataName $1 }

-----------------------------------------------------------------------------
-- VarSyms

qvarsym :: { RdrName }
	: varsym		{ $1 }
	| qvarsym1		{ $1 }

qvarsym_no_minus :: { RdrName }
	: varsym_no_minus	{ $1 }
	| qvarsym1		{ $1 }

qvarsym1 :: { RdrName }
qvarsym1 : QVARSYM		{ mkQual varName $1 }

varsym :: { RdrName }
	: varsym_no_minus 	{ $1 }
	| '-'			{ mkUnqual varName SLIT("-") }

varsym_no_minus :: { RdrName } -- varsym not including '-'
	: VARSYM		{ mkUnqual varName $1 }
	| special_sym		{ mkUnqual varName $1 }


-- See comments with special_id
special_sym :: { UserFS }
special_sym : '!'	{ SLIT("!") }
	    | '.' 	{ SLIT(".") }
 	    | '*' 	{ SLIT("*") }

-----------------------------------------------------------------------------
-- Literals

literal :: { HsLit }
	: CHAR 			{ HsChar       $1 }
	| STRING		{ HsString     $1 }
	| PRIMINTEGER		{ HsIntPrim    $1 }
	| PRIMCHAR		{ HsCharPrim   $1 }
	| PRIMSTRING		{ HsStringPrim $1 }
	| PRIMFLOAT		{ HsFloatPrim  $1 }
	| PRIMDOUBLE		{ HsDoublePrim $1 }
	| CLITLIT		{ HsLitLit     $1 placeHolderType }

srcloc :: { SrcLoc }	:	{% getSrcLocP }
 
-----------------------------------------------------------------------------
-- Layout

close :: { () }
	: vccurly		{ () } -- context popped in lexer.
	| error			{% popContext }

layout_on  	  :: { () }	: {% layoutOn True{-strict-} }
layout_on_for_do  :: { () }	: {% layoutOn False }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid 	:: { ModuleName }
	: CONID			{ mkModuleNameFS $1 }
        | QCONID		{ mkModuleNameFS
				   (mkFastString
				     (unpackFS (fst $1) ++ 
					'.':unpackFS (snd $1)))
				}

tycon 	:: { RdrName }
	: CONID			{ mkUnqual tcClsName $1 }

tyconop	:: { RdrName }
	: CONSYM		{ mkUnqual tcClsName $1 }

qtycon :: { RdrName }	-- Qualified or unqualified
	: QCONID		{ mkQual tcClsName $1 }
	| tycon			{ $1 }

qtyconop :: { RdrName }	-- Qualified or unqualified
	  : QCONSYM		{ mkQual tcClsName $1 }
	  | tyconop		{ $1 }

commas :: { Int }
	: commas ','			{ $1 + 1 }
	| ','				{ 2 }

-----------------------------------------------------------------------------

{
happyError :: P a
happyError buf PState{ loc = loc } = PFailed (srcParseErr buf loc)
}
