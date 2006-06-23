--								-*-haskell-*-
-- ---------------------------------------------------------------------------
-- (c) The University of Glasgow 1997-2003
---
-- The GHC grammar.
--
-- Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-- ---------------------------------------------------------------------------

{
module Parser ( parseModule, parseStmt, parseIdentifier, parseType,
		parseHeader ) where

#define INCLUDE #include 
INCLUDE "HsVersions.h"

import HsSyn
import RdrHsSyn
import HscTypes		( IsBootInterface, DeprecTxt )
import Lexer
import RdrName
import TysWiredIn	( unitTyCon, unitDataCon, tupleTyCon, tupleCon, nilDataCon,
			  listTyCon_RDR, parrTyCon_RDR, consDataCon_RDR )
import Type		( funTyCon )
import ForeignCall	( Safety(..), CExportSpec(..), CLabelString,
			  CCallConv(..), CCallTarget(..), defaultCCallConv
			)
import OccName		( varName, dataName, tcClsName, tvName )
import DataCon		( DataCon, dataConName )
import SrcLoc		( Located(..), unLoc, getLoc, noLoc, combineSrcSpans,
			  SrcSpan, combineLocs, srcLocFile, 
			  mkSrcLoc, mkSrcSpan )
import Module
import StaticFlags	( opt_SccProfilingOn )
import Type		( Kind, mkArrowKind, liftedTypeKind, unliftedTypeKind )
import BasicTypes	( Boxity(..), Fixity(..), FixityDirection(..), IPName(..),
			  Activation(..), defaultInlineSpec )
import OrdList

import FastString
import Maybes		( orElse )
import Outputable
import GLAEXTS
}

{-
-----------------------------------------------------------------------------
Conflicts: 36 shift/reduce (1.25)

10 for abiguity in 'if x then y else z + 1'		[State 178]
	(shift parses as 'if x then y else (z + 1)', as per longest-parse rule)
	10 because op might be: : - ! * . `x` VARSYM CONSYM QVARSYM QCONSYM

1 for ambiguity in 'if x then y else z :: T'		[State 178]
	(shift parses as 'if x then y else (z :: T)', as per longest-parse rule)

4 for ambiguity in 'if x then y else z -< e'		[State 178]
	(shift parses as 'if x then y else (z -< T)', as per longest-parse rule)
	There are four such operators: -<, >-, -<<, >>-


2 for ambiguity in 'case v of { x :: T -> T ... } ' 	[States 11, 253]
 	Which of these two is intended?
	  case v of
	    (x::T) -> T		-- Rhs is T
    or
	  case v of
	    (x::T -> T) -> ..	-- Rhs is ...

10 for ambiguity in 'e :: a `b` c'.  Does this mean 	[States 11, 253]
	(e::a) `b` c, or 
	(e :: (a `b` c))
    As well as `b` we can have !, VARSYM, QCONSYM, and CONSYM, hence 5 cases
    Same duplication between states 11 and 253 as the previous case

1 for ambiguity in 'let ?x ...'				[State 329]
	the parser can't tell whether the ?x is the lhs of a normal binding or
	an implicit binding.  Fortunately resolving as shift gives it the only
	sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity in '{-# RULES "name" [ ... #-}		[State 382]
	we don't know whether the '[' starts the activation or not: it
  	might be the start of the declaration with the activation being
	empty.  --SDM 1/4/2002

6 for conflicts between `fdecl' and `fdeclDEPRECATED', 	[States 393,394]
  	which are resolved correctly, and moreover, 
  	should go away when `fdeclDEPRECATED' is removed.

1 for ambiguity in '{-# RULES "name" forall = ... #-}' 	[State 474]
	since 'forall' is a valid variable name, we don't know whether
	to treat a forall on the input as the beginning of a quantifier
	or the beginning of the rule itself.  Resolving to shift means
	it's always treated as a quantifier, hence the above is disallowed.
	This saves explicitly defining a grammar for the rule lhs that
	doesn't include 'forall'.

-- ---------------------------------------------------------------------------
-- Adding location info

This is done in a stylised way using the three macros below, L0, L1
and LL.  Each of these macros can be thought of as having type

   L0, L1, LL :: a -> Located a

They each add a SrcSpan to their argument.

   L0	adds 'noSrcSpan', used for empty productions

   L1   for a production with a single token on the lhs.  Grabs the SrcSpan
	from that token.

   LL   for a production with >1 token on the lhs.  Makes up a SrcSpan from
        the first and last tokens.

These suffice for the majority of cases.  However, we must be
especially careful with empty productions: LL won't work if the first
or last token on the lhs can represent an empty span.  In these cases,
we have to calculate the span using more of the tokens from the lhs, eg.

	| 'newtype' tycl_hdr '=' newconstr deriving
		{ L (comb3 $1 $4 $5)
		    (mkTyData NewType (unLoc $2) [$4] (unLoc $5)) }

We provide comb3 and comb4 functions which are useful in such cases.

Be careful: there's no checking that you actually got this right, the
only symptom will be that the SrcSpans of your syntax will be
incorrect.

/*
 * We must expand these macros *before* running Happy, which is why this file is
 * Parser.y.pp rather than just Parser.y - we run the C pre-processor first.
 */
#define L0   L noSrcSpan
#define L1   sL (getLoc $1)
#define LL   sL (comb2 $1 $>)

-- -----------------------------------------------------------------------------

-}

%token
 '_'            { L _ ITunderscore }		-- Haskell keywords
 'as' 		{ L _ ITas }
 'case' 	{ L _ ITcase }  	
 'class' 	{ L _ ITclass } 
 'data' 	{ L _ ITdata } 
 'default' 	{ L _ ITdefault }
 'deriving' 	{ L _ ITderiving }
 'do' 		{ L _ ITdo }
 'else' 	{ L _ ITelse }
 'hiding' 	{ L _ IThiding }
 'if' 		{ L _ ITif }
 'import' 	{ L _ ITimport }
 'in' 		{ L _ ITin }
 'infix' 	{ L _ ITinfix }
 'infixl' 	{ L _ ITinfixl }
 'infixr' 	{ L _ ITinfixr }
 'instance' 	{ L _ ITinstance }
 'let' 		{ L _ ITlet }
 'module' 	{ L _ ITmodule }
 'newtype' 	{ L _ ITnewtype }
 'of' 		{ L _ ITof }
 'qualified' 	{ L _ ITqualified }
 'then' 	{ L _ ITthen }
 'type' 	{ L _ ITtype }
 'where' 	{ L _ ITwhere }
 '_scc_'	{ L _ ITscc }	      -- ToDo: remove

 'forall'	{ L _ ITforall }			-- GHC extension keywords
 'foreign'	{ L _ ITforeign }
 'export'	{ L _ ITexport }
 'label'	{ L _ ITlabel } 
 'dynamic'	{ L _ ITdynamic }
 'safe'		{ L _ ITsafe }
 'threadsafe'	{ L _ ITthreadsafe }
 'unsafe'	{ L _ ITunsafe }
 'mdo'		{ L _ ITmdo }
 'stdcall'      { L _ ITstdcallconv }
 'ccall'        { L _ ITccallconv }
 'dotnet'       { L _ ITdotnet }
 'proc'		{ L _ ITproc }		-- for arrow notation extension
 'rec'		{ L _ ITrec }		-- for arrow notation extension

 '{-# INLINE'      	  { L _ (ITinline_prag _) }
 '{-# SPECIALISE'  	  { L _ ITspec_prag }
 '{-# SPECIALISE_INLINE'  { L _ (ITspec_inline_prag _) }
 '{-# SOURCE'	   { L _ ITsource_prag }
 '{-# RULES'	   { L _ ITrules_prag }
 '{-# CORE'        { L _ ITcore_prag }              -- hdaume: annotated core
 '{-# SCC'	   { L _ ITscc_prag }
 '{-# DEPRECATED'  { L _ ITdeprecated_prag }
 '{-# UNPACK'      { L _ ITunpack_prag }
 '#-}'		   { L _ ITclose_prag }

 '..'		{ L _ ITdotdot }  			-- reserved symbols
 ':'		{ L _ ITcolon }
 '::'		{ L _ ITdcolon }
 '='		{ L _ ITequal }
 '\\'		{ L _ ITlam }
 '|'		{ L _ ITvbar }
 '<-'		{ L _ ITlarrow }
 '->'		{ L _ ITrarrow }
 '@'		{ L _ ITat }
 '~'		{ L _ ITtilde }
 '=>'		{ L _ ITdarrow }
 '-'		{ L _ ITminus }
 '!'		{ L _ ITbang }
 '*'		{ L _ ITstar }
 '-<'		{ L _ ITlarrowtail }		-- for arrow notation
 '>-'		{ L _ ITrarrowtail }		-- for arrow notation
 '-<<'		{ L _ ITLarrowtail }		-- for arrow notation
 '>>-'		{ L _ ITRarrowtail }		-- for arrow notation
 '.'		{ L _ ITdot }

 '{'		{ L _ ITocurly } 			-- special symbols
 '}'		{ L _ ITccurly }
 '{|'           { L _ ITocurlybar }
 '|}'           { L _ ITccurlybar }
 vocurly	{ L _ ITvocurly } -- virtual open curly (from layout)
 vccurly	{ L _ ITvccurly } -- virtual close curly (from layout)
 '['		{ L _ ITobrack }
 ']'		{ L _ ITcbrack }
 '[:'		{ L _ ITopabrack }
 ':]'		{ L _ ITcpabrack }
 '('		{ L _ IToparen }
 ')'		{ L _ ITcparen }
 '(#'		{ L _ IToubxparen }
 '#)'		{ L _ ITcubxparen }
 '(|'		{ L _ IToparenbar }
 '|)'		{ L _ ITcparenbar }
 ';'		{ L _ ITsemi }
 ','		{ L _ ITcomma }
 '`'		{ L _ ITbackquote }

 VARID   	{ L _ (ITvarid    _) }		-- identifiers
 CONID   	{ L _ (ITconid    _) }
 VARSYM  	{ L _ (ITvarsym   _) }
 CONSYM  	{ L _ (ITconsym   _) }
 QVARID  	{ L _ (ITqvarid   _) }
 QCONID  	{ L _ (ITqconid   _) }
 QVARSYM 	{ L _ (ITqvarsym  _) }
 QCONSYM 	{ L _ (ITqconsym  _) }

 IPDUPVARID   	{ L _ (ITdupipvarid   _) }		-- GHC extension
 IPSPLITVARID  	{ L _ (ITsplitipvarid _) }		-- GHC extension

 CHAR		{ L _ (ITchar     _) }
 STRING		{ L _ (ITstring   _) }
 INTEGER	{ L _ (ITinteger  _) }
 RATIONAL	{ L _ (ITrational _) }
		    
 PRIMCHAR	{ L _ (ITprimchar   _) }
 PRIMSTRING	{ L _ (ITprimstring _) }
 PRIMINTEGER	{ L _ (ITprimint    _) }
 PRIMFLOAT	{ L _ (ITprimfloat  _) }
 PRIMDOUBLE	{ L _ (ITprimdouble _) }
 		    
-- Template Haskell 
'[|'            { L _ ITopenExpQuote  }       
'[p|'           { L _ ITopenPatQuote  }      
'[t|'           { L _ ITopenTypQuote  }      
'[d|'           { L _ ITopenDecQuote  }      
'|]'            { L _ ITcloseQuote    }
TH_ID_SPLICE    { L _ (ITidEscape _)  }     -- $x
'$('	        { L _ ITparenEscape   }     -- $( exp )
TH_VAR_QUOTE	{ L _ ITvarQuote      }     -- 'x
TH_TY_QUOTE	{ L _ ITtyQuote       }      -- ''T

%monad { P } { >>= } { return }
%lexer { lexer } { L _ ITeof }
%name parseModule module
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%name parseType ctype
%partial parseHeader header
%tokentype { (Located Token) }
%%

-----------------------------------------------------------------------------
-- Identifiers; one of the entry points
identifier :: { Located RdrName }
	: qvar				{ $1 }
	| qcon				{ $1 }
	| qvarop			{ $1 }
	| qconop			{ $1 }

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

module 	:: { Located (HsModule RdrName) }
 	: 'module' modid maybemoddeprec maybeexports 'where' body 
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule (Just $2) $4 (fst $6) (snd $6) $3)) }
	| missing_module_keyword top close
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule Nothing Nothing 
				(fst $2) (snd $2) Nothing)) }

missing_module_keyword :: { () }
	: {- empty -}				{% pushCurrentContext }

maybemoddeprec :: { Maybe DeprecTxt }
	: '{-# DEPRECATED' STRING '#-}' 	{ Just (getSTRING $2) }
	|  {- empty -}				{ Nothing }

body 	:: { ([LImportDecl RdrName], [LHsDecl RdrName]) }
	:  '{'            top '}'		{ $2 }
 	|      vocurly    top close		{ $2 }

top 	:: { ([LImportDecl RdrName], [LHsDecl RdrName]) }
	: importdecls				{ (reverse $1,[]) }
	| importdecls ';' cvtopdecls		{ (reverse $1,$3) }
	| cvtopdecls				{ ([],$1) }

cvtopdecls :: { [LHsDecl RdrName] }
	: topdecls				{ cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header 	:: { Located (HsModule RdrName) }
 	: 'module' modid maybemoddeprec maybeexports 'where' header_body
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule (Just $2) $4 $6 [] $3)) }
	| missing_module_keyword importdecls
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule Nothing Nothing $2 [] Nothing)) }

header_body :: { [LImportDecl RdrName] }
	:  '{'            importdecls		{ $2 }
 	|      vocurly    importdecls		{ $2 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { Maybe [LIE RdrName] }
	:  '(' exportlist ')'			{ Just $2 }
	|  {- empty -}				{ Nothing }

exportlist :: { [LIE RdrName] }
 	:  exportlist ',' export		{ $3 : $1 }
	|  exportlist ','			{ $1 }
 	|  export				{ [$1]  }
	|  {- empty -}				{ [] }

   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export 	:: { LIE RdrName }
	:  qvar				{ L1 (IEVar (unLoc $1)) }
	|  oqtycon			{ L1 (IEThingAbs (unLoc $1)) }
	|  oqtycon '(' '..' ')'		{ LL (IEThingAll (unLoc $1)) }
	|  oqtycon '(' ')'		{ LL (IEThingWith (unLoc $1) []) }
	|  oqtycon '(' qcnames ')'	{ LL (IEThingWith (unLoc $1) (reverse $3)) }
	|  'module' modid		{ LL (IEModuleContents (unLoc $2)) }

qcnames :: { [RdrName] }
	:  qcnames ',' qcname			{ unLoc $3 : $1 }
	|  qcname				{ [unLoc $1]  }

qcname 	:: { Located RdrName }	-- Variable or data constructor
	:  qvar					{ $1 }
	|  qcon					{ $1 }

-----------------------------------------------------------------------------
-- Import Declarations

-- import decls can be *empty*, or even just a string of semicolons
-- whereas topdecls must contain at least one topdecl.

importdecls :: { [LImportDecl RdrName] }
	: importdecls ';' importdecl		{ $3 : $1 }
	| importdecls ';'			{ $1 }
	| importdecl				{ [ $1 ] }
	| {- empty -}				{ [] }

importdecl :: { LImportDecl RdrName }
	: 'import' maybe_src optqualified modid maybeas maybeimpspec 
		{ L (comb4 $1 $4 $5 $6) (ImportDecl $4 $2 $3 (unLoc $5) (unLoc $6)) }

maybe_src :: { IsBootInterface }
	: '{-# SOURCE' '#-}'			{ True }
	| {- empty -}				{ False }

optqualified :: { Bool }
      	: 'qualified'                           { True  }
      	| {- empty -}				{ False }

maybeas :: { Located (Maybe Module) }
      	: 'as' modid                            { LL (Just (unLoc $2)) }
      	| {- empty -}				{ noLoc Nothing }

maybeimpspec :: { Located (Maybe (Bool, [LIE RdrName])) }
	: impspec				{ L1 (Just (unLoc $1)) }
	| {- empty -}				{ noLoc Nothing }

impspec :: { Located (Bool, [LIE RdrName]) }
	:  '(' exportlist ')'  			{ LL (False, reverse $2) }
	|  'hiding' '(' exportlist ')' 		{ LL (True,  reverse $3) }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec 	:: { Int }
	: {- empty -}		{ 9 }
	| INTEGER		{% checkPrecP (L1 (fromInteger (getINTEGER $1))) }

infix 	:: { Located FixityDirection }
	: 'infix'				{ L1 InfixN  }
	| 'infixl'				{ L1 InfixL  }
	| 'infixr'				{ L1 InfixR }

ops   	:: { Located [Located RdrName] }
	: ops ',' op				{ LL ($3 : unLoc $1) }
	| op					{ L1 [$1] }

-----------------------------------------------------------------------------
-- Top-Level Declarations

topdecls :: { OrdList (LHsDecl RdrName) }
	: topdecls ';' topdecl		{ $1 `appOL` $3 }
	| topdecls ';'			{ $1 }
	| topdecl			{ $1 }

topdecl :: { OrdList (LHsDecl RdrName) }
  	: tycl_decl			{ unitOL (L1 (TyClD (unLoc $1))) }
	| 'instance' inst_type where
		{ let (binds,sigs) = cvBindsAndSigs (unLoc $3)
		  in unitOL (L (comb3 $1 $2 $3) (InstD (InstDecl $2 binds sigs))) }
	| 'default' '(' comma_types0 ')'	{ unitOL (LL $ DefD (DefaultDecl $3)) }
	| 'foreign' fdecl			{ unitOL (LL (unLoc $2)) }
	| '{-# DEPRECATED' deprecations '#-}'	{ $2 }
	| '{-# RULES' rules '#-}'		{ $2 }
      	| decl					{ unLoc $1 }

	-- Template Haskell Extension
	| '$(' exp ')'				{ unitOL (LL $ SpliceD (SpliceDecl $2)) }
	| TH_ID_SPLICE				{ unitOL (LL $ SpliceD (SpliceDecl $
							L1 $ HsVar (mkUnqual varName (getTH_ID_SPLICE $1))
						  )) }

tycl_decl :: { LTyClDecl RdrName }
 	: 'type' type '=' ctype	
		-- Note type on the left of the '='; this allows
		-- infix type constructors to be declared
		-- 
		-- Note ctype, not sigtype, on the right
		-- We allow an explicit for-all but we don't insert one
		-- in 	type Foo a = (b,b)
		-- Instead we just say b is out of scope
 		{% do { (tc,tvs) <- checkSynHdr $2
		      ; return (LL (TySynonym tc tvs $4)) } }

	| data_or_newtype tycl_hdr constrs deriving
		{ L (comb4 $1 $2 $3 $4)	-- We need the location on tycl_hdr 
					-- in case constrs and deriving are both empty
		    (mkTyData (unLoc $1) (unLoc $2) Nothing (reverse (unLoc $3)) (unLoc $4)) }

        | data_or_newtype tycl_hdr opt_kind_sig 
		 'where' gadt_constrlist
		 deriving
		{ L (comb4 $1 $2 $4 $5)
		    (mkTyData (unLoc $1) (unLoc $2) $3 (reverse (unLoc $5)) (unLoc $6)) }

	| 'class' tycl_hdr fds where
		{ let 
			(binds,sigs) = cvBindsAndSigs (unLoc $4)
		  in
	 	  L (comb4 $1 $2 $3 $4) (mkClassDecl (unLoc $2) (unLoc $3) sigs 
					  binds) }

data_or_newtype :: { Located NewOrData }
	: 'data'	{ L1 DataType }
	| 'newtype'	{ L1 NewType }

opt_kind_sig :: { Maybe Kind }
	: 				{ Nothing }
	| '::' kind			{ Just $2 }

-- tycl_hdr parses the header of a type or class decl,
-- which takes the form
--	T a b
-- 	Eq a => T a
--	(Eq a, Ord b) => T a b
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (LHsContext RdrName, Located RdrName, [LHsTyVarBndr RdrName]) }
	: context '=>' type		{% checkTyClHdr $1         $3 >>= return.LL }
	| type				{% checkTyClHdr (noLoc []) $1 >>= return.L1 }

-----------------------------------------------------------------------------
-- Nested declarations

decls 	:: { Located (OrdList (LHsDecl RdrName)) }	
	: decls ';' decl		{ LL (unLoc $1 `appOL` unLoc $3) }
	| decls ';'			{ LL (unLoc $1) }
	| decl				{ $1 }
	| {- empty -}			{ noLoc nilOL }


decllist :: { Located (OrdList (LHsDecl RdrName)) }
	: '{'            decls '}'	{ LL (unLoc $2) }
	|     vocurly    decls close	{ $2 }

where 	:: { Located (OrdList (LHsDecl RdrName)) }
				-- No implicit parameters
	: 'where' decllist		{ LL (unLoc $2) }
	| {- empty -}			{ noLoc nilOL }

binds 	::  { Located (HsLocalBinds RdrName) } 		-- May have implicit parameters
	: decllist			{ L1 (HsValBinds (cvBindGroup (unLoc $1))) }
	| '{'            dbinds '}'	{ LL (HsIPBinds (IPBinds (unLoc $2) emptyLHsBinds)) }
	|     vocurly    dbinds close	{ L (getLoc $2) (HsIPBinds (IPBinds (unLoc $2) emptyLHsBinds)) }

wherebinds :: { Located (HsLocalBinds RdrName) }	-- May have implicit parameters
	: 'where' binds			{ LL (unLoc $2) }
	| {- empty -}			{ noLoc emptyLocalBinds }


-----------------------------------------------------------------------------
-- Transformation Rules

rules	:: { OrdList (LHsDecl RdrName) }
	:  rules ';' rule			{ $1 `snocOL` $3 }
        |  rules ';'				{ $1 }
        |  rule					{ unitOL $1 }
	|  {- empty -}				{ nilOL }

rule  	:: { LHsDecl RdrName }
	: STRING activation rule_forall infixexp '=' exp
	     { LL $ RuleD (HsRule (getSTRING $1) 
				  ($2 `orElse` AlwaysActive) 
				  $3 $4 placeHolderNames $6 placeHolderNames) }

activation :: { Maybe Activation } 
        : {- empty -}                           { Nothing }
        | explicit_activation                   { Just $1 }

explicit_activation :: { Activation }  -- In brackets
        : '[' INTEGER ']'		{ ActiveAfter  (fromInteger (getINTEGER $2)) }
        | '[' '~' INTEGER ']'		{ ActiveBefore (fromInteger (getINTEGER $3)) }

rule_forall :: { [RuleBndr RdrName] }
	: 'forall' rule_var_list '.'            { $2 }
        | {- empty -}				{ [] }

rule_var_list :: { [RuleBndr RdrName] }
        : rule_var				{ [$1] }
        | rule_var rule_var_list		{ $1 : $2 }

rule_var :: { RuleBndr RdrName }
	: varid                              	{ RuleBndr $1 }
       	| '(' varid '::' ctype ')'             	{ RuleBndrSig $2 $4 }

-----------------------------------------------------------------------------
-- Deprecations (c.f. rules)

deprecations :: { OrdList (LHsDecl RdrName) }
	: deprecations ';' deprecation		{ $1 `appOL` $3 }
	| deprecations ';' 			{ $1 }
	| deprecation				{ $1 }
	| {- empty -}				{ nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LHsDecl RdrName) }
	: depreclist STRING
		{ toOL [ LL $ DeprecD (Deprecation n (getSTRING $2)) 
		       | n <- unLoc $1 ] }


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
fdecl :: { LHsDecl RdrName }
fdecl : 'import' callconv safety1 fspec
		{% mkImport $2 $3 (unLoc $4) >>= return.LL }
      | 'import' callconv         fspec		
		{% do { d <- mkImport $2 (PlaySafe False) (unLoc $3);
			return (LL d) } }
      | 'export' callconv fspec
		{% mkExport $2 (unLoc $3) >>= return.LL }
        -- the following syntax is DEPRECATED
      | fdecl1DEPRECATED			{ L1 (ForD (unLoc $1)) }
      | fdecl2DEPRECATED			{ L1 (unLoc $1) }

fdecl1DEPRECATED :: { LForeignDecl RdrName }
fdecl1DEPRECATED 
  ----------- DEPRECATED label decls ------------
  : 'label' ext_name varid '::' sigtype
    { LL $ ForeignImport $3 $5 (CImport defaultCCallConv (PlaySafe False) nilFS nilFS 
				   (CLabel ($2 `orElse` mkExtName (unLoc $3)))) True }

  ----------- DEPRECATED ccall/stdcall decls ------------
  --
  -- NB: This business with the case expression below may seem overly
  --	 complicated, but it is necessary to avoid some conflicts.

    -- DEPRECATED variant #1: lack of a calling convention specification
    --			      (import) 
  | 'import' {-no callconv-} ext_name safety varid_no_unsafe '::' sigtype
    { let
	target = StaticTarget ($2 `orElse` mkExtName (unLoc $4))
      in
      LL $ ForeignImport $4 $6 (CImport defaultCCallConv $3 nilFS nilFS 
				   (CFunction target)) True }

    -- DEPRECATED variant #2: external name consists of two separate strings
    --			      (module name and function name) (import)
  | 'import' callconv STRING STRING safety varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError (comb2 $1 $>) "Illegal format of .NET foreign import"
	 CCall cconv -> return $
           let
	     imp = CFunction (StaticTarget (getSTRING $4))
	   in
	   LL $ ForeignImport $6 $8 (CImport cconv $5 nilFS nilFS imp) True }

    -- DEPRECATED variant #3: `unsafe' after entity
  | 'import' callconv STRING 'unsafe' varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError (comb2 $1 $>) "Illegal format of .NET foreign import"
	 CCall cconv -> return $
           let
	     imp = CFunction (StaticTarget (getSTRING $3))
	   in
	   LL $ ForeignImport $5 $7 (CImport cconv PlayRisky nilFS nilFS imp) True }

    -- DEPRECATED variant #4: use of the special identifier `dynamic' without
    --			      an explicit calling convention (import)
  | 'import' {-no callconv-} 'dynamic' safety varid_no_unsafe '::' sigtype
    { LL $ ForeignImport $4 $6 (CImport defaultCCallConv $3 nilFS nilFS 
				   (CFunction DynamicTarget)) True }

    -- DEPRECATED variant #5: use of the special identifier `dynamic' (import)
  | 'import' callconv 'dynamic' safety varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError (comb2 $1 $>) "Illegal format of .NET foreign import"
	 CCall cconv -> return $
	   LL $ ForeignImport $5 $7 (CImport cconv $4 nilFS nilFS 
					(CFunction DynamicTarget)) True }

    -- DEPRECATED variant #6: lack of a calling convention specification
    --			      (export) 
  | 'export' {-no callconv-} ext_name varid '::' sigtype
    { LL $ ForeignExport $3 $5 (CExport (CExportStatic ($2 `orElse` mkExtName (unLoc $3))
				   defaultCCallConv)) True }

    -- DEPRECATED variant #7: external name consists of two separate strings
    --			      (module name and function name) (export)
  | 'export' callconv STRING STRING varid '::' sigtype
    {% case $2 of
         DNCall      -> parseError (comb2 $1 $>) "Illegal format of .NET foreign import"
	 CCall cconv -> return $
           LL $ ForeignExport $5 $7 
			 (CExport (CExportStatic (getSTRING $4) cconv)) True }

    -- DEPRECATED variant #8: use of the special identifier `dynamic' without
    --			      an explicit calling convention (export)
  | 'export' {-no callconv-} 'dynamic' varid '::' sigtype
    { LL $ ForeignImport $3 $5 (CImport defaultCCallConv (PlaySafe False) nilFS nilFS 
				   CWrapper) True }

    -- DEPRECATED variant #9: use of the special identifier `dynamic' (export)
  | 'export' callconv 'dynamic' varid '::' sigtype
    {% case $2 of
         DNCall      -> parseError (comb2 $1 $>) "Illegal format of .NET foreign import"
	 CCall cconv -> return $
	   LL $ ForeignImport $4 $6 
		 (CImport cconv (PlaySafe False) nilFS nilFS CWrapper) True }

  ----------- DEPRECATED .NET decls ------------
  -- NB: removed the .NET call declaration, as it is entirely subsumed
  --     by the new standard FFI declarations

fdecl2DEPRECATED :: { LHsDecl RdrName }
fdecl2DEPRECATED 
  : 'import' 'dotnet' 'type' ext_name tycon { LL $ TyClD (ForeignType $5 $4 DNType) }
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

fspec :: { Located (Located FastString, Located RdrName, LHsType RdrName) }
       : STRING var '::' sigtype      { LL (L (getLoc $1) (getSTRING $1), $2, $4) }
       |        var '::' sigtype      { LL (noLoc nilFS, $1, $3) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-- DEPRECATED syntax
ext_name :: { Maybe CLabelString }
	: STRING		{ Just (getSTRING $1) }
	| STRING STRING		{ Just (getSTRING $2) }	-- Ignore "module name" for now
	| {- empty -}           { Nothing }


-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { Maybe (LHsType RdrName) }
	: {- empty -}			{ Nothing }
	| '::' sigtype			{ Just $2 }

opt_asig :: { Maybe (LHsType RdrName) }
	: {- empty -}			{ Nothing }
	| '::' atype			{ Just $2 }

sigtypes1 :: { [LHsType RdrName] }
	: sigtype			{ [ $1 ] }
	| sigtype ',' sigtypes1		{ $1 : $3 }

sigtype :: { LHsType RdrName }
	: ctype				{ L1 (mkImplicitHsForAllTy (noLoc []) $1) }
	-- Wrap an Implicit forall if there isn't one there already

sig_vars :: { Located [Located RdrName] }
	 : sig_vars ',' var		{ LL ($3 : unLoc $1) }
	 | var				{ L1 [$1] }

-----------------------------------------------------------------------------
-- Types

strict_mark :: { Located HsBang }
	: '!'				{ L1 HsStrict }
	| '{-# UNPACK' '#-}' '!'	{ LL HsUnbox }

-- A ctype is a for-all type
ctype	:: { LHsType RdrName }
	: 'forall' tv_bndrs '.' ctype	{ LL $ mkExplicitHsForAllTy $2 (noLoc []) $4 }
	| context '=>' type		{ LL $ mkImplicitHsForAllTy   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| type				{ $1 }

-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--	(Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>
context :: { LHsContext RdrName }
	: btype 			{% checkContext $1 }

type :: { LHsType RdrName }
	: ipvar '::' gentype		{ LL (HsPredTy (HsIParam (unLoc $1) $3)) }
	| gentype			{ $1 }

gentype :: { LHsType RdrName }
        : btype                         { $1 }
        | btype qtyconop gentype        { LL $ HsOpTy $1 $2 $3 }
        | btype tyvarop  gentype  	{ LL $ HsOpTy $1 $2 $3 }
 	| btype '->' ctype		{ LL $ HsFunTy $1 $3 }

btype :: { LHsType RdrName }
	: btype atype			{ LL $ HsAppTy $1 $2 }
	| atype				{ $1 }

atype :: { LHsType RdrName }
	: gtycon			{ L1 (HsTyVar (unLoc $1)) }
	| tyvar				{ L1 (HsTyVar (unLoc $1)) }
	| strict_mark atype		{ LL (HsBangTy (unLoc $1) $2) }
	| '(' ctype ',' comma_types1 ')'  { LL $ HsTupleTy Boxed  ($2:$4) }
	| '(#' comma_types1 '#)'	{ LL $ HsTupleTy Unboxed $2     }
	| '[' ctype ']'			{ LL $ HsListTy  $2 }
	| '[:' ctype ':]'		{ LL $ HsPArrTy  $2 }
	| '(' ctype ')'		        { LL $ HsParTy   $2 }
	| '(' ctype '::' kind ')'	{ LL $ HsKindSig $2 $4 }
-- Generics
        | INTEGER                       { L1 (HsNumTy (getINTEGER $1)) }

-- An inst_type is what occurs in the head of an instance decl
--	e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type, with a MonoDictTy at the right
-- hand corner, for convenience.
inst_type :: { LHsType RdrName }
	: sigtype			{% checkInstType $1 }

inst_types1 :: { [LHsType RdrName] }
	: inst_type			{ [$1] }
	| inst_type ',' inst_types1	{ $1 : $3 }

comma_types0  :: { [LHsType RdrName] }
	: comma_types1			{ $1 }
	| {- empty -}			{ [] }

comma_types1	:: { [LHsType RdrName] }
	: ctype				{ [$1] }
	| ctype  ',' comma_types1	{ $1 : $3 }

tv_bndrs :: { [LHsTyVarBndr RdrName] }
	 : tv_bndr tv_bndrs		{ $1 : $2 }
	 | {- empty -}			{ [] }

tv_bndr :: { LHsTyVarBndr RdrName }
	: tyvar				{ L1 (UserTyVar (unLoc $1)) }
	| '(' tyvar '::' kind ')'	{ LL (KindedTyVar (unLoc $2) $4) }

fds :: { Located [Located ([RdrName], [RdrName])] }
	: {- empty -}			{ noLoc [] }
	| '|' fds1			{ LL (reverse (unLoc $2)) }

fds1 :: { Located [Located ([RdrName], [RdrName])] }
	: fds1 ',' fd			{ LL ($3 : unLoc $1) }
	| fd				{ L1 [$1] }

fd :: { Located ([RdrName], [RdrName]) }
	: varids0 '->' varids0		{ L (comb3 $1 $2 $3)
					   (reverse (unLoc $1), reverse (unLoc $3)) }

varids0	:: { Located [RdrName] }
	: {- empty -}			{ noLoc [] }
	| varids0 tyvar			{ LL (unLoc $2 : unLoc $1) }

-----------------------------------------------------------------------------
-- Kinds

kind	:: { Kind }
	: akind			{ $1 }
	| akind '->' kind	{ mkArrowKind $1 $3 }

akind	:: { Kind }
	: '*'			{ liftedTypeKind }
	| '!'			{ unliftedTypeKind }
	| '(' kind ')'		{ $2 }


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located [LConDecl RdrName] }
	: '{'            gadt_constrs '}'	{ LL (unLoc $2) }
	|     vocurly    gadt_constrs close	{ $2 }

gadt_constrs :: { Located [LConDecl RdrName] }
        : gadt_constrs ';' gadt_constr  { LL ($3 : unLoc $1) }
        | gadt_constrs ';' 		{ $1 }
        | gadt_constr                   { L1 [$1] } 

-- We allow the following forms:
--	C :: Eq a => a -> T a
--	C :: forall a. Eq a => !a -> T a
--	D { x,y :: a } :: T a
--	forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { LConDecl RdrName }
        : con '::' sigtype
              { LL (mkGadtDecl $1 $3) } 
        -- Syntax: Maybe merge the record stuff with the single-case above?
        --         (to kill the mostly harmless reduce/reduce error)
        -- XXX revisit autrijus
	| constr_stuff_record '::' sigtype
		{ let (con,details) = unLoc $1 in 
		  LL (ConDecl con Implicit [] (noLoc []) details (ResTyGADT $3)) }
{-
	| forall context '=>' constr_stuff_record '::' sigtype
		{ let (con,details) = unLoc $4 in 
		  LL (ConDecl con Implicit (unLoc $1) $2 details (ResTyGADT $6)) }
	| forall constr_stuff_record '::' sigtype
		{ let (con,details) = unLoc $2 in 
		  LL (ConDecl con Implicit (unLoc $1) (noLoc []) details (ResTyGADT $4)) }
-}


constrs :: { Located [LConDecl RdrName] }
        : {- empty; a GHC extension -}  { noLoc [] }
        | '=' constrs1                  { LL (unLoc $2) }

constrs1 :: { Located [LConDecl RdrName] }
	: constrs1 '|' constr		{ LL ($3 : unLoc $1) }
	| constr			{ L1 [$1] }

constr :: { LConDecl RdrName }
	: forall context '=>' constr_stuff	
		{ let (con,details) = unLoc $4 in 
		  LL (ConDecl con Explicit (unLoc $1) $2 details ResTyH98) }
	| forall constr_stuff
		{ let (con,details) = unLoc $2 in 
		  LL (ConDecl con Explicit (unLoc $1) (noLoc []) details ResTyH98) }

forall :: { Located [LHsTyVarBndr RdrName] }
	: 'forall' tv_bndrs '.'		{ LL $2 }
	| {- empty -}			{ noLoc [] }

constr_stuff :: { Located (Located RdrName, HsConDetails RdrName (LBangType RdrName)) }
-- We parse the constructor declaration 
--	C t1 t2
-- as a btype (treating C as a type constructor) and then convert C to be
-- a data constructor.  Reason: it might continue like this:
--	C t1 t2 %: D Int
-- in which case C really would be a type constructor.  We can't resolve this
-- ambiguity till we come across the constructor oprerator :% (or not, more usually)
	: btype				{% mkPrefixCon $1 [] >>= return.LL }
	| oqtycon '{' '}' 		{% mkRecCon $1 [] >>= return.LL }
	| oqtycon '{' fielddecls '}' 	{% mkRecCon $1 $3 >>= return.LL }
	| btype conop btype		{ LL ($2, InfixCon $1 $3) }

constr_stuff_record :: { Located (Located RdrName, HsConDetails RdrName (LBangType RdrName)) }
	: oqtycon '{' '}' 		{% mkRecCon $1 [] >>= return.sL (comb2 $1 $>) }
	| oqtycon '{' fielddecls '}' 	{% mkRecCon $1 $3 >>= return.sL (comb2 $1 $>) }

fielddecls :: { [([Located RdrName], LBangType RdrName)] }
	: fielddecl ',' fielddecls	{ unLoc $1 : $3 }
	| fielddecl			{ [unLoc $1] }

fielddecl :: { Located ([Located RdrName], LBangType RdrName) }
	: sig_vars '::' ctype		{ LL (reverse (unLoc $1), $3) }

-- We allow the odd-looking 'inst_type' in a deriving clause, so that
-- we can do deriving( forall a. C [a] ) in a newtype (GHC extension).
-- The 'C [a]' part is converted to an HsPredTy by checkInstType
-- We don't allow a context, but that's sorted out by the type checker.
deriving :: { Located (Maybe [LHsType RdrName]) }
	: {- empty -}				{ noLoc Nothing }
	| 'deriving' qtycon	{% do { let { L loc tv = $2 }
				      ; p <- checkInstType (L loc (HsTyVar tv))
				      ; return (LL (Just [p])) } }
	| 'deriving' '(' ')'	 		{ LL (Just []) }
	| 'deriving' '(' inst_types1 ')' 	{ LL (Just $3) }
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

decl 	:: { Located (OrdList (LHsDecl RdrName)) }
	: sigdecl			{ $1 }
	| '!' infixexp rhs		{% do { pat <- checkPattern $2;
					        return (LL $ unitOL $ LL $ ValD $ 
							PatBind (LL $ BangPat pat) (unLoc $3)
								placeHolderType placeHolderNames) } }
	| infixexp opt_sig rhs		{% do { r <- checkValDef $1 $2 $3;
						return (LL $ unitOL (LL $ ValD r)) } }

rhs	:: { Located (GRHSs RdrName) }
	: '=' exp wherebinds	{ L (comb3 $1 $2 $3) $ GRHSs (unguardedRHS $2) (unLoc $3) }
	| gdrhs	wherebinds	{ LL $ GRHSs (reverse (unLoc $1)) (unLoc $2) }

gdrhs :: { Located [LGRHS RdrName] }
	: gdrhs gdrh		{ LL ($2 : unLoc $1) }
	| gdrh			{ L1 [$1] }

gdrh :: { LGRHS RdrName }
	: '|' quals '=' exp  	{ sL (comb2 $1 $>) $ GRHS (reverse (unLoc $2)) $4 }

sigdecl :: { Located (OrdList (LHsDecl RdrName)) }
	: infixexp '::' sigtype
				{% do s <- checkValSig $1 $3; 
				      return (LL $ unitOL (LL $ SigD s)) }
		-- See the above notes for why we need infixexp here
	| var ',' sig_vars '::' sigtype	
				{ LL $ toOL [ LL $ SigD (TypeSig n $5) | n <- $1 : unLoc $3 ] }
	| infix prec ops	{ LL $ toOL [ LL $ SigD (FixSig (FixitySig n (Fixity $2 (unLoc $1))))
					     | n <- unLoc $3 ] }
	| '{-# INLINE'   activation qvar '#-}'	      
				{ LL $ unitOL (LL $ SigD (InlineSig $3 (mkInlineSpec $2 (getINLINE $1)))) }
	| '{-# SPECIALISE' qvar '::' sigtypes1 '#-}'
			 	{ LL $ toOL [ LL $ SigD (SpecSig $2 t defaultInlineSpec)
					    | t <- $4] }
	| '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
			 	{ LL $ toOL [ LL $ SigD (SpecSig $3 t (mkInlineSpec $2 (getSPEC_INLINE $1)))
					    | t <- $5] }
	| '{-# SPECIALISE' 'instance' inst_type '#-}'
				{ LL $ unitOL (LL $ SigD (SpecInstSig $3)) }

-----------------------------------------------------------------------------
-- Expressions

exp   :: { LHsExpr RdrName }
	: infixexp '::' sigtype		{ LL $ ExprWithTySig $1 $3 }
	| infixexp '-<' exp		{ LL $ HsArrApp $1 $3 placeHolderType HsFirstOrderApp True }
	| infixexp '>-' exp		{ LL $ HsArrApp $3 $1 placeHolderType HsFirstOrderApp False }
	| infixexp '-<<' exp		{ LL $ HsArrApp $1 $3 placeHolderType HsHigherOrderApp True }
	| infixexp '>>-' exp		{ LL $ HsArrApp $3 $1 placeHolderType HsHigherOrderApp False}
	| infixexp			{ $1 }

infixexp :: { LHsExpr RdrName }
	: exp10				{ $1 }
	| infixexp qop exp10		{ LL (OpApp $1 $2 (panic "fixity") $3) }

exp10 :: { LHsExpr RdrName }
	: '\\' aexp aexps opt_asig '->' exp	
			{% checkPatterns ($2 : reverse $3) >>= \ ps -> 
			   return (LL $ HsLam (mkMatchGroup [LL $ Match ps $4
					    (GRHSs (unguardedRHS $6) emptyLocalBinds
							)])) }
  	| 'let' binds 'in' exp			{ LL $ HsLet (unLoc $2) $4 }
	| 'if' exp 'then' exp 'else' exp	{ LL $ HsIf $2 $4 $6 }
   	| 'case' exp 'of' altslist		{ LL $ HsCase $2 (mkMatchGroup (unLoc $4)) }
	| '-' fexp				{ LL $ mkHsNegApp $2 }

  	| 'do' stmtlist			{% let loc = comb2 $1 $2 in
					   checkDo loc (unLoc $2)  >>= \ (stmts,body) ->
					   return (L loc (mkHsDo DoExpr stmts body)) }
  	| 'mdo' stmtlist		{% let loc = comb2 $1 $2 in
					   checkDo loc (unLoc $2)  >>= \ (stmts,body) ->
					   return (L loc (mkHsDo (MDoExpr noPostTcTable) stmts body)) }
        | scc_annot exp		    		{ LL $ if opt_SccProfilingOn
							then HsSCC (unLoc $1) $2
							else HsPar $2 }

	| 'proc' aexp '->' exp	
			{% checkPattern $2 >>= \ p -> 
			   return (LL $ HsProc p (LL $ HsCmdTop $4 [] 
						   placeHolderType undefined)) }
						-- TODO: is LL right here?

        | '{-# CORE' STRING '#-}' exp           { LL $ HsCoreAnn (getSTRING $2) $4 }
						    -- hdaume: core annotation
	| fexp					{ $1 }

scc_annot :: { Located FastString }
	: '_scc_' STRING			{ LL $ getSTRING $2 }
	| '{-# SCC' STRING '#-}'		{ LL $ getSTRING $2 }

fexp 	:: { LHsExpr RdrName }
	: fexp aexp				{ LL $ HsApp $1 $2 }
  	| aexp					{ $1 }

aexps 	:: { [LHsExpr RdrName] }
	: aexps aexp				{ $2 : $1 }
  	| {- empty -}				{ [] }

aexp	:: { LHsExpr RdrName }
	: qvar '@' aexp			{ LL $ EAsPat $1 $3 }
	| '~' aexp			{ LL $ ELazyPat $2 }
--	| '!' aexp			{ LL $ EBangPat $2 }
	| aexp1				{ $1 }

aexp1	:: { LHsExpr RdrName }
        : aexp1 '{' fbinds '}' 	{% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4) 
							(reverse $3);
				        return (LL r) }}
  	| aexp2			{ $1 }

-- Here was the syntax for type applications that I was planning
-- but there are difficulties (e.g. what order for type args)
-- so it's not enabled yet.
-- But this case *is* used for the left hand side of a generic definition,
-- which is parsed as an expression before being munged into a pattern
 	| qcname '{|' gentype '|}'      { LL $ HsApp (sL (getLoc $1) (HsVar (unLoc $1)))
						     (sL (getLoc $3) (HsType $3)) }

aexp2	:: { LHsExpr RdrName }
	: ipvar				{ L1 (HsIPVar $! unLoc $1) }
	| qcname			{ L1 (HsVar   $! unLoc $1) }
	| literal			{ L1 (HsLit   $! unLoc $1) }
	| INTEGER			{ L1 (HsOverLit $! mkHsIntegral (getINTEGER $1)) }
	| RATIONAL			{ L1 (HsOverLit $! mkHsFractional (getRATIONAL $1)) }
	| '(' exp ')'			{ LL (HsPar $2) }
	| '(' texp ',' texps ')'	{ LL $ ExplicitTuple ($2 : reverse $4) Boxed }
	| '(#' texps '#)'		{ LL $ ExplicitTuple (reverse $2)      Unboxed }
	| '[' list ']'                  { LL (unLoc $2) }
	| '[:' parr ':]'                { LL (unLoc $2) }
	| '(' infixexp qop ')'		{ LL $ SectionL $2 $3 }
	| '(' qopm infixexp ')'		{ LL $ SectionR $2 $3 }
	| '_'				{ L1 EWildPat }
	
	-- Template Haskell Extension
	| TH_ID_SPLICE          { L1 $ HsSpliceE (mkHsSplice 
					(L1 $ HsVar (mkUnqual varName 
							(getTH_ID_SPLICE $1)))) } -- $x
	| '$(' exp ')'   	{ LL $ HsSpliceE (mkHsSplice $2) }               -- $( exp )

	| TH_VAR_QUOTE qvar 	{ LL $ HsBracket (VarBr (unLoc $2)) }
	| TH_VAR_QUOTE qcon 	{ LL $ HsBracket (VarBr (unLoc $2)) }
	| TH_TY_QUOTE tyvar 	{ LL $ HsBracket (VarBr (unLoc $2)) }
 	| TH_TY_QUOTE gtycon	{ LL $ HsBracket (VarBr (unLoc $2)) }
	| '[|' exp '|]'         { LL $ HsBracket (ExpBr $2) }                       
	| '[t|' ctype '|]'      { LL $ HsBracket (TypBr $2) }                       
	| '[p|' infixexp '|]'   {% checkPattern $2 >>= \p ->
					   return (LL $ HsBracket (PatBr p)) }
	| '[d|' cvtopbody '|]'	{ LL $ HsBracket (DecBr (mkGroup $2)) }

	-- arrow notation extension
	| '(|' aexp2 cmdargs '|)'	{ LL $ HsArrForm $2 Nothing (reverse $3) }

cmdargs	:: { [LHsCmdTop RdrName] }
	: cmdargs acmd			{ $2 : $1 }
  	| {- empty -}			{ [] }

acmd	:: { LHsCmdTop RdrName }
	: aexp2			{ L1 $ HsCmdTop $1 [] placeHolderType undefined }

cvtopbody :: { [LHsDecl RdrName] }
	:  '{'            cvtopdecls0 '}'		{ $2 }
	|      vocurly    cvtopdecls0 close		{ $2 }

cvtopdecls0 :: { [LHsDecl RdrName] }
	: {- empty -}		{ [] }
	| cvtopdecls		{ $1 }

texp :: { LHsExpr RdrName }
	: exp				{ $1 }
	| qopm infixexp			{ LL $ SectionR $1 $2 }
	-- The second production is really here only for bang patterns
	-- but 

texps :: { [LHsExpr RdrName] }
	: texps ',' texp		{ $3 : $1 }
	| texp				{ [$1] }


-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.

list :: { LHsExpr RdrName }
	: texp			{ L1 $ ExplicitList placeHolderType [$1] }
	| lexps 		{ L1 $ ExplicitList placeHolderType (reverse (unLoc $1)) }
	| texp '..'		{ LL $ ArithSeq noPostTcExpr (From $1) }
	| texp ',' exp '..' 	{ LL $ ArithSeq noPostTcExpr (FromThen $1 $3) }
	| texp '..' exp	 	{ LL $ ArithSeq noPostTcExpr (FromTo $1 $3) }
	| texp ',' exp '..' exp	{ LL $ ArithSeq noPostTcExpr (FromThenTo $1 $3 $5) }
	| texp pquals		{ sL (comb2 $1 $>) $ mkHsDo ListComp (reverse (unLoc $2)) $1 }

lexps :: { Located [LHsExpr RdrName] }
	: lexps ',' texp 		{ LL ($3 : unLoc $1) }
	| texp ',' texp			{ LL [$3,$1] }

-----------------------------------------------------------------------------
-- List Comprehensions

pquals :: { Located [LStmt RdrName] }	-- Either a singleton ParStmt, 
					-- or a reversed list of Stmts
	: pquals1			{ case unLoc $1 of
					    [qs] -> L1 qs
					    qss  -> L1 [L1 (ParStmt stmtss)]
						 where
						    stmtss = [ (reverse qs, undefined) 
						    	     | qs <- qss ]
					}
			
pquals1 :: { Located [[LStmt RdrName]] }
	: pquals1 '|' quals		{ LL (unLoc $3 : unLoc $1) }
	| '|' quals			{ L (getLoc $2) [unLoc $2] }

quals :: { Located [LStmt RdrName] }
	: quals ',' qual		{ LL ($3 : unLoc $1) }
	| qual				{ L1 [$1] }

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { LHsExpr RdrName }
	: 				{ noLoc (ExplicitPArr placeHolderType []) }
	| exp				{ L1 $ ExplicitPArr placeHolderType [$1] }
	| lexps 			{ L1 $ ExplicitPArr placeHolderType 
						       (reverse (unLoc $1)) }
	| exp '..' exp	 		{ LL $ PArrSeq noPostTcExpr (FromTo $1 $3) }
	| exp ',' exp '..' exp		{ LL $ PArrSeq noPostTcExpr (FromThenTo $1 $3 $5) }
	| exp pquals			{ sL (comb2 $1 $>) $ mkHsDo PArrComp (reverse (unLoc $2)) $1 }

-- We are reusing `lexps' and `pquals' from the list case.

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { Located [LMatch RdrName] }
	: '{'            alts '}'	{ LL (reverse (unLoc $2)) }
	|     vocurly    alts  close	{ L (getLoc $2) (reverse (unLoc $2)) }

alts    :: { Located [LMatch RdrName] }
        : alts1				{ L1 (unLoc $1) }
	| ';' alts			{ LL (unLoc $2) }

alts1 	:: { Located [LMatch RdrName] }
	: alts1 ';' alt			{ LL ($3 : unLoc $1) }
	| alts1 ';'			{ LL (unLoc $1) }
	| alt				{ L1 [$1] }

alt 	:: { LMatch RdrName }
	: infixexp opt_sig alt_rhs	{%  checkPattern $1 >>= \p ->
			    		    return (LL (Match [p] $2 (unLoc $3))) }

alt_rhs :: { Located (GRHSs RdrName) }
	: ralt wherebinds		{ LL (GRHSs (unLoc $1) (unLoc $2)) }

ralt :: { Located [LGRHS RdrName] }
	: '->' exp			{ LL (unguardedRHS $2) }
	| gdpats			{ L1 (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS RdrName] }
	: gdpats gdpat			{ LL ($2 : unLoc $1) }
	| gdpat				{ L1 [$1] }

gdpat	:: { LGRHS RdrName }
	: '|' quals '->' exp	 	{ sL (comb2 $1 $>) $ GRHS (reverse (unLoc $2)) $4 }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { Located [LStmt RdrName] }
	: '{'         	stmts '}'	{ LL (unLoc $2) }
	|     vocurly   stmts close	{ $2 }

--	do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use ExprStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
stmts :: { Located [LStmt RdrName] }
	: stmt stmts_help		{ LL ($1 : unLoc $2) }
	| ';' stmts			{ LL (unLoc $2) }
	| {- empty -}			{ noLoc [] }

stmts_help :: { Located [LStmt RdrName] } -- might be empty
	: ';' stmts			{ LL (unLoc $2) }
	| {- empty -}			{ noLoc [] }

-- For typing stmts at the GHCi prompt, where 
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt RdrName) }
	: stmt				{ Just $1 }
	| {- nothing -}			{ Nothing }

stmt  :: { LStmt RdrName }
	: qual				{ $1 }
	| infixexp '->' exp		{% checkPattern $3 >>= \p ->
					   return (LL $ mkBindStmt p $1) }
  	| 'rec' stmtlist		{ LL $ mkRecStmt (unLoc $2) }

qual  :: { LStmt RdrName }
	: exp '<-' exp			{% checkPattern $1 >>= \p ->
					   return (LL $ mkBindStmt p $3) }
	| exp				{ L1 $ mkExprStmt $1 }
  	| 'let' binds			{ LL $ LetStmt (unLoc $2) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds 	:: { HsRecordBinds RdrName }
	: fbinds1			{ $1 }
  	| {- empty -}			{ [] }

fbinds1	:: { HsRecordBinds RdrName }
	: fbinds1 ',' fbind		{ $3 : $1 }
	| fbind				{ [$1] }
  
fbind	:: { (Located RdrName, LHsExpr RdrName) }
	: qvar '=' exp			{ ($1,$3) }

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds 	:: { Located [LIPBind RdrName] }
	: dbinds ';' dbind		{ LL ($3 : unLoc $1) }
	| dbinds ';'			{ LL (unLoc $1) }
	| dbind				{ L1 [$1] }
--	| {- empty -}			{ [] }

dbind	:: { LIPBind RdrName }
dbind	: ipvar '=' exp			{ LL (IPBind (unLoc $1) $3) }

ipvar	:: { Located (IPName RdrName) }
	: IPDUPVARID		{ L1 (Dupable (mkUnqual varName (getIPDUPVARID $1))) }
	| IPSPLITVARID		{ L1 (Linear  (mkUnqual varName (getIPSPLITVARID $1))) }

-----------------------------------------------------------------------------
-- Deprecations

depreclist :: { Located [RdrName] }
depreclist : deprec_var			{ L1 [unLoc $1] }
	   | deprec_var ',' depreclist	{ LL (unLoc $1 : unLoc $3) }

deprec_var :: { Located RdrName }
deprec_var : var			{ $1 }
	   | con			{ $1 }

-----------------------------------------
-- Data constructors
qcon	:: { Located RdrName }
	: qconid		{ $1 }
	| '(' qconsym ')'	{ LL (unLoc $2) }
	| sysdcon		{ L1 $ nameRdrName (dataConName (unLoc $1)) }
-- The case of '[:' ':]' is part of the production `parr'

con	:: { Located RdrName }
	: conid			{ $1 }
	| '(' consym ')'	{ LL (unLoc $2) }
	| sysdcon		{ L1 $ nameRdrName (dataConName (unLoc $1)) }

sysdcon	:: { Located DataCon }	-- Wired in data constructors
	: '(' ')'		{ LL unitDataCon }
	| '(' commas ')'	{ LL $ tupleCon Boxed $2 }
	| '[' ']'		{ LL nilDataCon }

conop :: { Located RdrName }
	: consym		{ $1 }	
	| '`' conid '`'		{ LL (unLoc $2) }

qconop :: { Located RdrName }
	: qconsym		{ $1 }
	| '`' qconid '`'	{ LL (unLoc $2) }

-----------------------------------------------------------------------------
-- Type constructors

gtycon 	:: { Located RdrName }	-- A "general" qualified tycon
	: oqtycon			{ $1 }
	| '(' ')'			{ LL $ getRdrName unitTyCon }
	| '(' commas ')'		{ LL $ getRdrName (tupleTyCon Boxed $2) }
	| '(' '->' ')'			{ LL $ getRdrName funTyCon }
	| '[' ']'			{ LL $ listTyCon_RDR }
	| '[:' ':]'			{ LL $ parrTyCon_RDR }

oqtycon :: { Located RdrName }	-- An "ordinary" qualified tycon
	: qtycon			{ $1 }
 	| '(' qtyconsym ')'		{ LL (unLoc $2) }

qtyconop :: { Located RdrName }	-- Qualified or unqualified
	: qtyconsym			{ $1 }
	| '`' qtycon '`'		{ LL (unLoc $2) }

qtycon :: { Located RdrName }	-- Qualified or unqualified
	: QCONID			{ L1 $! mkQual tcClsName (getQCONID $1) }
	| tycon				{ $1 }

tycon 	:: { Located RdrName }	-- Unqualified
	: CONID				{ L1 $! mkUnqual tcClsName (getCONID $1) }

qtyconsym :: { Located RdrName }
	: QCONSYM			{ L1 $! mkQual tcClsName (getQCONSYM $1) }
	| tyconsym			{ $1 }

tyconsym :: { Located RdrName }
	: CONSYM			{ L1 $! mkUnqual tcClsName (getCONSYM $1) }

-----------------------------------------------------------------------------
-- Operators

op	:: { Located RdrName }   -- used in infix decls
	: varop			{ $1 }
	| conop 		{ $1 }

varop	:: { Located RdrName }
	: varsym		{ $1 }
	| '`' varid '`'		{ LL (unLoc $2) }

qop	:: { LHsExpr RdrName }   -- used in sections
	: qvarop		{ L1 $ HsVar (unLoc $1) }
	| qconop		{ L1 $ HsVar (unLoc $1) }

qopm	:: { LHsExpr RdrName }   -- used in sections
	: qvaropm		{ L1 $ HsVar (unLoc $1) }
	| qconop		{ L1 $ HsVar (unLoc $1) }

qvarop :: { Located RdrName }
	: qvarsym		{ $1 }
	| '`' qvarid '`'	{ LL (unLoc $2) }

qvaropm :: { Located RdrName }
	: qvarsym_no_minus	{ $1 }
	| '`' qvarid '`'	{ LL (unLoc $2) }

-----------------------------------------------------------------------------
-- Type variables

tyvar   :: { Located RdrName }
tyvar   : tyvarid		{ $1 }
	| '(' tyvarsym ')'	{ LL (unLoc $2) }

tyvarop :: { Located RdrName }
tyvarop : '`' tyvarid '`'	{ LL (unLoc $2) }
	| tyvarsym		{ $1 }

tyvarid	:: { Located RdrName }
	: VARID			{ L1 $! mkUnqual tvName (getVARID $1) }
	| special_id		{ L1 $! mkUnqual tvName (unLoc $1) }
	| 'unsafe' 		{ L1 $! mkUnqual tvName FSLIT("unsafe") }
	| 'safe' 		{ L1 $! mkUnqual tvName FSLIT("safe") }
	| 'threadsafe' 		{ L1 $! mkUnqual tvName FSLIT("threadsafe") }

tyvarsym :: { Located RdrName }
-- Does not include "!", because that is used for strictness marks
--	         or ".", because that separates the quantified type vars from the rest
--		 or "*", because that's used for kinds
tyvarsym : VARSYM		{ L1 $! mkUnqual tvName (getVARSYM $1) }

-----------------------------------------------------------------------------
-- Variables 

var 	:: { Located RdrName }
	: varid			{ $1 }
	| '(' varsym ')'	{ LL (unLoc $2) }

qvar 	:: { Located RdrName }
	: qvarid		{ $1 }
	| '(' varsym ')'	{ LL (unLoc $2) }
	| '(' qvarsym1 ')'	{ LL (unLoc $2) }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

qvarid :: { Located RdrName }
	: varid			{ $1 }
	| QVARID		{ L1 $ mkQual varName (getQVARID $1) }

varid :: { Located RdrName }
	: varid_no_unsafe 	{ $1 }
	| 'unsafe'		{ L1 $! mkUnqual varName FSLIT("unsafe") }
	| 'safe'		{ L1 $! mkUnqual varName FSLIT("safe") }
	| 'threadsafe'		{ L1 $! mkUnqual varName FSLIT("threadsafe") }

varid_no_unsafe :: { Located RdrName }
	: VARID			{ L1 $! mkUnqual varName (getVARID $1) }
	| special_id		{ L1 $! mkUnqual varName (unLoc $1) }
	| 'forall'		{ L1 $! mkUnqual varName FSLIT("forall") }

qvarsym :: { Located RdrName }
	: varsym		{ $1 }
	| qvarsym1		{ $1 }

qvarsym_no_minus :: { Located RdrName }
	: varsym_no_minus	{ $1 }
	| qvarsym1		{ $1 }

qvarsym1 :: { Located RdrName }
qvarsym1 : QVARSYM		{ L1 $ mkQual varName (getQVARSYM $1) }

varsym :: { Located RdrName }
	: varsym_no_minus 	{ $1 }
	| '-'			{ L1 $ mkUnqual varName FSLIT("-") }

varsym_no_minus :: { Located RdrName } -- varsym not including '-'
	: VARSYM		{ L1 $ mkUnqual varName (getVARSYM $1) }
	| special_sym		{ L1 $ mkUnqual varName (unLoc $1) }


-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe' and 'forall' whose treatment differs depending on context
special_id :: { Located FastString }
special_id
	: 'as'			{ L1 FSLIT("as") }
	| 'qualified'		{ L1 FSLIT("qualified") }
	| 'hiding'		{ L1 FSLIT("hiding") }
	| 'export'		{ L1 FSLIT("export") }
	| 'label'		{ L1 FSLIT("label")  }
	| 'dynamic'		{ L1 FSLIT("dynamic") }
	| 'stdcall'             { L1 FSLIT("stdcall") }
	| 'ccall'               { L1 FSLIT("ccall") }

special_sym :: { Located FastString }
special_sym : '!'	{ L1 FSLIT("!") }
	    | '.' 	{ L1 FSLIT(".") }
 	    | '*' 	{ L1 FSLIT("*") }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }	-- Qualified or unqualified
	: conid			{ $1 }
	| QCONID		{ L1 $ mkQual dataName (getQCONID $1) }

conid 	:: { Located RdrName }
	: CONID			{ L1 $ mkUnqual dataName (getCONID $1) }

qconsym :: { Located RdrName }	-- Qualified or unqualified
	: consym		{ $1 }
	| QCONSYM		{ L1 $ mkQual dataName (getQCONSYM $1) }

consym :: { Located RdrName }
	: CONSYM		{ L1 $ mkUnqual dataName (getCONSYM $1) }

	-- ':' means only list cons
	| ':'			{ L1 $ consDataCon_RDR }


-----------------------------------------------------------------------------
-- Literals

literal :: { Located HsLit }
	: CHAR 			{ L1 $ HsChar       $ getCHAR $1 }
	| STRING		{ L1 $ HsString     $ getSTRING $1 }
	| PRIMINTEGER		{ L1 $ HsIntPrim    $ getPRIMINTEGER $1 }
	| PRIMCHAR		{ L1 $ HsCharPrim   $ getPRIMCHAR $1 }
	| PRIMSTRING		{ L1 $ HsStringPrim $ getPRIMSTRING $1 }
	| PRIMFLOAT		{ L1 $ HsFloatPrim  $ getPRIMFLOAT $1 }
	| PRIMDOUBLE		{ L1 $ HsDoublePrim $ getPRIMDOUBLE $1 }

-----------------------------------------------------------------------------
-- Layout

close :: { () }
	: vccurly		{ () } -- context popped in lexer.
	| error			{% popContext }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid 	:: { Located Module }
	: CONID			{ L1 $ mkModuleFS (getCONID $1) }
        | QCONID		{ L1 $ let (mod,c) = getQCONID $1 in
				  mkModuleFS
				   (mkFastString
				     (unpackFS mod ++ '.':unpackFS c))
				}

commas :: { Int }
	: commas ','			{ $1 + 1 }
	| ','				{ 2 }

-----------------------------------------------------------------------------

{
happyError :: P a
happyError = srcParseFail

getVARID   	(L _ (ITvarid    x)) = x
getCONID   	(L _ (ITconid    x)) = x
getVARSYM  	(L _ (ITvarsym   x)) = x
getCONSYM  	(L _ (ITconsym   x)) = x
getQVARID  	(L _ (ITqvarid   x)) = x
getQCONID  	(L _ (ITqconid   x)) = x
getQVARSYM 	(L _ (ITqvarsym  x)) = x
getQCONSYM 	(L _ (ITqconsym  x)) = x
getIPDUPVARID   (L _ (ITdupipvarid   x)) = x
getIPSPLITVARID (L _ (ITsplitipvarid x)) = x
getCHAR		(L _ (ITchar     x)) = x
getSTRING	(L _ (ITstring   x)) = x
getINTEGER	(L _ (ITinteger  x)) = x
getRATIONAL	(L _ (ITrational x)) = x
getPRIMCHAR	(L _ (ITprimchar   x)) = x
getPRIMSTRING	(L _ (ITprimstring x)) = x
getPRIMINTEGER	(L _ (ITprimint    x)) = x
getPRIMFLOAT	(L _ (ITprimfloat  x)) = x
getPRIMDOUBLE	(L _ (ITprimdouble x)) = x
getTH_ID_SPLICE (L _ (ITidEscape x)) = x
getINLINE	(L _ (ITinline_prag b)) = b
getSPEC_INLINE	(L _ (ITspec_inline_prag b)) = b

-- Utilities for combining source spans
comb2 :: Located a -> Located b -> SrcSpan
comb2 = combineLocs

comb3 :: Located a -> Located b -> Located c -> SrcSpan
comb3 a b c = combineSrcSpans (getLoc a) (combineSrcSpans (getLoc b) (getLoc c))

comb4 :: Located a -> Located b -> Located c -> Located d -> SrcSpan
comb4 a b c d = combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
		combineSrcSpans (getLoc c) (getLoc d)

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` L span a

-- Make a source location for the file.  We're a bit lazy here and just
-- make a point SrcSpan at line 1, column 0.  Strictly speaking we should
-- try to find the span of the whole file (ToDo).
fileSrcSpan :: P SrcSpan
fileSrcSpan = do 
  l <- getSrcLoc; 
  let loc = mkSrcLoc (srcLocFile l) 1 0;
  return (mkSrcSpan loc loc)
}
