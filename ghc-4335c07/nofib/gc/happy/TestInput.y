

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


#include "HsVersions.h"

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
import StaticFlags	( opt_SccProfilingOn, opt_Hpc )
import Type		( Kind, mkArrowKind, liftedTypeKind, unliftedTypeKind )
import BasicTypes	( Boxity(..), Fixity(..), FixityDirection(..), IPName(..),
			  Activation(..), defaultInlineSpec )
import OrdList
import HaddockParse
import {-# SOURCE #-} HaddockLex hiding ( Token )
import HaddockUtils

import FastString
import Maybes		( orElse )
import Outputable

import Control.Monad    ( unless )
import GHC.Exts
import Data.Char
import Control.Monad    ( mplus )
}

{-
-----------------------------------------------------------------------------
24 Februar 2006

Conflicts: 33 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
31 December 2006

Conflicts: 34 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
6 December 2006

Conflicts: 32 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
26 July 2006

Conflicts: 37 shift/reduce
           1 reduce/reduce

The reduce/reduce conflict is weird.  It's between tyconsym and consym, and I
would think the two should never occur in the same context.

  -=chak

-----------------------------------------------------------------------------
Conflicts: 38 shift/reduce (1.25)

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

1 for ambiguity in '{-# RULES "name" forall = ... #-}' 	[State 474]
	since 'forall' is a valid variable name, we don't know whether
	to treat a forall on the input as the beginning of a quantifier
	or the beginning of the rule itself.  Resolving to shift means
	it's always treated as a quantifier, hence the above is disallowed.
	This saves explicitly defining a grammar for the rule lhs that
	doesn't include 'forall'.

1 for ambiguity when the source file starts with "-- | doc". We need another
  token of lookahead to determine if a top declaration or the 'module' keyword
  follows. Shift parses as if the 'module' keyword follows.   

-- ---------------------------------------------------------------------------
-- Adding location info

This is done in a stylised way using the three macros below, L0, L1
and LL.  Each of these macros can be thought of as having type

   L0, L1, LL :: a -> Located a

They each add a SrcSpan to their argument.

   L0	adds 'noSrcSpan', used for empty productions
     -- This doesn't seem to work anymore -=chak

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
 'derive' 	{ L _ ITderive }
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

 'forall'	{ L _ ITforall }		-- GHC extension keywords
 'foreign'	{ L _ ITforeign }
 'export'	{ L _ ITexport }
 'label'	{ L _ ITlabel } 
 'dynamic'	{ L _ ITdynamic }
 'safe'		{ L _ ITsafe }
 'threadsafe'	{ L _ ITthreadsafe }
 'unsafe'	{ L _ ITunsafe }
 'mdo'		{ L _ ITmdo }
 'family'	{ L _ ITfamily }
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
 '{-# GENERATED'   { L _ ITgenerated_prag }
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

 CHAR		{ L _ (ITchar     _) }
 STRING		{ L _ (ITstring   _) }
 INTEGER	{ L _ (ITinteger  _) }
 RATIONAL	{ L _ (ITrational _) }
		    
 PRIMCHAR	{ L _ (ITprimchar   _) }
 PRIMSTRING	{ L _ (ITprimstring _) }
 PRIMINTEGER	{ L _ (ITprimint    _) }
 PRIMFLOAT	{ L _ (ITprimfloat  _) }
 PRIMDOUBLE	{ L _ (ITprimdouble _) }

 DOCNEXT	{ L _ (ITdocCommentNext _) }
 DOCPREV	{ L _ (ITdocCommentPrev _) }
 DOCNAMED	{ L _ (ITdocCommentNamed _) }
 DOCSECTION	{ L _ (ITdocSection _ _) }
 DOCOPTIONS	{ L _ (ITdocOptions _) }

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
 	: optdoc 'module' modid maybemoddeprec maybeexports 'where' body 
		{% fileSrcSpan >>= \ loc -> case $1 of { (opt, info, doc) -> 
		   return (L loc (HsModule (Just $3) $5 (fst $7) (snd $7) $4 
                          opt info doc) )}}
	| missing_module_keyword top close
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule Nothing Nothing 
                          (fst $2) (snd $2) Nothing Nothing emptyHaddockModInfo 
                          Nothing)) }

optdoc :: { (Maybe String, HaddockModInfo RdrName, Maybe (HsDoc RdrName)) }                             
        : moduleheader            { (Nothing, fst $1, snd $1) }
        | docoptions              { (Just $1, emptyHaddockModInfo, Nothing)} 
        | docoptions moduleheader { (Just $1, fst $2, snd $2) } 
        | moduleheader docoptions { (Just $2, fst $1, snd $1) } 
        | {- empty -}             { (Nothing, emptyHaddockModInfo, Nothing) }  

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
 	: optdoc 'module' modid maybemoddeprec maybeexports 'where' header_body
		{% fileSrcSpan >>= \ loc -> case $1 of { (opt, info, doc) -> 
		   return (L loc (HsModule (Just $3) $5 $7 [] $4 
                   opt info doc))}}
	| missing_module_keyword importdecls
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule Nothing Nothing $2 [] Nothing 
                   Nothing emptyHaddockModInfo Nothing)) }

header_body :: { [LImportDecl RdrName] }
	:  '{'            importdecls		{ $2 }
 	|      vocurly    importdecls		{ $2 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { Maybe [LIE RdrName] }
	:  '(' exportlist ')'			{ Just $2 }
	|  {- empty -}				{ Nothing }

exportlist :: { [LIE RdrName] }
	: expdoclist ',' expdoclist		{ $1 ++ $3 }
	| exportlist1				{ $1 }

exportlist1 :: { [LIE RdrName] }
        : expdoclist export expdoclist ',' exportlist  { $1 ++ ($2 : $3) ++ $5 }
 	| expdoclist export expdoclist	               { $1 ++ ($2 : $3) }
	| expdoclist				       { $1 }

expdoclist :: { [LIE RdrName] }
        : exp_doc expdoclist                           { $1 : $2 }
        | {- empty -}                                  { [] }

exp_doc :: { LIE RdrName }                                                   
        : docsection    { sL (getLoc $1) (case (unLoc $1) of (n, doc) -> IEGroup n doc) }
        | docnamed      { sL (getLoc $1) (IEDocNamed ((fst . unLoc) $1)) } 
        | docnext       { sL (getLoc $1) (IEDoc (unLoc $1)) }       
                       
   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export 	:: { LIE RdrName }
	:  qvar				{ sL (getLoc $1) (IEVar (unLoc $1)) }
	|  oqtycon			{ sL (getLoc $1) (IEThingAbs (unLoc $1)) }
	|  oqtycon '(' '..' ')'		{ sL (comb2 $1 $>) (IEThingAll (unLoc $1)) }
	|  oqtycon '(' ')'		{ sL (comb2 $1 $>) (IEThingWith (unLoc $1) []) }
	|  oqtycon '(' qcnames ')'	{ sL (comb2 $1 $>) (IEThingWith (unLoc $1) (reverse $3)) }
	|  'module' modid		{ sL (comb2 $1 $>) (IEModuleContents (unLoc $2)) }

qcnames :: { [RdrName] }
	:  qcnames ',' qcname_ext	{ unLoc $3 : $1 }
	|  qcname_ext			{ [unLoc $1]  }

qcname_ext :: { Located RdrName }	-- Variable or data constructor
					-- or tagged type constructor
	:  qcname			{ $1 }
	|  'type' qcon			{ sL (comb2 $1 $2) 
					     (setRdrNameSpace (unLoc $2) 
							      tcClsName)  }

-- Cannot pull into qcname_ext, as qcname is also used in expression.
qcname 	:: { Located RdrName }	-- Variable or data constructor
	:  qvar				{ $1 }
	|  qcon				{ $1 }

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

maybeas :: { Located (Maybe ModuleName) }
      	: 'as' modid                            { sL (comb2 $1 $>) (Just (unLoc $2)) }
      	| {- empty -}				{ noLoc Nothing }

maybeimpspec :: { Located (Maybe (Bool, [LIE RdrName])) }
	: impspec				{ sL (getLoc $1) (Just (unLoc $1)) }
	| {- empty -}				{ noLoc Nothing }

impspec :: { Located (Bool, [LIE RdrName]) }
	:  '(' exportlist ')'  			{ sL (comb2 $1 $>) (False, $2) }
	|  'hiding' '(' exportlist ')' 		{ sL (comb2 $1 $>) (True,  $3) }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec 	:: { Int }
	: {- empty -}		{ 9 }
	| INTEGER		{% checkPrecP (sL (getLoc $1) (fromInteger (getINTEGER $1))) }

infix 	:: { Located FixityDirection }
	: 'infix'				{ sL (getLoc $1) InfixN  }
	| 'infixl'				{ sL (getLoc $1) InfixL  }
	| 'infixr'				{ sL (getLoc $1) InfixR }

ops   	:: { Located [Located RdrName] }
	: ops ',' op				{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	| op					{ sL (getLoc $1) [$1] }

-----------------------------------------------------------------------------
-- Top-Level Declarations

topdecls :: { OrdList (LHsDecl RdrName) }
        : topdecls ';' topdecl		        { $1 `appOL` $3 }
        | topdecls ';'			        { $1 }
	| topdecl			        { $1 }

topdecl :: { OrdList (LHsDecl RdrName) }
  	: cl_decl			{ unitOL (sL (getLoc $1) (TyClD (unLoc $1))) }
  	| ty_decl			{ unitOL (sL (getLoc $1) (TyClD (unLoc $1))) }
	| 'instance' inst_type where_inst
	    { let (binds, sigs, ats, _) = cvBindsAndSigs (unLoc $3)
	      in 
	      unitOL (L (comb3 $1 $2 $3) (InstD (InstDecl $2 binds sigs ats)))}
        | stand_alone_deriving                  { unitOL (sL (comb2 $1 $>) (DerivD (unLoc $1))) }
	| 'default' '(' comma_types0 ')'	{ unitOL (sL (comb2 $1 $>) $ DefD (DefaultDecl $3)) }
	| 'foreign' fdecl			{ unitOL (sL (comb2 $1 $>) (unLoc $2)) }
	| '{-# DEPRECATED' deprecations '#-}'	{ $2 }
	| '{-# RULES' rules '#-}'		{ $2 }
      	| decl					{ unLoc $1 }

	-- Template Haskell Extension
	| '$(' exp ')'				{ unitOL (sL (comb2 $1 $>) $ SpliceD (SpliceDecl $2)) }
	| TH_ID_SPLICE				{ unitOL (sL (comb2 $1 $>) $ SpliceD (SpliceDecl $
							sL (getLoc $1) $ HsVar (mkUnqual varName (getTH_ID_SPLICE $1))
						  )) }

-- Type classes
--
cl_decl :: { LTyClDecl RdrName }
	: 'class' tycl_hdr fds where_cls
		{% do { let { (binds, sigs, ats, docs)           = 
			        cvBindsAndSigs (unLoc $4)
		            ; (ctxt, tc, tvs, tparms) = unLoc $2}
                      ; checkTyVars tparms      -- only type vars allowed
		      ; checkKindSigs ats
		      ; return $ L (comb4 $1 $2 $3 $4) 
				   (mkClassDecl (ctxt, tc, tvs) 
					        (unLoc $3) sigs binds ats docs) } }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl RdrName }
           -- ordinary type synonyms
        : 'type' type '=' ctype
		-- Note ctype, not sigtype, on the right of '='
		-- We allow an explicit for-all but we don't insert one
		-- in 	type Foo a = (b,b)
		-- Instead we just say b is out of scope
	        --
		-- Note the use of type for the head; this allows
		-- infix type constructors to be declared 
 		{% do { (tc, tvs, _) <- checkSynHdr $2 False
		      ; return (L (comb2 $1 $4) 
				  (TySynonym tc tvs Nothing $4))
                      } }

           -- type family declarations
        | 'type' 'family' type opt_kind_sig 
		-- Note the use of type for the head; this allows
		-- infix type constructors to be declared
		--
 		{% do { (tc, tvs, _) <- checkSynHdr $3 False
		      ; return (L (comb3 $1 $3 $4) 
				  (TyFamily TypeFamily tc tvs (unLoc $4)))
		      } }

           -- type instance declarations
        | 'type' 'instance' type '=' ctype
		-- Note the use of type for the head; this allows
		-- infix type constructors and type patterns
		--
 		{% do { (tc, tvs, typats) <- checkSynHdr $3 True
		      ; return (L (comb2 $1 $5) 
				  (TySynonym tc tvs (Just typats) $5)) 
                      } }

          -- ordinary data type or newtype declaration
	| data_or_newtype tycl_hdr constrs deriving
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $2}
                      ; checkTyVars tparms    -- no type pattern
		      ; return $
			  L (comb4 $1 $2 $3 $4)
			           -- We need the location on tycl_hdr in case 
				   -- constrs and deriving are both empty
			    (mkTyData (unLoc $1) (ctxt, tc, tvs, Nothing) 
			       Nothing (reverse (unLoc $3)) (unLoc $4)) } }

          -- ordinary GADT declaration
        | data_or_newtype tycl_hdr opt_kind_sig 
		 'where' gadt_constrlist
		 deriving
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $2}
                      ; checkTyVars tparms    -- can have type pats
		      ; return $
			  L (comb4 $1 $2 $4 $5)
			    (mkTyData (unLoc $1) (ctxt, tc, tvs, Nothing) 
			      (unLoc $3) (reverse (unLoc $5)) (unLoc $6)) } }

          -- data/newtype family
        | data_or_newtype 'family' tycl_hdr opt_kind_sig
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $3}
                      ; checkTyVars tparms            -- no type pattern
		      ; unless (null (unLoc ctxt)) $  -- and no context
			  parseError (getLoc ctxt) 
			    "A family declaration cannot have a context"
		      ; return $
			  L (comb3 $1 $2 $4)
			    (TyFamily (DataFamily (unLoc $1)) tc tvs 
				      (unLoc $4)) } }

          -- data/newtype instance declaration
	| data_or_newtype 'instance' tycl_hdr constrs deriving
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $3}
                                             -- can have type pats
		      ; return $
			  L (comb4 $1 $3 $4 $5)
			           -- We need the location on tycl_hdr in case 
				   -- constrs and deriving are both empty
			    (mkTyData (unLoc $1) (ctxt, tc, tvs, Just tparms) 
			      Nothing (reverse (unLoc $4)) (unLoc $5)) } }

          -- GADT instance declaration
        | data_or_newtype 'instance' tycl_hdr opt_kind_sig 
		 'where' gadt_constrlist
		 deriving
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $3}
                                             -- can have type pats
		      ; return $
			  L (comb4 $1 $3 $6 $7)
			    (mkTyData (unLoc $1) (ctxt, tc, tvs, Just tparms) 
			       (unLoc $4) (reverse (unLoc $6)) (unLoc $7)) } }

-- Associate type family declarations
--
-- * They have a different syntax than on the toplevel (no family special
--   identifier).
--
-- * They also need to be separate from instances; otherwise, data family
--   declarations without a kind signature cause parsing conflicts with empty
--   data declarations. 
--
at_decl_cls :: { LTyClDecl RdrName }
           -- type family declarations
        : 'type' type opt_kind_sig
		-- Note the use of type for the head; this allows
		-- infix type constructors to be declared
		--
 		{% do { (tc, tvs, _) <- checkSynHdr $2 False
		      ; return (L (comb3 $1 $2 $3) 
				  (TyFamily TypeFamily tc tvs (unLoc $3)))
		      } }

           -- default type instance
        | 'type' type '=' ctype
		-- Note the use of type for the head; this allows
		-- infix type constructors and type patterns
		--
 		{% do { (tc, tvs, typats) <- checkSynHdr $2 True
		      ; return (L (comb2 $1 $4) 
				  (TySynonym tc tvs (Just typats) $4)) 
                      } }

          -- data/newtype family declaration
        | data_or_newtype tycl_hdr opt_kind_sig
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $2}
                      ; checkTyVars tparms            -- no type pattern
		      ; unless (null (unLoc ctxt)) $  -- and no context
			  parseError (getLoc ctxt) 
			    "A family declaration cannot have a context"
		      ; return $
			  L (comb3 $1 $2 $3)
			    (TyFamily (DataFamily (unLoc $1)) tc tvs
				      (unLoc $3)) 
                      } }

-- Associate type instances
--
at_decl_inst :: { LTyClDecl RdrName }
           -- type instance declarations
        : 'type' type '=' ctype
		-- Note the use of type for the head; this allows
		-- infix type constructors and type patterns
		--
 		{% do { (tc, tvs, typats) <- checkSynHdr $2 True
		      ; return (L (comb2 $1 $4) 
				  (TySynonym tc tvs (Just typats) $4)) 
                      } }

        -- data/newtype instance declaration
	| data_or_newtype tycl_hdr constrs deriving
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $2}
                                             -- can have type pats
		      ; return $
			  L (comb4 $1 $2 $3 $4)
			           -- We need the location on tycl_hdr in case 
				   -- constrs and deriving are both empty
			    (mkTyData (unLoc $1) (ctxt, tc, tvs, Just tparms) 
			      Nothing (reverse (unLoc $3)) (unLoc $4)) } }

        -- GADT instance declaration
        | data_or_newtype tycl_hdr opt_kind_sig 
		 'where' gadt_constrlist
		 deriving
		{% do { let {(ctxt, tc, tvs, tparms) = unLoc $2}
                                             -- can have type pats
		      ; return $
			  L (comb4 $1 $2 $5 $6)
			    (mkTyData (unLoc $1) (ctxt, tc, tvs, Just tparms) 
			     (unLoc $3) (reverse (unLoc $5)) (unLoc $6)) } }

data_or_newtype :: { Located NewOrData }
	: 'data'	{ sL (getLoc $1) DataType }
	| 'newtype'	{ sL (getLoc $1) NewType }

opt_kind_sig :: { Located (Maybe Kind) }
	: 				{ noLoc Nothing }
	| '::' kind			{ sL (comb2 $1 $>) (Just (unLoc $2)) }

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--	T a b
-- 	Eq a => T a
--	(Eq a, Ord b) => T a b
--      T Int [a]			-- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (LHsContext RdrName, 
		       Located RdrName, 
		       [LHsTyVarBndr RdrName],
		       [LHsType RdrName]) }
	: context '=>' type		{% checkTyClHdr $1         $3 >>= return.sL (comb2 $1 $>) }
	| type				{% checkTyClHdr (noLoc []) $1 >>= return.sL (getLoc $1) }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl RdrName }
  	: 'derive' 'instance' inst_type {% checkDerivDecl (sL (comb2 $1 $>) (DerivDecl $3)) }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { Located (OrdList (LHsDecl RdrName)) }
decl_cls  : at_decl_cls		        { sL (comb2 $1 $>) (unitOL (sL (getLoc $1) (TyClD (unLoc $1)))) }
	  | decl                        { $1 }

decls_cls :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	  : decls_cls ';' decl_cls	{ sL (comb2 $1 $>) (unLoc $1 `appOL` unLoc $3) }
	  | decls_cls ';'		{ sL (comb2 $1 $>) (unLoc $1) }
	  | decl_cls			{ $1 }
	  | {- empty -}			{ noLoc nilOL }


decllist_cls
        :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	: '{'         decls_cls '}'	{ sL (comb2 $1 $>) (unLoc $2) }
	|     vocurly decls_cls close	{ $2 }

-- Class body
--
where_cls :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
				-- No implicit parameters
				-- May have type declarations
	: 'where' decllist_cls	        { sL (comb2 $1 $>) (unLoc $2) }
	| {- empty -}		        { noLoc nilOL }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl RdrName)) }
decl_inst  : at_decl_inst	        { sL (comb2 $1 $>) (unitOL (sL (getLoc $1) (TyClD (unLoc $1)))) }
	   | decl                       { $1 }

decls_inst :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	   : decls_inst ';' decl_inst	{ sL (comb2 $1 $>) (unLoc $1 `appOL` unLoc $3) }
	   | decls_inst ';'		{ sL (comb2 $1 $>) (unLoc $1) }
	   | decl_inst			{ $1 }
	   | {- empty -}		{ noLoc nilOL }

decllist_inst 
        :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	: '{'         decls_inst '}'	{ sL (comb2 $1 $>) (unLoc $2) }
	|     vocurly decls_inst close	{ $2 }

-- Instance body
--
where_inst :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
				-- No implicit parameters
				-- May have type declarations
	: 'where' decllist_inst		{ sL (comb2 $1 $>) (unLoc $2) }
	| {- empty -}			{ noLoc nilOL }

-- Declarations in binding groups other than classes and instances
--
decls 	:: { Located (OrdList (LHsDecl RdrName)) }	
	: decls ';' decl		{ sL (comb2 $1 $>) (unLoc $1 `appOL` unLoc $3) }
	| decls ';'			{ sL (comb2 $1 $>) (unLoc $1) }
	| decl				{ $1 }
	| {- empty -}			{ noLoc nilOL }

decllist :: { Located (OrdList (LHsDecl RdrName)) }
	: '{'            decls '}'	{ sL (comb2 $1 $>) (unLoc $2) }
	|     vocurly    decls close	{ $2 }

-- Binding groups other than those of class and instance declarations
--
binds 	::  { Located (HsLocalBinds RdrName) } 		-- May have implicit parameters
						-- No type declarations
	: decllist			{ sL (getLoc $1) (HsValBinds (cvBindGroup (unLoc $1))) }
	| '{'            dbinds '}'	{ sL (comb2 $1 $>) (HsIPBinds (IPBinds (unLoc $2) emptyLHsBinds)) }
	|     vocurly    dbinds close	{ L (getLoc $2) (HsIPBinds (IPBinds (unLoc $2) emptyLHsBinds)) }

wherebinds :: { Located (HsLocalBinds RdrName) }	-- May have implicit parameters
						-- No type declarations
	: 'where' binds			{ sL (comb2 $1 $>) (unLoc $2) }
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
	     { sL (comb2 $1 $>) $ RuleD (HsRule (getSTRING $1) 
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
		{ toOL [ sL (comb2 $1 $>) $ DeprecD (Deprecation n (getSTRING $2)) 
		       | n <- unLoc $1 ] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { LHsDecl RdrName }
fdecl : 'import' callconv safety fspec
		{% mkImport $2 $3 (unLoc $4) >>= return.sL (comb2 $1 $>) }
      | 'import' callconv        fspec		
		{% do { d <- mkImport $2 (PlaySafe False) (unLoc $3);
			return (sL (comb2 $1 $>) d) } }
      | 'export' callconv fspec
		{% mkExport $2 (unLoc $3) >>= return.sL (comb2 $1 $>) }

callconv :: { CallConv }
	  : 'stdcall'			{ CCall  StdCallConv }
	  | 'ccall'			{ CCall  CCallConv   }
	  | 'dotnet'			{ DNCall	     }

safety :: { Safety }
	: 'unsafe'			{ PlayRisky }
	| 'safe'			{ PlaySafe  False }
	| 'threadsafe'			{ PlaySafe  True }

fspec :: { Located (Located FastString, Located RdrName, LHsType RdrName) }
       : STRING var '::' sigtypedoc     { sL (comb2 $1 $>) (L (getLoc $1) (getSTRING $1), $2, $4) }
       |        var '::' sigtypedoc     { sL (comb2 $1 $>) (noLoc nilFS, $1, $3) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

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
	: ctype				{ sL (getLoc $1) (mkImplicitHsForAllTy (noLoc []) $1) }
	-- Wrap an Implicit forall if there isn't one there already

sigtypedoc :: { LHsType RdrName }
	: ctypedoc			{ sL (getLoc $1) (mkImplicitHsForAllTy (noLoc []) $1) }
	-- Wrap an Implicit forall if there isn't one there already

sig_vars :: { Located [Located RdrName] }
	 : sig_vars ',' var		{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	 | var				{ sL (getLoc $1) [$1] }

-----------------------------------------------------------------------------
-- Types

infixtype :: { LHsType RdrName }
	: btype qtyconop gentype         { sL (comb2 $1 $>) $ HsOpTy $1 $2 $3 }
        | btype tyvarop  gentype  	 { sL (comb2 $1 $>) $ HsOpTy $1 $2 $3 }

infixtypedoc :: { LHsType RdrName }
        : infixtype                      { $1 }
	| infixtype docprev              { sL (comb2 $1 $>) $ HsDocTy $1 $2 }

gentypedoc :: { LHsType RdrName }
        : btype                          { $1 }
        | btypedoc                       { $1 }
        | infixtypedoc                   { $1 }
        | btype '->' ctypedoc            { sL (comb2 $1 $>) $ HsFunTy $1 $3 }
        | btypedoc '->' ctypedoc         { sL (comb2 $1 $>) $ HsFunTy $1 $3 }

ctypedoc  :: { LHsType RdrName }
        : 'forall' tv_bndrs '.' ctypedoc { sL (comb2 $1 $>) $ mkExplicitHsForAllTy $2 (noLoc []) $4 }
        | context '=>' gentypedoc        { sL (comb2 $1 $>) $ mkImplicitHsForAllTy   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| gentypedoc			 { $1 }
	
strict_mark :: { Located HsBang }
	: '!'				{ sL (getLoc $1) HsStrict }
	| '{-# UNPACK' '#-}' '!'	{ sL (comb2 $1 $>) HsUnbox }

-- A ctype is a for-all type
ctype	:: { LHsType RdrName }
	: 'forall' tv_bndrs '.' ctype	{ sL (comb2 $1 $>) $ mkExplicitHsForAllTy $2 (noLoc []) $4 }
	| context '=>' type		{ sL (comb2 $1 $>) $ mkImplicitHsForAllTy   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| type				{ $1 }

-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--	(Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>
--
-- We have the t1 ~ t2 form here and in gentype, to permit an individual
-- equational constraint without parenthesis.
context :: { LHsContext RdrName }
        : btype '~'      btype  	{% checkContext
					     (sL (comb2 $1 $>) $ HsPredTy (HsEqualP $1 $3)) }
	| btype 			{% checkContext $1 }

type :: { LHsType RdrName }
	: ipvar '::' gentype		{ sL (comb2 $1 $>) (HsPredTy (HsIParam (unLoc $1) $3)) }
	| gentype			{ $1 }

gentype :: { LHsType RdrName }
        : btype                         { $1 }
        | btype qtyconop gentype        { sL (comb2 $1 $>) $ HsOpTy $1 $2 $3 }
        | btype tyvarop  gentype  	{ sL (comb2 $1 $>) $ HsOpTy $1 $2 $3 }
 	| btype '->'     ctype		{ sL (comb2 $1 $>) $ HsFunTy $1 $3 }
        | btype '~'      btype  	{ sL (comb2 $1 $>) $ HsPredTy (HsEqualP $1 $3) }

btype :: { LHsType RdrName }
	: btype atype			{ sL (comb2 $1 $>) $ HsAppTy $1 $2 }
	| atype				{ $1 }

btypedoc :: { LHsType RdrName }
	: btype atype docprev		{ sL (comb2 $1 $>) $ HsDocTy (L (comb2 $1 $2) (HsAppTy $1 $2)) $3 }
        | atype docprev                 { sL (comb2 $1 $>) $ HsDocTy $1 $2 }

atype :: { LHsType RdrName }
	: gtycon			{ sL (getLoc $1) (HsTyVar (unLoc $1)) }
	| tyvar				{ sL (getLoc $1) (HsTyVar (unLoc $1)) }
	| strict_mark atype		{ sL (comb2 $1 $>) (HsBangTy (unLoc $1) $2) }
	| '(' ctype ',' comma_types1 ')'  { sL (comb2 $1 $>) $ HsTupleTy Boxed  ($2:$4) }
	| '(#' comma_types1 '#)'	{ sL (comb2 $1 $>) $ HsTupleTy Unboxed $2     }
	| '[' ctype ']'			{ sL (comb2 $1 $>) $ HsListTy  $2 }
	| '[:' ctype ':]'		{ sL (comb2 $1 $>) $ HsPArrTy  $2 }
	| '(' ctype ')'		        { sL (comb2 $1 $>) $ HsParTy   $2 }
	| '(' ctype '::' kind ')'	{ sL (comb2 $1 $>) $ HsKindSig $2 (unLoc $4) }
-- Generics
        | INTEGER                       { sL (getLoc $1) (HsNumTy (getINTEGER $1)) }

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
	: tyvar				{ sL (getLoc $1) (UserTyVar (unLoc $1)) }
	| '(' tyvar '::' kind ')'	{ sL (comb2 $1 $>) (KindedTyVar (unLoc $2) 
							  (unLoc $4)) }

fds :: { Located [Located ([RdrName], [RdrName])] }
	: {- empty -}			{ noLoc [] }
	| '|' fds1			{ sL (comb2 $1 $>) (reverse (unLoc $2)) }

fds1 :: { Located [Located ([RdrName], [RdrName])] }
	: fds1 ',' fd			{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	| fd				{ sL (getLoc $1) [$1] }

fd :: { Located ([RdrName], [RdrName]) }
	: varids0 '->' varids0		{ L (comb3 $1 $2 $3)
					   (reverse (unLoc $1), reverse (unLoc $3)) }

varids0	:: { Located [RdrName] }
	: {- empty -}			{ noLoc [] }
	| varids0 tyvar			{ sL (comb2 $1 $>) (unLoc $2 : unLoc $1) }

-----------------------------------------------------------------------------
-- Kinds

kind	:: { Located Kind }
	: akind			{ $1 }
	| akind '->' kind	{ sL (comb2 $1 $>) (mkArrowKind (unLoc $1) (unLoc $3)) }

akind	:: { Located Kind }
	: '*'			{ sL (getLoc $1) liftedTypeKind }
	| '!'			{ sL (getLoc $1) unliftedTypeKind }
	| '(' kind ')'		{ sL (comb2 $1 $>) (unLoc $2) }


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located [LConDecl RdrName] }
	: '{'            gadt_constrs '}'	{ sL (comb2 $1 $>) (unLoc $2) }
	|     vocurly    gadt_constrs close	{ $2 }

gadt_constrs :: { Located [LConDecl RdrName] }
        : gadt_constrs ';' gadt_constr  { sL (comb2 $1 $>) ($3 : unLoc $1) }
        | gadt_constrs ';' 		{ $1 }
        | gadt_constr                   { sL (getLoc $1) [$1] } 

-- We allow the following forms:
--	C :: Eq a => a -> T a
--	C :: forall a. Eq a => !a -> T a
--	D { x,y :: a } :: T a
--	forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { LConDecl RdrName }
        : con '::' sigtype
              { sL (comb2 $1 $>) (mkGadtDecl $1 $3) } 
        -- Syntax: Maybe merge the record stuff with the single-case above?
        --         (to kill the mostly harmless reduce/reduce error)
        -- XXX revisit audreyt
	| constr_stuff_record '::' sigtype
		{ let (con,details) = unLoc $1 in 
		  sL (comb2 $1 $>) (ConDecl con Implicit [] (noLoc []) details (ResTyGADT $3) Nothing) }
{-
	| forall context '=>' constr_stuff_record '::' sigtype
		{ let (con,details) = unLoc $4 in 
		  sL (comb2 $1 $>) (ConDecl con Implicit (unLoc $1) $2 details (ResTyGADT $6) Nothing ) }
	| forall constr_stuff_record '::' sigtype
		{ let (con,details) = unLoc $2 in 
		  sL (comb2 $1 $>) (ConDecl con Implicit (unLoc $1) (noLoc []) details (ResTyGADT $4) Nothing) }
-}


constrs :: { Located [LConDecl RdrName] }
        : {- empty; a GHC extension -}  { noLoc [] }
        | maybe_docnext '=' constrs1    { L (comb2 $2 $3) (addConDocs (unLoc $3) $1) }

constrs1 :: { Located [LConDecl RdrName] }
	: constrs1 maybe_docnext '|' maybe_docprev constr { sL (comb2 $1 $>) (addConDoc $5 $2 : addConDocFirst (unLoc $1) $4) }
	| constr			                  { sL (getLoc $1) [$1] }

constr :: { LConDecl RdrName }
	: maybe_docnext forall context '=>' constr_stuff maybe_docprev	
		{ let (con,details) = unLoc $5 in 
		  L (comb4 $2 $3 $4 $5) (ConDecl con Explicit (unLoc $2) $3 details ResTyH98 ($1 `mplus` $6)) }
	| maybe_docnext forall constr_stuff maybe_docprev
		{ let (con,details) = unLoc $3 in 
		  L (comb2 $2 $3) (ConDecl con Explicit (unLoc $2) (noLoc []) details ResTyH98 ($1 `mplus` $4)) }

forall :: { Located [LHsTyVarBndr RdrName] }
	: 'forall' tv_bndrs '.'		{ sL (comb2 $1 $>) $2 }
	| {- empty -}			{ noLoc [] }

constr_stuff :: { Located (Located RdrName, HsConDetails RdrName (LBangType RdrName)) }
-- We parse the constructor declaration 
--	C t1 t2
-- as a btype (treating C as a type constructor) and then convert C to be
-- a data constructor.  Reason: it might continue like this:
--	C t1 t2 %: D Int
-- in which case C really would be a type constructor.  We can't resolve this
-- ambiguity till we come across the constructor oprerator :% (or not, more usually)
	: btype				{% mkPrefixCon $1 [] >>= return.sL (comb2 $1 $>) }
	| oqtycon '{' '}' 		{% mkRecCon $1 [] >>= return.sL (comb2 $1 $>) }
	| oqtycon '{' fielddecls '}' 	{% mkRecCon $1 $3 >>= return.sL (comb2 $1 $>) }
	| btype conop btype		{ sL (comb2 $1 $>) ($2, InfixCon $1 $3) }

constr_stuff_record :: { Located (Located RdrName, HsConDetails RdrName (LBangType RdrName)) }
	: oqtycon '{' '}' 		{% mkRecCon $1 [] >>= return.sL (comb2 $1 $>) }
	| oqtycon '{' fielddecls '}' 	{% mkRecCon $1 $3 >>= return.sL (comb2 $1 $>) }

fielddecls :: { [([Located RdrName], LBangType RdrName, Maybe (LHsDoc RdrName))] }
	: fielddecl maybe_docnext ',' maybe_docprev fielddecls { addFieldDoc (unLoc $1) $4 : addFieldDocs $5 $2 }
	| fielddecl			                       { [unLoc $1] }

fielddecl :: { Located ([Located RdrName], LBangType RdrName, Maybe (LHsDoc RdrName)) }
	: maybe_docnext sig_vars '::' ctype maybe_docprev      { L (comb3 $2 $3 $4) (reverse (unLoc $2), $4, $1 `mplus` $5) }

-- We allow the odd-looking 'inst_type' in a deriving clause, so that
-- we can do deriving( forall a. C [a] ) in a newtype (GHC extension).
-- The 'C [a]' part is converted to an HsPredTy by checkInstType
-- We don't allow a context, but that's sorted out by the type checker.
deriving :: { Located (Maybe [LHsType RdrName]) }
	: {- empty -}				{ noLoc Nothing }
	| 'deriving' qtycon	{% do { let { L loc tv = $2 }
				      ; p <- checkInstType (L loc (HsTyVar tv))
				      ; return (sL (comb2 $1 $>) (Just [p])) } }
	| 'deriving' '(' ')'	 		{ sL (comb2 $1 $>) (Just []) }
	| 'deriving' '(' inst_types1 ')' 	{ sL (comb2 $1 $>) (Just $3) }
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

docdecl :: { LHsDecl RdrName }
        : docdecld { sL (getLoc $1) (DocD (unLoc $1)) }

docdecld :: { LDocDecl RdrName }
        : docnext                               { sL (getLoc $1) (DocCommentNext (unLoc $1)) }
        | docprev                               { sL (getLoc $1) (DocCommentPrev (unLoc $1)) }
        | docnamed                              { sL (getLoc $1) (case (unLoc $1) of (n, doc) -> DocCommentNamed n doc) }
        | docsection                            { sL (getLoc $1) (case (unLoc $1) of (n, doc) -> DocGroup n doc) }

decl 	:: { Located (OrdList (LHsDecl RdrName)) }
	: sigdecl			{ $1 }
	| '!' aexp rhs			{% do { pat <- checkPattern $2;
					        return (sL (comb2 $1 $>) $ unitOL $ sL (comb2 $1 $>) $ ValD ( 
							PatBind (sL (comb2 $1 $>) $ BangPat pat) (unLoc $3)
								placeHolderType placeHolderNames)) } }
	| infixexp opt_sig rhs		{% do { r <- checkValDef $1 $2 $3;
						return (sL (comb2 $1 $>) $ unitOL (sL (comb2 $1 $>) $ ValD r)) } }
        | docdecl                       { sL (comb2 $1 $>) $ unitOL $1 }

rhs	:: { Located (GRHSs RdrName) }
	: '=' exp wherebinds	{ L (comb3 $1 $2 $3) $ GRHSs (unguardedRHS $2) (unLoc $3) }
	| gdrhs	wherebinds	{ sL (comb2 $1 $>) $ GRHSs (reverse (unLoc $1)) (unLoc $2) }

gdrhs :: { Located [LGRHS RdrName] }
	: gdrhs gdrh		{ sL (comb2 $1 $>) ($2 : unLoc $1) }
	| gdrh			{ sL (getLoc $1) [$1] }

gdrh :: { LGRHS RdrName }
	: '|' quals '=' exp  	{ sL (comb2 $1 $>) $ GRHS (reverse (unLoc $2)) $4 }

sigdecl :: { Located (OrdList (LHsDecl RdrName)) }
	: infixexp '::' sigtypedoc
				{% do s <- checkValSig $1 $3; 
				      return (sL (comb2 $1 $>) $ unitOL (sL (comb2 $1 $>) $ SigD s)) }
		-- See the above notes for why we need infixexp here
	| var ',' sig_vars '::' sigtypedoc
				{ sL (comb2 $1 $>) $ toOL [ sL (comb2 $1 $>) $ SigD (TypeSig n $5) | n <- $1 : unLoc $3 ] }
	| infix prec ops	{ sL (comb2 $1 $>) $ toOL [ sL (comb2 $1 $>) $ SigD (FixSig (FixitySig n (Fixity $2 (unLoc $1))))
					     | n <- unLoc $3 ] }
	| '{-# INLINE'   activation qvar '#-}'	      
				{ sL (comb2 $1 $>) $ unitOL (sL (comb2 $1 $>) $ SigD (InlineSig $3 (mkInlineSpec $2 (getINLINE $1)))) }
	| '{-# SPECIALISE' qvar '::' sigtypes1 '#-}'
			 	{ sL (comb2 $1 $>) $ toOL [ sL (comb2 $1 $>) $ SigD (SpecSig $2 t defaultInlineSpec) 
					    | t <- $4] }
	| '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
			 	{ sL (comb2 $1 $>) $ toOL [ sL (comb2 $1 $>) $ SigD (SpecSig $3 t (mkInlineSpec $2 (getSPEC_INLINE $1)))
					    | t <- $5] }
	| '{-# SPECIALISE' 'instance' inst_type '#-}'
				{ sL (comb2 $1 $>) $ unitOL (sL (comb2 $1 $>) $ SigD (SpecInstSig $3)) }

-----------------------------------------------------------------------------
-- Expressions

exp   :: { LHsExpr RdrName }
	: infixexp '::' sigtype		{ sL (comb2 $1 $>) $ ExprWithTySig $1 $3 }
	| infixexp '-<' exp		{ sL (comb2 $1 $>) $ HsArrApp $1 $3 placeHolderType HsFirstOrderApp True }
	| infixexp '>-' exp		{ sL (comb2 $1 $>) $ HsArrApp $3 $1 placeHolderType HsFirstOrderApp False }
	| infixexp '-<<' exp		{ sL (comb2 $1 $>) $ HsArrApp $1 $3 placeHolderType HsHigherOrderApp True }
	| infixexp '>>-' exp		{ sL (comb2 $1 $>) $ HsArrApp $3 $1 placeHolderType HsHigherOrderApp False}
	| infixexp			{ $1 }

infixexp :: { LHsExpr RdrName }
	: exp10				{ $1 }
	| infixexp qop exp10		{ sL (comb2 $1 $>) (OpApp $1 $2 (panic "fixity") $3) }

exp10 :: { LHsExpr RdrName }
	: '\\' apat apats opt_asig '->' exp	
			{ sL (comb2 $1 $>) $ HsLam (mkMatchGroup [sL (comb2 $1 $>) $ Match ($2:$3) $4
							   	(unguardedGRHSs $6)
							    ]) }
  	| 'let' binds 'in' exp			{ sL (comb2 $1 $>) $ HsLet (unLoc $2) $4 }
	| 'if' exp 'then' exp 'else' exp	{ sL (comb2 $1 $>) $ HsIf $2 $4 $6 }
   	| 'case' exp 'of' altslist		{ sL (comb2 $1 $>) $ HsCase $2 (mkMatchGroup (unLoc $4)) }
	| '-' fexp				{ sL (comb2 $1 $>) $ mkHsNegApp $2 }

  	| 'do' stmtlist			{% let loc = comb2 $1 $2 in
					   checkDo loc (unLoc $2)  >>= \ (stmts,body) ->
					   return (L loc (mkHsDo DoExpr stmts body)) }
  	| 'mdo' stmtlist		{% let loc = comb2 $1 $2 in
					   checkDo loc (unLoc $2)  >>= \ (stmts,body) ->
					   return (L loc (mkHsDo (MDoExpr noPostTcTable) stmts body)) }
        | scc_annot exp		    		{ sL (comb2 $1 $>) $ if opt_SccProfilingOn
							then HsSCC (unLoc $1) $2
							else HsPar $2 }
        | hpc_annot exp		    		{ sL (comb2 $1 $>) $ if opt_Hpc
							then HsTickPragma (unLoc $1) $2
							else HsPar $2 }

	| 'proc' aexp '->' exp	
			{% checkPattern $2 >>= \ p -> 
			   return (sL (comb2 $1 $>) $ HsProc p (sL (comb2 $1 $>) $ HsCmdTop $4 [] 
						   placeHolderType undefined)) }
						-- TODO: is sL (comb2 $1 $>) right here?

        | '{-# CORE' STRING '#-}' exp           { sL (comb2 $1 $>) $ HsCoreAnn (getSTRING $2) $4 }
						    -- hdaume: core annotation
	| fexp					{ $1 }

scc_annot :: { Located FastString }
	: '_scc_' STRING			{ sL (comb2 $1 $>) $ getSTRING $2 }
	| '{-# SCC' STRING '#-}'		{ sL (comb2 $1 $>) $ getSTRING $2 }

hpc_annot :: { Located (FastString,(Int,Int),(Int,Int)) }
	: '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
						{ sL (comb2 $1 $>) $ (getSTRING $2
						       ,( fromInteger $ getINTEGER $3
 							, fromInteger $ getINTEGER $5
							)
                         			       ,( fromInteger $ getINTEGER $7
 							, fromInteger $ getINTEGER $9
							)
						       )
					         }

fexp 	:: { LHsExpr RdrName }
	: fexp aexp				{ sL (comb2 $1 $>) $ HsApp $1 $2 }
  	| aexp					{ $1 }

aexp	:: { LHsExpr RdrName }
	: qvar '@' aexp			{ sL (comb2 $1 $>) $ EAsPat $1 $3 }
	| '~' aexp			{ sL (comb2 $1 $>) $ ELazyPat $2 }
	| aexp1				{ $1 }

aexp1	:: { LHsExpr RdrName }
        : aexp1 '{' fbinds '}' 	{% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4) 
							$3;
				        return (sL (comb2 $1 $>) r) }}
  	| aexp2			{ $1 }

-- Here was the syntax for type applications that I was planning
-- but there are difficulties (e.g. what order for type args)
-- so it's not enabled yet.
-- But this case *is* used for the left hand side of a generic definition,
-- which is parsed as an expression before being munged into a pattern
 	| qcname '{|' gentype '|}'      { sL (comb2 $1 $>) $ HsApp (sL (getLoc $1) (HsVar (unLoc $1)))
						     (sL (getLoc $3) (HsType $3)) }

aexp2	:: { LHsExpr RdrName }
	: ipvar				{ sL (getLoc $1) (HsIPVar $! unLoc $1) }
	| qcname			{ sL (getLoc $1) (HsVar   $! unLoc $1) }
	| literal			{ sL (getLoc $1) (HsLit   $! unLoc $1) }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--	| STRING			{ sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRING $1)) }
	| INTEGER			{ sL (getLoc $1) (HsOverLit $! mkHsIntegral (getINTEGER $1)) }
	| RATIONAL			{ sL (getLoc $1) (HsOverLit $! mkHsFractional (getRATIONAL $1)) }
	| '(' exp ')'			{ sL (comb2 $1 $>) (HsPar $2) }
	| '(' texp ',' texps ')'	{ sL (comb2 $1 $>) $ ExplicitTuple ($2 : reverse $4) Boxed }
	| '(#' texps '#)'		{ sL (comb2 $1 $>) $ ExplicitTuple (reverse $2)      Unboxed }
	| '[' list ']'                  { sL (comb2 $1 $>) (unLoc $2) }
	| '[:' parr ':]'                { sL (comb2 $1 $>) (unLoc $2) }
	| '(' infixexp qop ')'		{ sL (comb2 $1 $>) $ SectionL $2 $3 }
	| '(' qopm infixexp ')'		{ sL (comb2 $1 $>) $ SectionR $2 $3 }
	| '_'				{ sL (getLoc $1) EWildPat }
	
	-- Template Haskell Extension
	| TH_ID_SPLICE          { sL (getLoc $1) $ HsSpliceE (mkHsSplice 
					(sL (getLoc $1) $ HsVar (mkUnqual varName 
							(getTH_ID_SPLICE $1)))) } -- $x
	| '$(' exp ')'   	{ sL (comb2 $1 $>) $ HsSpliceE (mkHsSplice $2) }               -- $( exp )

	| TH_VAR_QUOTE qvar 	{ sL (comb2 $1 $>) $ HsBracket (VarBr (unLoc $2)) }
	| TH_VAR_QUOTE qcon 	{ sL (comb2 $1 $>) $ HsBracket (VarBr (unLoc $2)) }
	| TH_TY_QUOTE tyvar 	{ sL (comb2 $1 $>) $ HsBracket (VarBr (unLoc $2)) }
 	| TH_TY_QUOTE gtycon	{ sL (comb2 $1 $>) $ HsBracket (VarBr (unLoc $2)) }
	| '[|' exp '|]'         { sL (comb2 $1 $>) $ HsBracket (ExpBr $2) }                       
	| '[t|' ctype '|]'      { sL (comb2 $1 $>) $ HsBracket (TypBr $2) }                       
	| '[p|' infixexp '|]'   {% checkPattern $2 >>= \p ->
					   return (sL (comb2 $1 $>) $ HsBracket (PatBr p)) }
	| '[d|' cvtopbody '|]'	{ sL (comb2 $1 $>) $ HsBracket (DecBr (mkGroup $2)) }

	-- arrow notation extension
	| '(|' aexp2 cmdargs '|)'	{ sL (comb2 $1 $>) $ HsArrForm $2 Nothing (reverse $3) }

cmdargs	:: { [LHsCmdTop RdrName] }
	: cmdargs acmd			{ $2 : $1 }
  	| {- empty -}			{ [] }

acmd	:: { LHsCmdTop RdrName }
	: aexp2			{ sL (getLoc $1) $ HsCmdTop $1 [] placeHolderType undefined }

cvtopbody :: { [LHsDecl RdrName] }
	:  '{'            cvtopdecls0 '}'		{ $2 }
	|      vocurly    cvtopdecls0 close		{ $2 }

cvtopdecls0 :: { [LHsDecl RdrName] }
	: {- empty -}		{ [] }
	| cvtopdecls		{ $1 }

texp :: { LHsExpr RdrName }
	: exp				{ $1 }
	| qopm infixexp			{ sL (comb2 $1 $>) $ SectionR $1 $2 }
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
	: texp			{ sL (getLoc $1) $ ExplicitList placeHolderType [$1] }
	| lexps 		{ sL (getLoc $1) $ ExplicitList placeHolderType (reverse (unLoc $1)) }
	| texp '..'		{ sL (comb2 $1 $>) $ ArithSeq noPostTcExpr (From $1) }
	| texp ',' exp '..' 	{ sL (comb2 $1 $>) $ ArithSeq noPostTcExpr (FromThen $1 $3) }
	| texp '..' exp	 	{ sL (comb2 $1 $>) $ ArithSeq noPostTcExpr (FromTo $1 $3) }
	| texp ',' exp '..' exp	{ sL (comb2 $1 $>) $ ArithSeq noPostTcExpr (FromThenTo $1 $3 $5) }
	| texp pquals		{ sL (comb2 $1 $>) $ mkHsDo ListComp (reverse (unLoc $2)) $1 }

lexps :: { Located [LHsExpr RdrName] }
	: lexps ',' texp 		{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	| texp ',' texp			{ sL (comb2 $1 $>) [$3,$1] }

-----------------------------------------------------------------------------
-- List Comprehensions

pquals :: { Located [LStmt RdrName] }	-- Either a singleton ParStmt, 
					-- or a reversed list of Stmts
	: pquals1			{ case unLoc $1 of
					    [qs] -> sL (getLoc $1) qs
					    qss  -> sL (getLoc $1) [sL (getLoc $1) (ParStmt stmtss)]
						 where
						    stmtss = [ (reverse qs, undefined) 
						    	     | qs <- qss ]
					}
			
pquals1 :: { Located [[LStmt RdrName]] }
	: pquals1 '|' quals		{ sL (comb2 $1 $>) (unLoc $3 : unLoc $1) }
	| '|' quals			{ L (getLoc $2) [unLoc $2] }

quals :: { Located [LStmt RdrName] }
	: quals ',' qual		{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	| qual				{ sL (getLoc $1) [$1] }

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { LHsExpr RdrName }
	: 				{ noLoc (ExplicitPArr placeHolderType []) }
	| texp				{ sL (getLoc $1) $ ExplicitPArr placeHolderType [$1] }
	| lexps 			{ sL (getLoc $1) $ ExplicitPArr placeHolderType 
						       (reverse (unLoc $1)) }
	| texp '..' exp	 		{ sL (comb2 $1 $>) $ PArrSeq noPostTcExpr (FromTo $1 $3) }
	| texp ',' exp '..' exp		{ sL (comb2 $1 $>) $ PArrSeq noPostTcExpr (FromThenTo $1 $3 $5) }
	| texp pquals			{ sL (comb2 $1 $>) $ mkHsDo PArrComp (reverse (unLoc $2)) $1 }

-- We are reusing `lexps' and `pquals' from the list case.

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { Located [LMatch RdrName] }
	: '{'            alts '}'	{ sL (comb2 $1 $>) (reverse (unLoc $2)) }
	|     vocurly    alts  close	{ L (getLoc $2) (reverse (unLoc $2)) }

alts    :: { Located [LMatch RdrName] }
        : alts1				{ sL (getLoc $1) (unLoc $1) }
	| ';' alts			{ sL (comb2 $1 $>) (unLoc $2) }

alts1 	:: { Located [LMatch RdrName] }
	: alts1 ';' alt			{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	| alts1 ';'			{ sL (comb2 $1 $>) (unLoc $1) }
	| alt				{ sL (getLoc $1) [$1] }

alt 	:: { LMatch RdrName }
	: pat opt_sig alt_rhs		{ sL (comb2 $1 $>) (Match [$1] $2 (unLoc $3)) }

alt_rhs :: { Located (GRHSs RdrName) }
	: ralt wherebinds		{ sL (comb2 $1 $>) (GRHSs (unLoc $1) (unLoc $2)) }

ralt :: { Located [LGRHS RdrName] }
	: '->' exp			{ sL (comb2 $1 $>) (unguardedRHS $2) }
	| gdpats			{ sL (getLoc $1) (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS RdrName] }
	: gdpats gdpat			{ sL (comb2 $1 $>) ($2 : unLoc $1) }
	| gdpat				{ sL (getLoc $1) [$1] }

gdpat	:: { LGRHS RdrName }
	: '|' quals '->' exp	 	{ sL (comb2 $1 $>) $ GRHS (reverse (unLoc $2)) $4 }

-- 'pat' recognises a pattern, including one with a bang at the top
-- 	e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat RdrName }
pat 	: infixexp		{% checkPattern $1 }
	| '!' aexp		{% checkPattern (sL (comb2 $1 $>) (SectionR (sL (getLoc $1) (HsVar bang_RDR)) $2)) }

apat   :: { LPat RdrName }	
apat 	: aexp			{% checkPattern $1 }
	| '!' aexp		{% checkPattern (sL (comb2 $1 $>) (SectionR (sL (getLoc $1) (HsVar bang_RDR)) $2)) }

apats  :: { [LPat RdrName] }
	: apat apats		{ $1 : $2 }
  	| {- empty -}		{ [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { Located [LStmt RdrName] }
	: '{'         	stmts '}'	{ sL (comb2 $1 $>) (unLoc $2) }
	|     vocurly   stmts close	{ $2 }

--	do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use ExprStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
stmts :: { Located [LStmt RdrName] }
	: stmt stmts_help		{ sL (comb2 $1 $>) ($1 : unLoc $2) }
	| ';' stmts			{ sL (comb2 $1 $>) (unLoc $2) }
	| {- empty -}			{ noLoc [] }

stmts_help :: { Located [LStmt RdrName] } -- might be empty
	: ';' stmts			{ sL (comb2 $1 $>) (unLoc $2) }
	| {- empty -}			{ noLoc [] }

-- For typing stmts at the GHCi prompt, where 
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt RdrName) }
	: stmt				{ Just $1 }
	| {- nothing -}			{ Nothing }

stmt  :: { LStmt RdrName }
	: qual				{ $1 }
-- What is this next production doing?  I have no clue!  SLPJ Dec06
	| infixexp '->' exp		{% checkPattern $3 >>= \p ->
					   return (sL (comb2 $1 $>) $ mkBindStmt p $1) }
  	| 'rec' stmtlist		{ sL (comb2 $1 $>) $ mkRecStmt (unLoc $2) }

qual  :: { LStmt RdrName }
	: pat '<-' exp			{ sL (comb2 $1 $>) $ mkBindStmt $1 $3 }
	| exp				{ sL (getLoc $1) $ mkExprStmt $1 }
  	| 'let' binds			{ sL (comb2 $1 $>) $ LetStmt (unLoc $2) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds 	:: { HsRecordBinds RdrName }
	: fbinds1			{ HsRecordBinds (reverse $1) }
  	| {- empty -}			{ HsRecordBinds [] }

fbinds1	:: { [(Located id, LHsExpr id)] }
	: fbinds1 ',' fbind		{ $3 : $1 }
	| fbind				{ [$1] }
  
fbind	:: { (Located RdrName, LHsExpr RdrName) }
	: qvar '=' exp			{ ($1,$3) }

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds 	:: { Located [LIPBind RdrName] }
	: dbinds ';' dbind		{ sL (comb2 $1 $>) ($3 : unLoc $1) }
	| dbinds ';'			{ sL (comb2 $1 $>) (unLoc $1) }
	| dbind				{ sL (getLoc $1) [$1] }
--	| {- empty -}			{ [] }

dbind	:: { LIPBind RdrName }
dbind	: ipvar '=' exp			{ sL (comb2 $1 $>) (IPBind (unLoc $1) $3) }

ipvar	:: { Located (IPName RdrName) }
	: IPDUPVARID		{ sL (getLoc $1) (IPName (mkUnqual varName (getIPDUPVARID $1))) }

-----------------------------------------------------------------------------
-- Deprecations

depreclist :: { Located [RdrName] }
depreclist : deprec_var			{ sL (getLoc $1) [unLoc $1] }
	   | deprec_var ',' depreclist	{ sL (comb2 $1 $>) (unLoc $1 : unLoc $3) }

deprec_var :: { Located RdrName }
deprec_var : var			{ $1 }
	   | con			{ $1 }

-----------------------------------------
-- Data constructors
qcon	:: { Located RdrName }
	: qconid		{ $1 }
	| '(' qconsym ')'	{ sL (comb2 $1 $>) (unLoc $2) }
	| sysdcon		{ sL (getLoc $1) $ nameRdrName (dataConName (unLoc $1)) }
-- The case of '[:' ':]' is part of the production `parr'

con	:: { Located RdrName }
	: conid			{ $1 }
	| '(' consym ')'	{ sL (comb2 $1 $>) (unLoc $2) }
	| sysdcon		{ sL (getLoc $1) $ nameRdrName (dataConName (unLoc $1)) }

sysdcon	:: { Located DataCon }	-- Wired in data constructors
	: '(' ')'		{ sL (comb2 $1 $>) unitDataCon }
	| '(' commas ')'	{ sL (comb2 $1 $>) $ tupleCon Boxed $2 }
	| '[' ']'		{ sL (comb2 $1 $>) nilDataCon }

conop :: { Located RdrName }
	: consym		{ $1 }	
	| '`' conid '`'		{ sL (comb2 $1 $>) (unLoc $2) }

qconop :: { Located RdrName }
	: qconsym		{ $1 }
	| '`' qconid '`'	{ sL (comb2 $1 $>) (unLoc $2) }

-----------------------------------------------------------------------------
-- Type constructors

gtycon 	:: { Located RdrName }	-- A "general" qualified tycon
	: oqtycon			{ $1 }
	| '(' ')'			{ sL (comb2 $1 $>) $ getRdrName unitTyCon }
	| '(' commas ')'		{ sL (comb2 $1 $>) $ getRdrName (tupleTyCon Boxed $2) }
	| '(' '->' ')'			{ sL (comb2 $1 $>) $ getRdrName funTyCon }
	| '[' ']'			{ sL (comb2 $1 $>) $ listTyCon_RDR }
	| '[:' ':]'			{ sL (comb2 $1 $>) $ parrTyCon_RDR }

oqtycon :: { Located RdrName }	-- An "ordinary" qualified tycon
	: qtycon			{ $1 }
 	| '(' qtyconsym ')'		{ sL (comb2 $1 $>) (unLoc $2) }

qtyconop :: { Located RdrName }	-- Qualified or unqualified
	: qtyconsym			{ $1 }
	| '`' qtycon '`'		{ sL (comb2 $1 $>) (unLoc $2) }

qtycon :: { Located RdrName }	-- Qualified or unqualified
	: QCONID			{ sL (getLoc $1) $! mkQual tcClsName (getQCONID $1) }
	| tycon				{ $1 }

tycon 	:: { Located RdrName }	-- Unqualified
	: CONID				{ sL (getLoc $1) $! mkUnqual tcClsName (getCONID $1) }

qtyconsym :: { Located RdrName }
	: QCONSYM			{ sL (getLoc $1) $! mkQual tcClsName (getQCONSYM $1) }
	| tyconsym			{ $1 }

tyconsym :: { Located RdrName }
	: CONSYM			{ sL (getLoc $1) $! mkUnqual tcClsName (getCONSYM $1) }

-----------------------------------------------------------------------------
-- Operators

op	:: { Located RdrName }   -- used in infix decls
	: varop			{ $1 }
	| conop 		{ $1 }

varop	:: { Located RdrName }
	: varsym		{ $1 }
	| '`' varid '`'		{ sL (comb2 $1 $>) (unLoc $2) }

qop	:: { LHsExpr RdrName }   -- used in sections
	: qvarop		{ sL (getLoc $1) $ HsVar (unLoc $1) }
	| qconop		{ sL (getLoc $1) $ HsVar (unLoc $1) }

qopm	:: { LHsExpr RdrName }   -- used in sections
	: qvaropm		{ sL (getLoc $1) $ HsVar (unLoc $1) }
	| qconop		{ sL (getLoc $1) $ HsVar (unLoc $1) }

qvarop :: { Located RdrName }
	: qvarsym		{ $1 }
	| '`' qvarid '`'	{ sL (comb2 $1 $>) (unLoc $2) }

qvaropm :: { Located RdrName }
	: qvarsym_no_minus	{ $1 }
	| '`' qvarid '`'	{ sL (comb2 $1 $>) (unLoc $2) }

-----------------------------------------------------------------------------
-- Type variables

tyvar   :: { Located RdrName }
tyvar   : tyvarid		{ $1 }
	| '(' tyvarsym ')'	{ sL (comb2 $1 $>) (unLoc $2) }

tyvarop :: { Located RdrName }
tyvarop : '`' tyvarid '`'	{ sL (comb2 $1 $>) (unLoc $2) }
	| tyvarsym		{ $1 }

tyvarid	:: { Located RdrName }
	: VARID			{ sL (getLoc $1) $! mkUnqual tvName (getVARID $1) }
	| special_id		{ sL (getLoc $1) $! mkUnqual tvName (unLoc $1) }
	| 'unsafe' 		{ sL (getLoc $1) $! mkUnqual tvName FSLIT("unsafe") }
	| 'safe' 		{ sL (getLoc $1) $! mkUnqual tvName FSLIT("safe") }
	| 'threadsafe' 		{ sL (getLoc $1) $! mkUnqual tvName FSLIT("threadsafe") }

tyvarsym :: { Located RdrName }
-- Does not include "!", because that is used for strictness marks
--	         or ".", because that separates the quantified type vars from the rest
--		 or "*", because that's used for kinds
tyvarsym : VARSYM		{ sL (getLoc $1) $! mkUnqual tvName (getVARSYM $1) }

-----------------------------------------------------------------------------
-- Variables 

var 	:: { Located RdrName }
	: varid			{ $1 }
	| '(' varsym ')'	{ sL (comb2 $1 $>) (unLoc $2) }

qvar 	:: { Located RdrName }
	: qvarid		{ $1 }
	| '(' varsym ')'	{ sL (comb2 $1 $>) (unLoc $2) }
	| '(' qvarsym1 ')'	{ sL (comb2 $1 $>) (unLoc $2) }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

qvarid :: { Located RdrName }
	: varid			{ $1 }
	| QVARID		{ sL (getLoc $1) $ mkQual varName (getQVARID $1) }

varid :: { Located RdrName }
	: varid_no_unsafe 	{ $1 }
	| 'unsafe'		{ sL (getLoc $1) $! mkUnqual varName FSLIT("unsafe") }
	| 'safe'		{ sL (getLoc $1) $! mkUnqual varName FSLIT("safe") }
	| 'threadsafe'		{ sL (getLoc $1) $! mkUnqual varName FSLIT("threadsafe") }

varid_no_unsafe :: { Located RdrName }
	: VARID			{ sL (getLoc $1) $! mkUnqual varName (getVARID $1) }
	| special_id		{ sL (getLoc $1) $! mkUnqual varName (unLoc $1) }
	| 'forall'		{ sL (getLoc $1) $! mkUnqual varName FSLIT("forall") }
	| 'family'              { sL (getLoc $1) $! mkUnqual varName FSLIT("family") }

qvarsym :: { Located RdrName }
	: varsym		{ $1 }
	| qvarsym1		{ $1 }

qvarsym_no_minus :: { Located RdrName }
	: varsym_no_minus	{ $1 }
	| qvarsym1		{ $1 }

qvarsym1 :: { Located RdrName }
qvarsym1 : QVARSYM		{ sL (getLoc $1) $ mkQual varName (getQVARSYM $1) }

varsym :: { Located RdrName }
	: varsym_no_minus 	{ $1 }
	| '-'			{ sL (getLoc $1) $ mkUnqual varName FSLIT("-") }

varsym_no_minus :: { Located RdrName } -- varsym not including '-'
	: VARSYM		{ sL (getLoc $1) $ mkUnqual varName (getVARSYM $1) }
	| special_sym		{ sL (getLoc $1) $ mkUnqual varName (unLoc $1) }


-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe', 'forall', and 'family' whose treatment differs
-- depending on context 
special_id :: { Located FastString }
special_id
	: 'as'			{ sL (getLoc $1) FSLIT("as") }
	| 'qualified'		{ sL (getLoc $1) FSLIT("qualified") }
	| 'hiding'		{ sL (getLoc $1) FSLIT("hiding") }
	| 'derive'		{ sL (getLoc $1) FSLIT("derive") }
	| 'export'		{ sL (getLoc $1) FSLIT("export") }
	| 'label'		{ sL (getLoc $1) FSLIT("label")  }
	| 'dynamic'		{ sL (getLoc $1) FSLIT("dynamic") }
	| 'stdcall'             { sL (getLoc $1) FSLIT("stdcall") }
	| 'ccall'               { sL (getLoc $1) FSLIT("ccall") }

special_sym :: { Located FastString }
special_sym : '!'	{ sL (getLoc $1) FSLIT("!") }
	    | '.' 	{ sL (getLoc $1) FSLIT(".") }
 	    | '*' 	{ sL (getLoc $1) FSLIT("*") }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }	-- Qualified or unqualified
	: conid			{ $1 }
	| QCONID		{ sL (getLoc $1) $ mkQual dataName (getQCONID $1) }

conid 	:: { Located RdrName }
	: CONID			{ sL (getLoc $1) $ mkUnqual dataName (getCONID $1) }

qconsym :: { Located RdrName }	-- Qualified or unqualified
	: consym		{ $1 }
	| QCONSYM		{ sL (getLoc $1) $ mkQual dataName (getQCONSYM $1) }

consym :: { Located RdrName }
	: CONSYM		{ sL (getLoc $1) $ mkUnqual dataName (getCONSYM $1) }

	-- ':' means only list cons
	| ':'			{ sL (getLoc $1) $ consDataCon_RDR }


-----------------------------------------------------------------------------
-- Literals

literal :: { Located HsLit }
	: CHAR 			{ sL (getLoc $1) $ HsChar       $ getCHAR $1 }
	| STRING 		{ sL (getLoc $1) $ HsString     $ getSTRING $1 }
	| PRIMINTEGER		{ sL (getLoc $1) $ HsIntPrim    $ getPRIMINTEGER $1 }
	| PRIMCHAR		{ sL (getLoc $1) $ HsCharPrim   $ getPRIMCHAR $1 }
	| PRIMSTRING		{ sL (getLoc $1) $ HsStringPrim $ getPRIMSTRING $1 }
	| PRIMFLOAT		{ sL (getLoc $1) $ HsFloatPrim  $ getPRIMFLOAT $1 }
	| PRIMDOUBLE		{ sL (getLoc $1) $ HsDoublePrim $ getPRIMDOUBLE $1 }

-----------------------------------------------------------------------------
-- Layout

close :: { () }
	: vccurly		{ () } -- context popped in lexer.
	| error			{% popContext }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid 	:: { Located ModuleName }
	: CONID			{ sL (getLoc $1) $ mkModuleNameFS (getCONID $1) }
        | QCONID		{ sL (getLoc $1) $ let (mod,c) = getQCONID $1 in
				  mkModuleNameFS
				   (mkFastString
				     (unpackFS mod ++ '.':unpackFS c))
				}

commas :: { Int }
	: commas ','			{ $1 + 1 }
	| ','				{ 2 }

-----------------------------------------------------------------------------
-- Documentation comments

docnext :: { LHsDoc RdrName }
  : DOCNEXT {% case parseHaddockParagraphs (tokenise (getDOCNEXT $1)) of {
      Left  err -> parseError (getLoc $1) err;
      Right doc -> return (sL (getLoc $1) doc) } }

docprev :: { LHsDoc RdrName }
  : DOCPREV {% case parseHaddockParagraphs (tokenise (getDOCPREV $1)) of {
      Left  err -> parseError (getLoc $1) err;
      Right doc -> return (sL (getLoc $1) doc) } }

docnamed :: { Located (String, (HsDoc RdrName)) }
  : DOCNAMED {%
      let string = getDOCNAMED $1 
          (name, rest) = break isSpace string
      in case parseHaddockParagraphs (tokenise rest) of {
        Left  err -> parseError (getLoc $1) err;
        Right doc -> return (sL (getLoc $1) (name, doc)) } }

docsection :: { Located (n, HsDoc RdrName) }
  : DOCSECTION {% let (n, doc) = getDOCSECTION $1 in
        case parseHaddockString (tokenise doc) of {
      Left  err -> parseError (getLoc $1) err;
      Right doc -> return (sL (getLoc $1) (n, doc)) } }

docoptions :: { String }
  : DOCOPTIONS { getDOCOPTIONS $1 }

moduleheader :: { (HaddockModInfo RdrName, Maybe (HsDoc RdrName)) }                                    
        : DOCNEXT {% let string = getDOCNEXT $1 in
               case parseModuleHeader string of {                       
                 Right (str, info) ->                                  
                   case parseHaddockParagraphs (tokenise str) of {               
                     Left err -> parseError (getLoc $1) err;                    
                     Right doc -> return (info, Just doc);          
                   };                                             
                 Left err -> parseError (getLoc $1) err
            }  }                                                  

maybe_docprev :: { Maybe (LHsDoc RdrName) }
	: docprev                       { Just $1 }
	| {- empty -}                   { Nothing }

maybe_docnext :: { Maybe (LHsDoc RdrName) }
	: docnext                       { Just $1 }
	| {- empty -}                   { Nothing }

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

getDOCNEXT (L _ (ITdocCommentNext x)) = x
getDOCPREV (L _ (ITdocCommentPrev x)) = x
getDOCNAMED (L _ (ITdocCommentNamed x)) = x
getDOCSECTION (L _ (ITdocSection n x)) = (n, x)
getDOCOPTIONS (L _ (ITdocOptions x)) = x

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
