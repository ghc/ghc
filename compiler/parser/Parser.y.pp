--								-*-haskell-*-
-- ---------------------------------------------------------------------------
-- (c) The University of Glasgow 1997-2003
---
-- The GHC grammar.
--
-- Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-- ---------------------------------------------------------------------------

{
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-
Careful optimisation of the parser: we don't want to throw everything
at it, because that takes too long and doesn't buy much, but we do want
to inline certain key external functions, so we instruct GHC not to
throw away inlinings as it would normally do in -O0 mode.
-}

module Parser ( parseModule, parseStmt, parseIdentifier, parseType,
		parseHeader ) where

import HsSyn
import RdrHsSyn
import HscTypes		( IsBootInterface, WarningTxt(..) )
import Lexer
import RdrName
import TysWiredIn	( unitTyCon, unitDataCon, tupleTyCon, tupleCon, nilDataCon,
			  unboxedSingletonTyCon, unboxedSingletonDataCon,
			  listTyCon_RDR, parrTyCon_RDR, consDataCon_RDR )
import Type		( funTyCon )
import ForeignCall	( Safety(..), CExportSpec(..), CLabelString,
			  CCallConv(..), CCallTarget(..), defaultCCallConv
			)
import OccName		( varName, dataName, tcClsName, tvName )
import DataCon		( DataCon, dataConName )
import SrcLoc
import Module
import StaticFlags	( opt_SccProfilingOn, opt_Hpc )
import Type		( Kind, liftedTypeKind, unliftedTypeKind )
import Coercion		( mkArrowKind )
import Class		( FunDep )
import BasicTypes
import DynFlags
import OrdList
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

 'forall'	{ L _ ITforall }		-- GHC extension keywords
 'foreign'	{ L _ ITforeign }
 'export'	{ L _ ITexport }
 'label'	{ L _ ITlabel } 
 'dynamic'	{ L _ ITdynamic }
 'safe'		{ L _ ITsafe }
 'threadsafe'	{ L _ ITthreadsafe }  -- ToDo: remove deprecated alias
 'interruptible' { L _ ITinterruptible }
 'unsafe'	{ L _ ITunsafe }
 'mdo'		{ L _ ITmdo }
 'family'	{ L _ ITfamily }
 'stdcall'      { L _ ITstdcallconv }
 'ccall'        { L _ ITccallconv }
 'prim'         { L _ ITprimcallconv }
 'proc'		{ L _ ITproc }		-- for arrow notation extension
 'rec'		{ L _ ITrec }		-- for arrow notation extension
 'group'    { L _ ITgroup }     -- for list transform extension
 'by'       { L _ ITby }        -- for list transform extension
 'using'    { L _ ITusing }     -- for list transform extension

 '{-# INLINE'             { L _ (ITinline_prag _ _) }
 '{-# SPECIALISE'         { L _ ITspec_prag }
 '{-# SPECIALISE_INLINE'  { L _ (ITspec_inline_prag _) }
 '{-# SOURCE'      				{ L _ ITsource_prag }
 '{-# RULES'       				{ L _ ITrules_prag }
 '{-# CORE'        				{ L _ ITcore_prag }              -- hdaume: annotated core
 '{-# SCC'                { L _ ITscc_prag }
 '{-# GENERATED'          { L _ ITgenerated_prag }
 '{-# DEPRECATED'         { L _ ITdeprecated_prag }
 '{-# WARNING'            { L _ ITwarning_prag }
 '{-# UNPACK'             { L _ ITunpack_prag }
 '{-# ANN'                { L _ ITann_prag }
 '{-# VECTORISE'          { L _ ITvect_prag }
 '{-# VECTORISE_SCALAR'   { L _ ITvect_scalar_prag }
 '{-# NOVECTORISE'        { L _ ITnovect_prag }
 '#-}'             				{ L _ ITclose_prag }

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
 PREFIXQVARSYM  { L _ (ITprefixqvarsym  _) }
 PREFIXQCONSYM  { L _ (ITprefixqconsym  _) }

 IPDUPVARID   	{ L _ (ITdupipvarid   _) }		-- GHC extension

 CHAR		{ L _ (ITchar     _) }
 STRING		{ L _ (ITstring   _) }
 INTEGER	{ L _ (ITinteger  _) }
 RATIONAL	{ L _ (ITrational _) }
		    
 PRIMCHAR	{ L _ (ITprimchar   _) }
 PRIMSTRING	{ L _ (ITprimstring _) }
 PRIMINTEGER	{ L _ (ITprimint    _) }
 PRIMWORD 	{ L _ (ITprimword  _) }
 PRIMFLOAT	{ L _ (ITprimfloat  _) }
 PRIMDOUBLE	{ L _ (ITprimdouble _) }

 DOCNEXT	{ L _ (ITdocCommentNext _) }
 DOCPREV	{ L _ (ITdocCommentPrev _) }
 DOCNAMED	{ L _ (ITdocCommentNamed _) }
 DOCSECTION	{ L _ (ITdocSection _ _) }

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
TH_QUASIQUOTE	{ L _ (ITquasiQuote _) }

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
    | '(' '->' ')'      { LL $ getRdrName funTyCon }

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

module 	:: { Located (HsModule RdrName) }
 	: maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule (Just $3) $5 (fst $7) (snd $7) $4 $1
                          ) )}
        | body2
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule Nothing Nothing
                          (fst $1) (snd $1) Nothing Nothing
                          )) }

maybedocheader :: { Maybe LHsDocString }
        : moduleheader            { $1 }
        | {- empty -}             { Nothing }

missing_module_keyword :: { () }
	: {- empty -}				{% pushCurrentContext }

maybemodwarning :: { Maybe WarningTxt }
    : '{-# DEPRECATED' strings '#-}' { Just (DeprecatedTxt $ unLoc $2) }
    | '{-# WARNING' strings '#-}'    { Just (WarningTxt $ unLoc $2) }
    |  {- empty -}                  { Nothing }

body 	:: { ([LImportDecl RdrName], [LHsDecl RdrName]) }
	:  '{'            top '}'		{ $2 }
 	|      vocurly    top close		{ $2 }

body2 	:: { ([LImportDecl RdrName], [LHsDecl RdrName]) }
	:  '{' top '}'          		{ $2 }
 	|  missing_module_keyword top close     { $2 }

top 	:: { ([LImportDecl RdrName], [LHsDecl RdrName]) }
	: importdecls				{ (reverse $1,[]) }
	| importdecls ';' cvtopdecls		{ (reverse $1,$3) }
	| cvtopdecls				{ ([],$1) }

cvtopdecls :: { [LHsDecl RdrName] }
	: topdecls				{ cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header 	:: { Located (HsModule RdrName) }
 	: maybedocheader 'module' modid maybemodwarning maybeexports 'where' header_body
		{% fileSrcSpan >>= \ loc ->
		   return (L loc (HsModule (Just $3) $5 $7 [] $4 $1
                          ))}
        | header_body2
		{% fileSrcSpan >>= \ loc ->
                   return (L loc (HsModule Nothing Nothing $1 [] Nothing
                          Nothing)) }

header_body :: { [LImportDecl RdrName] }
	:  '{'            importdecls		{ $2 }
        |      vocurly    importdecls           { $2 }

header_body2 :: { [LImportDecl RdrName] }
        :  '{' importdecls                      { $2 }
        |  missing_module_keyword importdecls   { $2 }

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
        : docsection    { L1 (case (unLoc $1) of (n, doc) -> IEGroup n doc) }
        | docnamed      { L1 (IEDocNamed ((fst . unLoc) $1)) } 
        | docnext       { L1 (IEDoc (unLoc $1)) }       
                       
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
	: 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec 
		{ L (comb4 $1 $6 $7 $8) (ImportDecl $6 $5 $2 $3 $4 (unLoc $7) (unLoc $8)) }

maybe_src :: { IsBootInterface }
	: '{-# SOURCE' '#-}'			{ True }
	| {- empty -}				{ False }

maybe_safe :: { Bool }
	: 'safe'				{ True }
	| {- empty -}				{ False }

maybe_pkg :: { Maybe FastString }
        : STRING                                { Just (getSTRING $1) }
        | {- empty -}                           { Nothing }

optqualified :: { Bool }
      	: 'qualified'                           { True  }
      	| {- empty -}				{ False }

maybeas :: { Located (Maybe ModuleName) }
      	: 'as' modid                            { LL (Just (unLoc $2)) }
      	| {- empty -}				{ noLoc Nothing }

maybeimpspec :: { Located (Maybe (Bool, [LIE RdrName])) }
	: impspec				{ L1 (Just (unLoc $1)) }
	| {- empty -}				{ noLoc Nothing }

impspec :: { Located (Bool, [LIE RdrName]) }
	:  '(' exportlist ')'  			{ LL (False, $2) }
	|  'hiding' '(' exportlist ')' 		{ LL (True,  $3) }

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
        : topdecls ';' topdecl                  { $1 `appOL` $3 }
        | topdecls ';'                          { $1 }
        | topdecl                               { $1 }

topdecl :: { OrdList (LHsDecl RdrName) }
        : cl_decl                       { unitOL (L1 (TyClD (unLoc $1))) }
        | ty_decl                       { unitOL (L1 (TyClD (unLoc $1))) }
        | 'instance' inst_type where_inst
            { let (binds, sigs, ats, _) = cvBindsAndSigs (unLoc $3)
              in 
              unitOL (L (comb3 $1 $2 $3) (InstD (InstDecl $2 binds sigs ats)))}
        | stand_alone_deriving                  { unitOL (LL (DerivD (unLoc $1))) }
        | 'default' '(' comma_types0 ')'        { unitOL (LL $ DefD (DefaultDecl $3)) }
        | 'foreign' fdecl                       { unitOL (LL (unLoc $2)) }
        | '{-# DEPRECATED' deprecations '#-}'   { $2 }
        | '{-# WARNING' warnings '#-}'          { $2 }
        | '{-# RULES' rules '#-}'               { $2 }
        | '{-# VECTORISE_SCALAR' qvar '#-}'     { unitOL $ LL $ VectD (HsVect   $2 Nothing) }
        | '{-# VECTORISE' qvar '=' exp '#-}'    { unitOL $ LL $ VectD (HsVect   $2 (Just $4)) }
        | '{-# NOVECTORISE' qvar '#-}'     			{ unitOL $ LL $ VectD (HsNoVect $2) }
        | annotation { unitOL $1 }
        | decl                                  { unLoc $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if 
        -- it had a $(..) wrapped around it
        | infixexp                              { unitOL (LL $ mkTopSpliceDecl $1) } 

-- Type classes
--
cl_decl :: { LTyClDecl RdrName }
	: 'class' tycl_hdr fds where_cls	{% mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 $4 }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl RdrName }
           -- ordinary type synonyms
        : 'type' type '=' ctypedoc
		-- Note ctype, not sigtype, on the right of '='
		-- We allow an explicit for-all but we don't insert one
		-- in 	type Foo a = (b,b)
		-- Instead we just say b is out of scope
	        --
		-- Note the use of type for the head; this allows
		-- infix type constructors to be declared 
 		{% mkTySynonym (comb2 $1 $4) False $2 $4 }

           -- type family declarations
        | 'type' 'family' type opt_kind_sig 
		-- Note the use of type for the head; this allows
		-- infix type constructors to be declared
 		{% mkTyFamily (comb3 $1 $3 $4) TypeFamily $3 (unLoc $4) }

           -- type instance declarations
        | 'type' 'instance' type '=' ctype
		-- Note the use of type for the head; this allows
		-- infix type constructors and type patterns
 		{% mkTySynonym (comb2 $1 $5) True $3 $5 }

          -- ordinary data type or newtype declaration
	| data_or_newtype tycl_hdr constrs deriving
		{% mkTyData (comb4 $1 $2 $3 $4) (unLoc $1) False $2 
                            Nothing (reverse (unLoc $3)) (unLoc $4) }
			           -- We need the location on tycl_hdr in case 
				   -- constrs and deriving are both empty

          -- ordinary GADT declaration
        | data_or_newtype tycl_hdr opt_kind_sig 
		 gadt_constrlist
		 deriving
		{% mkTyData (comb4 $1 $2 $4 $5) (unLoc $1) False $2 
                            (unLoc $3) (unLoc $4) (unLoc $5) }
			           -- We need the location on tycl_hdr in case 
				   -- constrs and deriving are both empty

          -- data/newtype family
        | 'data' 'family' type opt_kind_sig
		{% mkTyFamily (comb3 $1 $2 $4) DataFamily $3 (unLoc $4) }

          -- data/newtype instance declaration
	| data_or_newtype 'instance' tycl_hdr constrs deriving
		{% mkTyData (comb4 $1 $3 $4 $5) (unLoc $1) True $3
			    Nothing (reverse (unLoc $4)) (unLoc $5) }

          -- GADT instance declaration
        | data_or_newtype 'instance' tycl_hdr opt_kind_sig 
	         gadt_constrlist
		 deriving
		{% mkTyData (comb4 $1 $3 $5 $6) (unLoc $1) True $3
			    (unLoc $4) (unLoc $5) (unLoc $6) }

-- Associated type family declarations
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
 		{% mkTyFamily (comb3 $1 $2 $3) TypeFamily $2 (unLoc $3) }

           -- default type instance
        | 'type' type '=' ctype
		-- Note the use of type for the head; this allows
		-- infix type constructors and type patterns
 		{% mkTySynonym (comb2 $1 $4) True $2 $4 }

          -- data/newtype family declaration
        | 'data' type opt_kind_sig
		{% mkTyFamily (comb3 $1 $2 $3) DataFamily $2 (unLoc $3) }

-- Associated type instances
--
at_decl_inst :: { LTyClDecl RdrName }
           -- type instance declarations
        : 'type' type '=' ctype
		-- Note the use of type for the head; this allows
		-- infix type constructors and type patterns
 		{% mkTySynonym (comb2 $1 $4) True $2 $4 }

        -- data/newtype instance declaration
	| data_or_newtype tycl_hdr constrs deriving
		{% mkTyData (comb4 $1 $2 $3 $4) (unLoc $1) True $2 
                            Nothing (reverse (unLoc $3)) (unLoc $4) }

        -- GADT instance declaration
        | data_or_newtype tycl_hdr opt_kind_sig 
		 gadt_constrlist
		 deriving
		{% mkTyData (comb4 $1 $2 $4 $5) (unLoc $1) True $2 
		   	    (unLoc $3) (unLoc $4) (unLoc $5) }

data_or_newtype :: { Located NewOrData }
	: 'data'	{ L1 DataType }
	| 'newtype'	{ L1 NewType }

opt_kind_sig :: { Located (Maybe Kind) }
	: 				{ noLoc Nothing }
	| '::' kind			{ LL (Just (unLoc $2)) }

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--	T a b
-- 	Eq a => T a
--	(Eq a, Ord b) => T a b
--      T Int [a]			-- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (Maybe (LHsContext RdrName), LHsType RdrName) }
	: context '=>' type		{ LL (Just $1, $3) }
	| type                          { L1 (Nothing, $1) }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl RdrName }
  	: 'deriving' 'instance' inst_type { LL (DerivDecl $3) }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { Located (OrdList (LHsDecl RdrName)) }
decl_cls  : at_decl_cls		        { LL (unitOL (L1 (TyClD (unLoc $1)))) }
	  | decl                        { $1 }

	  -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtypedoc
                    {% do { (TypeSig l ty) <- checkValSig $2 $4
                          ; return (LL $ unitOL (LL $ SigD (GenericSig l ty))) } }

decls_cls :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	  : decls_cls ';' decl_cls	{ LL (unLoc $1 `appOL` unLoc $3) }
	  | decls_cls ';'		{ LL (unLoc $1) }
	  | decl_cls			{ $1 }
	  | {- empty -}			{ noLoc nilOL }


decllist_cls
        :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	: '{'         decls_cls '}'	{ LL (unLoc $2) }
	|     vocurly decls_cls close	{ $2 }

-- Class body
--
where_cls :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
				-- No implicit parameters
				-- May have type declarations
	: 'where' decllist_cls	        { LL (unLoc $2) }
	| {- empty -}		        { noLoc nilOL }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl RdrName)) }
decl_inst  : at_decl_inst	        { LL (unitOL (L1 (TyClD (unLoc $1)))) }
	   | decl                       { $1 }

decls_inst :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	   : decls_inst ';' decl_inst	{ LL (unLoc $1 `appOL` unLoc $3) }
	   | decls_inst ';'		{ LL (unLoc $1) }
	   | decl_inst			{ $1 }
	   | {- empty -}		{ noLoc nilOL }

decllist_inst 
        :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
	: '{'         decls_inst '}'	{ LL (unLoc $2) }
	|     vocurly decls_inst close	{ $2 }

-- Instance body
--
where_inst :: { Located (OrdList (LHsDecl RdrName)) }	-- Reversed
				-- No implicit parameters
				-- May have type declarations
	: 'where' decllist_inst		{ LL (unLoc $2) }
	| {- empty -}			{ noLoc nilOL }

-- Declarations in binding groups other than classes and instances
--
decls 	:: { Located (OrdList (LHsDecl RdrName)) }	
	: decls ';' decl		{ let { this = unLoc $3;
                                    rest = unLoc $1;
                                    these = rest `appOL` this }
                              in rest `seq` this `seq` these `seq`
                                    LL these }
	| decls ';'			{ LL (unLoc $1) }
	| decl				{ $1 }
	| {- empty -}			{ noLoc nilOL }

decllist :: { Located (OrdList (LHsDecl RdrName)) }
	: '{'            decls '}'	{ LL (unLoc $2) }
	|     vocurly    decls close	{ $2 }

-- Binding groups other than those of class and instance declarations
--
binds 	::  { Located (HsLocalBinds RdrName) } 		-- May have implicit parameters
						-- No type declarations
	: decllist			{ L1 (HsValBinds (cvBindGroup (unLoc $1))) }
	| '{'            dbinds '}'	{ LL (HsIPBinds (IPBinds (unLoc $2) emptyTcEvBinds)) }
	|     vocurly    dbinds close	{ L (getLoc $2) (HsIPBinds (IPBinds (unLoc $2) emptyTcEvBinds)) }

wherebinds :: { Located (HsLocalBinds RdrName) }	-- May have implicit parameters
						-- No type declarations
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
-- Warnings and deprecations (c.f. rules)

warnings :: { OrdList (LHsDecl RdrName) }
	: warnings ';' warning		{ $1 `appOL` $3 }
	| warnings ';' 			{ $1 }
	| warning				{ $1 }
	| {- empty -}				{ nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
warning :: { OrdList (LHsDecl RdrName) }
	: namelist strings
		{ toOL [ LL $ WarningD (Warning n (WarningTxt $ unLoc $2))
		       | n <- unLoc $1 ] }

deprecations :: { OrdList (LHsDecl RdrName) }
	: deprecations ';' deprecation		{ $1 `appOL` $3 }
	| deprecations ';' 			{ $1 }
	| deprecation				{ $1 }
	| {- empty -}				{ nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LHsDecl RdrName) }
	: namelist strings
		{ toOL [ LL $ WarningD (Warning n (DeprecatedTxt $ unLoc $2))
		       | n <- unLoc $1 ] }

strings :: { Located [FastString] }
    : STRING { L1 [getSTRING $1] }
    | '[' stringlist ']' { LL $ fromOL (unLoc $2) }

stringlist :: { Located (OrdList FastString) }
    : stringlist ',' STRING { LL (unLoc $1 `snocOL` getSTRING $3) }
    | STRING                { LL (unitOL (getSTRING $1)) }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl RdrName }
    : '{-# ANN' name_var aexp '#-}'      { LL (AnnD $ HsAnnotation (ValueAnnProvenance (unLoc $2)) $3) }
    | '{-# ANN' 'type' tycon aexp '#-}'  { LL (AnnD $ HsAnnotation (TypeAnnProvenance (unLoc $3)) $4) }
    | '{-# ANN' 'module' aexp '#-}'      { LL (AnnD $ HsAnnotation ModuleAnnProvenance $3) }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { LHsDecl RdrName }
fdecl : 'import' callconv safety fspec
		{% mkImport $2 $3 (unLoc $4) >>= return.LL }
      | 'import' callconv        fspec		
		{% do { d <- mkImport $2 (PlaySafe False) (unLoc $3);
			return (LL d) } }
      | 'export' callconv fspec
		{% mkExport $2 (unLoc $3) >>= return.LL }

callconv :: { CCallConv }
	  : 'stdcall'			{ StdCallConv }
	  | 'ccall'			{ CCallConv   }
	  | 'prim'			{ PrimCallConv}

safety :: { Safety }
	: 'unsafe'			{ PlayRisky }
	| 'safe'			{ PlaySafe  False }
	| 'interruptible'		{ PlayInterruptible }
	| 'threadsafe'			{ PlaySafe  True } -- deprecated alias

fspec :: { Located (Located FastString, Located RdrName, LHsType RdrName) }
       : STRING var '::' sigtypedoc     { LL (L (getLoc $1) (getSTRING $1), $2, $4) }
       |        var '::' sigtypedoc     { LL (noLoc nilFS, $1, $3) }
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

sigtype :: { LHsType RdrName }		-- Always a HsForAllTy,
                                        -- to tell the renamer where to generalise
	: ctype				{ L1 (mkImplicitHsForAllTy (noLoc []) $1) }
	-- Wrap an Implicit forall if there isn't one there already

sigtypedoc :: { LHsType RdrName }       -- Always a HsForAllTy
	: ctypedoc			{ L1 (mkImplicitHsForAllTy (noLoc []) $1) }
	-- Wrap an Implicit forall if there isn't one there already

sig_vars :: { Located [Located RdrName] }
	 : sig_vars ',' var		{ LL ($3 : unLoc $1) }
	 | var				{ L1 [$1] }

sigtypes1 :: { [LHsType RdrName] }	-- Always HsForAllTys
	: sigtype			{ [ $1 ] }
	| sigtype ',' sigtypes1		{ $1 : $3 }

-----------------------------------------------------------------------------
-- Types

infixtype :: { LHsType RdrName }
	: btype qtyconop type         { LL $ HsOpTy $1 $2 $3 }
        | btype tyvarop  type  	 { LL $ HsOpTy $1 $2 $3 }

strict_mark :: { Located HsBang }
	: '!'				{ L1 HsStrict }
	| '{-# UNPACK' '#-}' '!'	{ LL HsUnpack }

-- A ctype is a for-all type
ctype	:: { LHsType RdrName }
	: 'forall' tv_bndrs '.' ctype	{ LL $ mkExplicitHsForAllTy $2 (noLoc []) $4 }
	| context '=>' ctype		{ LL $ mkImplicitHsForAllTy   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| ipvar '::' type		{ LL (HsPredTy (HsIParam (unLoc $1) $3)) }
	| type  			{ $1 }

----------------------
-- Notes for 'ctypedoc'
-- It would have been nice to simplify the grammar by unifying `ctype` and 
-- ctypedoc` into one production, allowing comments on types everywhere (and
-- rejecting them after parsing, where necessary).  This is however not possible
-- since it leads to ambiguity. The reason is the support for comments on record
-- fields: 
--         data R = R { field :: Int -- ^ comment on the field }
-- If we allow comments on types here, it's not clear if the comment applies
-- to 'field' or to 'Int'. So we must use `ctype` to describe the type.

ctypedoc :: { LHsType RdrName }
	: 'forall' tv_bndrs '.' ctypedoc	{ LL $ mkExplicitHsForAllTy $2 (noLoc []) $4 }
	| context '=>' ctypedoc		{ LL $ mkImplicitHsForAllTy   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| ipvar '::' type		{ LL (HsPredTy (HsIParam (unLoc $1) $3)) }
	| typedoc			{ $1 }

----------------------
-- Notes for 'context'
-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--	(Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>

-- We have the t1 ~ t2 form both in 'context' and in type, 
-- to permit an individual equational constraint without parenthesis.
-- Thus for some reason we allow    f :: a~b => blah
-- but not 	                    f :: ?x::Int => blah
context :: { LHsContext RdrName }
        : btype '~'      btype  	{% checkContext
					     (LL $ HsPredTy (HsEqualP $1 $3)) }
	| btype 			{% checkContext $1 }

type :: { LHsType RdrName }
        : btype                         { $1 }
        | btype qtyconop type           { LL $ HsOpTy $1 $2 $3 }
        | btype tyvarop  type     	{ LL $ HsOpTy $1 $2 $3 }
 	| btype '->'     ctype		{ LL $ HsFunTy $1 $3 }
        | btype '~'      btype  	{ LL $ HsPredTy (HsEqualP $1 $3) }

typedoc :: { LHsType RdrName }
        : btype                          { $1 }
        | btype docprev                  { LL $ HsDocTy $1 $2 }
        | btype qtyconop type            { LL $ HsOpTy $1 $2 $3 }
        | btype qtyconop type docprev    { LL $ HsDocTy (L (comb3 $1 $2 $3) (HsOpTy $1 $2 $3)) $4 }
        | btype tyvarop  type            { LL $ HsOpTy $1 $2 $3 }
        | btype tyvarop  type docprev    { LL $ HsDocTy (L (comb3 $1 $2 $3) (HsOpTy $1 $2 $3)) $4 }
        | btype '->'     ctypedoc        { LL $ HsFunTy $1 $3 }
        | btype docprev '->' ctypedoc    { LL $ HsFunTy (L (comb2 $1 $2) (HsDocTy $1 $2)) $4 }
        | btype '~'      btype           { LL $ HsPredTy (HsEqualP $1 $3) }

btype :: { LHsType RdrName }
	: btype atype			{ LL $ HsAppTy $1 $2 }
	| atype				{ $1 }

atype :: { LHsType RdrName }
	: gtycon			{ L1 (HsTyVar (unLoc $1)) }
	| tyvar				{ L1 (HsTyVar (unLoc $1)) }
	| strict_mark atype		{ LL (HsBangTy (unLoc $1) $2) }  -- Constructor sigs only
	| '{' fielddecls '}'		{ LL $ HsRecTy $2 }              -- Constructor sigs only
	| '(' ctype ',' comma_types1 ')'  { LL $ HsTupleTy Boxed  ($2:$4) }
	| '(#' comma_types1 '#)'	{ LL $ HsTupleTy Unboxed $2     }
	| '[' ctype ']'			{ LL $ HsListTy  $2 }
	| '[:' ctype ':]'		{ LL $ HsPArrTy  $2 }
	| '(' ctype ')'		        { LL $ HsParTy   $2 }
	| '(' ctype '::' kind ')'	{ LL $ HsKindSig $2 (unLoc $4) }
	| quasiquote       	        { L1 (HsQuasiQuoteTy (unLoc $1)) }
	| '$(' exp ')'	      		{ LL $ mkHsSpliceTy $2 }
	| TH_ID_SPLICE	      		{ LL $ mkHsSpliceTy $ L1 $ HsVar $ 
					  mkUnqual varName (getTH_ID_SPLICE $1) }

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
	: tyvar				{ L1 (UserTyVar (unLoc $1) placeHolderKind) }
	| '(' tyvar '::' kind ')'	{ LL (KindedTyVar (unLoc $2) 
							  (unLoc $4)) }

fds :: { Located [Located (FunDep RdrName)] }
	: {- empty -}			{ noLoc [] }
	| '|' fds1			{ LL (reverse (unLoc $2)) }

fds1 :: { Located [Located (FunDep RdrName)] }
	: fds1 ',' fd			{ LL ($3 : unLoc $1) }
	| fd				{ L1 [$1] }

fd :: { Located (FunDep RdrName) }
	: varids0 '->' varids0		{ L (comb3 $1 $2 $3)
					   (reverse (unLoc $1), reverse (unLoc $3)) }

varids0	:: { Located [RdrName] }
	: {- empty -}			{ noLoc [] }
	| varids0 tyvar			{ LL (unLoc $2 : unLoc $1) }

-----------------------------------------------------------------------------
-- Kinds

kind	:: { Located Kind }
	: akind			{ $1 }
	| akind '->' kind	{ LL (mkArrowKind (unLoc $1) (unLoc $3)) }

akind	:: { Located Kind }
	: '*'			{ L1 liftedTypeKind }
	| '!'			{ L1 unliftedTypeKind }
	| '(' kind ')'		{ LL (unLoc $2) }


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located [LConDecl RdrName] }	-- Returned in order
	: 'where' '{'        gadt_constrs '}'      { L (comb2 $1 $3) (unLoc $3) }
	| 'where' vocurly    gadt_constrs close	   { L (comb2 $1 $3) (unLoc $3) }
	| {- empty -}                              { noLoc [] }

gadt_constrs :: { Located [LConDecl RdrName] }
        : gadt_constr ';' gadt_constrs  { L (comb2 (head $1) $3) ($1 ++ unLoc $3) }
        | gadt_constr                   { L (getLoc (head $1)) $1 }
        | {- empty -}	 		{ noLoc [] }

-- We allow the following forms:
--	C :: Eq a => a -> T a
--	C :: forall a. Eq a => !a -> T a
--	D { x,y :: a } :: T a
--	forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { [LConDecl RdrName] }	-- Returns a list because of:   C,D :: ty
        : con_list '::' sigtype
                { map (sL (comb2 $1 $3)) (mkGadtDecl (unLoc $1) $3) } 

		-- Deprecated syntax for GADT record declarations
	| oqtycon '{' fielddecls '}' '::' sigtype
		{% do { cd <- mkDeprecatedGadtRecordDecl (comb2 $1 $6) $1 $3 $6
                      ; return [cd] } }

constrs :: { Located [LConDecl RdrName] }
        : maybe_docnext '=' constrs1    { L (comb2 $2 $3) (addConDocs (unLoc $3) $1) }

constrs1 :: { Located [LConDecl RdrName] }
	: constrs1 maybe_docnext '|' maybe_docprev constr { LL (addConDoc $5 $2 : addConDocFirst (unLoc $1) $4) }
	| constr			                  { L1 [$1] }

constr :: { LConDecl RdrName }
	: maybe_docnext forall context '=>' constr_stuff maybe_docprev	
		{ let (con,details) = unLoc $5 in 
		  addConDoc (L (comb4 $2 $3 $4 $5) (mkSimpleConDecl con (unLoc $2) $3 details))
                            ($1 `mplus` $6) }
	| maybe_docnext forall constr_stuff maybe_docprev
		{ let (con,details) = unLoc $3 in 
		  addConDoc (L (comb2 $2 $3) (mkSimpleConDecl con (unLoc $2) (noLoc []) details))
                            ($1 `mplus` $4) }

forall :: { Located [LHsTyVarBndr RdrName] }
	: 'forall' tv_bndrs '.'		{ LL $2 }
	| {- empty -}			{ noLoc [] }

constr_stuff :: { Located (Located RdrName, HsConDeclDetails RdrName) }
-- We parse the constructor declaration 
--	C t1 t2
-- as a btype (treating C as a type constructor) and then convert C to be
-- a data constructor.  Reason: it might continue like this:
--	C t1 t2 %: D Int
-- in which case C really would be a type constructor.  We can't resolve this
-- ambiguity till we come across the constructor oprerator :% (or not, more usually)
	: btype				{% splitCon $1 >>= return.LL }
	| btype conop btype		{  LL ($2, InfixCon $1 $3) }

fielddecls :: { [ConDeclField RdrName] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [ConDeclField RdrName] }
	: fielddecl maybe_docnext ',' maybe_docprev fielddecls1
                      { [ addFieldDoc f $4 | f <- $1 ] ++ addFieldDocs $5 $2 }
                             -- This adds the doc $4 to each field separately
	| fielddecl   { $1 }

fielddecl :: { [ConDeclField RdrName] }    -- A list because of   f,g :: Int
	: maybe_docnext sig_vars '::' ctype maybe_docprev      { [ ConDeclField fld $4 ($1 `mplus` $5) 
                                                                 | fld <- reverse (unLoc $2) ] }

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

{- Note [Declaration/signature overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's an awkward overlap with a type signature.  Consider
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
        : docdecld { L1 (DocD (unLoc $1)) }

docdecld :: { LDocDecl }
        : docnext                               { L1 (DocCommentNext (unLoc $1)) }
        | docprev                               { L1 (DocCommentPrev (unLoc $1)) }
        | docnamed                              { L1 (case (unLoc $1) of (n, doc) -> DocCommentNamed n doc) }
        | docsection                            { L1 (case (unLoc $1) of (n, doc) -> DocGroup n doc) }

decl 	:: { Located (OrdList (LHsDecl RdrName)) }
	: sigdecl		{ $1 }

        | '!' aexp rhs          {% do { let { e = LL (SectionR (LL (HsVar bang_RDR)) $2) };
                                        pat <- checkPattern e;
                                        return $ LL $ unitOL $ LL $ ValD $
                                               PatBind pat (unLoc $3)
                                                       placeHolderType placeHolderNames } }
                                -- Turn it all into an expression so that
                                -- checkPattern can check that bangs are enabled

        | infixexp opt_sig rhs  {% do { r <- checkValDef $1 $2 $3;
                                        let { l = comb2 $1 $> };
                                        return $! (sL l (unitOL $! (sL l $ ValD r))) } }
        | docdecl               { LL $ unitOL $1 }

rhs	:: { Located (GRHSs RdrName) }
	: '=' exp wherebinds	{ sL (comb3 $1 $2 $3) $ GRHSs (unguardedRHS $2) (unLoc $3) }
	| gdrhs	wherebinds	{ LL $ GRHSs (reverse (unLoc $1)) (unLoc $2) }

gdrhs :: { Located [LGRHS RdrName] }
	: gdrhs gdrh		{ LL ($2 : unLoc $1) }
	| gdrh			{ L1 [$1] }

gdrh :: { LGRHS RdrName }
	: '|' guardquals '=' exp  	{ sL (comb2 $1 $>) $ GRHS (unLoc $2) $4 }

sigdecl :: { Located (OrdList (LHsDecl RdrName)) }
        : 
	-- See Note [Declaration/signature overlap] for why we need infixexp here
	  infixexp '::' sigtypedoc
                        {% do s <- checkValSig $1 $3 
                        ; return (LL $ unitOL (LL $ SigD s)) }
	| var ',' sig_vars '::' sigtypedoc
				{ LL $ toOL [ LL $ SigD (TypeSig ($1 : unLoc $3) $5) ] }
	| infix prec ops	{ LL $ toOL [ LL $ SigD (FixSig (FixitySig n (Fixity $2 (unLoc $1))))
					     | n <- unLoc $3 ] }
	| '{-# INLINE'   activation qvar '#-}'	      
		{ LL $ unitOL (LL $ SigD (InlineSig $3 (mkInlinePragma (getINLINE $1) $2))) }
	| '{-# SPECIALISE' qvar '::' sigtypes1 '#-}'
		{ LL $ toOL [ LL $ SigD (SpecSig $2 t defaultInlinePragma) 
					    | t <- $4] }
	| '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
		{ LL $ toOL [ LL $ SigD (SpecSig $3 t (mkInlinePragma (getSPEC_INLINE $1) $2))
					    | t <- $5] }
	| '{-# SPECIALISE' 'instance' inst_type '#-}'
		{ LL $ unitOL (LL $ SigD (SpecInstSig $3)) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsQuasiQuote RdrName) }
	: TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in L1 (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }

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
	: '\\' apat apats opt_asig '->' exp	
			{ LL $ HsLam (mkMatchGroup [LL $ Match ($2:$3) $4
							   	(unguardedGRHSs $6)
							    ]) }
  	| 'let' binds 'in' exp			{ LL $ HsLet (unLoc $2) $4 }
	| 'if' exp optSemi 'then' exp optSemi 'else' exp
                                        {% checkDoAndIfThenElse $2 $3 $5 $6 $8 >>
                                           return (LL $ mkHsIf $2 $5 $8) }
   	| 'case' exp 'of' altslist		{ LL $ HsCase $2 (mkMatchGroup (unLoc $4)) }
	| '-' fexp				{ LL $ NegApp $2 noSyntaxExpr }

  	| 'do' stmtlist			{ L (comb2 $1 $2) (mkHsDo DoExpr  (unLoc $2)) }
  	| 'mdo' stmtlist		{ L (comb2 $1 $2) (mkHsDo MDoExpr (unLoc $2)) }

        | scc_annot exp		    		{ LL $ if opt_SccProfilingOn
							then HsSCC (unLoc $1) $2
							else HsPar $2 }
        | hpc_annot exp		    		{ LL $ if opt_Hpc
							then HsTickPragma (unLoc $1) $2
							else HsPar $2 }

	| 'proc' aexp '->' exp	
			{% checkPattern $2 >>= \ p -> 
			   return (LL $ HsProc p (LL $ HsCmdTop $4 [] 
						   placeHolderType undefined)) }
						-- TODO: is LL right here?

        | '{-# CORE' STRING '#-}' exp           { LL $ HsCoreAnn (getSTRING $2) $4 }
						    -- hdaume: core annotation
	| fexp					{ $1 }

optSemi :: { Bool }
	: ';'         { True }
	| {- empty -} { False }

scc_annot :: { Located FastString }
	: '_scc_' STRING			{% (addWarning Opt_WarnWarningsDeprecations (getLoc $1) (text "_scc_ is deprecated; use an SCC pragma instead")) >>= \_ ->
                                   ( do scc <- getSCC $2; return $ LL scc ) }
	| '{-# SCC' STRING '#-}'		{% do scc <- getSCC $2; return $ LL scc }

hpc_annot :: { Located (FastString,(Int,Int),(Int,Int)) }
	: '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
						{ LL $ (getSTRING $2
						       ,( fromInteger $ getINTEGER $3
 							, fromInteger $ getINTEGER $5
							)
                         			       ,( fromInteger $ getINTEGER $7
 							, fromInteger $ getINTEGER $9
							)
						       )
					         }

fexp 	:: { LHsExpr RdrName }
	: fexp aexp				{ LL $ HsApp $1 $2 }
  	| aexp					{ $1 }

aexp	:: { LHsExpr RdrName }
	: qvar '@' aexp			{ LL $ EAsPat $1 $3 }
	| '~' aexp			{ LL $ ELazyPat $2 }
	| aexp1			{ $1 }

aexp1	:: { LHsExpr RdrName }
        : aexp1 '{' fbinds '}' 	{% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4) $3
				      ; return (LL r) }}
  	| aexp2			{ $1 }

-- Here was the syntax for type applications that I was planning
-- but there are difficulties (e.g. what order for type args)
-- so it's not enabled yet.
-- But this case *is* used for the left hand side of a generic definition,
-- which is parsed as an expression before being munged into a pattern
 	| qcname '{|' type '|}'         { LL $ HsApp (sL (getLoc $1) (HsVar (unLoc $1)))
						     (sL (getLoc $3) (HsType $3)) }

aexp2	:: { LHsExpr RdrName }
	: ipvar				{ L1 (HsIPVar $! unLoc $1) }
	| qcname			{ L1 (HsVar   $! unLoc $1) }
	| literal			{ L1 (HsLit   $! unLoc $1) }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--	| STRING			{ sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRING $1) placeHolderType) }
	| INTEGER			{ sL (getLoc $1) (HsOverLit $! mkHsIntegral (getINTEGER $1) placeHolderType) }
	| RATIONAL			{ sL (getLoc $1) (HsOverLit $! mkHsFractional (getRATIONAL $1) placeHolderType) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
	| '(' texp ')'			{ LL (HsPar $2) }
	| '(' tup_exprs ')'             { LL (ExplicitTuple $2 Boxed) }

	| '(#' texp '#)'		{ LL (ExplicitTuple [Present $2] Unboxed) }
	| '(#' tup_exprs '#)'		{ LL (ExplicitTuple $2 Unboxed) }

	| '[' list ']'                  { LL (unLoc $2) }
	| '[:' parr ':]'                { LL (unLoc $2) }
	| '_'				{ L1 EWildPat }
	
	-- Template Haskell Extension
	| TH_ID_SPLICE          { L1 $ HsSpliceE (mkHsSplice 
					(L1 $ HsVar (mkUnqual varName 
							(getTH_ID_SPLICE $1)))) } 
	| '$(' exp ')'   	{ LL $ HsSpliceE (mkHsSplice $2) }               


	| TH_VAR_QUOTE qvar 	{ LL $ HsBracket (VarBr (unLoc $2)) }
	| TH_VAR_QUOTE qcon 	{ LL $ HsBracket (VarBr (unLoc $2)) }
	| TH_TY_QUOTE tyvar 	{ LL $ HsBracket (VarBr (unLoc $2)) }
 	| TH_TY_QUOTE gtycon	{ LL $ HsBracket (VarBr (unLoc $2)) }
	| '[|' exp '|]'         { LL $ HsBracket (ExpBr $2) }                       
	| '[t|' ctype '|]'      { LL $ HsBracket (TypBr $2) }                       
	| '[p|' infixexp '|]'   {% checkPattern $2 >>= \p ->
					return (LL $ HsBracket (PatBr p)) }
	| '[d|' cvtopbody '|]'	{ LL $ HsBracket (DecBrL $2) }
	| quasiquote       	{ L1 (HsQuasiQuoteE (unLoc $1)) }

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

-----------------------------------------------------------------------------
-- Tuple expressions

-- "texp" is short for tuple expressions: 
-- things that can appear unparenthesized as long as they're
-- inside parens or delimitted by commas
texp :: { LHsExpr RdrName }
	: exp				{ $1 }

	-- Note [Parsing sections]
	-- ~~~~~~~~~~~~~~~~~~~~~~~
	-- We include left and right sections here, which isn't
	-- technically right according to the Haskell standard.
        -- For example (3 +, True) isn't legal.
	-- However, we want to parse bang patterns like
	--	(!x, !y)
	-- and it's convenient to do so here as a section
        -- Then when converting expr to pattern we unravel it again
	-- Meanwhile, the renamer checks that real sections appear
	-- inside parens.
        | infixexp qop 	{ LL $ SectionL $1 $2 }
	| qopm infixexp       { LL $ SectionR $1 $2 }

       -- View patterns get parenthesized above
	| exp '->' texp   { LL $ EViewPat $1 $3 }

-- Always at least one comma
tup_exprs :: { [HsTupArg RdrName] }
           : texp commas_tup_tail  { Present $1 : $2 }
           | commas tup_tail	   { replicate $1 missingTupArg ++ $2 }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { [HsTupArg RdrName] }
commas_tup_tail : commas tup_tail  { replicate ($1-1) missingTupArg ++ $2 }

-- Always follows a comma
tup_tail :: { [HsTupArg RdrName] }
          : texp commas_tup_tail	{ Present $1 : $2 }
	  | texp			{ [Present $1] }
          | {- empty -}		        { [missingTupArg] }

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
	| texp '|' flattenedpquals	
             {% checkMonadComp >>= \ ctxt ->
		return (sL (comb2 $1 $>) $ 
                        mkHsComp ctxt (unLoc $3) $1) }

lexps :: { Located [LHsExpr RdrName] }
	: lexps ',' texp 		{ LL (((:) $! $3) $! unLoc $1) }
	| texp ',' texp			{ LL [$3,$1] }

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt RdrName] }
    : pquals   { case (unLoc $1) of
                    [qs] -> L1 qs
                    -- We just had one thing in our "parallel" list so 
                    -- we simply return that thing directly
                    
                    qss -> L1 [L1 $ ParStmt [(qs, undefined) | qs <- qss] noSyntaxExpr noSyntaxExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt RdrName]] }
    : squals '|' pquals     { L (getLoc $2) (reverse (unLoc $1) : unLoc $3) }
    | squals                { L (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt RdrName] }	-- In reverse order, because the last 
					-- one can "grab" the earlier ones
    : squals ',' transformqual         	     { LL [L (getLoc $3) ((unLoc $3) (reverse (unLoc $1)))] }
    | squals ',' qual                  	     { LL ($3 : unLoc $1) }
    | transformqual                          { LL [L (getLoc $1) ((unLoc $1) [])] }
    | qual                                   { L1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { LL ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { L1 [$2] }


-- It is possible to enable bracketing (associating) qualifier lists by uncommenting the lines with {| |}
-- above. Due to a lack of consensus on the syntax, this feature is not being used until we get user
-- demand.

transformqual :: { Located ([LStmt RdrName] -> Stmt RdrName) }
			-- Function is applied to a list of stmts *in order*
    : 'then' exp                { LL $ \leftStmts -> (mkTransformStmt leftStmts $2) }
    -- >>>
    | 'then' exp 'by' exp       { LL $ \leftStmts -> (mkTransformByStmt leftStmts $2 $4) }
    | 'then' 'group' 'by' exp   { LL $ \leftStmts -> (mkGroupByStmt leftStmts $4) }
    -- <<<
    -- These two productions deliberately have a shift-reduce conflict. I have made 'group' into a special_id,
    -- which means you can enable TransformListComp while still using Data.List.group. However, this makes the two
    -- productions ambiguous. I've set things up so that Happy chooses to resolve the conflict in that case by
    -- choosing the "group by" variant, which is what we want.
    --
    -- This is rather dubious: the user might be confused as to how to parse this statement. However, it is a good
    -- practical choice. NB: Data.List.group :: [a] -> [[a]], so using the first production would not even type check
    -- if /that/ is the group function we conflict with.
    | 'then' 'group' 'using' exp           { LL $ \leftStmts -> (mkGroupUsingStmt leftStmts $4) }
    | 'then' 'group' 'by' exp 'using' exp  { LL $ \leftStmts -> (mkGroupByUsingStmt leftStmts $4 $6) }

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { LHsExpr RdrName }
	: 				{ noLoc (ExplicitPArr placeHolderType []) }
	| texp				{ L1 $ ExplicitPArr placeHolderType [$1] }
	| lexps 			{ L1 $ ExplicitPArr placeHolderType 
						       (reverse (unLoc $1)) }
	| texp '..' exp	 		{ LL $ PArrSeq noPostTcExpr (FromTo $1 $3) }
	| texp ',' exp '..' exp		{ LL $ PArrSeq noPostTcExpr (FromThenTo $1 $3 $5) }
	| texp '|' flattenedpquals	{ LL $ mkHsComp PArrComp (unLoc $3) $1 }

-- We are reusing `lexps' and `flattenedpquals' from the list case.

-----------------------------------------------------------------------------
-- Guards

guardquals :: { Located [LStmt RdrName] }
    : guardquals1           { L (getLoc $1) (reverse (unLoc $1)) }

guardquals1 :: { Located [LStmt RdrName] }
    : guardquals1 ',' qual  { LL ($3 : unLoc $1) }
    | qual                  { L1 [$1] }

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
	: pat opt_sig alt_rhs		{ LL (Match [$1] $2 (unLoc $3)) }

alt_rhs :: { Located (GRHSs RdrName) }
	: ralt wherebinds		{ LL (GRHSs (unLoc $1) (unLoc $2)) }

ralt :: { Located [LGRHS RdrName] }
	: '->' exp			{ LL (unguardedRHS $2) }
	| gdpats			{ L1 (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS RdrName] }
	: gdpats gdpat			{ LL ($2 : unLoc $1) }
	| gdpat				{ L1 [$1] }

gdpat	:: { LGRHS RdrName }
	: '|' guardquals '->' exp	 	{ sL (comb2 $1 $>) $ GRHS (unLoc $2) $4 }

-- 'pat' recognises a pattern, including one with a bang at the top
-- 	e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat RdrName }
pat 	:  exp		 	{% checkPattern $1 }
	| '!' aexp		{% checkPattern (LL (SectionR (L1 (HsVar bang_RDR)) $2)) }

apat   :: { LPat RdrName }	
apat 	: aexp			{% checkPattern $1 }
	| '!' aexp		{% checkPattern (LL (SectionR (L1 (HsVar bang_RDR)) $2)) }

apats  :: { [LPat RdrName] }
	: apat apats		{ $1 : $2 }
  	| {- empty -}		{ [] }

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
	: qual				    { $1 }
  	| 'rec' stmtlist		{ LL $ mkRecStmt (unLoc $2) }

qual  :: { LStmt RdrName }
    : pat '<-' exp			{ LL $ mkBindStmt $1 $3 }
    | exp				    { L1 $ mkExprStmt $1 }
    | 'let' binds			{ LL $ LetStmt (unLoc $2) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds 	:: { ([HsRecField RdrName (LHsExpr RdrName)], Bool) }
	: fbinds1			{ $1 }
  	| {- empty -}			{ ([], False) }

fbinds1	:: { ([HsRecField RdrName (LHsExpr RdrName)], Bool) }
	: fbind ',' fbinds1		{ case $3 of (flds, dd) -> ($1 : flds, dd) } 
	| fbind				{ ([$1], False) }
	| '..'				{ ([],   True) }
  
fbind	:: { HsRecField RdrName (LHsExpr RdrName) }
	: qvar '=' exp	{ HsRecField $1 $3                False }
        | qvar          { HsRecField $1 placeHolderPunRhs True }
			-- In the punning case, use a place-holder
                        -- The renamer fills in the final value

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds 	:: { Located [LIPBind RdrName] }
	: dbinds ';' dbind		{ let { this = $3; rest = unLoc $1 }
                              in rest `seq` this `seq` LL (this : rest) }
	| dbinds ';'			{ LL (unLoc $1) }
	| dbind				{ let this = $1 in this `seq` L1 [this] }
--	| {- empty -}			{ [] }

dbind	:: { LIPBind RdrName }
dbind	: ipvar '=' exp			{ LL (IPBind (unLoc $1) $3) }

ipvar	:: { Located (IPName RdrName) }
	: IPDUPVARID		{ L1 (IPName (mkUnqual varName (getIPDUPVARID $1))) }

-----------------------------------------------------------------------------
-- Warnings and deprecations

namelist :: { Located [RdrName] }
namelist : name_var              { L1 [unLoc $1] }
         | name_var ',' namelist { LL (unLoc $1 : unLoc $3) }

name_var :: { Located RdrName }
name_var : var { $1 }
         | con { $1 }

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

con_list :: { Located [Located RdrName] }
con_list : con                  { L1 [$1] }
         | con ',' con_list     { LL ($1 : unLoc $3) }

sysdcon	:: { Located DataCon }	-- Wired in data constructors
	: '(' ')'		{ LL unitDataCon }
	| '(' commas ')'	{ LL $ tupleCon Boxed ($2 + 1) }
	| '(#' '#)'		{ LL $ unboxedSingletonDataCon }
	| '(#' commas '#)'	{ LL $ tupleCon Unboxed ($2 + 1) }
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
	| '(' commas ')'		{ LL $ getRdrName (tupleTyCon Boxed ($2 + 1)) }
	| '(#' '#)'			{ LL $ getRdrName unboxedSingletonTyCon }
	| '(#' commas '#)'		{ LL $ getRdrName (tupleTyCon Unboxed ($2 + 1)) }
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
        | PREFIXQCONSYM                 { L1 $! mkQual tcClsName (getPREFIXQCONSYM $1) }
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
	| '.'			{% parseErrorSDoc (getLoc $1) 
	  			      (vcat [ptext (sLit "Illegal symbol '.' in type"), 
				             ptext (sLit "Perhaps you intended -XRankNTypes or similar flag"),
					     ptext (sLit "to enable explicit-forall syntax: forall <tvs>. <type>")])
	                        }

tyvarid	:: { Located RdrName }
	: VARID			{ L1 $! mkUnqual tvName (getVARID $1) }
	| special_id		{ L1 $! mkUnqual tvName (unLoc $1) }
	| 'unsafe' 		{ L1 $! mkUnqual tvName (fsLit "unsafe") }
	| 'safe' 		{ L1 $! mkUnqual tvName (fsLit "safe") }
	| 'interruptible' 	{ L1 $! mkUnqual tvName (fsLit "interruptible") }
	| 'threadsafe' 		{ L1 $! mkUnqual tvName (fsLit "threadsafe") }

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
	| QVARID		{ L1 $! mkQual varName (getQVARID $1) }
        | PREFIXQVARSYM         { L1 $! mkQual varName (getPREFIXQVARSYM $1) }

varid :: { Located RdrName }
	: VARID			{ L1 $! mkUnqual varName (getVARID $1) }
	| special_id		{ L1 $! mkUnqual varName (unLoc $1) }
	| 'unsafe'		{ L1 $! mkUnqual varName (fsLit "unsafe") }
	| 'safe'		{ L1 $! mkUnqual varName (fsLit "safe") }
	| 'interruptible'	{ L1 $! mkUnqual varName (fsLit "interruptible") }
	| 'threadsafe'		{ L1 $! mkUnqual varName (fsLit "threadsafe") }
	| 'forall'		{ L1 $! mkUnqual varName (fsLit "forall") }
	| 'family'              { L1 $! mkUnqual varName (fsLit "family") }

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
	| '-'			{ L1 $ mkUnqual varName (fsLit "-") }

varsym_no_minus :: { Located RdrName } -- varsym not including '-'
	: VARSYM		{ L1 $ mkUnqual varName (getVARSYM $1) }
	| special_sym		{ L1 $ mkUnqual varName (unLoc $1) }


-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe', 'interruptible', 'forall', and 'family' whose treatment differs
-- depending on context 
special_id :: { Located FastString }
special_id
	: 'as'			{ L1 (fsLit "as") }
	| 'qualified'		{ L1 (fsLit "qualified") }
	| 'hiding'		{ L1 (fsLit "hiding") }
	| 'export'		{ L1 (fsLit "export") }
	| 'label'		{ L1 (fsLit "label")  }
	| 'dynamic'		{ L1 (fsLit "dynamic") }
	| 'stdcall'             { L1 (fsLit "stdcall") }
	| 'ccall'               { L1 (fsLit "ccall") }
	| 'prim'                { L1 (fsLit "prim") }
	| 'group'               { L1 (fsLit "group") }

special_sym :: { Located FastString }
special_sym : '!'	{ L1 (fsLit "!") }
	    | '.' 	{ L1 (fsLit ".") }
 	    | '*' 	{ L1 (fsLit "*") }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }	-- Qualified or unqualified
	: conid			{ $1 }
	| QCONID		{ L1 $! mkQual dataName (getQCONID $1) }
        | PREFIXQCONSYM         { L1 $! mkQual dataName (getPREFIXQCONSYM $1) }

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
	| STRING 		{ L1 $ HsString     $ getSTRING $1 }
	| PRIMINTEGER		{ L1 $ HsIntPrim    $ getPRIMINTEGER $1 }
	| PRIMWORD   		{ L1 $ HsWordPrim    $ getPRIMWORD $1 }
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

modid 	:: { Located ModuleName }
	: CONID			{ L1 $ mkModuleNameFS (getCONID $1) }
        | QCONID		{ L1 $ let (mod,c) = getQCONID $1 in
				  mkModuleNameFS
				   (mkFastString
				     (unpackFS mod ++ '.':unpackFS c))
				}

commas :: { Int }
	: commas ','			{ $1 + 1 }
	| ','				{ 1 }

-----------------------------------------------------------------------------
-- Documentation comments

docnext :: { LHsDocString }
  : DOCNEXT {% return (L1 (HsDocString (mkFastString (getDOCNEXT $1)))) }

docprev :: { LHsDocString }
  : DOCPREV {% return (L1 (HsDocString (mkFastString (getDOCPREV $1)))) }

docnamed :: { Located (String, HsDocString) }
  : DOCNAMED {%
      let string = getDOCNAMED $1 
          (name, rest) = break isSpace string
      in return (L1 (name, HsDocString (mkFastString rest))) }

docsection :: { Located (Int, HsDocString) }
  : DOCSECTION {% let (n, doc) = getDOCSECTION $1 in
        return (L1 (n, HsDocString (mkFastString doc))) }

moduleheader :: { Maybe LHsDocString }
        : DOCNEXT {% let string = getDOCNEXT $1 in
                     return (Just (L1 (HsDocString (mkFastString string)))) }

maybe_docprev :: { Maybe LHsDocString }
	: docprev                       { Just $1 }
	| {- empty -}                   { Nothing }

maybe_docnext :: { Maybe LHsDocString }
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
getPREFIXQVARSYM (L _ (ITprefixqvarsym  x)) = x
getPREFIXQCONSYM (L _ (ITprefixqconsym  x)) = x
getIPDUPVARID   (L _ (ITdupipvarid   x)) = x
getCHAR		(L _ (ITchar     x)) = x
getSTRING	(L _ (ITstring   x)) = x
getINTEGER	(L _ (ITinteger  x)) = x
getRATIONAL	(L _ (ITrational x)) = x
getPRIMCHAR	(L _ (ITprimchar   x)) = x
getPRIMSTRING	(L _ (ITprimstring x)) = x
getPRIMINTEGER	(L _ (ITprimint    x)) = x
getPRIMWORD	(L _ (ITprimword x)) = x
getPRIMFLOAT	(L _ (ITprimfloat  x)) = x
getPRIMDOUBLE	(L _ (ITprimdouble x)) = x
getTH_ID_SPLICE (L _ (ITidEscape x)) = x
getINLINE	(L _ (ITinline_prag inl conl)) = (inl,conl)
getSPEC_INLINE	(L _ (ITspec_inline_prag True))  = (Inline,  FunLike)
getSPEC_INLINE	(L _ (ITspec_inline_prag False)) = (NoInline,FunLike)

getDOCNEXT (L _ (ITdocCommentNext x)) = x
getDOCPREV (L _ (ITdocCommentPrev x)) = x
getDOCNAMED (L _ (ITdocCommentNamed x)) = x
getDOCSECTION (L _ (ITdocSection n x)) = (n, x)

getSCC :: Located Token -> P FastString
getSCC lt = do let s = getSTRING lt
                   err = "Spaces are not allowed in SCCs"
               -- We probably actually want to be more restrictive than this
               if ' ' `elem` unpackFS s
                   then failSpanMsgP (getLoc lt) (text err)
                   else return s

-- Utilities for combining source spans
comb2 :: Located a -> Located b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

comb3 :: Located a -> Located b -> Located c -> SrcSpan
comb3 a b c = a `seq` b `seq` c `seq`
    combineSrcSpans (getLoc a) (combineSrcSpans (getLoc b) (getLoc c))

comb4 :: Located a -> Located b -> Located c -> Located d -> SrcSpan
comb4 a b c d = a `seq` b `seq` c `seq` d `seq`
    (combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
		combineSrcSpans (getLoc c) (getLoc d))

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` a `seq` L span a

-- Make a source location for the file.  We're a bit lazy here and just
-- make a point SrcSpan at line 1, column 0.  Strictly speaking we should
-- try to find the span of the whole file (ToDo).
fileSrcSpan :: P SrcSpan
fileSrcSpan = do 
  l <- getSrcLoc; 
  let loc = mkSrcLoc (srcLocFile l) 1 1;
  return (mkSrcSpan loc loc)
}
