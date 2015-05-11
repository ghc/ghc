--                                                              -*-haskell-*-
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
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-- | This module provides the generated Happy parser for Haskell. It exports
-- a number of parsers which may be used in any library that uses the GHC API.
-- A common usage pattern is to initialize the parser state with a given string
-- and then parse that string:
--
-- @
--     runParser :: DynFlags -> String -> P a -> ParseResult a
--     runParser flags str parser = unP parser parseState
--     where
--       filename = "\<interactive\>"
--       location = mkRealSrcLoc (mkFastString filename) 1 1
--       buffer = stringToStringBuffer str
--       parseState = mkPState flags buffer location in
-- @
module Parser (parseModule, parseImport, parseStatement,
               parseDeclaration, parseExpression, parseTypeSignature,
               parseFullStmt, parseStmt, parseIdentifier,
               parseType, parseHeader) where

-- base
import Control.Monad    ( unless, liftM )
import GHC.Exts
import Data.Char
import Control.Monad    ( mplus )

-- compiler/hsSyn
import HsSyn

-- compiler/main
import HscTypes         ( IsBootInterface, WarningTxt(..) )
import DynFlags

-- compiler/utils
import OrdList
import BooleanFormula   ( BooleanFormula(..), mkTrue )
import FastString
import Maybes           ( orElse )
import Outputable

-- compiler/basicTypes
import RdrName
import OccName          ( varName, dataName, tcClsName, tvName, startsWithUnderscore )
import DataCon          ( DataCon, dataConName )
import SrcLoc
import Module
import BasicTypes

-- compiler/types
import Type             ( funTyCon )
import Kind             ( Kind, liftedTypeKind, unliftedTypeKind, mkArrowKind )
import Class            ( FunDep )

-- compiler/parser
import RdrHsSyn
import Lexer
import HaddockUtils
import ApiAnnotation

-- compiler/typecheck
import TcEvidence       ( emptyTcEvBinds )

-- compiler/prelude
import ForeignCall
import TysPrim          ( liftedTypeKindTyConName, eqPrimTyCon )
import TysWiredIn       ( unitTyCon, unitDataCon, tupleTyCon, tupleCon, nilDataCon,
                          unboxedUnitTyCon, unboxedUnitDataCon,
                          listTyCon_RDR, parrTyCon_RDR, consDataCon_RDR, eqTyCon_RDR )

}

{-
-----------------------------------------------------------------------------
14 Dec 2014

Conflicts: 48 shift/reduce
           1  reduce/reduce

-----------------------------------------------------------------------------
20 Nov 2014

Conflicts: 60 shift/reduce
           12 reduce/reduce

-----------------------------------------------------------------------------
25 June 2014

Conflicts: 47 shift/reduce
           1 reduce/reduce

-----------------------------------------------------------------------------
12 October 2012

Conflicts: 43 shift/reduce
           1 reduce/reduce

-----------------------------------------------------------------------------
24 February 2006

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

10 for abiguity in 'if x then y else z + 1'             [State 178]
        (shift parses as 'if x then y else (z + 1)', as per longest-parse rule)
        10 because op might be: : - ! * . `x` VARSYM CONSYM QVARSYM QCONSYM

1 for ambiguity in 'if x then y else z :: T'            [State 178]
        (shift parses as 'if x then y else (z :: T)', as per longest-parse rule)

4 for ambiguity in 'if x then y else z -< e'            [State 178]
        (shift parses as 'if x then y else (z -< T)', as per longest-parse rule)
        There are four such operators: -<, >-, -<<, >>-


2 for ambiguity in 'case v of { x :: T -> T ... } '     [States 11, 253]
        Which of these two is intended?
          case v of
            (x::T) -> T         -- Rhs is T
    or
          case v of
            (x::T -> T) -> ..   -- Rhs is ...

10 for ambiguity in 'e :: a `b` c'.  Does this mean     [States 11, 253]
        (e::a) `b` c, or
        (e :: (a `b` c))
    As well as `b` we can have !, VARSYM, QCONSYM, and CONSYM, hence 5 cases
    Same duplication between states 11 and 253 as the previous case

1 for ambiguity in 'let ?x ...'                         [State 329]
        the parser can't tell whether the ?x is the lhs of a normal binding or
        an implicit binding.  Fortunately resolving as shift gives it the only
        sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity in '{-# RULES "name" [ ... #-}          [State 382]
        we don't know whether the '[' starts the activation or not: it
        might be the start of the declaration with the activation being
        empty.  --SDM 1/4/2002

1 for ambiguity in '{-# RULES "name" forall = ... #-}'  [State 474]
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

This is done using the three functions below, sL0, sL1
and sLL.  Note that these functions were mechanically
converted from the three macros that used to exist before,
namely L0, L1 and LL.

They each add a SrcSpan to their argument.

   sL0  adds 'noSrcSpan', used for empty productions
     -- This doesn't seem to work anymore -=chak

   sL1  for a production with a single token on the lhs.  Grabs the SrcSpan
        from that token.

   sLL  for a production with >1 token on the lhs.  Makes up a SrcSpan from
        the first and last tokens.

These suffice for the majority of cases.  However, we must be
especially careful with empty productions: sLL won't work if the first
or last token on the lhs can represent an empty span.  In these cases,
we have to calculate the span using more of the tokens from the lhs, eg.

        | 'newtype' tycl_hdr '=' newconstr deriving
                { L (comb3 $1 $4 $5)
                    (mkTyData NewType (unLoc $2) $4 (unLoc $5)) }

We provide comb3 and comb4 functions which are useful in such cases.

Be careful: there's no checking that you actually got this right, the
only symptom will be that the SrcSpans of your syntax will be
incorrect.

-- -----------------------------------------------------------------------------
-- API Annotations

A lot of the productions are now cluttered with calls to
aa,am,ams,amms etc.

These are helper functions to make sure that the locations of the
various keywords such as do / let / in are captured for use by tools
that want to do source to source conversions, such as refactorers or
structured editors.

The helper functions are defined at the bottom of this file.

See
  https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations and
  https://ghc.haskell.org/trac/ghc/wiki/GhcAstAnnotations
for some background.

-- -----------------------------------------------------------------------------

-}

%token
 '_'            { L _ ITunderscore }            -- Haskell keywords
 'as'           { L _ ITas }
 'case'         { L _ ITcase }
 'class'        { L _ ITclass }
 'data'         { L _ ITdata }
 'default'      { L _ ITdefault }
 'deriving'     { L _ ITderiving }
 'do'           { L _ ITdo }
 'else'         { L _ ITelse }
 'hiding'       { L _ IThiding }
 'if'           { L _ ITif }
 'import'       { L _ ITimport }
 'in'           { L _ ITin }
 'infix'        { L _ ITinfix }
 'infixl'       { L _ ITinfixl }
 'infixr'       { L _ ITinfixr }
 'instance'     { L _ ITinstance }
 'let'          { L _ ITlet }
 'module'       { L _ ITmodule }
 'newtype'      { L _ ITnewtype }
 'of'           { L _ ITof }
 'qualified'    { L _ ITqualified }
 'then'         { L _ ITthen }
 'type'         { L _ ITtype }
 'where'        { L _ ITwhere }

 'forall'       { L _ ITforall }                -- GHC extension keywords
 'foreign'      { L _ ITforeign }
 'export'       { L _ ITexport }
 'label'        { L _ ITlabel }
 'dynamic'      { L _ ITdynamic }
 'safe'         { L _ ITsafe }
 'interruptible' { L _ ITinterruptible }
 'unsafe'       { L _ ITunsafe }
 'mdo'          { L _ ITmdo }
 'family'       { L _ ITfamily }
 'role'         { L _ ITrole }
 'stdcall'      { L _ ITstdcallconv }
 'ccall'        { L _ ITccallconv }
 'capi'         { L _ ITcapiconv }
 'prim'         { L _ ITprimcallconv }
 'javascript'   { L _ ITjavascriptcallconv }
 'proc'         { L _ ITproc }          -- for arrow notation extension
 'rec'          { L _ ITrec }           -- for arrow notation extension
 'group'    { L _ ITgroup }     -- for list transform extension
 'by'       { L _ ITby }        -- for list transform extension
 'using'    { L _ ITusing }     -- for list transform extension
 'pattern'      { L _ ITpattern } -- for pattern synonyms
 'static'       { L _ ITstatic }  -- for static pointers extension

 '{-# INLINE'             { L _ (ITinline_prag _ _ _) }
 '{-# SPECIALISE'         { L _ (ITspec_prag _) }
 '{-# SPECIALISE_INLINE'  { L _ (ITspec_inline_prag _ _) }
 '{-# SOURCE'             { L _ (ITsource_prag _) }
 '{-# RULES'              { L _ (ITrules_prag _) }
 '{-# CORE'               { L _ (ITcore_prag _) }      -- hdaume: annotated core
 '{-# SCC'                { L _ (ITscc_prag _)}
 '{-# GENERATED'          { L _ (ITgenerated_prag _) }
 '{-# DEPRECATED'         { L _ (ITdeprecated_prag _) }
 '{-# WARNING'            { L _ (ITwarning_prag _) }
 '{-# UNPACK'             { L _ (ITunpack_prag _) }
 '{-# NOUNPACK'           { L _ (ITnounpack_prag _) }
 '{-# ANN'                { L _ (ITann_prag _) }
 '{-# VECTORISE'          { L _ (ITvect_prag _) }
 '{-# VECTORISE_SCALAR'   { L _ (ITvect_scalar_prag _) }
 '{-# NOVECTORISE'        { L _ (ITnovect_prag _) }
 '{-# MINIMAL'            { L _ (ITminimal_prag _) }
 '{-# CTYPE'              { L _ (ITctype _) }
 '{-# OVERLAPPING'        { L _ (IToverlapping_prag _) }
 '{-# OVERLAPPABLE'       { L _ (IToverlappable_prag _) }
 '{-# OVERLAPS'           { L _ (IToverlaps_prag _) }
 '{-# INCOHERENT'         { L _ (ITincoherent_prag _) }
 '#-}'                    { L _ ITclose_prag }

 '..'           { L _ ITdotdot }                        -- reserved symbols
 ':'            { L _ ITcolon }
 '::'           { L _ ITdcolon }
 '='            { L _ ITequal }
 '\\'           { L _ ITlam }
 'lcase'        { L _ ITlcase }
 '|'            { L _ ITvbar }
 '<-'           { L _ ITlarrow }
 '->'           { L _ ITrarrow }
 '@'            { L _ ITat }
 '~'            { L _ ITtilde }
 '~#'           { L _ ITtildehsh }
 '=>'           { L _ ITdarrow }
 '-'            { L _ ITminus }
 '!'            { L _ ITbang }
 '*'            { L _ ITstar }
 '-<'           { L _ ITlarrowtail }            -- for arrow notation
 '>-'           { L _ ITrarrowtail }            -- for arrow notation
 '-<<'          { L _ ITLarrowtail }            -- for arrow notation
 '>>-'          { L _ ITRarrowtail }            -- for arrow notation
 '.'            { L _ ITdot }

 '{'            { L _ ITocurly }                        -- special symbols
 '}'            { L _ ITccurly }
 vocurly        { L _ ITvocurly } -- virtual open curly (from layout)
 vccurly        { L _ ITvccurly } -- virtual close curly (from layout)
 '['            { L _ ITobrack }
 ']'            { L _ ITcbrack }
 '[:'           { L _ ITopabrack }
 ':]'           { L _ ITcpabrack }
 '('            { L _ IToparen }
 ')'            { L _ ITcparen }
 '(#'           { L _ IToubxparen }
 '#)'           { L _ ITcubxparen }
 '(|'           { L _ IToparenbar }
 '|)'           { L _ ITcparenbar }
 ';'            { L _ ITsemi }
 ','            { L _ ITcomma }
 '`'            { L _ ITbackquote }
 SIMPLEQUOTE    { L _ ITsimpleQuote      }     -- 'x

 VARID          { L _ (ITvarid    _) }          -- identifiers
 CONID          { L _ (ITconid    _) }
 VARSYM         { L _ (ITvarsym   _) }
 CONSYM         { L _ (ITconsym   _) }
 QVARID         { L _ (ITqvarid   _) }
 QCONID         { L _ (ITqconid   _) }
 QVARSYM        { L _ (ITqvarsym  _) }
 QCONSYM        { L _ (ITqconsym  _) }
 PREFIXQVARSYM  { L _ (ITprefixqvarsym  _) }
 PREFIXQCONSYM  { L _ (ITprefixqconsym  _) }

 IPDUPVARID     { L _ (ITdupipvarid   _) }              -- GHC extension

 CHAR           { L _ (ITchar   _ _) }
 STRING         { L _ (ITstring _ _) }
 INTEGER        { L _ (ITinteger _ _) }
 RATIONAL       { L _ (ITrational _) }

 PRIMCHAR       { L _ (ITprimchar   _ _) }
 PRIMSTRING     { L _ (ITprimstring _ _) }
 PRIMINTEGER    { L _ (ITprimint    _ _) }
 PRIMWORD       { L _ (ITprimword   _ _) }
 PRIMFLOAT      { L _ (ITprimfloat  _) }
 PRIMDOUBLE     { L _ (ITprimdouble _) }

 DOCNEXT        { L _ (ITdocCommentNext _) }
 DOCPREV        { L _ (ITdocCommentPrev _) }
 DOCNAMED       { L _ (ITdocCommentNamed _) }
 DOCSECTION     { L _ (ITdocSection _ _) }

-- Template Haskell
'[|'            { L _ ITopenExpQuote  }
'[p|'           { L _ ITopenPatQuote  }
'[t|'           { L _ ITopenTypQuote  }
'[d|'           { L _ ITopenDecQuote  }
'|]'            { L _ ITcloseQuote    }
'[||'           { L _ ITopenTExpQuote   }
'||]'           { L _ ITcloseTExpQuote  }
TH_ID_SPLICE    { L _ (ITidEscape _)  }     -- $x
'$('            { L _ ITparenEscape   }     -- $( exp )
TH_ID_TY_SPLICE { L _ (ITidTyEscape _)  }   -- $$x
'$$('           { L _ ITparenTyEscape   }   -- $$( exp )
TH_TY_QUOTE     { L _ ITtyQuote       }      -- ''T
TH_QUASIQUOTE   { L _ (ITquasiQuote _) }
TH_QQUASIQUOTE  { L _ (ITqQuasiQuote _) }

%monad { P } { >>= } { return }
%lexer { (lexer True) } { L _ ITeof }
%tokentype { (Located Token) }

-- Exported parsers
%name parseModule module
%name parseImport importdecl
%name parseStatement stmt
%name parseDeclaration topdecl
%name parseExpression exp
%name parseTypeSignature sigdecl
%name parseFullStmt   stmt
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%name parseType ctype
%partial parseHeader header
%%

-----------------------------------------------------------------------------
-- Identifiers; one of the entry points
identifier :: { Located RdrName }
        : qvar                          { $1 }
        | qcon                          { $1 }
        | qvarop                        { $1 }
        | qconop                        { $1 }
    | '(' '->' ')'      {% ams (sLL $1 $> $ getRdrName funTyCon)
                               [mj AnnOpenP $1,mj AnnRarrow $2,mj AnnCloseP $3] }

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

module :: { Located (HsModule RdrName) }
       : maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (L loc (HsModule (Just $3) $5 (fst $ snd $7)
                              (snd $ snd $7) $4 $1)
                    )
                    ([mj AnnModule $2, mj AnnWhere $6] ++ fst $7) }
        | body2
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule Nothing Nothing
                               (fst $ snd $1) (snd $ snd $1) Nothing Nothing))
                       (fst $1) }

maybedocheader :: { Maybe LHsDocString }
        : moduleheader            { $1 }
        | {- empty -}             { Nothing }

missing_module_keyword :: { () }
        : {- empty -}                           {% pushCurrentContext }

maybemodwarning :: { Maybe (Located WarningTxt) }
    : '{-# DEPRECATED' strings '#-}'
                      {% ajs (Just (sLL $1 $> $ DeprecatedTxt (sL1 $1 (getDEPRECATED_PRAGs $1)) (snd $ unLoc $2)))
                             (mo $1:mc $3: (fst $ unLoc $2)) }
    | '{-# WARNING' strings '#-}'
                         {% ajs (Just (sLL $1 $> $ WarningTxt (sL1 $1 (getWARNING_PRAGs $1)) (snd $ unLoc $2)))
                                (mo $1:mc $3 : (fst $ unLoc $2)) }
    |  {- empty -}                  { Nothing }

body    :: { ([AddAnn]
             ,([LImportDecl RdrName], [LHsDecl RdrName])) }
        :  '{'            top '}'      { (moc $1:mcc $3:(fst $2)
                                         , snd $2) }
        |      vocurly    top close    { (fst $2, snd $2) }

body2   :: { ([AddAnn]
             ,([LImportDecl RdrName], [LHsDecl RdrName])) }
        :  '{' top '}'                          { (moc $1:mcc $3
                                                   :(fst $2), snd $2) }
        |  missing_module_keyword top close     { ([],snd $2) }

top     :: { ([AddAnn]
             ,([LImportDecl RdrName], [LHsDecl RdrName])) }
        : importdecls                   { (fst $1
                                          ,(reverse $ snd $1,[]))}
        | importdecls ';' cvtopdecls    {% if null (snd $1)
                                             then return ((mj AnnSemi $2:(fst $1))
                                                         ,(reverse $ snd $1,$3))
                                             else do
                                              { addAnnotation (gl $ head $ snd $1)
                                                              AnnSemi (gl $2)
                                              ; return (fst $1
                                                       ,(reverse $ snd $1,$3)) }}
        | cvtopdecls                    { ([],([],$1)) }

cvtopdecls :: { [LHsDecl RdrName] }
        : topdecls                              { cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header  :: { Located (HsModule RdrName) }
        : maybedocheader 'module' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule (Just $3) $5 $7 [] $4 $1
                          )) [mj AnnModule $2,mj AnnWhere $6] }
        | header_body2
                {% fileSrcSpan >>= \ loc ->
                   return (L loc (HsModule Nothing Nothing $1 [] Nothing
                          Nothing)) }

header_body :: { [LImportDecl RdrName] }
        :  '{'            importdecls           { snd $2 }
        |      vocurly    importdecls           { snd $2 }

header_body2 :: { [LImportDecl RdrName] }
        :  '{' importdecls                      { snd $2 }
        |  missing_module_keyword importdecls   { snd $2 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { (Maybe (Located [LIE RdrName])) }
        :  '(' exportlist ')'       {% ams (sLL $1 $> ()) [mop $1,mcp $3] >>
                                       return (Just (sLL $1 $> (fromOL $2))) }
        |  {- empty -}              { Nothing }

exportlist :: { OrdList (LIE RdrName) }
        : expdoclist ',' expdoclist   {% addAnnotation (oll $1) AnnComma (gl $2)
                                         >> return ($1 `appOL` $3) }
        | exportlist1                 { $1 }

exportlist1 :: { OrdList (LIE RdrName) }
        : expdoclist export expdoclist ',' exportlist1
                          {% (addAnnotation (oll ($1 `appOL` $2 `appOL` $3))
                                            AnnComma (gl $4) ) >>
                              return ($1 `appOL` $2 `appOL` $3 `appOL` $5) }
        | expdoclist export expdoclist             { $1 `appOL` $2 `appOL` $3 }
        | expdoclist                               { $1 }

expdoclist :: { OrdList (LIE RdrName) }
        : exp_doc expdoclist                           { $1 `appOL` $2 }
        | {- empty -}                                  { nilOL }

exp_doc :: { OrdList (LIE RdrName) }
        : docsection    { unitOL (sL1 $1 (case (unLoc $1) of (n, doc) -> IEGroup n doc)) }
        | docnamed      { unitOL (sL1 $1 (IEDocNamed ((fst . unLoc) $1))) }
        | docnext       { unitOL (sL1 $1 (IEDoc (unLoc $1))) }


   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export  :: { OrdList (LIE RdrName) }
        : qcname_ext export_subspec  {% amsu (sLL $1 $> (mkModuleImpExp $1
                                                    (snd $ unLoc $2)))
                                             (fst $ unLoc $2) }
        |  'module' modid            {% amsu (sLL $1 $> (IEModuleContents $2))
                                             [mj AnnModule $1] }
        |  'pattern' qcon            {% amsu (sLL $1 $> (IEVar $2))
                                             [mj AnnPattern $1] }

export_subspec :: { Located ([AddAnn],ImpExpSubSpec) }
        : {- empty -}             { sL0 ([],ImpExpAbs) }
        | '(' '..' ')'            { sLL $1 $> ([mop $1,mcp $3,mj AnnDotdot $2]
                                       , ImpExpAll) }
        | '(' ')'                 { sLL $1 $> ([mop $1,mcp $2],ImpExpList []) }
        | '(' qcnames ')'         { sLL $1 $> ([mop $1,mcp $3],ImpExpList (reverse $2)) }

qcnames :: { [Located RdrName] }     -- A reversed list
        :  qcnames ',' qcname_ext       {% (aa (head $1) (AnnComma, $2)) >>
                                           return ($3  : $1) }
        |  qcname_ext                   { [$1]  }

qcname_ext :: { Located RdrName }       -- Variable or data constructor
                                        -- or tagged type constructor
        :  qcname                   { $1 }
        |  'type' qcname            {% amms (mkTypeImpExp (sLL $1 $> (unLoc $2)))
                                            [mj AnnType $1,mj AnnVal $2] }

-- Cannot pull into qcname_ext, as qcname is also used in expression.
qcname  :: { Located RdrName }  -- Variable or data constructor
        :  qvar                         { $1 }
        |  qcon                         { $1 }

-----------------------------------------------------------------------------
-- Import Declarations

-- import decls can be *empty*, or even just a string of semicolons
-- whereas topdecls must contain at least one topdecl.

importdecls :: { ([AddAnn],[LImportDecl RdrName]) }
        : importdecls ';' importdecl
                                {% if null (snd $1)
                                     then return (mj AnnSemi $2:fst $1,$3 : snd $1)
                                     else do
                                      { addAnnotation (gl $ head $ snd $1)
                                                      AnnSemi (gl $2)
                                      ; return (fst $1,$3 : snd $1) } }
        | importdecls ';'       {% if null (snd $1)
                                     then return ((mj AnnSemi $2:fst $1),snd $1)
                                     else do
                                       { addAnnotation (gl $ head $ snd $1)
                                                       AnnSemi (gl $2)
                                       ; return $1} }
        | importdecl             { ([],[$1]) }
        | {- empty -}            { ([],[]) }

importdecl :: { LImportDecl RdrName }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
                {% ams (L (comb4 $1 $6 (snd $7) $8) $
                  ImportDecl { ideclSourceSrc = snd $ fst $2
                             , ideclName = $6, ideclPkgQual = snd $5
                             , ideclSource = snd $2, ideclSafe = snd $3
                             , ideclQualified = snd $4, ideclImplicit = False
                             , ideclAs = unLoc (snd $7)
                             , ideclHiding = unLoc $8 })
                   ((mj AnnImport $1 : (fst $ fst $2) ++ fst $3 ++ fst $4
                                    ++ fst $5 ++ fst $7)) }

maybe_src :: { (([AddAnn],Maybe SourceText),IsBootInterface) }
        : '{-# SOURCE' '#-}'        { (([mo $1,mc $2],Just (getSOURCE_PRAGs $1))
                                      ,True) }
        | {- empty -}               { (([],Nothing),False) }

maybe_safe :: { ([AddAnn],Bool) }
        : 'safe'                                { ([mj AnnSafe $1],True) }
        | {- empty -}                           { ([],False) }

maybe_pkg :: { ([AddAnn],Maybe FastString) }
        : STRING                                { ([mj AnnPackageName $1]
                                                  ,Just (getSTRING $1)) }
        | {- empty -}                           { ([],Nothing) }

optqualified :: { ([AddAnn],Bool) }
        : 'qualified'                           { ([mj AnnQualified $1],True)  }
        | {- empty -}                           { ([],False) }

maybeas :: { ([AddAnn],Located (Maybe ModuleName)) }
        : 'as' modid                           { ([mj AnnAs $1,mj AnnVal $2]
                                                 ,sLL $1 $> (Just (unLoc $2))) }
        | {- empty -}                          { ([],noLoc Nothing) }

maybeimpspec :: { Located (Maybe (Bool, Located [LIE RdrName])) }
        : impspec                  { L (gl $1) (Just (unLoc $1)) }
        | {- empty -}              { noLoc Nothing }

impspec :: { Located (Bool, Located [LIE RdrName]) }
        :  '(' exportlist ')'               {% ams (sLL $1 $> (False,
                                                      sLL $1 $> $ fromOL $2))
                                                   [mop $1,mcp $3] }
        |  'hiding' '(' exportlist ')'      {% ams (sLL $1 $> (True,
                                                      sLL $1 $> $ fromOL $3))
                                               [mj AnnHiding $1,mop $2,mcp $4] }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec    :: { Located Int }
        : {- empty -}           { noLoc 9 }
        | INTEGER
                 {% checkPrecP (sL1 $1 (fromInteger (getINTEGER $1))) }

infix   :: { Located FixityDirection }
        : 'infix'                               { sL1 $1 InfixN  }
        | 'infixl'                              { sL1 $1 InfixL  }
        | 'infixr'                              { sL1 $1 InfixR }

ops     :: { Located (OrdList (Located RdrName)) }
        : ops ',' op       {% addAnnotation (oll $ unLoc $1) AnnComma (gl $2) >>
                              return (sLL $1 $> ((unLoc $1) `appOL` unitOL $3))}
        | op               { sL1 $1 (unitOL $1) }

-----------------------------------------------------------------------------
-- Top-Level Declarations

topdecls :: { OrdList (LHsDecl RdrName) }
        : topdecls ';' topdecl        {% addAnnotation (oll $1) AnnSemi (gl $2)
                                         >> return ($1 `appOL` $3) }
        | topdecls ';'                {% addAnnotation (oll $1) AnnSemi (gl $2)
                                         >> return $1 }
        | topdecl                     { $1 }

topdecl :: { OrdList (LHsDecl RdrName) }
        : cl_decl                               { unitOL (sL1 $1 (TyClD (unLoc $1))) }
        | ty_decl                               { unitOL (sL1 $1 (TyClD (unLoc $1))) }
        | inst_decl                             { unitOL (sL1 $1 (InstD (unLoc $1))) }
        | stand_alone_deriving                  { unitOL (sLL $1 $> (DerivD (unLoc $1))) }
        | role_annot                            { unitOL (sL1 $1 (RoleAnnotD (unLoc $1))) }
        | 'default' '(' comma_types0 ')'    {% do { def <- checkValidDefaults $3
                                                  ; amsu (sLL $1 $> (DefD def))
                                                         [mj AnnDefault $1
                                                         ,mop $2,mcp $4] }}
        | 'foreign' fdecl          {% amsu (sLL $1 $> (snd $ unLoc $2))
                                           (mj AnnForeign $1:(fst $ unLoc $2)) }
        | '{-# DEPRECATED' deprecations '#-}'   {% amsu (sLL $1 $> $ WarningD (Warnings (getDEPRECATED_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# WARNING' warnings '#-}'          {% amsu (sLL $1 $> $ WarningD (Warnings (getWARNING_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# RULES' rules '#-}'               {% amsu (sLL $1 $> $ RuleD (HsRules (getRULES_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# VECTORISE' qvar '=' exp '#-}' {% amsu (sLL $1 $> $ VectD (HsVect (getVECT_PRAGs $1) $2 $4))
                                                    [mo $1,mj AnnEqual $3
                                                    ,mc $5] }
        | '{-# NOVECTORISE' qvar '#-}'       {% amsu (sLL $1 $> $ VectD (HsNoVect (getNOVECT_PRAGs $1) $2))
                                                     [mo $1,mc $3] }
        | '{-# VECTORISE' 'type' gtycon '#-}'
                                {% amsu (sLL $1 $> $
                                    VectD (HsVectTypeIn (getVECT_PRAGs $1) False $3 Nothing))
                                    [mo $1,mj AnnType $2,mc $4] }

        | '{-# VECTORISE_SCALAR' 'type' gtycon '#-}'
                                {% amsu (sLL $1 $> $
                                    VectD (HsVectTypeIn (getVECT_SCALAR_PRAGs $1) True $3 Nothing))
                                    [mo $1,mj AnnType $2,mc $4] }

        | '{-# VECTORISE' 'type' gtycon '=' gtycon '#-}'
                                {% amsu (sLL $1 $> $
                                    VectD (HsVectTypeIn (getVECT_PRAGs $1) False $3 (Just $5)))
                                    [mo $1,mj AnnType $2,mj AnnEqual $4,mc $6] }
        | '{-# VECTORISE_SCALAR' 'type' gtycon '=' gtycon '#-}'
                                {% amsu (sLL $1 $> $
                                    VectD (HsVectTypeIn (getVECT_SCALAR_PRAGs $1) True $3 (Just $5)))
                                    [mo $1,mj AnnType $2,mj AnnEqual $4,mc $6] }

        | '{-# VECTORISE' 'class' gtycon '#-}'
                                         {% amsu (sLL $1 $>  $ VectD (HsVectClassIn (getVECT_PRAGs $1) $3))
                                                 [mo $1,mj AnnClass $2,mc $4] }
        | annotation { unitOL $1 }
        | decl_no_th                            { unLoc $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if
        -- it had a $(..) wrapped around it
        | infixexp                              { unitOL (sLL $1 $> $ mkSpliceDecl $1) }

-- Type classes
--
cl_decl :: { LTyClDecl RdrName }
        : 'class' tycl_hdr fds where_cls
                {% amms (mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 (snd $ unLoc $4))
                        (mj AnnClass $1:(fst $ unLoc $3)++(fst $ unLoc $4)) }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl RdrName }
           -- ordinary type synonyms
        : 'type' type '=' ctypedoc
                -- Note ctype, not sigtype, on the right of '='
                -- We allow an explicit for-all but we don't insert one
                -- in   type Foo a = (b,b)
                -- Instead we just say b is out of scope
                --
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkTySynonym (comb2 $1 $4) $2 $4)
                        [mj AnnType $1,mj AnnEqual $3] }

           -- type family declarations
        | 'type' 'family' type opt_kind_sig where_type_family
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkFamDecl (comb4 $1 $3 $4 $5) (snd $ unLoc $5) $3
                                   (snd $ unLoc $4))
                        (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)++(fst $ unLoc $5)) }

          -- ordinary data type or newtype declaration
        | data_or_newtype capi_ctype tycl_hdr constrs deriving
                {% amms (mkTyData (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                           Nothing (reverse (snd $ unLoc $4))
                                   (unLoc $5))
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                        ((fst $ unLoc $1):(fst $ unLoc $4)) }

          -- ordinary GADT declaration
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 deriving
            {% amms (mkTyData (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2 $3
                            (snd $ unLoc $4) (snd $ unLoc $5) (unLoc $6) )
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                    ((fst $ unLoc $1):(fst $ unLoc $4)++(fst $ unLoc $5)) }

          -- data/newtype family
        | 'data' 'family' type opt_kind_sig
                {% amms (mkFamDecl (comb3 $1 $2 $4) DataFamily $3 (snd $ unLoc $4))
                        (mj AnnData $1:mj AnnFamily $2:(fst $ unLoc $4)) }

inst_decl :: { LInstDecl RdrName }
        : 'instance' overlap_pragma inst_type where_inst
       {% do { (binds, sigs, _, ats, adts, _) <- cvBindsAndSigs (snd $ unLoc $4)
             ; let cid = ClsInstDecl { cid_poly_ty = $3, cid_binds = binds
                                     , cid_sigs = sigs, cid_tyfam_insts = ats
                                     , cid_overlap_mode = $2
                                     , cid_datafam_insts = adts }
             ; let err = text "In instance head:" <+> ppr $3
             ; checkNoPartialType err $3
             ; sequence_ [ checkNoPartialType err ty
                         | sig@(L _ (TypeSig _ ty _ )) <- sigs
                         , let err = text "in instance signature" <> colon
                                     <+> quotes (ppr sig) ]
             ; ams (L (comb3 $1 $3 $4) (ClsInstD { cid_inst = cid }))
                   (mj AnnInstance $1 : (fst $ unLoc $4)) } }

           -- type instance declarations
        | 'type' 'instance' ty_fam_inst_eqn
                {% ams $3 (fst $ unLoc $3)
                >> amms (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3))
                    (mj AnnType $1:mj AnnInstance $2:(fst $ unLoc $3)) }

          -- data/newtype instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr constrs deriving
            {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 $4
                                      Nothing (reverse (snd  $ unLoc $5))
                                              (unLoc $6))
                    ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)) }

          -- GADT instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 deriving
            {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3 $4
                                   (snd $ unLoc $5) (snd $ unLoc $6) (unLoc $7))
                    ((fst $ unLoc $1):mj AnnInstance $2
                       :(fst $ unLoc $5)++(fst $ unLoc $6)) }

overlap_pragma :: { Maybe (Located OverlapMode) }
  : '{-# OVERLAPPABLE'    '#-}' {% ajs (Just (sLL $1 $> (Overlappable (getOVERLAPPABLE_PRAGs $1))))
                                       [mo $1,mc $2] }
  | '{-# OVERLAPPING'     '#-}' {% ajs (Just (sLL $1 $> (Overlapping (getOVERLAPPING_PRAGs $1))))
                                       [mo $1,mc $2] }
  | '{-# OVERLAPS'        '#-}' {% ajs (Just (sLL $1 $> (Overlaps (getOVERLAPS_PRAGs $1))))
                                       [mo $1,mc $2] }
  | '{-# INCOHERENT'      '#-}' {% ajs (Just (sLL $1 $> (Incoherent (getINCOHERENT_PRAGs $1))))
                                       [mo $1,mc $2] }
  | {- empty -}                 { Nothing }


-- Closed type families

where_type_family :: { Located ([AddAnn],FamilyInfo RdrName) }
        : {- empty -}                      { noLoc ([],OpenTypeFamily) }
        | 'where' ty_fam_inst_eqn_list
               { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                    ,ClosedTypeFamily (reverse (snd $ unLoc $2))) }

ty_fam_inst_eqn_list :: { Located ([AddAnn],[LTyFamInstEqn RdrName]) }
        :     '{' ty_fam_inst_eqns '}'     { sLL $1 $> ([moc $1,mcc $3]
                                                ,unLoc $2) }
        | vocurly ty_fam_inst_eqns close   { let L loc _ = $2 in
                                             L loc ([],unLoc $2) }
        |     '{' '..' '}'                 { sLL $1 $> ([moc $1,mj AnnDotdot $2
                                                 ,mcc $3],[]) }
        | vocurly '..' close               { let L loc _ = $2 in
                                             L loc ([mj AnnDotdot $2],[]) }

ty_fam_inst_eqns :: { Located [LTyFamInstEqn RdrName] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
                                      {% asl (unLoc $1) $2 (snd $ unLoc $3)
                                         >> ams $3 (fst $ unLoc $3)
                                         >> return (sLL $1 $> ((snd $ unLoc $3) : unLoc $1)) }
        | ty_fam_inst_eqns ';'        {% addAnnotation (gl $1) AnnSemi (gl $2)
                                         >> return (sLL $1 $>  (unLoc $1)) }
        | ty_fam_inst_eqn             {% ams $1 (fst $ unLoc $1)
                                         >> return (sLL $1 $> [snd $ unLoc $1]) }

ty_fam_inst_eqn :: { Located ([AddAnn],LTyFamInstEqn RdrName) }
        : type '=' ctype
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
              {% do { (eqn,ann) <- mkTyFamInstEqn $1 $3
                    ; return (sLL $1 $> (mj AnnEqual $2:ann, sLL $1 $> eqn))  } }

-- Associated type family declarations
--
-- * They have a different syntax than on the toplevel (no family special
--   identifier).
--
-- * They also need to be separate from instances; otherwise, data family
--   declarations without a kind signature cause parsing conflicts with empty
--   data declarations.
--
at_decl_cls :: { LHsDecl RdrName }
        :  -- data family declarations, with optional 'family' keyword
          'data' opt_family type opt_kind_sig
                {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4) DataFamily $3
                                                  (snd $ unLoc $4)))
                        (mj AnnData $1:$2++(fst $ unLoc $4)) }

           -- type family declarations, with optional 'family' keyword
           -- (can't use opt_instance because you get shift/reduce errors
        | 'type' type opt_kind_sig
               {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $2 $3)
                                                  OpenTypeFamily $2 (snd $ unLoc $3)))
                       (mj AnnType $1:(fst $ unLoc $3)) }
        | 'type' 'family' type opt_kind_sig
               {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4)
                                                  OpenTypeFamily $3 (snd $ unLoc $4)))
                       (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)) }

           -- default type instances, with optional 'instance' keyword
        | 'type' ty_fam_inst_eqn
                {% ams $2 (fst $ unLoc $2) >>
                   amms (liftM mkInstD (mkTyFamInst (comb2 $1 $2) (snd $ unLoc $2)))
                        (mj AnnType $1:(fst $ unLoc $2)) }
        | 'type' 'instance' ty_fam_inst_eqn
                {% ams $3 (fst $ unLoc $3) >>
                   amms (liftM mkInstD (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3)))
                        (mj AnnType $1:mj AnnInstance $2:(fst $ unLoc $3)) }

opt_family   :: { [AddAnn] }
              : {- empty -}   { [] }
              | 'family'      { [mj AnnFamily $1] }

-- Associated type instances
--
at_decl_inst :: { LInstDecl RdrName }
           -- type instance declarations
        : 'type' ty_fam_inst_eqn
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
                {% ams $2 (fst $ unLoc $2) >>
                   amms (mkTyFamInst (comb2 $1 $2) (snd $ unLoc $2))
                        (mj AnnType $1:(fst $ unLoc $2)) }

        -- data/newtype instance declaration
        | data_or_newtype capi_ctype tycl_hdr constrs deriving
               {% amms (mkDataFamInst (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                                    Nothing (reverse (snd $ unLoc $4))
                                            (unLoc $5))
                       ((fst $ unLoc $1):(fst $ unLoc $4)) }

        -- GADT instance declaration
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 deriving
                {% amms (mkDataFamInst (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2
                                $3 (snd $ unLoc $4) (snd $ unLoc $5) (unLoc $6))
                        ((fst $ unLoc $1):(fst $ unLoc $4)++(fst $ unLoc $5)) }

data_or_newtype :: { Located (AddAnn,NewOrData) }
        : 'data'        { sL1 $1 (mj AnnData    $1,DataType) }
        | 'newtype'     { sL1 $1 (mj AnnNewtype $1,NewType) }

opt_kind_sig :: { Located ([AddAnn],Maybe (LHsKind RdrName)) }
        :                             { noLoc ([],Nothing) }
        | '::' kind                   { sLL $1 $> ([mj AnnDcolon $1],Just ($2)) }

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--      T a b
--      Eq a => T a
--      (Eq a, Ord b) => T a b
--      T Int [a]                       -- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (Maybe (LHsContext RdrName), LHsType RdrName) }
        : context '=>' type         {% addAnnotation (gl $1) AnnDarrow (gl $2)
                                       >> (return (sLL $1 $> (Just $1, $3)))
                                    }
        | type                      { sL1 $1 (Nothing, $1) }

capi_ctype :: { Maybe (Located CType) }
capi_ctype : '{-# CTYPE' STRING STRING '#-}'
                       {% ajs (Just (sLL $1 $> (CType (getCTYPEs $1) (Just (Header (getSTRING $2)))
                                        (getSTRING $3))))
                              [mo $1,mj AnnHeader $2,mj AnnVal $3,mc $4] }

           | '{-# CTYPE'        STRING '#-}'
                       {% ajs (Just (sLL $1 $> (CType (getCTYPEs $1) Nothing  (getSTRING $2))))
                              [mo $1,mj AnnVal $2,mc $3] }

           |           { Nothing }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl RdrName }
  : 'deriving' 'instance' overlap_pragma inst_type
                         {% do {
                                 let err = text "in the stand-alone deriving instance"
                                            <> colon <+> quotes (ppr $4)
                               ; checkNoPartialType err $4
                               ; ams (sLL $1 $> (DerivDecl $4 $3))
                                     [mj AnnDeriving $1,mj AnnInstance $2] }}

-----------------------------------------------------------------------------
-- Role annotations

role_annot :: { LRoleAnnotDecl RdrName }
role_annot : 'type' 'role' oqtycon maybe_roles
          {% amms (mkRoleAnnotDecl (comb3 $1 $3 $4) $3 (reverse (unLoc $4)))
                  [mj AnnType $1,mj AnnRole $2] }

-- Reversed!
maybe_roles :: { Located [Located (Maybe FastString)] }
maybe_roles : {- empty -}    { noLoc [] }
            | roles          { $1 }

roles :: { Located [Located (Maybe FastString)] }
roles : role             { sLL $1 $> [$1] }
      | roles role       { sLL $1 $> $ $2 : unLoc $1 }

-- read it in as a varid for better error messages
role :: { Located (Maybe FastString) }
role : VARID             { sL1 $1 $ Just $ getVARID $1 }
     | '_'               { sL1 $1 Nothing }

-- Pattern synonyms

-- Glasgow extension: pattern synonyms
pattern_synonym_decl :: { LHsDecl RdrName }
        : 'pattern' pattern_synonym_lhs '=' pat
         {%ams ( let (name, args) = $2
                 in sLL $1 $> . ValD $ mkPatSynBind name args $4
                                                    ImplicitBidirectional)
               [mj AnnPattern $1,mj AnnEqual $3]
         }
        | 'pattern' pattern_synonym_lhs '<-' pat
         {%ams (let (name, args) = $2
                in sLL $1 $> . ValD $ mkPatSynBind name args $4 Unidirectional)
               [mj AnnPattern $1,mj AnnLarrow $3] }
        | 'pattern' pattern_synonym_lhs '<-' pat where_decls
            {% do { let (name, args) = $2
                  ; mg <- mkPatSynMatchGroup name (snd $ unLoc $5)
                  ; ams (sLL $1 $> . ValD $
                           mkPatSynBind name args $4 (ExplicitBidirectional mg))
                        (mj AnnPattern $1:mj AnnLarrow $3:(fst $ unLoc $5))
                   }}

pattern_synonym_lhs :: { (Located RdrName, HsPatSynDetails (Located RdrName)) }
        : con vars0 { ($1, PrefixPatSyn $2) }
        | varid consym varid { ($2, InfixPatSyn $1 $3) }

vars0 :: { [Located RdrName] }
        : {- empty -}                 { [] }
        | varid vars0                 { $1 : $2 }

where_decls :: { Located ([AddAnn]
                         , Located (OrdList (LHsDecl RdrName))) }
        : 'where' '{' decls '}'       { sLL $1 $> ((mj AnnWhere $1:moc $2
                                           :mcc $4:(fst $ unLoc $3)),sL1 $3 (snd $ unLoc $3)) }
        | 'where' vocurly decls close { L (comb2 $1 $3) ((mj AnnWhere $1:(fst $ unLoc $3))
                                          ,sL1 $3 (snd $ unLoc $3)) }
pattern_synonym_sig :: { LSig RdrName }
        : 'pattern' con '::' ptype
            {% do { let (flag, qtvs, prov, req, ty) = snd $ unLoc $4
                  ; let sig = PatSynSig $2 (flag, mkHsQTvs qtvs) prov req ty
                  ; checkValidPatSynSig sig
                  ; ams (sLL $1 $> $ sig)
                        (mj AnnPattern $1:mj AnnDcolon $3:(fst $ unLoc $4)) } }

ptype :: { Located ([AddAnn]
                  ,( HsExplicitFlag, [LHsTyVarBndr RdrName], LHsContext RdrName
                   , LHsContext RdrName, LHsType RdrName)) }
        : 'forall' tv_bndrs '.' ptype
            {% do { hintExplicitForall (getLoc $1)
                  ; let (_, qtvs', prov, req, ty) = snd $ unLoc $4
                  ; return $ sLL $1 $>
                                ((mj AnnForall $1:mj AnnDot $3:(fst $ unLoc $4))
                                ,(Explicit, $2 ++ qtvs', prov, req ,ty)) }}
        | context '=>' context '=>' type
            { sLL $1 $> ([mj AnnDarrow $2,mj AnnDarrow $4]
                        ,(Implicit, [], $1, $3, $5)) }
        | context '=>' type
            { sLL $1 $> ([mj AnnDarrow $2],(Implicit, [], $1, noLoc [], $3)) }
        | type
            { sL1 $1 ([],(Implicit, [], noLoc [], noLoc [], $1)) }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { Located (OrdList (LHsDecl RdrName)) }
decl_cls  : at_decl_cls                 { sLL $1 $> (unitOL $1) }
          | decl                        { $1 }

          -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtypedoc
                    {% do { (TypeSig l ty _) <- checkValSig $2 $4
                          ; let err = text "in default signature" <> colon <+>
                                      quotes (ppr ty)
                          ; checkNoPartialType err ty
                          ; ams (sLL $1 $> $ unitOL (sLL $1 $> $ SigD (GenericSig l ty)))
                                [mj AnnDefault $1,mj AnnDcolon $3] } }

decls_cls :: { Located ([AddAnn],OrdList (LHsDecl RdrName)) }  -- Reversed
          : decls_cls ';' decl_cls      {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                    , unLoc $3))
                                             else ams (lastOL (snd $ unLoc $1)) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (fst $ unLoc $1
                                                                ,(snd $ unLoc $1) `appOL` unLoc $3)) }
          | decls_cls ';'               {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                                   ,snd $ unLoc $1))
                                             else ams (lastOL (snd $ unLoc $1)) [mj AnnSemi $2]
                                           >> return (sLL $1 $>  (unLoc $1)) }
          | decl_cls                    { sL1 $1 ([],unLoc $1) }
          | {- empty -}                 { noLoc ([],nilOL) }

decllist_cls
        :: { Located ([AddAnn]
                     , OrdList (LHsDecl RdrName)) }      -- Reversed
        : '{'         decls_cls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        |     vocurly decls_cls close   { $2 }

-- Class body
--
where_cls :: { Located ([AddAnn]
                       ,(OrdList (LHsDecl RdrName))) }    -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_cls          { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl RdrName)) }
decl_inst  : at_decl_inst               { sLL $1 $> (unitOL (sL1 $1 (InstD (unLoc $1)))) }
           | decl                       { $1 }

decls_inst :: { Located ([AddAnn],OrdList (LHsDecl RdrName)) }   -- Reversed
           : decls_inst ';' decl_inst   {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                    , unLoc $3))
                                             else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return
                                            (sLL $1 $> (fst $ unLoc $1
                                                       ,(snd $ unLoc $1) `appOL` unLoc $3)) }
           | decls_inst ';'             {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                                   ,snd $ unLoc $1))
                                             else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (unLoc $1)) }
           | decl_inst                  { sL1 $1 ([],unLoc $1) }
           | {- empty -}                { noLoc ([],nilOL) }

decllist_inst
        :: { Located ([AddAnn]
                     , OrdList (LHsDecl RdrName)) }      -- Reversed
        : '{'         decls_inst '}'    { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2),snd $ unLoc $2) }
        |     vocurly decls_inst close  { L (gl $2) (unLoc $2) }

-- Instance body
--
where_inst :: { Located ([AddAnn]
                        , OrdList (LHsDecl RdrName)) }   -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_inst         { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,(snd $ unLoc $2)) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in binding groups other than classes and instances
--
decls   :: { Located ([AddAnn],OrdList (LHsDecl RdrName)) }
        : decls ';' decl    {% if isNilOL (snd $ unLoc $1)
                                 then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                        , unLoc $3))
                                 else do ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (
                                          let { this = unLoc $3;
                                                rest = snd $ unLoc $1;
                                                these = rest `appOL` this }
                                          in rest `seq` this `seq` these `seq`
                                             (sLL $1 $> (fst $ unLoc $1,these))) }
        | decls ';'          {% if isNilOL (snd $ unLoc $1)
                                  then return (sLL $1 $> ((mj AnnSemi $2:(fst $ unLoc $1)
                                                          ,snd $ unLoc $1)))
                                  else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (unLoc $1)) }
        | decl                          { sL1 $1 ([],unLoc $1) }
        | {- empty -}                   { noLoc ([],nilOL) }

decllist :: { Located ([AddAnn],OrdList (LHsDecl RdrName)) }
        : '{'            decls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                                   ,snd $ unLoc $2) }
        |     vocurly    decls close   { L (gl $2) (fst $ unLoc $2,snd $ unLoc $2) }

-- Binding groups other than those of class and instance declarations
--
binds   ::  { Located ([AddAnn],HsLocalBinds RdrName) }
                                         -- May have implicit parameters
                                                -- No type declarations
        : decllist          {% do { val_binds <- cvBindGroup (snd $ unLoc $1)
                                  ; return (sL1 $1 (fst $ unLoc $1
                                                    ,HsValBinds val_binds)) } }

        | '{'            dbinds '}'     { sLL $1 $> ([moc $1,mcc $3]
                                             ,HsIPBinds (IPBinds (unLoc $2)
                                                         emptyTcEvBinds)) }

        |     vocurly    dbinds close   { L (getLoc $2) ([]
                                            ,HsIPBinds (IPBinds (unLoc $2)
                                                        emptyTcEvBinds)) }


wherebinds :: { Located ([AddAnn],HsLocalBinds RdrName) }
                                                -- May have implicit parameters
                                                -- No type declarations
        : 'where' binds                 { sLL $1 $> (mj AnnWhere $1 : (fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],emptyLocalBinds) }


-----------------------------------------------------------------------------
-- Transformation Rules

rules   :: { OrdList (LRuleDecl RdrName) }
        :  rules ';' rule              {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `snocOL` $3) }
        |  rules ';'                   {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        |  rule                        { unitOL $1 }
        |  {- empty -}                 { nilOL }

rule    :: { LRuleDecl RdrName }
        : STRING rule_activation rule_forall infixexp '=' exp
         {%ams (sLL $1 $> $ (HsRule (L (gl $1) (getSTRING $1))
                                  ((snd $2) `orElse` AlwaysActive)
                                  (snd $3) $4 placeHolderNames $6
                                  placeHolderNames))
               (mj AnnEqual $5 : (fst $2) ++ (fst $3)) }

-- Rules can be specified to be NeverActive, unlike inline/specialize pragmas
rule_activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | rule_explicit_activation              { (fst $1,Just (snd $1)) }

rule_explicit_activation :: { ([AddAnn]
                              ,Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mos $1,mj AnnVal $2,mcs $3]
                                  ,ActiveAfter  (fromInteger (getINTEGER $2))) }
        | '[' '~' INTEGER ']'   { ([mos $1,mj AnnTilde $2,mj AnnVal $3,mcs $4]
                                  ,ActiveBefore (fromInteger (getINTEGER $3))) }
        | '[' '~' ']'           { ([mos $1,mj AnnTilde $2,mcs $3]
                                  ,NeverActive) }

rule_forall :: { ([AddAnn],[LRuleBndr RdrName]) }
        : 'forall' rule_var_list '.'     { ([mj AnnForall $1,mj AnnDot $3],$2) }
        | {- empty -}                    { ([],[]) }

rule_var_list :: { [LRuleBndr RdrName] }
        : rule_var                              { [$1] }
        | rule_var rule_var_list                { $1 : $2 }

rule_var :: { LRuleBndr RdrName }
        : varid                         { sLL $1 $> (RuleBndr $1) }
        | '(' varid '::' ctype ')'      {% ams (sLL $1 $> (RuleBndrSig $2
                                                       (mkHsWithBndrs $4)))
                                               [mop $1,mj AnnDcolon $3,mcp $5] }

-----------------------------------------------------------------------------
-- Warnings and deprecations (c.f. rules)

warnings :: { OrdList (LWarnDecl RdrName) }
        : warnings ';' warning         {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | warnings ';'                 {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | warning                      { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
warning :: { OrdList (LWarnDecl RdrName) }
        : namelist strings
                {% amsu (sLL $1 $> (Warning (unLoc $1) (WarningTxt (noLoc "") $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

deprecations :: { OrdList (LWarnDecl RdrName) }
        : deprecations ';' deprecation
                                       {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | deprecations ';'             {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | deprecation                  { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LWarnDecl RdrName) }
        : namelist strings
             {% amsu (sLL $1 $> $ (Warning (unLoc $1) (DeprecatedTxt (noLoc "") $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

strings :: { Located ([AddAnn],[Located FastString]) }
    : STRING { sL1 $1 ([],[L (gl $1) (getSTRING $1)]) }
    | '[' stringlist ']' { sLL $1 $> $ ([mos $1,mcs $3],fromOL (unLoc $2)) }

stringlist :: { Located (OrdList (Located FastString)) }
    : stringlist ',' STRING {% addAnnotation (oll $ unLoc $1) AnnComma (gl $2) >>
                               return (sLL $1 $> (unLoc $1 `snocOL`
                                                  (L (gl $3) (getSTRING $3)))) }
    | STRING                { sLL $1 $> (unitOL (L (gl $1) (getSTRING $1))) }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl RdrName }
    : '{-# ANN' name_var aexp '#-}'      {% ams (sLL $1 $> (AnnD $ HsAnnotation
                                            (getANN_PRAGs $1)
                                            (ValueAnnProvenance $2) $3))
                                            [mo $1,mc $4] }

    | '{-# ANN' 'type' tycon aexp '#-}'  {% ams (sLL $1 $> (AnnD $ HsAnnotation
                                            (getANN_PRAGs $1)
                                            (TypeAnnProvenance $3) $4))
                                            [mo $1,mj AnnType $2,mc $5] }

    | '{-# ANN' 'module' aexp '#-}'      {% ams (sLL $1 $> (AnnD $ HsAnnotation
                                                (getANN_PRAGs $1)
                                                 ModuleAnnProvenance $3))
                                                [mo $1,mj AnnModule $2,mc $4] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { Located ([AddAnn],HsDecl RdrName) }
fdecl : 'import' callconv safety fspec
               {% mkImport $2 $3 (snd $ unLoc $4) >>= \i ->
                 return (sLL $1 $> (mj AnnImport $1 : (fst $ unLoc $4),i))  }
      | 'import' callconv        fspec
               {% do { d <- mkImport $2 (noLoc PlaySafe) (snd $ unLoc $3);
                    return (sLL $1 $> (mj AnnImport $1 : (fst $ unLoc $3),d)) }}
      | 'export' callconv fspec
               {% mkExport $2 (snd $ unLoc $3) >>= \i ->
                  return (sLL $1 $> (mj AnnExport $1 : (fst $ unLoc $3),i) ) }

callconv :: { Located CCallConv }
          : 'stdcall'                   { sLL $1 $> StdCallConv }
          | 'ccall'                     { sLL $1 $> CCallConv   }
          | 'capi'                      { sLL $1 $> CApiConv    }
          | 'prim'                      { sLL $1 $> PrimCallConv}
          | 'javascript'                { sLL $1 $> JavaScriptCallConv }

safety :: { Located Safety }
        : 'unsafe'                      { sLL $1 $> PlayRisky }
        | 'safe'                        { sLL $1 $> PlaySafe }
        | 'interruptible'               { sLL $1 $> PlayInterruptible }

fspec :: { Located ([AddAnn]
                    ,(Located FastString, Located RdrName, LHsType RdrName)) }
       : STRING var '::' sigtypedoc     { sLL $1 $> ([mj AnnDcolon $3]
                                             ,(L (getLoc $1)
                                                    (getSTRING $1), $2, $4)) }
       |        var '::' sigtypedoc     { sLL $1 $> ([mj AnnDcolon $2]
                                             ,(noLoc nilFS, $1, $3)) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { ([AddAnn],Maybe (LHsType RdrName)) }
        : {- empty -}                   { ([],Nothing) }
        | '::' sigtype                  { ([mj AnnDcolon $1],Just $2) }

opt_asig :: { ([AddAnn],Maybe (LHsType RdrName)) }
        : {- empty -}                   { ([],Nothing) }
        | '::' atype                    { ([mj AnnDcolon $1],Just $2) }

sigtype :: { LHsType RdrName }          -- Always a HsForAllTy,
                                        -- to tell the renamer where to generalise
        : ctype                         { sL1 $1 (mkImplicitHsForAllTy (noLoc []) $1) }
        -- Wrap an Implicit forall if there isn't one there already

sigtypedoc :: { LHsType RdrName }       -- Always a HsForAllTy
        : ctypedoc                      { sL1 $1 (mkImplicitHsForAllTy (noLoc []) $1) }
        -- Wrap an Implicit forall if there isn't one there already

sig_vars :: { Located [Located RdrName] }    -- Returned in reversed order
         : sig_vars ',' var           {% addAnnotation (gl $ head $ unLoc $1)
                                                       AnnComma (gl $2)
                                         >> return (sLL $1 $> ($3 : unLoc $1)) }
         | var                        { sL1 $1 [$1] }

sigtypes1 :: { (OrdList (LHsType RdrName)) }      -- Always HsForAllTys
        : sigtype                      { unitOL $1 }
        | sigtype ',' sigtypes1        {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ((unitOL $1) `appOL` $3) }

-----------------------------------------------------------------------------
-- Types

strict_mark :: { Located ([AddAnn],HsBang) }
        : '!'                        { sL1 $1 ([mj AnnBang $1]
                                              ,HsSrcBang Nothing                       Nothing      True) }
        | '{-# UNPACK' '#-}'         { sLL $1 $> ([mo $1,mc $2]
                                              ,HsSrcBang (Just $ getUNPACK_PRAGs $1)   (Just True)  False) }
        | '{-# NOUNPACK' '#-}'       { sLL $1 $> ([mo $1,mc $2]
                                              ,HsSrcBang (Just $ getNOUNPACK_PRAGs $1) (Just False) False) }
        | '{-# UNPACK' '#-}' '!'     { sLL $1 $> ([mo $1,mc $2,mj AnnBang $3]
                                              ,HsSrcBang (Just $ getUNPACK_PRAGs $1)   (Just True)  True) }
        | '{-# NOUNPACK' '#-}' '!'   { sLL $1 $> ([mo $1,mc $2,mj AnnBang $3]
                                              ,HsSrcBang (Just $ getNOUNPACK_PRAGs $1) (Just False) True) }
        -- Although UNPACK with no '!' is illegal, we get a
        -- better error message if we parse it here

-- A ctype is a for-all type
ctype   :: { LHsType RdrName }
        : 'forall' tv_bndrs '.' ctype   {% hintExplicitForall (getLoc $1) >>
                                           ams (sLL $1 $> $ mkExplicitHsForAllTy $2
                                                                 (noLoc []) $4)
                                               [mj AnnForall $1,mj AnnDot $3] }
        | context '=>' ctype          {% addAnnotation (gl $1) AnnDarrow (gl $2)
                                         >> return (sLL $1 $> $
                                               mkQualifiedHsForAllTy $1 $3) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy (unLoc $1) $3))
                                             [mj AnnVal $1,mj AnnDcolon $2] }
        | type                        { $1 }

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
        : 'forall' tv_bndrs '.' ctypedoc {% hintExplicitForall (getLoc $1) >>
                                            ams (sLL $1 $> $ mkExplicitHsForAllTy $2
                                                                  (noLoc []) $4)
                                                [mj AnnForall $1,mj AnnDot $3] }
        | context '=>' ctypedoc       {% addAnnotation (gl $1) AnnDarrow (gl $2)
                                         >> return (sLL $1 $> $
                                                  mkQualifiedHsForAllTy $1 $3) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy (unLoc $1) $3))
                                             [mj AnnDcolon $2] }
        | typedoc                     { $1 }

----------------------
-- Notes for 'context'
-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--      (Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>

-- We have the t1 ~ t2 form both in 'context' and in type,
-- to permit an individual equational constraint without parenthesis.
-- Thus for some reason we allow    f :: a~b => blah
-- but not                          f :: ?x::Int => blah
context :: { LHsContext RdrName }
        : btype '~'      btype          {% amms (checkContext
                                             (sLL $1 $> $ HsEqTy $1 $3))
                                             [mj AnnTilde $2] }
        | btype                         {% do { ctx <- checkContext $1
                                              ; if null (unLoc ctx)
                                                 then addAnnotation (gl $1) AnnUnit (gl $1)
                                                 else return ()
                                              ; return ctx
                                              } }

type :: { LHsType RdrName }
        : btype                         { $1 }
        | btype qtyconop type           { sLL $1 $> $ mkHsOpTy $1 $2 $3 }
        | btype tyvarop  type           { sLL $1 $> $ mkHsOpTy $1 $2 $3 }
        | btype '->'     ctype          {% ams $1 [mj AnnRarrow $2]
                                        >> ams (sLL $1 $> $ HsFunTy $1 $3)
                                               [mj AnnRarrow $2] }
        | btype '~'      btype          {% ams (sLL $1 $> $ HsEqTy $1 $3)
                                               [mj AnnTilde $2] }
                                        -- see Note [Promotion]
        | btype SIMPLEQUOTE qconop type  {% ams (sLL $1 $> $ mkHsOpTy $1 $3 $4)
                                                [mj AnnSimpleQuote $2] }
        | btype SIMPLEQUOTE varop  type  {% ams (sLL $1 $> $ mkHsOpTy $1 $3 $4)
                                                [mj AnnSimpleQuote $2] }

typedoc :: { LHsType RdrName }
        : btype                          { $1 }
        | btype docprev                  { sLL $1 $> $ HsDocTy $1 $2 }
        | btype qtyconop type            { sLL $1 $> $ mkHsOpTy $1 $2 $3 }
        | btype qtyconop type docprev    { sLL $1 $> $ HsDocTy (L (comb3 $1 $2 $3) (mkHsOpTy $1 $2 $3)) $4 }
        | btype tyvarop  type            { sLL $1 $> $ mkHsOpTy $1 $2 $3 }
        | btype tyvarop  type docprev    { sLL $1 $> $ HsDocTy (L (comb3 $1 $2 $3) (mkHsOpTy $1 $2 $3)) $4 }
        | btype '->'     ctypedoc        {% ams (sLL $1 $> $ HsFunTy $1 $3)
                                                [mj AnnRarrow $2] }
        | btype docprev '->' ctypedoc    {% ams (sLL $1 $> $ HsFunTy (L (comb2 $1 $2)
                                                            (HsDocTy $1 $2)) $4)
                                                [mj AnnRarrow $3] }
        | btype '~'      btype           {% ams (sLL $1 $> $ HsEqTy $1 $3)
                                                [mj AnnTilde $2] }
                                        -- see Note [Promotion]
        | btype SIMPLEQUOTE qconop type  {% ams (sLL $1 $> $ mkHsOpTy $1 $3 $4)
                                                [mj AnnSimpleQuote $2] }
        | btype SIMPLEQUOTE varop  type  {% ams (sLL $1 $> $ mkHsOpTy $1 $3 $4)
                                                [mj AnnSimpleQuote $2] }

btype :: { LHsType RdrName }
        : btype atype                   { sLL $1 $> $ HsAppTy $1 $2 }
        | atype                         { $1 }

atype :: { LHsType RdrName }
        : ntgtycon                       { sL1 $1 (HsTyVar (unLoc $1)) }      -- Not including unit tuples
        | tyvar                          {% do { nwc <- namedWildcardsEnabled -- (See Note [Unit tuples])
                                               ; let tv@(Unqual name) = unLoc $1
                                               ; return $ if (startsWithUnderscore name && nwc)
                                                          then (sL1 $1 (HsNamedWildcardTy tv))
                                                          else (sL1 $1 (HsTyVar tv)) } }

        | strict_mark atype              {% ams (sLL $1 $> (HsBangTy (snd $ unLoc $1) $2))
                                                (fst $ unLoc $1) }  -- Constructor sigs only
        | '{' fielddecls '}'             {% amms (checkRecordSyntax
                                                    (sLL $1 $> $ HsRecTy $2))
                                                        -- Constructor sigs only
                                                 [moc $1,mcc $3] }
        | '(' ')'                        {% ams (sLL $1 $> $ HsTupleTy
                                                    HsBoxedOrConstraintTuple [])
                                                [mop $1,mcp $2] }
        | '(' ctype ',' comma_types1 ')' {% addAnnotation (gl $2) AnnComma
                                                          (gl $3) >>
                                            ams (sLL $1 $> $ HsTupleTy
                                             HsBoxedOrConstraintTuple ($2 : $4))
                                                [mop $1,mcp $5] }
        | '(#' '#)'                   {% ams (sLL $1 $> $ HsTupleTy HsUnboxedTuple [])
                                             [mo $1,mc $2] }
        | '(#' comma_types1 '#)'      {% ams (sLL $1 $> $ HsTupleTy HsUnboxedTuple $2)
                                             [mo $1,mc $3] }
        | '[' ctype ']'               {% ams (sLL $1 $> $ HsListTy  $2) [mos $1,mcs $3] }
        | '[:' ctype ':]'             {% ams (sLL $1 $> $ HsPArrTy  $2) [mo $1,mc $3] }
        | '(' ctype ')'               {% ams (sLL $1 $> $ HsParTy   $2) [mop $1,mcp $3] }
        | '(' ctype '::' kind ')'     {% ams (sLL $1 $> $ HsKindSig $2 $4)
                                             [mop $1,mj AnnDcolon $3,mcp $5] }
        | quasiquote                  { sL1 $1 (HsQuasiQuoteTy (unLoc $1)) }
        | '$(' exp ')'                {% ams (sLL $1 $> $ mkHsSpliceTy $2)
                                             [mo $1,mc $3] }
        | TH_ID_SPLICE                { sLL $1 $> $ mkHsSpliceTy $ sL1 $1 $ HsVar $
                                        mkUnqual varName (getTH_ID_SPLICE $1) }
                                      -- see Note [Promotion] for the followings
        | SIMPLEQUOTE qcon_nowiredlist {% ams (sLL $1 $> $ HsTyVar $ unLoc $2) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  '(' ctype ',' comma_types1 ')'
                             {% addAnnotation (gl $3) AnnComma (gl $4) >>
                                ams (sLL $1 $> $ HsExplicitTupleTy [] ($3 : $5))
                                    [mj AnnSimpleQuote $1,mop $2,mcp $6] }
        | SIMPLEQUOTE  '[' comma_types0 ']'     {% ams (sLL $1 $> $ HsExplicitListTy
                                                            placeHolderKind $3)
                                                       [mj AnnSimpleQuote $1,mos $2,mcs $4] }
        | SIMPLEQUOTE var                       {% ams (sLL $1 $> $ HsTyVar $ unLoc $2)
                                                       [mj AnnSimpleQuote $1,mj AnnName $2] }

        -- Two or more [ty, ty, ty] must be a promoted list type, just as
        -- if you had written '[ty, ty, ty]
        -- (One means a list type, zero means the list type constructor, 
        -- so you have to quote those.)
        | '[' ctype ',' comma_types1 ']'  {% addAnnotation (gl $2) AnnComma
                                                           (gl $3) >>
                                             ams (sLL $1 $> $ HsExplicitListTy
                                                     placeHolderKind ($2 : $4))
                                                 [mos $1,mcs $5] }
        | INTEGER              { sLL $1 $> $ HsTyLit $ HsNumTy (getINTEGERs $1)
                                                               (getINTEGER $1) }
        | STRING               { sLL $1 $> $ HsTyLit $ HsStrTy (getSTRINGs $1)
                                                               (getSTRING  $1) }
        | '_'                  { sL1 $1 $ HsWildcardTy }

-- An inst_type is what occurs in the head of an instance decl
--      e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type, with a MonoDictTy at the right
-- hand corner, for convenience.
inst_type :: { LHsType RdrName }
        : sigtype                       { $1 }

inst_types1 :: { [LHsType RdrName] }
        : inst_type                     { [$1] }

        | inst_type ',' inst_types1    {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

comma_types0  :: { [LHsType RdrName] }  -- Zero or more:  ty,ty,ty
        : comma_types1                  { $1 }
        | {- empty -}                   { [] }

comma_types1    :: { [LHsType RdrName] }  -- One or more:  ty,ty,ty
        : ctype                        { [$1] }
        | ctype  ',' comma_types1      {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

tv_bndrs :: { [LHsTyVarBndr RdrName] }
         : tv_bndr tv_bndrs             { $1 : $2 }
         | {- empty -}                  { [] }

tv_bndr :: { LHsTyVarBndr RdrName }
        : tyvar                         { sL1 $1 (UserTyVar (unLoc $1)) }
        | '(' tyvar '::' kind ')'       {% ams (sLL $1 $>  (KindedTyVar $2 $4))
                                               [mop $1,mj AnnDcolon $3
                                               ,mcp $5] }

fds :: { Located ([AddAnn],[Located (FunDep (Located RdrName))]) }
        : {- empty -}                   { noLoc ([],[]) }
        | '|' fds1                      { (sLL $1 $> ([mj AnnVbar $1]
                                                 ,reverse (unLoc $2))) }

fds1 :: { Located [Located (FunDep (Located RdrName))] }
        : fds1 ',' fd   {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2)
                           >> return (sLL $1 $> ($3 : unLoc $1)) }
        | fd            { sL1 $1 [$1] }

fd :: { Located (FunDep (Located RdrName)) }
        : varids0 '->' varids0  {% ams (L (comb3 $1 $2 $3)
                                       (reverse (unLoc $1), reverse (unLoc $3)))
                                       [mj AnnRarrow $2] }

varids0 :: { Located [Located RdrName] }
        : {- empty -}                   { noLoc [] }
        | varids0 tyvar                 { sLL $1 $> ($2 : unLoc $1) }

-----------------------------------------------------------------------------
-- Kinds

kind :: { LHsKind RdrName }
        : bkind                  { $1 }
        | bkind '->' kind        {% ams (sLL $1 $> $ HsFunTy $1 $3)
                                        [mj AnnRarrow $2] }

bkind :: { LHsKind RdrName }
        : akind                  { $1 }
        | bkind akind            { sLL $1 $> $ HsAppTy $1 $2 }

akind :: { LHsKind RdrName }
        : '*'                    { sL1 $1 $ HsTyVar (nameRdrName liftedTypeKindTyConName) }
        | '(' kind ')'           {% ams (sLL $1 $>  $ HsParTy $2)
                                        [mop $1,mcp $3] }
        | pkind                  { $1 }
        | tyvar                  { sL1 $1 $ HsTyVar (unLoc $1) }

pkind :: { LHsKind RdrName }  -- promoted type, see Note [Promotion]
        : qtycon                          { sL1 $1 $ HsTyVar $ unLoc $1 }
        | '(' ')'                   {% ams (sLL $1 $> $ HsTyVar $ getRdrName unitTyCon)
                                           [mop $1,mcp $2] }
        | '(' kind ',' comma_kinds1 ')'
                          {% addAnnotation (gl $2) AnnComma (gl $3) >>
                             ams (sLL $1 $> $ HsTupleTy HsBoxedTuple ( $2 : $4))
                                 [mop $1,mcp $5] }
        | '[' kind ']'                    {% ams (sLL $1 $> $ HsListTy $2)
                                                 [mos $1,mcs $3] }

comma_kinds1 :: { [LHsKind RdrName] }
        : kind                         { [$1] }
        | kind  ',' comma_kinds1       {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

{- Note [Promotion]
   ~~~~~~~~~~~~~~~~

- Syntax of promoted qualified names
We write 'Nat.Zero instead of Nat.'Zero when dealing with qualified
names. Moreover ticks are only allowed in types, not in kinds, for a
few reasons:
  1. we don't need quotes since we cannot define names in kinds
  2. if one day we merge types and kinds, tick would mean look in DataName
  3. we don't have a kind namespace anyway

- Syntax of explicit kind polymorphism  (IA0_TODO: not yet implemented)
Kind abstraction is implicit. We write
> data SList (s :: k -> *) (as :: [k]) where ...
because it looks like what we do in terms
> id (x :: a) = x

- Name resolution
When the user write Zero instead of 'Zero in types, we parse it a
HsTyVar ("Zero", TcClsName) instead of HsTyVar ("Zero", DataName). We
deal with this in the renamer. If a HsTyVar ("Zero", TcClsName) is not
bounded in the type level, then we look for it in the term level (we
change its namespace to DataName, see Note [Demotion] in OccName). And
both become a HsTyVar ("Zero", DataName) after the renamer.

-}


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located ([AddAnn]
                          ,[LConDecl RdrName]) } -- Returned in order
        : 'where' '{'        gadt_constrs '}'   { L (comb2 $1 $3)
                                                    ([mj AnnWhere $1
                                                     ,moc $2
                                                     ,mcc $4]
                                                    , unLoc $3) }
        | 'where' vocurly    gadt_constrs close  { L (comb2 $1 $3)
                                                     ([mj AnnWhere $1]
                                                     , unLoc $3) }
        | {- empty -}                            { noLoc ([],[]) }

gadt_constrs :: { Located [LConDecl RdrName] }
        : gadt_constr ';' gadt_constrs
                  {% addAnnotation (gl $1) AnnSemi (gl $2)
                     >> return (L (comb2 $1 $3) ($1 : unLoc $3)) }
        | gadt_constr                   { L (gl $1) [$1] }
        | {- empty -}                   { noLoc [] }

-- We allow the following forms:
--      C :: Eq a => a -> T a
--      C :: forall a. Eq a => !a -> T a
--      D { x,y :: a } :: T a
--      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { LConDecl RdrName }
                   -- Returns a list because of:   C,D :: ty
        : con_list '::' sigtype
                {% do { gadtDecl <- mkGadtDecl (unLoc $1) $3
                      ; ams (sLL $1 $> $ gadtDecl)
                            [mj AnnDcolon $2] } }

                -- Deprecated syntax for GADT record declarations
        | oqtycon '{' fielddecls '}' '::' sigtype
                {% do { cd <- mkDeprecatedGadtRecordDecl (comb2 $1 $6) $1 (noLoc $3) $6
                      ; cd' <- checkRecordSyntax cd
                      ; ams (L (comb2 $1 $6) (unLoc cd'))
                            [moc $2,mcc $4,mj AnnDcolon $5] } }

constrs :: { Located ([AddAnn],[LConDecl RdrName]) }
        : maybe_docnext '=' constrs1    { L (comb2 $2 $3) ([mj AnnEqual $2]
                                                     ,addConDocs (unLoc $3) $1)}

constrs1 :: { Located [LConDecl RdrName] }
        : constrs1 maybe_docnext '|' maybe_docprev constr
            {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $3)
               >> return (sLL $1 $> (addConDoc $5 $2 : addConDocFirst (unLoc $1) $4)) }
        | constr                                          { sL1 $1 [$1] }

constr :: { LConDecl RdrName }
        : maybe_docnext forall context '=>' constr_stuff maybe_docprev
                {% ams (let (con,details) = unLoc $5 in
                  addConDoc (L (comb4 $2 $3 $4 $5) (mkSimpleConDecl con
                                                   (snd $ unLoc $2) $3 details))
                            ($1 `mplus` $6))
                        (mj AnnDarrow $4:(fst $ unLoc $2)) }
        | maybe_docnext forall constr_stuff maybe_docprev
                {% ams ( let (con,details) = unLoc $3 in
                  addConDoc (L (comb2 $2 $3) (mkSimpleConDecl con
                                           (snd $ unLoc $2) (noLoc []) details))
                            ($1 `mplus` $4))
                       (fst $ unLoc $2) }

forall :: { Located ([AddAnn],[LHsTyVarBndr RdrName]) }
        : 'forall' tv_bndrs '.'       { sLL $1 $> ([mj AnnForall $1,mj AnnDot $3],$2) }
        | {- empty -}                 { noLoc ([],[]) }

constr_stuff :: { Located (Located RdrName, HsConDeclDetails RdrName) }
-- We parse the constructor declaration
--      C t1 t2
-- as a btype (treating C as a type constructor) and then convert C to be
-- a data constructor.  Reason: it might continue like this:
--      C t1 t2 %: D Int
-- in which case C really would be a type constructor.  We can't resolve this
-- ambiguity till we come across the constructor oprerator :% (or not, more usually)
        : btype                         {% splitCon $1 >>= return.sLL $1 $> }
        | btype conop btype             {  sLL $1 $> ($2, InfixCon $1 $3) }

fielddecls :: { [LConDeclField RdrName] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [LConDeclField RdrName] }
        : fielddecl maybe_docnext ',' maybe_docprev fielddecls1
            {% addAnnotation (gl $1) AnnComma (gl $3) >>
               return ((addFieldDoc $1 $4) : addFieldDocs $5 $2) }
        | fielddecl   { [$1] }

fielddecl :: { LConDeclField RdrName }
                                              -- A list because of   f,g :: Int
        : maybe_docnext sig_vars '::' ctype maybe_docprev
            {% ams (L (comb2 $2 $4)
                      (ConDeclField (reverse (unLoc $2)) $4 ($1 `mplus` $5)))
                   [mj AnnDcolon $3] }

-- We allow the odd-looking 'inst_type' in a deriving clause, so that
-- we can do deriving( forall a. C [a] ) in a newtype (GHC extension).
-- The 'C [a]' part is converted to an HsPredTy by checkInstType
-- We don't allow a context, but that's sorted out by the type checker.
deriving :: { Located (Maybe (Located [LHsType RdrName])) }
        : {- empty -}             { noLoc Nothing }
        | 'deriving' qtycon       {% aljs ( let { L loc tv = $2 }
                                            in (sLL $1 $> (Just (sLL $1 $>
                                                       [L loc (HsTyVar tv)]))))
                                          [mj AnnDeriving $1] }
        | 'deriving' '(' ')'      {% aljs (sLL $1 $> (Just (sLL $1 $> [])))
                                          [mj AnnDeriving $1,mop $2,mcp $3] }

        | 'deriving' '(' inst_types1 ')'  {% aljs (sLL $1 $> (Just (sLL $1 $> $3)))
                                                 [mj AnnDeriving $1,mop $2,mcp $4] }
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
        : docdecld { sL1 $1 (DocD (unLoc $1)) }

docdecld :: { LDocDecl }
        : docnext                               { sL1 $1 (DocCommentNext (unLoc $1)) }
        | docprev                               { sL1 $1 (DocCommentPrev (unLoc $1)) }
        | docnamed                              { sL1 $1 (case (unLoc $1) of (n, doc) -> DocCommentNamed n doc) }
        | docsection                            { sL1 $1 (case (unLoc $1) of (n, doc) -> DocGroup n doc) }

decl_no_th :: { Located (OrdList (LHsDecl RdrName)) }
        : sigdecl               { $1 }

        | '!' aexp rhs          {% do { let { e = sLL $1 $> (SectionR (sL1 $1 (HsVar bang_RDR)) $2) };
                                        pat <- checkPattern empty e;
                                        _ <- ams (sLL $1 $> ())
                                               (mj AnnBang $1:(fst $ unLoc $3));
                                        return $ sLL $1 $> $ unitOL $ sLL $1 $> $ ValD $
                                            PatBind pat (snd $ unLoc $3)
                                                    placeHolderType
                                                    placeHolderNames
                                                    ([],[]) } }
                                -- Turn it all into an expression so that
                                -- checkPattern can check that bangs are enabled

        | infixexp opt_sig rhs  {% do { (ann,r) <- checkValDef empty $1 (snd $2) $3;
                                        let { l = comb2 $1 $> };
                                        case r of {
                                          (FunBind n _ _ _ _ _) ->
                                                ams (L l ()) (mj AnnFunId n:(fst $2)) >> return () ;
                                          _ -> return () } ;
                                        _ <- ams (L l ()) (ann ++ (fst $2) ++ (fst $ unLoc $3));
                                        return $! (sL l (unitOL $! (sL l $ ValD r))) } }
        | pattern_synonym_decl  { sLL $1 $> $ unitOL $1 }
        | docdecl               { sLL $1 $> $ unitOL $1 }

decl    :: { Located (OrdList (LHsDecl RdrName)) }
        : decl_no_th            { $1 }

        -- Why do we only allow naked declaration splices in top-level
        -- declarations and not here? Short answer: because readFail009
        -- fails terribly with a panic in cvBindsAndSigs otherwise.
        | splice_exp            { sLL $1 $> $ unitOL (sLL $1 $> $ mkSpliceDecl $1) }

rhs     :: { Located ([AddAnn],GRHSs RdrName (LHsExpr RdrName)) }
        : '=' exp wherebinds    { sL (comb3 $1 $2 $3)
                                    ((mj AnnEqual $1 : (fst $ unLoc $3))
                                    ,GRHSs (unguardedRHS (comb3 $1 $2 $3) $2)
                                   (snd $ unLoc $3)) }
        | gdrhs wherebinds      { sLL $1 $>  (fst $ unLoc $2
                                    ,GRHSs (reverse (unLoc $1))
                                                    (snd $ unLoc $2)) }

gdrhs :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : gdrhs gdrh            { sLL $1 $> ($2 : unLoc $1) }
        | gdrh                  { sL1 $1 [$1] }

gdrh :: { LGRHS RdrName (LHsExpr RdrName) }
        : '|' guardquals '=' exp  {% ams (sL (comb2 $1 $>) $ GRHS (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnEqual $3] }

sigdecl :: { Located (OrdList (LHsDecl RdrName)) }
        :
        -- See Note [Declaration/signature overlap] for why we need infixexp here
          infixexp '::' sigtypedoc
                        {% do ty <- checkPartialTypeSignature $3
                        ; s <- checkValSig $1 ty
                        ; _ <- ams (sLL $1 $> ()) [mj AnnDcolon $2]
                        ; return (sLL $1 $> $ unitOL (sLL $1 $> $ SigD s)) }

        | var ',' sig_vars '::' sigtypedoc
           {% do { ty <- checkPartialTypeSignature $5
                 ; let sig = TypeSig ($1 : reverse (unLoc $3)) ty PlaceHolder
                 ; addAnnotation (gl $1) AnnComma (gl $2)
                 ; ams (sLL $1 $> $ toOL [ sLL $1 $> $ SigD sig ])
                       [mj AnnDcolon $4] } }

        | infix prec ops
              {% ams (sLL $1 $> $ toOL [ sLL $1 $> $ SigD
                        (FixSig (FixitySig (fromOL $ unLoc $3)
                                (Fixity (unLoc $2) (unLoc $1)))) ])
                     [mj AnnInfix $1,mj AnnVal $2] }

        | pattern_synonym_sig   { sLL $1 $> $ unitOL $ sLL $1 $> . SigD . unLoc $ $1 }

        | '{-# INLINE' activation qvar '#-}'
                {% ams (sLL $1 $> $ unitOL (sLL $1 $> $ SigD (InlineSig $3
                            (mkInlinePragma (getINLINE_PRAGs $1) (getINLINE $1)
                                            (snd $2)))))
                       ((mo $1:fst $2) ++ [mc $4]) }

        | '{-# SPECIALISE' activation qvar '::' sigtypes1 '#-}'
             {% ams (
                 let inl_prag = mkInlinePragma (getSPEC_PRAGs $1)
                                             (EmptyInlineSpec, FunLike) (snd $2)
                  in sLL $1 $> $
                            toOL [ sLL $1 $> $ SigD (SpecSig $3 (fromOL $5) inl_prag) ])
                    (mo $1:mj AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
             {% ams (sLL $1 $> $ toOL [ sLL $1 $> $ SigD (SpecSig $3 (fromOL $5)
                               (mkInlinePragma (getSPEC_INLINE_PRAGs $1)
                                               (getSPEC_INLINE $1) (snd $2))) ])
                       (mo $1:mj AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE' 'instance' inst_type '#-}'
                {% ams (sLL $1 $> $ unitOL (sLL $1 $>
                                  $ SigD (SpecInstSig (getSPEC_PRAGs $1) $3)))
                       [mo $1,mj AnnInstance $2,mc $4] }

        -- AZ TODO: Do we need locations in the name_formula_opt?
        -- A minimal complete definition
        | '{-# MINIMAL' name_boolformula_opt '#-}'
            {% ams (sLL $1 $> $ unitOL (sLL $1 $> $ SigD (MinimalSig (getMINIMAL_PRAGs $1) (snd $2))))
                   (mo $1:mc $3:fst $2) }

activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | explicit_activation                   { (fst $1,Just (snd $1)) }

explicit_activation :: { ([AddAnn],Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mj AnnOpenS $1,mj AnnVal $2,mj AnnCloseS $3]
                                  ,ActiveAfter  (fromInteger (getINTEGER $2))) }
        | '[' '~' INTEGER ']'   { ([mj AnnOpenS $1,mj AnnTilde $2,mj AnnVal $3
                                                 ,mj AnnCloseS $4]
                                  ,ActiveBefore (fromInteger (getINTEGER $3))) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsQuasiQuote RdrName) }
        : TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in sL1 $1 (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }
        | TH_QQUASIQUOTE  { let { loc = getLoc $1
                                ; ITqQuasiQuote (qual, quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkQual varName (qual, quoter) }
                            in sL (getLoc $1) (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }

exp   :: { LHsExpr RdrName }
        : infixexp '::' sigtype {% ams (sLL $1 $> $ ExprWithTySig $1 $3 PlaceHolder)
                                       [mj AnnDcolon $2] }
        | infixexp '-<' exp     {% ams (sLL $1 $> $ HsArrApp $1 $3 placeHolderType
                                                        HsFirstOrderApp True)
                                       [mj Annlarrowtail $2] }
        | infixexp '>-' exp     {% ams (sLL $1 $> $ HsArrApp $3 $1 placeHolderType
                                                      HsFirstOrderApp False)
                                       [mj Annrarrowtail $2] }
        | infixexp '-<<' exp    {% ams (sLL $1 $> $ HsArrApp $1 $3 placeHolderType
                                                      HsHigherOrderApp True)
                                       [mj AnnLarrowtail $2] }
        | infixexp '>>-' exp    {% ams (sLL $1 $> $ HsArrApp $3 $1 placeHolderType
                                                      HsHigherOrderApp False)
                                       [mj AnnRarrowtail $2] }
        | infixexp              { $1 }

infixexp :: { LHsExpr RdrName }
        : exp10                   { $1 }
        | infixexp qop exp10      {% ams (sLL $1 $>
                                             (OpApp $1 $2 placeHolderFixity $3))
                                         [mj AnnVal $2] }
                 -- AnnVal annotation for NPlusKPat, which discards the operator


exp10 :: { LHsExpr RdrName }
        : '\\' apat apats opt_asig '->' exp
                   {% ams (sLL $1 $> $ HsLam (mkMatchGroup FromSource
                            [sLL $1 $> $ Match Nothing ($2:$3) (snd $4) (unguardedGRHSs $6)]))
                          (mj AnnLam $1:mj AnnRarrow $5:(fst $4)) }
        | 'let' binds 'in' exp          {% ams (sLL $1 $> $ HsLet (snd $ unLoc $2) $4)
                                               (mj AnnLet $1:mj AnnIn $3
                                                 :(fst $ unLoc $2)) }
        | '\\' 'lcase' altslist
            {% ams (sLL $1 $> $ HsLamCase placeHolderType
                                   (mkMatchGroup FromSource (snd $ unLoc $3)))
                   (mj AnnLam $1:mj AnnCase $2:(fst $ unLoc $3)) }
        | 'if' exp optSemi 'then' exp optSemi 'else' exp
                           {% checkDoAndIfThenElse $2 (snd $3) $5 (snd $6) $8 >>
                              ams (sLL $1 $> $ mkHsIf $2 $5 $8)
                                  (mj AnnIf $1:mj AnnThen $4
                                     :mj AnnElse $7
                                     :(map (\l -> mj AnnSemi l) (fst $3))
                                    ++(map (\l -> mj AnnSemi l) (fst $6))) }
        | 'if' ifgdpats                 {% hintMultiWayIf (getLoc $1) >>
                                           ams (sLL $1 $> $ HsMultiIf
                                                     placeHolderType
                                                     (reverse $ snd $ unLoc $2))
                                               (mj AnnIf $1:(fst $ unLoc $2)) }
        | 'case' exp 'of' altslist      {% ams (sLL $1 $> $ HsCase $2 (mkMatchGroup
                                                   FromSource (snd $ unLoc $4)))
                                               (mj AnnCase $1:mj AnnOf $3
                                                  :(fst $ unLoc $4)) }
        | '-' fexp                      {% ams (sLL $1 $> $ NegApp $2 noSyntaxExpr)
                                               [mj AnnMinus $1] }

        | 'do' stmtlist              {% ams (L (comb2 $1 $2)
                                               (mkHsDo DoExpr (snd $ unLoc $2)))
                                               (mj AnnDo $1:(fst $ unLoc $2)) }
        | 'mdo' stmtlist            {% ams (L (comb2 $1 $2)
                                              (mkHsDo MDoExpr (snd $ unLoc $2)))
                                           (mj AnnMdo $1:(fst $ unLoc $2)) }

        | scc_annot exp        {% ams (sLL $1 $> $ HsSCC (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2)
                                      (fst $ fst $ unLoc $1) }

        | hpc_annot exp        {% ams (sLL $1 $> $ HsTickPragma (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2)
                                      (fst $ fst $ unLoc $1) }

        | 'proc' aexp '->' exp
                       {% checkPattern empty $2 >>= \ p ->
                           checkCommand $4 >>= \ cmd ->
                           ams (sLL $1 $> $ HsProc p (sLL $1 $> $ HsCmdTop cmd placeHolderType
                                                placeHolderType []))
                                            -- TODO: is LL right here?
                               [mj AnnProc $1,mj AnnRarrow $3] }

        | '{-# CORE' STRING '#-}' exp  {% ams (sLL $1 $> $ HsCoreAnn (getCORE_PRAGs $1) (getSTRING $2) $4)
                                              [mo $1,mj AnnVal $2
                                              ,mc $3] }
                                          -- hdaume: core annotation
        | fexp                         { $1 }

        -- parsing error messages go below here
        | '\\' apat apats opt_asig '->' error        {% parseErrorSDoc (combineLocs $1 $5) $ text
                                                        "parse error in lambda: no expression after '->'"
                                                     }
        | '\\' error                                 {% parseErrorSDoc (getLoc $1) $ text
                                                        "parse error: naked lambda expression '\'"
                                                     }
        | 'let' binds 'in' error                     {% parseErrorSDoc (combineLocs $1 $2) $ text
                                                        "parse error in let binding: missing expression after 'in'"
                                                     }
        | 'let' binds error                          {% parseErrorSDoc (combineLocs $1 $2) $ text
                                                        "parse error in let binding: missing required 'in'"
                                                     }
        | 'let' error                                {% parseErrorSDoc (getLoc $1) $ text
                                                        "parse error: naked let binding"
                                                     }
        | 'if' exp optSemi 'then' exp optSemi
          'else' error                               {% hintIf (combineLocs $1 $5) "else clause empty" }
        | 'if' exp optSemi 'then' exp optSemi error  {% hintIf (combineLocs $1 $5) "missing required else clause" }
        | 'if' exp optSemi 'then' error              {% hintIf (combineLocs $1 $2) "then clause empty" }
        | 'if' exp optSemi error                     {% hintIf (combineLocs $1 $2) "missing required then and else clauses" }
        | 'if' error                                 {% hintIf (getLoc $1) "naked if statement" }
        | 'case' exp 'of' error                      {% parseErrorSDoc (combineLocs $1 $2) $ text
                                                        "parse error in case statement: missing list after '->'"
                                                     }
        | 'case' exp error                           {% parseErrorSDoc (combineLocs $1 $2) $ text
                                                        "parse error in case statement: missing required 'of'"
                                                     }
        | 'case' error                               {% parseErrorSDoc (getLoc $1) $ text
                                                        "parse error: naked case statement"
                                                     }
optSemi :: { ([Located a],Bool) }
        : ';'         { ([$1],True) }
        | {- empty -} { ([],False) }

scc_annot :: { Located (([AddAnn],SourceText),FastString) }
        : '{-# SCC' STRING '#-}'      {% do scc <- getSCC $2
                                            ; return $ sLL $1 $>
                                               (([mo $1,mj AnnValStr $2
                                                ,mc $3],getSCC_PRAGs $1),scc) }
        | '{-# SCC' VARID  '#-}'      { sLL $1 $> (([mo $1,mj AnnVal $2
                                         ,mc $3],getSCC_PRAGs $1)
                                        ,(getVARID $2)) }

hpc_annot :: { Located (([AddAnn],SourceText),(FastString,(Int,Int),(Int,Int))) }
      : '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
                                      { sLL $1 $> $ (([mo $1,mj AnnVal $2
                                              ,mj AnnVal $3,mj AnnColon $4
                                              ,mj AnnVal $5,mj AnnMinus $6
                                              ,mj AnnVal $7,mj AnnColon $8
                                              ,mj AnnVal $9,mc $10],
                                                getGENERATED_PRAGs $1)
                                              ,(getSTRING $2
                                               ,( fromInteger $ getINTEGER $3
                                                , fromInteger $ getINTEGER $5
                                                )
                                               ,( fromInteger $ getINTEGER $7
                                                , fromInteger $ getINTEGER $9
                                                )
                                               ))
                                         }

fexp    :: { LHsExpr RdrName }
        : fexp aexp                             { sLL $1 $> $ HsApp $1 $2 }
        | 'static' aexp                         {% ams (sLL $1 $> $ HsStatic $2)
                                                       [mj AnnStatic $1] }
        | aexp                                  { $1 }

aexp    :: { LHsExpr RdrName }
        : qvar '@' aexp         {% ams (sLL $1 $> $ EAsPat $1 $3) [mj AnnAt $2] }
        | '~' aexp              {% ams (sLL $1 $> $ ELazyPat $2) [mj AnnTilde $1] }
        | aexp1                 { $1 }

aexp1   :: { LHsExpr RdrName }
        : aexp1 '{' fbinds '}' {% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4)
                                                                   (snd $3)
                                     ; _ <- ams (sLL $1 $> ()) (moc $2:mcc $4:(fst $3))
                                     ; checkRecordSyntax (sLL $1 $> r) }}
        | aexp2                { $1 }

aexp2   :: { LHsExpr RdrName }
        : ipvar                         { sL1 $1 (HsIPVar $! unLoc $1) }
        | qcname                        { sL1 $1 (HsVar   $! unLoc $1) }
        | literal                       { sL1 $1 (HsLit   $! unLoc $1) }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--      | STRING    { sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRINGs $1)
--                                       (getSTRING $1) placeHolderType) }
        | INTEGER   { sL (getLoc $1) (HsOverLit $! mkHsIntegral (getINTEGERs $1)
                                         (getINTEGER $1) placeHolderType) }
        | RATIONAL  { sL (getLoc $1) (HsOverLit $! mkHsFractional
                                          (getRATIONAL $1) placeHolderType) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
        | '(' texp ')'                  {% ams (sLL $1 $> (HsPar $2)) [mop $1,mcp $3] }
        | '(' tup_exprs ')'             {% ams (sLL $1 $> (ExplicitTuple $2 Boxed))
                                               [mop $1,mcp $3] }

        | '(#' texp '#)'                {% ams (sLL $1 $> (ExplicitTuple [L (gl $2)
                                                         (Present $2)] Unboxed))
                                               [mo $1,mc $3] }
        | '(#' tup_exprs '#)'           {% ams (sLL $1 $> (ExplicitTuple $2 Unboxed))
                                               [mo $1,mc $3] }

        | '[' list ']'      {% ams (sLL $1 $> (snd $2)) (mos $1:mcs $3:(fst $2)) }
        | '[:' parr ':]'    {% ams (sLL $1 $> (snd $2)) (mo $1:mc $3:(fst $2)) }
        | '_'               { sL1 $1 EWildPat }

        -- Template Haskell Extension
        | splice_exp            { $1 }

        | SIMPLEQUOTE  qvar     {% ams (sLL $1 $> $ HsBracket (VarBr True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  qcon     {% ams (sLL $1 $> $ HsBracket (VarBr True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE tyvar     {% ams (sLL $1 $> $ HsBracket (VarBr False (unLoc $2))) [mj AnnThIdSplice $1,mj AnnName $2] }
        | TH_TY_QUOTE gtycon    {% ams (sLL $1 $> $ HsBracket (VarBr False (unLoc $2))) [mj AnnThIdSplice $1,mj AnnName $2] }
        | '[|' exp '|]'       {% ams (sLL $1 $> $ HsBracket (ExpBr $2)) [mo $1,mc $3] }
        | '[||' exp '||]'     {% ams (sLL $1 $> $ HsBracket (TExpBr $2)) [mo $1,mc $3]}
        | '[t|' ctype '|]'    {% checkNoPartialType
                                   (text "in type brackets" <> colon
                                    <+> quotes (text "[t|" <+> ppr $2 <+> text "|]")) $2 >>
                                 ams (sLL $1 $> $ HsBracket (TypBr $2)) [mo $1,mc $3] }
        | '[p|' infixexp '|]' {% checkPattern empty $2 >>= \p ->
                                      ams (sLL $1 $> $ HsBracket (PatBr p))
                                          [mo $1,mc $3] }
        | '[d|' cvtopbody '|]' {% ams (sLL $1 $> $ HsBracket (DecBrL (snd $2)))
                                      (mo $1:mc $3:fst $2) }
        | quasiquote          { sL1 $1 (HsQuasiQuoteE (unLoc $1)) }

        -- arrow notation extension
        | '(|' aexp2 cmdargs '|)'  {% ams (sLL $1 $> $ HsArrForm $2
                                                           Nothing (reverse $3))
                                          [mo $1,mc $4] }

splice_exp :: { LHsExpr RdrName }
        : TH_ID_SPLICE          {% ams (sL1 $1 $ mkHsSpliceE
                                        (sL1 $1 $ HsVar (mkUnqual varName
                                                        (getTH_ID_SPLICE $1))))
                                       [mj AnnThIdSplice $1] }
        | '$(' exp ')'          {% ams (sLL $1 $> $ mkHsSpliceE $2) [mo $1,mc $3] }
        | TH_ID_TY_SPLICE       {% ams (sL1 $1 $ mkHsSpliceTE
                                        (sL1 $1 $ HsVar (mkUnqual varName
                                                     (getTH_ID_TY_SPLICE $1))))
                                       [mj AnnThIdTySplice $1] }
        | '$$(' exp ')'         {% ams (sLL $1 $> $ mkHsSpliceTE $2) [mo $1,mc $3] }

cmdargs :: { [LHsCmdTop RdrName] }
        : cmdargs acmd                  { $2 : $1 }
        | {- empty -}                   { [] }

acmd    :: { LHsCmdTop RdrName }
        : aexp2                 {% checkCommand $1 >>= \ cmd ->
                                    return (sL1 $1 $ HsCmdTop cmd
                                           placeHolderType placeHolderType []) }

cvtopbody :: { ([AddAnn],[LHsDecl RdrName]) }
        :  '{'            cvtopdecls0 '}'      { ([mj AnnOpenC $1
                                                  ,mj AnnCloseC $3],$2) }
        |      vocurly    cvtopdecls0 close    { ([],$2) }

cvtopdecls0 :: { [LHsDecl RdrName] }
        : {- empty -}           { [] }
        | cvtopdecls            { $1 }

-----------------------------------------------------------------------------
-- Tuple expressions

-- "texp" is short for tuple expressions:
-- things that can appear unparenthesized as long as they're
-- inside parens or delimitted by commas
texp :: { LHsExpr RdrName }
        : exp                           { $1 }

        -- Note [Parsing sections]
        -- ~~~~~~~~~~~~~~~~~~~~~~~
        -- We include left and right sections here, which isn't
        -- technically right according to the Haskell standard.
        -- For example (3 +, True) isn't legal.
        -- However, we want to parse bang patterns like
        --      (!x, !y)
        -- and it's convenient to do so here as a section
        -- Then when converting expr to pattern we unravel it again
        -- Meanwhile, the renamer checks that real sections appear
        -- inside parens.
        | infixexp qop        { sLL $1 $> $ SectionL $1 $2 }
        | qopm infixexp       { sLL $1 $> $ SectionR $1 $2 }

       -- View patterns get parenthesized above
        | exp '->' texp   {% ams (sLL $1 $> $ EViewPat $1 $3) [mj AnnRarrow $2] }

-- Always at least one comma
tup_exprs :: { [LHsTupArg RdrName] }
           : texp commas_tup_tail
                          {% do { addAnnotation (gl $1) AnnComma (fst $2)
                                ; return ((sL1 $1 (Present $1)) : snd $2) } }

           | commas tup_tail
                {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (fst $1)
                      ; return
                           (let tt = if null $2
                                       then [noLoc missingTupArg]
                                       else $2
                            in map (\l -> L l missingTupArg) (fst $1) ++ tt) } }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { (SrcSpan,[LHsTupArg RdrName]) }
commas_tup_tail : commas tup_tail
       {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (tail $ fst $1)
             ; return (
         let tt = if null $2
                    then [L (last $ fst $1) missingTupArg]
                    else $2
         in (head $ fst $1
            ,(map (\l -> L l missingTupArg) (init $ fst $1)) ++ tt)) } }

-- Always follows a comma
tup_tail :: { [LHsTupArg RdrName] }
          : texp commas_tup_tail {% addAnnotation (gl $1) AnnComma (fst $2) >>
                                    return ((L (gl $1) (Present $1)) : snd $2) }
          | texp                 { [L (gl $1) (Present $1)] }
          | {- empty -}          { [] {- [noLoc missingTupArg] -} }

-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.
list :: { ([AddAnn],HsExpr RdrName) }
        : texp    { ([],ExplicitList placeHolderType Nothing [$1]) }
        | lexps   { ([],ExplicitList placeHolderType Nothing
                                                   (reverse (unLoc $1))) }
        | texp '..'             { ([mj AnnDotdot $2],
                                      ArithSeq noPostTcExpr Nothing (From $1)) }
        | texp ',' exp '..'     { ([mj AnnComma $2,mj AnnDotdot $4],
                                  ArithSeq noPostTcExpr Nothing
                                                             (FromThen $1 $3)) }
        | texp '..' exp         { ([mj AnnDotdot $2],
                                   ArithSeq noPostTcExpr Nothing
                                                               (FromTo $1 $3)) }
        | texp ',' exp '..' exp { ([mj AnnComma $2,mj AnnDotdot $4],
                                    ArithSeq noPostTcExpr Nothing
                                                (FromThenTo $1 $3 $5)) }
        | texp '|' flattenedpquals
             {% checkMonadComp >>= \ ctxt ->
                return ([mj AnnVbar $2],
                        mkHsComp ctxt (unLoc $3) $1) }

lexps :: { Located [LHsExpr RdrName] }
        : lexps ',' texp          {% addAnnotation (gl $ head $ unLoc $1)
                                                            AnnComma (gl $2) >>
                                      return (sLL $1 $> (((:) $! $3) $! unLoc $1)) }
        | texp ',' texp            {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                      return (sLL $1 $> [$3,$1]) }

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt RdrName (LHsExpr RdrName)] }
    : pquals   { case (unLoc $1) of
                    [qs] -> sL1 $1 qs
                    -- We just had one thing in our "parallel" list so
                    -- we simply return that thing directly

                    qss -> sL1 $1 [sL1 $1 $ ParStmt [ParStmtBlock qs [] noSyntaxExpr |
                                            qs <- qss]
                                            noSyntaxExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt RdrName (LHsExpr RdrName)]] }
    : squals '|' pquals
                     {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $2) >>
                        return (sLL $1 $> (reverse (unLoc $1) : unLoc $3)) }
    | squals         { L (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt RdrName (LHsExpr RdrName)] }   -- In reverse order, because the last
                                        -- one can "grab" the earlier ones
    : squals ',' transformqual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                ams (sLL $1 $> ()) (fst $ unLoc $3) >>
                return (sLL $1 $> [sLL $1 $> ((snd $ unLoc $3) (reverse (unLoc $1)))]) }
    | squals ',' qual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                return (sLL $1 $> ($3 : unLoc $1)) }
    | transformqual        {% ams $1 (fst $ unLoc $1) >>
                              return (sLL $1 $> [L (getLoc $1) ((snd $ unLoc $1) [])]) }
    | qual                                { sL1 $1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { sLL $1 $> ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { sL1 $1 [$2] }

-- It is possible to enable bracketing (associating) qualifier lists
-- by uncommenting the lines with {| |} above. Due to a lack of
-- consensus on the syntax, this feature is not being used until we
-- get user demand.

transformqual :: { Located ([AddAnn],[LStmt RdrName (LHsExpr RdrName)] -> Stmt RdrName (LHsExpr RdrName)) }
                        -- Function is applied to a list of stmts *in order*
    : 'then' exp               { sLL $1 $> ([mj AnnThen $1], \ss -> (mkTransformStmt ss $2)) }
    | 'then' exp 'by' exp      { sLL $1 $> ([mj AnnThen $1,mj AnnBy  $3],\ss -> (mkTransformByStmt ss $2 $4)) }
    | 'then' 'group' 'using' exp
             { sLL $1 $> ([mj AnnThen $1,mj AnnGroup $2,mj AnnUsing $3], \ss -> (mkGroupUsingStmt ss $4)) }

    | 'then' 'group' 'by' exp 'using' exp
             { sLL $1 $> ([mj AnnThen $1,mj AnnGroup $2,mj AnnBy $3,mj AnnUsing $5], \ss -> (mkGroupByUsingStmt ss $4 $6)) }

-- Note that 'group' is a special_id, which means that you can enable
-- TransformListComp while still using Data.List.group. However, this
-- introduces a shift/reduce conflict. Happy chooses to resolve the conflict
-- in by choosing the "group by" variant, which is what we want.

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { ([AddAnn],HsExpr RdrName) }
        :                      { ([],ExplicitPArr placeHolderType []) }
        | texp                 { ([],ExplicitPArr placeHolderType [$1]) }
        | lexps                { ([],ExplicitPArr placeHolderType
                                                          (reverse (unLoc $1))) }
        | texp '..' exp        { ([mj AnnDotdot $2]
                                 ,PArrSeq noPostTcExpr (FromTo $1 $3)) }
        | texp ',' exp '..' exp
                        { ([mj AnnComma $2,mj AnnDotdot $4]
                          ,PArrSeq noPostTcExpr (FromThenTo $1 $3 $5)) }
        | texp '|' flattenedpquals
                        { ([mj AnnVbar $2],mkHsComp PArrComp (unLoc $3) $1) }

-- We are reusing `lexps' and `flattenedpquals' from the list case.

-----------------------------------------------------------------------------
-- Guards

guardquals :: { Located [LStmt RdrName (LHsExpr RdrName)] }
    : guardquals1           { L (getLoc $1) (reverse (unLoc $1)) }

guardquals1 :: { Located [LStmt RdrName (LHsExpr RdrName)] }
    : guardquals1 ',' qual  {% addAnnotation (gl $ head $ unLoc $1) AnnComma
                                             (gl $2) >>
                               return (sLL $1 $> ($3 : unLoc $1)) }
    | qual                  { sL1 $1 [$1] }

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { Located ([AddAnn],[LMatch RdrName (LHsExpr RdrName)]) }
        : '{'            alts '}'  { sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                               ,(reverse (snd $ unLoc $2))) }
        |     vocurly    alts  close { L (getLoc $2) (fst $ unLoc $2
                                        ,(reverse (snd $ unLoc $2))) }
        | '{'                 '}'    { noLoc ([moc $1,mcc $2],[]) }
        |     vocurly          close { noLoc ([],[]) }

alts    :: { Located ([AddAnn],[LMatch RdrName (LHsExpr RdrName)]) }
        : alts1                    { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
        | ';' alts                 { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2))
                                               ,snd $ unLoc $2) }

alts1   :: { Located ([AddAnn],[LMatch RdrName (LHsExpr RdrName)]) }
        : alts1 ';' alt         {% if null (snd $ unLoc $1)
                                     then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                  ,[$3]))
                                     else (ams (head $ snd $ unLoc $1)
                                               (mj AnnSemi $2:(fst $ unLoc $1))
                                           >> return (sLL $1 $> ([],$3 : (snd $ unLoc $1))) ) }
        | alts1 ';'             {% if null (snd $ unLoc $1)
                                     then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                  ,snd $ unLoc $1))
                                     else (ams (head $ snd $ unLoc $1)
                                               (mj AnnSemi $2:(fst $ unLoc $1))
                                           >> return (sLL $1 $> ([],snd $ unLoc $1))) }
        | alt                   { sL1 $1 ([],[$1]) }

alt     :: { LMatch RdrName (LHsExpr RdrName) }
        : pat opt_sig alt_rhs      {%ams (sLL $1 $> (Match Nothing [$1] (snd $2)
                                                              (snd $ unLoc $3)))
                                         ((fst $2) ++ (fst $ unLoc $3))}

alt_rhs :: { Located ([AddAnn],GRHSs RdrName (LHsExpr RdrName)) }
        : ralt wherebinds           { sLL $1 $> (fst $ unLoc $2,
                                            GRHSs (unLoc $1) (snd $ unLoc $2)) }

ralt :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : '->' exp            {% ams (sLL $1 $> (unguardedRHS (comb2 $1 $2) $2))
                                     [mj AnnRarrow $1] }
        | gdpats              { sL1 $1 (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : gdpats gdpat                  { sLL $1 $> ($2 : unLoc $1) }
        | gdpat                         { sL1 $1 [$1] }

-- optional semi-colons between the guards of a MultiWayIf, because we use
-- layout here, but we don't need (or want) the semicolon as a separator (#7783).
gdpatssemi :: { Located [LGRHS RdrName (LHsExpr RdrName)] }
        : gdpatssemi gdpat optSemi  {% ams (sL (comb2 $1 $2) ($2 : unLoc $1))
                                           (map (\l -> mj AnnSemi l) $ fst $3) }
        | gdpat optSemi             {% ams (sL1 $1 [$1])
                                           (map (\l -> mj AnnSemi l) $ fst $2) }

-- layout for MultiWayIf doesn't begin with an open brace, because it's hard to
-- generate the open brace in addition to the vertical bar in the lexer, and
-- we don't need it.
ifgdpats :: { Located ([AddAnn],[LGRHS RdrName (LHsExpr RdrName)]) }
         : '{' gdpatssemi '}'             { sLL $1 $> ([moc $1,mcc $3],unLoc $2)  }
         |     gdpatssemi close           { sL1 $1 ([],unLoc $1) }

gdpat   :: { LGRHS RdrName (LHsExpr RdrName) }
        : '|' guardquals '->' exp
                                  {% ams (sL (comb2 $1 $>) $ GRHS (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnRarrow $3] }

-- 'pat' recognises a pattern, including one with a bang at the top
--      e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat RdrName }
pat     :  exp          {% checkPattern empty $1 }
        | '!' aexp      {% amms (checkPattern empty (sLL $1 $> (SectionR
                                                     (sL1 $1 (HsVar bang_RDR)) $2)))
                                [mj AnnBang $1] }

bindpat :: { LPat RdrName }
bindpat :  exp            {% checkPattern
                                (text "Possibly caused by a missing 'do'?") $1 }
        | '!' aexp        {% amms (checkPattern
                                     (text "Possibly caused by a missing 'do'?")
                                     (sLL $1 $> (SectionR (sL1 $1 (HsVar bang_RDR)) $2)))
                                  [mj AnnBang $1] }

apat   :: { LPat RdrName }
apat    : aexp                  {% checkPattern empty $1 }
        | '!' aexp              {% amms (checkPattern empty
                                            (sLL $1 $> (SectionR
                                                (sL1 $1 (HsVar bang_RDR)) $2)))
                                        [mj AnnBang $1] }

apats  :: { [LPat RdrName] }
        : apat apats            { $1 : $2 }
        | {- empty -}           { [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { Located ([AddAnn],[LStmt RdrName (LHsExpr RdrName)]) }
        : '{'           stmts '}'       { sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                             ,(reverse $ snd $ unLoc $2)) } -- AZ:performance of reverse?
        |     vocurly   stmts close     { L (gl $2) (fst $ unLoc $2
                                                    ,reverse $ snd $ unLoc $2) }

--      do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
-- AZ: TODO check that we can retrieve multiple semis.

stmts :: { Located ([AddAnn],[LStmt RdrName (LHsExpr RdrName)]) }
        : stmts ';' stmt  {% if null (snd $ unLoc $1)
                              then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                     ,$3 : (snd $ unLoc $1)))
                              else do
                               { ams (head $ snd $ unLoc $1) [mj AnnSemi $2]
                               ; return $ sLL $1 $> (fst $ unLoc $1,$3 :(snd $ unLoc $1)) }}

        | stmts ';'     {% if null (snd $ unLoc $1)
                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1),snd $ unLoc $1))
                             else do
                               { ams (head $ snd $ unLoc $1)
                                               [mj AnnSemi $2]
                               ; return $1 } }
        | stmt                   { sL1 $1 ([],[$1]) }
        | {- empty -}            { noLoc ([],[]) }


-- For typing stmts at the GHCi prompt, where
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt RdrName (LHsExpr RdrName)) }
        : stmt                          { Just $1 }
        | {- nothing -}                 { Nothing }

stmt  :: { LStmt RdrName (LHsExpr RdrName) }
        : qual                          { $1 }
        | 'rec' stmtlist                {% ams (sLL $1 $> $ mkRecStmt (snd $ unLoc $2))
                                               (mj AnnRec $1:(fst $ unLoc $2)) }

qual  :: { LStmt RdrName (LHsExpr RdrName) }
    : bindpat '<-' exp                  {% ams (sLL $1 $> $ mkBindStmt $1 $3)
                                               [mj AnnLarrow $2] }
    | exp                               { sL1 $1 $ mkBodyStmt $1 }
    | 'let' binds                       {% ams (sLL $1 $>$ LetStmt (snd $ unLoc $2))
                                               (mj AnnLet $1:(fst $ unLoc $2)) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds  :: { ([AddAnn],([LHsRecField RdrName (LHsExpr RdrName)], Bool)) }
        : fbinds1                       { $1 }
        | {- empty -}                   { ([],([], False)) }

fbinds1 :: { ([AddAnn],([LHsRecField RdrName (LHsExpr RdrName)], Bool)) }
        : fbind ',' fbinds1
                {% addAnnotation (gl $1) AnnComma (gl $2) >>
                   return (case $3 of (ma,(flds, dd)) -> (ma,($1 : flds, dd))) }
        | fbind                         { ([],([$1], False)) }
        | '..'                          { ([mj AnnDotdot $1],([],   True)) }

fbind   :: { LHsRecField RdrName (LHsExpr RdrName) }
        : qvar '=' texp {% ams  (sLL $1 $> $ HsRecField $1 $3             False)
                                [mj AnnEqual $2] }
                        -- RHS is a 'texp', allowing view patterns (Trac #6038)
                        -- and, incidentaly, sections.  Eg
                        -- f (R { x = show -> s }) = ...

        | qvar          { sLL $1 $> $ HsRecField $1 placeHolderPunRhs True }
                        -- In the punning case, use a place-holder
                        -- The renamer fills in the final value

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds  :: { Located [LIPBind RdrName] }
        : dbinds ';' dbind
                      {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (let { this = $3; rest = unLoc $1 }
                              in rest `seq` this `seq` sLL $1 $> (this : rest)) }
        | dbinds ';'  {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (sLL $1 $> (unLoc $1)) }
        | dbind                        { let this = $1 in this `seq` sL1 $1 [this] }
--      | {- empty -}                  { [] }

dbind   :: { LIPBind RdrName }
dbind   : ipvar '=' exp                {% ams (sLL $1 $> (IPBind (Left $1) $3))
                                              [mj AnnEqual $2] }

ipvar   :: { Located HsIPName }
        : IPDUPVARID            { sL1 $1 (HsIPName (getIPDUPVARID $1)) }

-----------------------------------------------------------------------------
-- Warnings and deprecations

name_boolformula_opt :: { ([AddAnn],BooleanFormula (Located RdrName)) }
        : name_boolformula          { $1 }
        | {- empty -}               { ([],mkTrue) }

name_boolformula :: { ([AddAnn],BooleanFormula (Located RdrName)) }
        : name_boolformula_and                      { $1 }
        | name_boolformula_and '|' name_boolformula
                                             { ((mj AnnVbar $2:fst $1)++(fst $3)
                                                ,Or [snd $1,snd $3]) }

name_boolformula_and :: { ([AddAnn],BooleanFormula (Located RdrName)) }
        : name_boolformula_atom                             { $1 }
        | name_boolformula_atom ',' name_boolformula_and
                  { ((mj AnnComma $2:fst $1)++(fst $3), And [snd $1,snd $3]) }

name_boolformula_atom :: { ([AddAnn],BooleanFormula (Located RdrName)) }
        : '(' name_boolformula ')'  { ((mop $1:mcp $3:(fst $2)),snd $2) }
        | name_var                  { ([],Var $1) }

namelist :: { Located [Located RdrName] }
namelist : name_var              { sL1 $1 [$1] }
         | name_var ',' namelist {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                    return (sLL $1 $> ($1 : unLoc $3)) }

name_var :: { Located RdrName }
name_var : var { $1 }
         | con { $1 }

-----------------------------------------
-- Data constructors
-- There are two different productions here as lifted list constructors
-- are parsed differently.

qcon_nowiredlist :: { Located RdrName }
        : gen_qcon                     { $1 }
        | sysdcon_nolist               { sL1 $1 $ nameRdrName (dataConName (unLoc $1)) }

qcon :: { Located RdrName }
  : gen_qcon              { $1}
  | sysdcon               { sL1 $1 $ nameRdrName (dataConName (unLoc $1)) }

gen_qcon :: { Located RdrName }
  : qconid                { $1 }
  | '(' qconsym ')'       {% ams (sLL $1 $> (unLoc $2))
                                   [mop $1,mj AnnVal $2,mcp $3] }

-- The case of '[:' ':]' is part of the production `parr'

con     :: { Located RdrName }
        : conid                 { $1 }
        | '(' consym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }
        | sysdcon               { sL1 $1 $ nameRdrName (dataConName (unLoc $1)) }

con_list :: { Located [Located RdrName] }
con_list : con                  { sL1 $1 [$1] }
         | con ',' con_list     {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                   return (sLL $1 $> ($1 : unLoc $3)) }

sysdcon_nolist :: { Located DataCon }  -- Wired in data constructors
        : '(' ')'               {% ams (sLL $1 $> unitDataCon) [mop $1,mcp $2] }
        | '(' commas ')'        {% ams (sLL $1 $> $ tupleCon BoxedTuple (snd $2 + 1))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' '#)'             {% ams (sLL $1 $> $ unboxedUnitDataCon) [mo $1,mc $2] }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ tupleCon UnboxedTuple (snd $2 + 1))
                                       (mo $1:mc $3:(mcommas (fst $2))) }

sysdcon :: { Located DataCon }
        : sysdcon_nolist                 { $1 }
        | '[' ']'               {% ams (sLL $1 $> nilDataCon) [mos $1,mcs $2] }

conop :: { Located RdrName }
        : consym                { $1 }
        | '`' conid '`'         {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qconop :: { Located RdrName }
        : qconsym               { $1 }
        | '`' qconid '`'        {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

----------------------------------------------------------------------------
-- Type constructors


-- See Note [Unit tuples] in HsTypes for the distinction
-- between gtycon and ntgtycon
gtycon :: { Located RdrName }  -- A "general" qualified tycon, including unit tuples
        : ntgtycon                     { $1 }
        | '(' ')'                      {% ams (sLL $1 $> $ getRdrName unitTyCon)
                                              [mop $1,mcp $2] }
        | '(#' '#)'                    {% ams (sLL $1 $> $ getRdrName unboxedUnitTyCon)
                                              [mo $1,mc $2] }

ntgtycon :: { Located RdrName }  -- A "general" qualified tycon, excluding unit tuples
        : oqtycon               { $1 }
        | '(' commas ')'        {% ams (sLL $1 $> $ getRdrName (tupleTyCon BoxedTuple
                                                        (snd $2 + 1)))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ getRdrName (tupleTyCon UnboxedTuple
                                                        (snd $2 + 1)))
                                       (mo $1:mc $3:(mcommas (fst $2))) }
        | '(' '->' ')'          {% ams (sLL $1 $> $ getRdrName funTyCon)
                                       [mop $1,mj AnnRarrow $2,mcp $3] }
        | '[' ']'               {% ams (sLL $1 $> $ listTyCon_RDR) [mos $1,mcs $2] }
        | '[:' ':]'             {% ams (sLL $1 $> $ parrTyCon_RDR) [mo $1,mc $2] }
        | '(' '~#' ')'          {% ams (sLL $1 $> $ getRdrName eqPrimTyCon)
                                        [mop $1,mj AnnTildehsh $2,mcp $3] }

oqtycon :: { Located RdrName }  -- An "ordinary" qualified tycon;
                                -- These can appear in export lists
        : qtycon                        { $1 }
        | '(' qtyconsym ')'             {% ams (sLL $1 $> (unLoc $2))
                                               [mop $1,mj AnnVal $2,mcp $3] }
        | '(' '~' ')'                   {% ams (sLL $1 $> $ eqTyCon_RDR)
                                               [mop $1,mj AnnTilde $2,mcp $3] }

qtyconop :: { Located RdrName } -- Qualified or unqualified
        : qtyconsym                     { $1 }
        | '`' qtycon '`'                {% ams (sLL $1 $> (unLoc $2))
                                               [mj AnnBackquote $1,mj AnnVal $2
                                               ,mj AnnBackquote $3] }

qtycon :: { Located RdrName }   -- Qualified or unqualified
        : QCONID            { sL1 $1 $! mkQual tcClsName (getQCONID $1) }
        | PREFIXQCONSYM     { sL1 $1 $! mkQual tcClsName (getPREFIXQCONSYM $1) }
        | tycon             { $1 }

tycon   :: { Located RdrName }  -- Unqualified
        : CONID                   { sL1 $1 $! mkUnqual tcClsName (getCONID $1) }

qtyconsym :: { Located RdrName }
        : QCONSYM            { sL1 $1 $! mkQual tcClsName (getQCONSYM $1) }
        | QVARSYM            { sL1 $1 $! mkQual tcClsName (getQVARSYM $1) }
        | tyconsym           { $1 }

-- Does not include "!", because that is used for strictness marks
--               or ".", because that separates the quantified type vars from the rest
tyconsym :: { Located RdrName }
        : CONSYM                { sL1 $1 $! mkUnqual tcClsName (getCONSYM $1) }
        | VARSYM                { sL1 $1 $! mkUnqual tcClsName (getVARSYM $1) }
        | '*'                   { sL1 $1 $! mkUnqual tcClsName (fsLit "*") }
        | '-'                   { sL1 $1 $! mkUnqual tcClsName (fsLit "-") }


-----------------------------------------------------------------------------
-- Operators

op      :: { Located RdrName }   -- used in infix decls
        : varop                 { $1 }
        | conop                 { $1 }

varop   :: { Located RdrName }
        : varsym                { $1 }
        | '`' varid '`'         {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qop     :: { LHsExpr RdrName }   -- used in sections
        : qvarop                { sL1 $1 $ HsVar (unLoc $1) }
        | qconop                { sL1 $1 $ HsVar (unLoc $1) }

qopm    :: { LHsExpr RdrName }   -- used in sections
        : qvaropm               { sL1 $1 $ HsVar (unLoc $1) }
        | qconop                { sL1 $1 $ HsVar (unLoc $1) }

qvarop :: { Located RdrName }
        : qvarsym               { $1 }
        | '`' qvarid '`'        {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qvaropm :: { Located RdrName }
        : qvarsym_no_minus      { $1 }
        | '`' qvarid '`'        {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

-----------------------------------------------------------------------------
-- Type variables

tyvar   :: { Located RdrName }
tyvar   : tyvarid               { $1 }

tyvarop :: { Located RdrName }
tyvarop : '`' tyvarid '`'       {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }
        | '.'                   {% parseErrorSDoc (getLoc $1)
                                      (vcat [ptext (sLit "Illegal symbol '.' in type"),
                                             ptext (sLit "Perhaps you intended to use RankNTypes or a similar language"),
                                             ptext (sLit "extension to enable explicit-forall syntax: forall <tvs>. <type>")])
                                }

tyvarid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual tvName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual tvName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual tvName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual tvName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual tvName (fsLit "interruptible") }

-----------------------------------------------------------------------------
-- Variables

var     :: { Located RdrName }
        : varid                 { $1 }
        | '(' varsym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }

qvar    :: { Located RdrName }
        : qvarid                { $1 }
        | '(' varsym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }
        | '(' qvarsym1 ')'      {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

qvarid :: { Located RdrName }
        : varid               { $1 }
        | QVARID              { sL1 $1 $! mkQual varName (getQVARID $1) }
        | PREFIXQVARSYM       { sL1 $1 $! mkQual varName (getPREFIXQVARSYM $1) }

-- Note that 'role' and 'family' get lexed separately regardless of
-- the use of extensions. However, because they are listed here, this
-- is OK and they can be used as normal varids.
varid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual varName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual varName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual varName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual varName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual varName (fsLit "interruptible")}
        | 'forall'         { sL1 $1 $! mkUnqual varName (fsLit "forall") }
        | 'family'         { sL1 $1 $! mkUnqual varName (fsLit "family") }
        | 'role'           { sL1 $1 $! mkUnqual varName (fsLit "role") }

qvarsym :: { Located RdrName }
        : varsym                { $1 }
        | qvarsym1              { $1 }

qvarsym_no_minus :: { Located RdrName }
        : varsym_no_minus       { $1 }
        | qvarsym1              { $1 }

qvarsym1 :: { Located RdrName }
qvarsym1 : QVARSYM              { sL1 $1 $ mkQual varName (getQVARSYM $1) }

varsym :: { Located RdrName }
        : varsym_no_minus       { $1 }
        | '-'                   { sL1 $1 $ mkUnqual varName (fsLit "-") }

varsym_no_minus :: { Located RdrName } -- varsym not including '-'
        : VARSYM               { sL1 $1 $ mkUnqual varName (getVARSYM $1) }
        | special_sym          { sL1 $1 $ mkUnqual varName (unLoc $1) }


-- These special_ids are treated as keywords in various places,
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe', 'interruptible', 'forall', 'family', and 'role',
-- whose treatment differs depending on context
special_id :: { Located FastString }
special_id
        : 'as'                  { sL1 $1 (fsLit "as") }
        | 'qualified'           { sL1 $1 (fsLit "qualified") }
        | 'hiding'              { sL1 $1 (fsLit "hiding") }
        | 'export'              { sL1 $1 (fsLit "export") }
        | 'label'               { sL1 $1 (fsLit "label")  }
        | 'dynamic'             { sL1 $1 (fsLit "dynamic") }
        | 'stdcall'             { sL1 $1 (fsLit "stdcall") }
        | 'ccall'               { sL1 $1 (fsLit "ccall") }
        | 'capi'                { sL1 $1 (fsLit "capi") }
        | 'prim'                { sL1 $1 (fsLit "prim") }
        | 'javascript'          { sL1 $1 (fsLit "javascript") }
        | 'group'               { sL1 $1 (fsLit "group") }

special_sym :: { Located FastString }
special_sym : '!'       {% ams (sL1 $1 (fsLit "!")) [mj AnnBang $1] }
            | '.'       { sL1 $1 (fsLit ".") }
            | '*'       { sL1 $1 (fsLit "*") }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }   -- Qualified or unqualified
        : conid              { $1 }
        | QCONID             { sL1 $1 $! mkQual dataName (getQCONID $1) }
        | PREFIXQCONSYM      { sL1 $1 $! mkQual dataName (getPREFIXQCONSYM $1) }

conid   :: { Located RdrName }
        : CONID                { sL1 $1 $ mkUnqual dataName (getCONID $1) }

qconsym :: { Located RdrName }  -- Qualified or unqualified
        : consym               { $1 }
        | QCONSYM              { sL1 $1 $ mkQual dataName (getQCONSYM $1) }

consym :: { Located RdrName }
        : CONSYM              { sL1 $1 $ mkUnqual dataName (getCONSYM $1) }

        -- ':' means only list cons
        | ':'                { sL1 $1 $ consDataCon_RDR }


-----------------------------------------------------------------------------
-- Literals

literal :: { Located HsLit }
        : CHAR              { sL1 $1 $ HsChar       (getCHARs $1) $ getCHAR $1 }
        | STRING            { sL1 $1 $ HsString     (getSTRINGs $1)
                                                   $ getSTRING $1 }
        | PRIMINTEGER       { sL1 $1 $ HsIntPrim    (getPRIMINTEGERs $1)
                                                   $ getPRIMINTEGER $1 }
        | PRIMWORD          { sL1 $1 $ HsWordPrim   (getPRIMWORDs $1)
                                                   $ getPRIMWORD $1 }
        | PRIMCHAR          { sL1 $1 $ HsCharPrim   (getPRIMCHARs $1)
                                                   $ getPRIMCHAR $1 }
        | PRIMSTRING        { sL1 $1 $ HsStringPrim (getPRIMSTRINGs $1)
                                                   $ getPRIMSTRING $1 }
        | PRIMFLOAT         { sL1 $1 $ HsFloatPrim  $ getPRIMFLOAT $1 }
        | PRIMDOUBLE        { sL1 $1 $ HsDoublePrim $ getPRIMDOUBLE $1 }

-----------------------------------------------------------------------------
-- Layout

close :: { () }
        : vccurly               { () } -- context popped in lexer.
        | error                 {% popContext }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid   :: { Located ModuleName }
        : CONID                 { sL1 $1 $ mkModuleNameFS (getCONID $1) }
        | QCONID                { sL1 $1 $ let (mod,c) = getQCONID $1 in
                                  mkModuleNameFS
                                   (mkFastString
                                     (unpackFS mod ++ '.':unpackFS c))
                                }

commas :: { ([SrcSpan],Int) }   -- One or more commas
        : commas ','             { ((fst $1)++[gl $2],snd $1 + 1) }
        | ','                    { ([gl $1],1) }

-----------------------------------------------------------------------------
-- Documentation comments

docnext :: { LHsDocString }
  : DOCNEXT {% return (sL1 $1 (HsDocString (mkFastString (getDOCNEXT $1)))) }

docprev :: { LHsDocString }
  : DOCPREV {% return (sL1 $1 (HsDocString (mkFastString (getDOCPREV $1)))) }

docnamed :: { Located (String, HsDocString) }
  : DOCNAMED {%
      let string = getDOCNAMED $1
          (name, rest) = break isSpace string
      in return (sL1 $1 (name, HsDocString (mkFastString rest))) }

docsection :: { Located (Int, HsDocString) }
  : DOCSECTION {% let (n, doc) = getDOCSECTION $1 in
        return (sL1 $1 (n, HsDocString (mkFastString doc))) }

moduleheader :: { Maybe LHsDocString }
        : DOCNEXT {% let string = getDOCNEXT $1 in
                     return (Just (sL1 $1 (HsDocString (mkFastString string)))) }

maybe_docprev :: { Maybe LHsDocString }
        : docprev                       { Just $1 }
        | {- empty -}                   { Nothing }

maybe_docnext :: { Maybe LHsDocString }
        : docnext                       { Just $1 }
        | {- empty -}                   { Nothing }

{
happyError :: P a
happyError = srcParseFail

getVARID        (L _ (ITvarid    x)) = x
getCONID        (L _ (ITconid    x)) = x
getVARSYM       (L _ (ITvarsym   x)) = x
getCONSYM       (L _ (ITconsym   x)) = x
getQVARID       (L _ (ITqvarid   x)) = x
getQCONID       (L _ (ITqconid   x)) = x
getQVARSYM      (L _ (ITqvarsym  x)) = x
getQCONSYM      (L _ (ITqconsym  x)) = x
getPREFIXQVARSYM (L _ (ITprefixqvarsym  x)) = x
getPREFIXQCONSYM (L _ (ITprefixqconsym  x)) = x
getIPDUPVARID   (L _ (ITdupipvarid   x)) = x
getCHAR         (L _ (ITchar   _ x)) = x
getSTRING       (L _ (ITstring _ x)) = x
getINTEGER      (L _ (ITinteger _ x)) = x
getRATIONAL     (L _ (ITrational x)) = x
getPRIMCHAR     (L _ (ITprimchar _ x)) = x
getPRIMSTRING   (L _ (ITprimstring _ x)) = x
getPRIMINTEGER  (L _ (ITprimint  _ x)) = x
getPRIMWORD     (L _ (ITprimword _ x)) = x
getPRIMFLOAT    (L _ (ITprimfloat x)) = x
getPRIMDOUBLE   (L _ (ITprimdouble x)) = x
getTH_ID_SPLICE (L _ (ITidEscape x)) = x
getTH_ID_TY_SPLICE (L _ (ITidTyEscape x)) = x
getINLINE       (L _ (ITinline_prag _ inl conl)) = (inl,conl)
getSPEC_INLINE  (L _ (ITspec_inline_prag _ True))  = (Inline,  FunLike)
getSPEC_INLINE  (L _ (ITspec_inline_prag _ False)) = (NoInline,FunLike)

getDOCNEXT (L _ (ITdocCommentNext x)) = x
getDOCPREV (L _ (ITdocCommentPrev x)) = x
getDOCNAMED (L _ (ITdocCommentNamed x)) = x
getDOCSECTION (L _ (ITdocSection n x)) = (n, x)

getCHARs        (L _ (ITchar       src _)) = src
getSTRINGs      (L _ (ITstring     src _)) = src
getINTEGERs     (L _ (ITinteger    src _)) = src
getPRIMCHARs    (L _ (ITprimchar   src _)) = src
getPRIMSTRINGs  (L _ (ITprimstring src _)) = src
getPRIMINTEGERs (L _ (ITprimint    src _)) = src
getPRIMWORDs    (L _ (ITprimword   src _)) = src

-- See Note [Pragma source text] in BasicTypes for the following
getINLINE_PRAGs       (L _ (ITinline_prag       src _ _)) = src
getSPEC_PRAGs         (L _ (ITspec_prag         src))     = src
getSPEC_INLINE_PRAGs  (L _ (ITspec_inline_prag  src _))   = src
getSOURCE_PRAGs       (L _ (ITsource_prag       src)) = src
getRULES_PRAGs        (L _ (ITrules_prag        src)) = src
getWARNING_PRAGs      (L _ (ITwarning_prag      src)) = src
getDEPRECATED_PRAGs   (L _ (ITdeprecated_prag   src)) = src
getSCC_PRAGs          (L _ (ITscc_prag          src)) = src
getGENERATED_PRAGs    (L _ (ITgenerated_prag    src)) = src
getCORE_PRAGs         (L _ (ITcore_prag         src)) = src
getUNPACK_PRAGs       (L _ (ITunpack_prag       src)) = src
getNOUNPACK_PRAGs     (L _ (ITnounpack_prag     src)) = src
getANN_PRAGs          (L _ (ITann_prag          src)) = src
getVECT_PRAGs         (L _ (ITvect_prag         src)) = src
getVECT_SCALAR_PRAGs  (L _ (ITvect_scalar_prag  src)) = src
getNOVECT_PRAGs       (L _ (ITnovect_prag       src)) = src
getMINIMAL_PRAGs      (L _ (ITminimal_prag      src)) = src
getOVERLAPPABLE_PRAGs (L _ (IToverlappable_prag src)) = src
getOVERLAPPING_PRAGs  (L _ (IToverlapping_prag  src)) = src
getOVERLAPS_PRAGs     (L _ (IToverlaps_prag     src)) = src
getINCOHERENT_PRAGs   (L _ (ITincoherent_prag   src)) = src
getCTYPEs             (L _ (ITctype             src)) = src


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

-- replaced last 3 CPP macros in this file
{-# INLINE sL0 #-}
sL0 = L noSrcSpan       -- #define L0   L noSrcSpan

{-# INLINE sL1 #-}
sL1 x = sL (getLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sLL #-}
sLL x y = sL (comb2 x y) -- #define LL   sL (comb2 $1 $>)

-- Make a source location for the file.  We're a bit lazy here and just
-- make a point SrcSpan at line 1, column 0.  Strictly speaking we should
-- try to find the span of the whole file (ToDo).
fileSrcSpan :: P SrcSpan
fileSrcSpan = do
  l <- getSrcLoc;
  let loc = mkSrcLoc (srcLocFile l) 1 1;
  return (mkSrcSpan loc loc)

-- Hint about the MultiWayIf extension
hintMultiWayIf :: SrcSpan -> P ()
hintMultiWayIf span = do
  mwiEnabled <- liftM ((Opt_MultiWayIf `xopt`) . dflags) getPState
  unless mwiEnabled $ parseErrorSDoc span $
    text "Multi-way if-expressions need MultiWayIf turned on"

-- Hint about if usage for beginners
hintIf :: SrcSpan -> String -> P (LHsExpr RdrName)
hintIf span msg = do
  mwiEnabled <- liftM ((Opt_MultiWayIf `xopt`) . dflags) getPState
  if mwiEnabled
    then parseErrorSDoc span $ text $ "parse error in if statement"
    else parseErrorSDoc span $ text $ "parse error in if statement: "++msg

-- Hint about explicit-forall, assuming UnicodeSyntax is on
hintExplicitForall :: SrcSpan -> P ()
hintExplicitForall span = do
    forall      <- extension explicitForallEnabled
    rulePrag    <- extension inRulePrag
    unless (forall || rulePrag) $ parseErrorSDoc span $ vcat
      [ text "Illegal symbol '\x2200' in type" -- U+2200 FOR ALL
      , text "Perhaps you intended to use RankNTypes or a similar language"
      , text "extension to enable explicit-forall syntax: \x2200 <tvs>. <type>"
      ]

namedWildcardsEnabled :: P Bool
namedWildcardsEnabled = liftM ((Opt_NamedWildCards `xopt`) . dflags) getPState

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************

For the general principles of the following routines, see Note [Api annotations]
in ApiAnnotation.hs

-}

-- |Construct an AddAnn from the annotation keyword and the location
-- of the keyword
mj :: AnnKeywordId -> Located e -> AddAnn
mj a l = (\s -> addAnnotation s a (gl l))


gl = getLoc

-- |Add an annotation to the located element, and return the located
-- element as a pass through
aa :: Located a -> (AnnKeywordId,Located c) -> P (Located a)
aa a@(L l _) (b,s) = addAnnotation l b (gl s) >> return a

-- |Add an annotation to a located element resulting from a monadic action
am :: P (Located a) -> (AnnKeywordId, Located b) -> P (Located a)
am a (b,s) = do
  av@(L l _) <- a
  addAnnotation l b (gl s)
  return av

-- |Add a list of AddAnns to the given AST element
ams :: Located a -> [AddAnn] -> P (Located a)
ams a@(L l _) bs = mapM_ (\a -> a l) bs >> return a


-- |Add a list of AddAnns to the given AST element, where the AST element is the
--  result of a monadic action
amms :: P (Located a) -> [AddAnn] -> P (Located a)
amms a bs = do
  av@(L l _) <- a
  (mapM_ (\a -> a l) bs) >> return av

-- |Add a list of AddAnns to the AST element, and return the element as a
--  OrdList
amsu :: Located a -> [AddAnn] -> P (OrdList (Located a))
amsu a@(L l _) bs = (mapM_ (\a -> a l) bs) >> return (unitOL a)

-- |Synonyms for AddAnn versions of AnnOpen and AnnClose
mo,mc :: Located Token -> SrcSpan -> P ()
mo ll = mj AnnOpen ll
mc ll = mj AnnClose ll

moc,mcc :: Located Token -> SrcSpan -> P ()
moc ll = mj AnnOpenC ll
mcc ll = mj AnnCloseC ll

mop,mcp :: Located Token -> SrcSpan -> P ()
mop ll = mj AnnOpenP ll
mcp ll = mj AnnCloseP ll

mos,mcs :: Located Token -> SrcSpan -> P ()
mos ll = mj AnnOpenS ll
mcs ll = mj AnnCloseS ll

-- |Given a list of the locations of commas, provide a [AddAnn] with an AnnComma
--  entry for each SrcSpan
mcommas :: [SrcSpan] -> [AddAnn]
mcommas ss = map (\s -> mj AnnCommaTuple (L s ())) ss

-- |Add the annotation to an AST element wrapped in a Just
ajl :: Located (Maybe (Located a)) -> AnnKeywordId -> SrcSpan
 -> P (Located (Maybe (Located a)))
ajl a@(L _ (Just (L l _))) b s = addAnnotation l b s >> return a

-- |Add all [AddAnn] to an AST element wrapped in a Just
aljs :: Located (Maybe (Located a)) -> [AddAnn]
  -> P (Located (Maybe (Located a)))
aljs a@(L _ (Just (L l _))) bs = (mapM_ (\a -> a l) bs) >> return a

-- |Add all [AddAnn] to an AST element wrapped in a Just
ajs a@(Just (L l _)) bs = (mapM_ (\a -> a l) bs) >> return a

-- |Get the location of the last element of a OrdList, or noSrcSpan
oll :: OrdList (Located a) -> SrcSpan
oll l =
  if isNilOL l then noSrcSpan
               else getLoc (lastOL l)

-- |Add a semicolon annotation in the right place in a list. If the
-- leading list is empty, add it to the tail
asl :: [Located a] -> Located b -> Located a -> P()
asl [] (L ls _) (L l _) = addAnnotation l          AnnSemi ls
asl (x:_xs) (L ls _) _x = addAnnotation (getLoc x) AnnSemi ls

}
