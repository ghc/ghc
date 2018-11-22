
--                                                              -*-haskell-*-
-- ---------------------------------------------------------------------------
-- (c) The University of Glasgow 1997-2003
---
-- The GHC grammar.
--
-- Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-- ---------------------------------------------------------------------------

{
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

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
--       parseState = mkPState flags buffer location
-- @
module Parser (parseModule, parseSignature, parseImport, parseStatement, parseBackpack,
               parseDeclaration, parseExpression, parsePattern,
               parseTypeSignature,
               parseStmt, parseIdentifier,
               parseType, parseHeader) where

-- base
import Control.Monad    ( unless, liftM, when )
import GHC.Exts
import Data.Char
import Control.Monad    ( mplus )
import Control.Applicative ((<$))

-- compiler/hsSyn
import HsSyn

-- compiler/main
import HscTypes         ( IsBootInterface, WarningTxt(..) )
import DynFlags
import BkpSyn
import PackageConfig

-- compiler/utils
import OrdList
import BooleanFormula   ( BooleanFormula(..), LBooleanFormula(..), mkTrue )
import FastString
import Maybes           ( isJust, orElse )
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
import Kind             ( Kind )
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
import TysPrim          ( eqPrimTyCon )
import TysWiredIn       ( unitTyCon, unitDataCon, tupleTyCon, tupleDataCon, nilDataCon,
                          unboxedUnitTyCon, unboxedUnitDataCon,
                          listTyCon_RDR, consDataCon_RDR, eqTyCon_RDR )

-- compiler/utils
import Util             ( looksLikePackageName, fstOf3, sndOf3, thdOf3 )
import GhcPrelude

import qualified GHC.LanguageExtensions as LangExt
}

%expect 236 -- shift/reduce conflicts

{- Last updated: 04 June 2018

If you modify this parser and add a conflict, please update this comment.
You can learn more about the conflicts by passing 'happy' the -i flag:

    happy -agc --strict compiler/parser/Parser.y -idetailed-info

How is this section formatted? Look up the state the conflict is
reported at, and copy the list of applicable rules (at the top, without the
rule numbers).  Mark *** for the rule that is the conflicting reduction (that
is, the interpretation which is NOT taken).  NB: Happy doesn't print a rule
in a state if it is empty, but you should include it in the list (you can
look these up in the Grammar section of the info file).

Obviously the state numbers are not stable across modifications to the parser,
the idea is to reproduce enough information on each conflict so you can figure
out what happened if the states were renumbered.  Try not to gratuitously move
productions around in this file.

-------------------------------------------------------------------------------

state 0 contains 1 shift/reduce conflicts.

    Conflicts: DOCNEXT (empty missing_module_keyword reduces)

Ambiguity when the source file starts with "-- | doc". We need another
token of lookahead to determine if a top declaration or the 'module' keyword
follows. Shift parses as if the 'module' keyword follows.

-------------------------------------------------------------------------------

state 60 contains 1 shift/reduce conflict.

        context -> btype .
    *** type -> btype .
        type -> btype . '->' ctype

    Conflicts: '->'

-------------------------------------------------------------------------------

state 61 contains 46 shift/reduce conflicts.

    *** btype -> tyapps .
        tyapps -> tyapps . tyapp

    Conflicts: '_' ':' '~' '!' '.' '`' '{' '[' '[:' '(' '(#' '`' SIMPLEQUOTE
      VARID CONID VARSYM CONSYM QCONID QVARSYM QCONSYM
      STRING INTEGER TH_ID_SPLICE '$(' TH_QUASIQUOTE TH_QQUASIQUOTE
      and all the special ids.

Example ambiguity:
    'if x then y else z :: F a'

Shift parses as (per longest-parse rule):
    'if x then y else z :: (F a)'

-------------------------------------------------------------------------------

state 143 contains 15 shift/reduce conflicts.

        exp -> infixexp . '::' sigtype
        exp -> infixexp . '-<' exp
        exp -> infixexp . '>-' exp
        exp -> infixexp . '-<<' exp
        exp -> infixexp . '>>-' exp
    *** exp -> infixexp .
        infixexp -> infixexp . qop exp10

    Conflicts: ':' '::' '-' '!' '-<' '>-' '-<<' '>>-'
               '.' '`' '*' VARSYM CONSYM QVARSYM QCONSYM

Examples of ambiguity:
    'if x then y else z -< e'
    'if x then y else z :: T'
    'if x then y else z + 1' (NB: '+' is in VARSYM)

Shift parses as (per longest-parse rule):
    'if x then y else (z -< T)'
    'if x then y else (z :: T)'
    'if x then y else (z + 1)'

-------------------------------------------------------------------------------

state 148 contains 67 shift/reduce conflicts.

    *** exp10 -> fexp .
        fexp -> fexp . aexp
        fexp -> fexp . TYPEAPP atype

    Conflicts: TYPEAPP and all the tokens that can start an aexp

Examples of ambiguity:
    'if x then y else f z'
    'if x then y else f @ z'

Shift parses as (per longest-parse rule):
    'if x then y else (f z)'
    'if x then y else (f @ z)'

-------------------------------------------------------------------------------

state 203 contains 27 shift/reduce conflicts.

        aexp2 -> TH_TY_QUOTE . tyvar
        aexp2 -> TH_TY_QUOTE . gtycon
    *** aexp2 -> TH_TY_QUOTE .

    Conflicts: two single quotes is error syntax with specific error message.

Example of ambiguity:
    'x = '''
    'x = ''a'
    'x = ''T'

Shift parses as (per longest-parse rule):
    'x = ''a'
    'x = ''T'

-------------------------------------------------------------------------------

state 299 contains 1 shift/reduce conflicts.

        rule -> STRING . rule_activation rule_forall infixexp '=' exp

    Conflict: '[' (empty rule_activation reduces)

We don't know whether the '[' starts the activation or not: it
might be the start of the declaration with the activation being
empty.  --SDM 1/4/2002

Example ambiguity:
    '{-# RULE [0] f = ... #-}'

We parse this as having a [0] rule activation for rewriting 'f', rather
a rule instructing how to rewrite the expression '[0] f'.

-------------------------------------------------------------------------------

state 309 contains 1 shift/reduce conflict.

    *** type -> btype .
        type -> btype . '->' ctype

    Conflict: '->'

Same as state 61 but without contexts.

-------------------------------------------------------------------------------

state 353 contains 1 shift/reduce conflicts.

        tup_exprs -> commas . tup_tail
        sysdcon_nolist -> '(' commas . ')'
        commas -> commas . ','

    Conflict: ')' (empty tup_tail reduces)

A tuple section with NO free variables '(,,)' is indistinguishable
from the Haskell98 data constructor for a tuple.  Shift resolves in
favor of sysdcon, which is good because a tuple section will get rejected
if -XTupleSections is not specified.

-------------------------------------------------------------------------------

state 408 contains 1 shift/reduce conflicts.

        tup_exprs -> commas . tup_tail
        sysdcon_nolist -> '(#' commas . '#)'
        commas -> commas . ','

    Conflict: '#)' (empty tup_tail reduces)

Same as State 354 for unboxed tuples.

-------------------------------------------------------------------------------

state 416 contains 67 shift/reduce conflicts.

    *** exp10 -> '-' fexp .
        fexp -> fexp . aexp
        fexp -> fexp . TYPEAPP atype

Same as 149 but with a unary minus.

-------------------------------------------------------------------------------

state 481 contains 1 shift/reduce conflict.

        oqtycon -> '(' qtyconsym . ')'
    *** qtyconop -> qtyconsym .

    Conflict: ')'

Example ambiguity: 'foo :: (:%)'

Shift means '(:%)' gets parsed as a type constructor, rather than than a
parenthesized infix type expression of length 1.

-------------------------------------------------------------------------------

state 678 contains 1 shift/reduce conflicts.

    *** aexp2 -> ipvar .
        dbind -> ipvar . '=' exp

    Conflict: '='

Example ambiguity: 'let ?x ...'

The parser can't tell whether the ?x is the lhs of a normal binding or
an implicit binding.  Fortunately, resolving as shift gives it the only
sensible meaning, namely the lhs of an implicit binding.

-------------------------------------------------------------------------------

state 756 contains 1 shift/reduce conflicts.

        rule -> STRING rule_activation . rule_forall infixexp '=' exp

    Conflict: 'forall' (empty rule_forall reduces)

Example ambiguity: '{-# RULES "name" forall = ... #-}'

'forall' is a valid variable name---we don't know whether
to treat a forall on the input as the beginning of a quantifier
or the beginning of the rule itself.  Resolving to shift means
it's always treated as a quantifier, hence the above is disallowed.
This saves explicitly defining a grammar for the rule lhs that
doesn't include 'forall'.

-------------------------------------------------------------------------------

state 992 contains 1 shift/reduce conflicts.

        transformqual -> 'then' 'group' . 'using' exp
        transformqual -> 'then' 'group' . 'by' exp 'using' exp
    *** special_id -> 'group' .

    Conflict: 'by'

-------------------------------------------------------------------------------

state 1089 contains 1 shift/reduce conflicts.

        rule_foralls -> 'forall' rule_vars '.' . 'forall' rule_vars '.'
    *** rule_foralls -> 'forall' rule_vars '.' .

    Conflict: 'forall'

Example ambigutiy: '{-# RULES "name" forall a. forall ... #-}'

Here the parser cannot tell whether the second 'forall' is the beginning of
a term-level quantifier, for example:

'{-# RULES "name" forall a. forall x. id @a x = x #-}'

or a valid variable named 'forall', for example a function @:: Int -> Int@

'{-# RULES "name" forall a. forall 0 = 0 #-}'

Shift means the parser only allows the former. Also see conflict 753 above.

-------------------------------------------------------------------------------

state 1390 contains 1 shift/reduce conflict.

    *** atype -> tyvar .
        tv_bndr -> '(' tyvar . '::' kind ')'

    Conflict: '::'

Example ambiguity: 'class C a where type D a = ( a :: * ...'

Here the parser cannot tell whether this is specifying a default for the
associated type like:

'class C a where type D a = ( a :: * ); type D a'

or it is an injectivity signature like:

'class C a where type D a = ( r :: * ) | r -> a'

Shift means the parser only allows the latter.

-------------------------------------------------------------------------------
-- API Annotations
--

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

If you modify the parser and want to ensure that the API annotations are processed
correctly, see the README in (REPO)/utils/check-api-annotations for details on
how to set up a test using the check-api-annotations utility, and interpret the
output it generates.

Note [Parsing lists]
---------------------
You might be wondering why we spend so much effort encoding our lists this
way:

importdecls
        : importdecls ';' importdecl
        | importdecls ';'
        | importdecl
        | {- empty -}

This might seem like an awfully roundabout way to declare a list; plus, to add
insult to injury you have to reverse the results at the end.  The answer is that
left recursion prevents us from running out of stack space when parsing long
sequences.  See: https://www.haskell.org/happy/doc/html/sec-sequences.html for
more guidance.

By adding/removing branches, you can affect what lists are accepted.  Here
are the most common patterns, rewritten as regular expressions for clarity:

    -- Equivalent to: ';'* (x ';'+)* x?  (can be empty, permits leading/trailing semis)
    xs : xs ';' x
       | xs ';'
       | x
       | {- empty -}

    -- Equivalent to x (';' x)* ';'*  (non-empty, permits trailing semis)
    xs : xs ';' x
       | xs ';'
       | x

    -- Equivalent to ';'* alts (';' alts)* ';'* (non-empty, permits leading/trailing semis)
    alts : alts1
         | ';' alts
    alts1 : alts1 ';' alt
          | alts1 ';'
          | alt

    -- Equivalent to x (',' x)+ (non-empty, no trailing semis)
    xs : x
       | x ',' xs

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

 'forall'       { L _ (ITforall _) }                -- GHC extension keywords
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
 'stock'        { L _ ITstock }    -- for DerivingStrategies extension
 'anyclass'     { L _ ITanyclass } -- for DerivingStrategies extension
 'via'          { L _ ITvia }      -- for DerivingStrategies extension

 'unit'         { L _ ITunit }
 'signature'    { L _ ITsignature }
 'dependency'   { L _ ITdependency }

 '{-# INLINE'             { L _ (ITinline_prag _ _ _) } -- INLINE or INLINABLE
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
 '{-# MINIMAL'            { L _ (ITminimal_prag _) }
 '{-# CTYPE'              { L _ (ITctype _) }
 '{-# OVERLAPPING'        { L _ (IToverlapping_prag _) }
 '{-# OVERLAPPABLE'       { L _ (IToverlappable_prag _) }
 '{-# OVERLAPS'           { L _ (IToverlaps_prag _) }
 '{-# INCOHERENT'         { L _ (ITincoherent_prag _) }
 '{-# COMPLETE'           { L _ (ITcomplete_prag _)   }
 '#-}'                    { L _ ITclose_prag }

 '..'           { L _ ITdotdot }                        -- reserved symbols
 ':'            { L _ ITcolon }
 '::'           { L _ (ITdcolon _) }
 '='            { L _ ITequal }
 '\\'           { L _ ITlam }
 'lcase'        { L _ ITlcase }
 '|'            { L _ ITvbar }
 '<-'           { L _ (ITlarrow _) }
 '->'           { L _ (ITrarrow _) }
 '@'            { L _ ITat }
 '~'            { L _ ITtilde }
 '=>'           { L _ (ITdarrow _) }
 '-'            { L _ ITminus }
 '!'            { L _ ITbang }
 '*'            { L _ (ITstar _) }
 '-<'           { L _ (ITlarrowtail _) }            -- for arrow notation
 '>-'           { L _ (ITrarrowtail _) }            -- for arrow notation
 '-<<'          { L _ (ITLarrowtail _) }            -- for arrow notation
 '>>-'          { L _ (ITRarrowtail _) }            -- for arrow notation
 '.'            { L _ ITdot }
 TYPEAPP        { L _ ITtypeApp }

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
 '(|'           { L _ (IToparenbar _) }
 '|)'           { L _ (ITcparenbar _) }
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

 IPDUPVARID     { L _ (ITdupipvarid   _) }              -- GHC extension
 LABELVARID     { L _ (ITlabelvarid   _) }

 CHAR           { L _ (ITchar   _ _) }
 STRING         { L _ (ITstring _ _) }
 INTEGER        { L _ (ITinteger _) }
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
'[|'            { L _ (ITopenExpQuote _ _) }
'[p|'           { L _ ITopenPatQuote  }
'[t|'           { L _ ITopenTypQuote  }
'[d|'           { L _ ITopenDecQuote  }
'|]'            { L _ (ITcloseQuote _) }
'[||'           { L _ (ITopenTExpQuote _) }
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
%name parseSignature signature
%name parseImport importdecl
%name parseStatement stmt
%name parseDeclaration topdecl
%name parseExpression exp
%name parsePattern pat
%name parseTypeSignature sigdecl
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%name parseType ktype
%name parseBackpack backpack
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
                               [mop $1,mu AnnRarrow $2,mcp $3] }
    | '(' '~' ')'       {% ams (sLL $1 $> $ eqTyCon_RDR)
                               [mop $1,mj AnnTilde $2,mcp $3] }

-----------------------------------------------------------------------------
-- Backpack stuff

backpack :: { [LHsUnit PackageName] }
         : implicit_top units close { fromOL $2 }
         | '{' units '}'            { fromOL $2 }

units :: { OrdList (LHsUnit PackageName) }
         : units ';' unit { $1 `appOL` unitOL $3 }
         | units ';'      { $1 }
         | unit           { unitOL $1 }

unit :: { LHsUnit PackageName }
        : 'unit' pkgname 'where' unitbody
            { sL1 $1 $ HsUnit { hsunitName = $2
                              , hsunitBody = fromOL $4 } }

unitid :: { LHsUnitId PackageName }
        : pkgname                  { sL1 $1 $ HsUnitId $1 [] }
        | pkgname '[' msubsts ']'  { sLL $1 $> $ HsUnitId $1 (fromOL $3) }

msubsts :: { OrdList (LHsModuleSubst PackageName) }
        : msubsts ',' msubst { $1 `appOL` unitOL $3 }
        | msubsts ','        { $1 }
        | msubst             { unitOL $1 }

msubst :: { LHsModuleSubst PackageName }
        : modid '=' moduleid { sLL $1 $> $ ($1, $3) }
        | modid VARSYM modid VARSYM { sLL $1 $> $ ($1, sLL $2 $> $ HsModuleVar $3) }

moduleid :: { LHsModuleId PackageName }
          : VARSYM modid VARSYM { sLL $1 $> $ HsModuleVar $2 }
          | unitid ':' modid    { sLL $1 $> $ HsModuleId $1 $3 }

pkgname :: { Located PackageName }
        : STRING     { sL1 $1 $ PackageName (getSTRING $1) }
        | litpkgname { sL1 $1 $ PackageName (unLoc $1) }

litpkgname_segment :: { Located FastString }
        : VARID  { sL1 $1 $ getVARID $1 }
        | CONID  { sL1 $1 $ getCONID $1 }
        | special_id { $1 }

litpkgname :: { Located FastString }
        : litpkgname_segment { $1 }
        -- a bit of a hack, means p - b is parsed same as p-b, enough for now.
        | litpkgname_segment '-' litpkgname  { sLL $1 $> $ appendFS (unLoc $1) (consFS '-' (unLoc $3)) }

mayberns :: { Maybe [LRenaming] }
        : {- empty -} { Nothing }
        | '(' rns ')' { Just (fromOL $2) }

rns :: { OrdList LRenaming }
        : rns ',' rn { $1 `appOL` unitOL $3 }
        | rns ','    { $1 }
        | rn         { unitOL $1 }

rn :: { LRenaming }
        : modid 'as' modid { sLL $1 $> $ Renaming $1 (Just $3) }
        | modid            { sL1 $1    $ Renaming $1 Nothing }

unitbody :: { OrdList (LHsUnitDecl PackageName) }
        : '{'     unitdecls '}'   { $2 }
        | vocurly unitdecls close { $2 }

unitdecls :: { OrdList (LHsUnitDecl PackageName) }
        : unitdecls ';' unitdecl { $1 `appOL` unitOL $3 }
        | unitdecls ';'         { $1 }
        | unitdecl              { unitOL $1 }

unitdecl :: { LHsUnitDecl PackageName }
        : maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
             -- XXX not accurate
             { sL1 $2 $ DeclD ModuleD $3 (Just (sL1 $2 (HsModule (Just $3) $5 (fst $ snd $7) (snd $ snd $7) $4 $1))) }
        | maybedocheader 'signature' modid maybemodwarning maybeexports 'where' body
             { sL1 $2 $ DeclD SignatureD $3 (Just (sL1 $2 (HsModule (Just $3) $5 (fst $ snd $7) (snd $ snd $7) $4 $1))) }
        -- NB: MUST have maybedocheader here, otherwise shift-reduce conflict
        -- will prevent us from parsing both forms.
        | maybedocheader 'module' modid
             { sL1 $2 $ DeclD ModuleD $3 Nothing }
        | maybedocheader 'signature' modid
             { sL1 $2 $ DeclD SignatureD $3 Nothing }
        | 'dependency' unitid mayberns
             { sL1 $1 $ IncludeD (IncludeDecl { idUnitId = $2
                                              , idModRenaming = $3
                                              , idSignatureInclude = False }) }
        | 'dependency' 'signature' unitid
             { sL1 $1 $ IncludeD (IncludeDecl { idUnitId = $3
                                              , idModRenaming = Nothing
                                              , idSignatureInclude = True }) }

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

signature :: { Located (HsModule GhcPs) }
       : maybedocheader 'signature' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (cL loc (HsModule (Just $3) $5 (fst $ snd $7)
                              (snd $ snd $7) $4 $1)
                    )
                    ([mj AnnSignature $2, mj AnnWhere $6] ++ fst $7) }

module :: { Located (HsModule GhcPs) }
       : maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (cL loc (HsModule (Just $3) $5 (fst $ snd $7)
                              (snd $ snd $7) $4 $1)
                    )
                    ([mj AnnModule $2, mj AnnWhere $6] ++ fst $7) }
        | body2
                {% fileSrcSpan >>= \ loc ->
                   ams (cL loc (HsModule Nothing Nothing
                               (fst $ snd $1) (snd $ snd $1) Nothing Nothing))
                       (fst $1) }

maybedocheader :: { Maybe LHsDocString }
        : moduleheader            { $1 }
        | {- empty -}             { Nothing }

missing_module_keyword :: { () }
        : {- empty -}                           {% pushModuleContext }

implicit_top :: { () }
        : {- empty -}                           {% pushModuleContext }

maybemodwarning :: { Maybe (Located WarningTxt) }
    : '{-# DEPRECATED' strings '#-}'
                      {% ajs (Just (sLL $1 $> $ DeprecatedTxt (sL1 $1 (getDEPRECATED_PRAGs $1)) (snd $ unLoc $2)))
                             (mo $1:mc $3: (fst $ unLoc $2)) }
    | '{-# WARNING' strings '#-}'
                         {% ajs (Just (sLL $1 $> $ WarningTxt (sL1 $1 (getWARNING_PRAGs $1)) (snd $ unLoc $2)))
                                (mo $1:mc $3 : (fst $ unLoc $2)) }
    |  {- empty -}                  { Nothing }

body    :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        :  '{'            top '}'      { (moc $1:mcc $3:(fst $2)
                                         , snd $2) }
        |      vocurly    top close    { (fst $2, snd $2) }

body2   :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        :  '{' top '}'                          { (moc $1:mcc $3
                                                   :(fst $2), snd $2) }
        |  missing_module_keyword top close     { ([],snd $2) }


top     :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        : semis top1                            { ($1, $2) }

top1    :: { ([LImportDecl GhcPs], [LHsDecl GhcPs]) }
        : importdecls_semi topdecls_semi        { (reverse $1, cvTopDecls $2) }
        | importdecls_semi topdecls             { (reverse $1, cvTopDecls $2) }
        | importdecls                           { (reverse $1, []) }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header  :: { Located (HsModule GhcPs) }
        : maybedocheader 'module' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (cL loc (HsModule (Just $3) $5 $7 [] $4 $1
                          )) [mj AnnModule $2,mj AnnWhere $6] }
        | maybedocheader 'signature' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (cL loc (HsModule (Just $3) $5 $7 [] $4 $1
                          )) [mj AnnModule $2,mj AnnWhere $6] }
        | header_body2
                {% fileSrcSpan >>= \ loc ->
                   return (cL loc (HsModule Nothing Nothing $1 [] Nothing
                          Nothing)) }

header_body :: { [LImportDecl GhcPs] }
        :  '{'            header_top            { $2 }
        |      vocurly    header_top            { $2 }

header_body2 :: { [LImportDecl GhcPs] }
        :  '{' header_top                       { $2 }
        |  missing_module_keyword header_top    { $2 }

header_top :: { [LImportDecl GhcPs] }
        :  semis header_top_importdecls         { $2 }

header_top_importdecls :: { [LImportDecl GhcPs] }
        :  importdecls_semi                     { $1 }
        |  importdecls                          { $1 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { (Maybe (Located [LIE GhcPs])) }
        :  '(' exportlist ')'       {% amsL (comb2 $1 $>) [mop $1,mcp $3] >>
                                       return (Just (sLL $1 $> (fromOL $2))) }
        |  {- empty -}              { Nothing }

exportlist :: { OrdList (LIE GhcPs) }
        : expdoclist ',' expdoclist   {% addAnnotation (oll $1) AnnComma (gl $2)
                                         >> return ($1 `appOL` $3) }
        | exportlist1                 { $1 }

exportlist1 :: { OrdList (LIE GhcPs) }
        : expdoclist export expdoclist ',' exportlist1
                          {% (addAnnotation (oll ($1 `appOL` $2 `appOL` $3))
                                            AnnComma (gl $4) ) >>
                              return ($1 `appOL` $2 `appOL` $3 `appOL` $5) }
        | expdoclist export expdoclist             { $1 `appOL` $2 `appOL` $3 }
        | expdoclist                               { $1 }

expdoclist :: { OrdList (LIE GhcPs) }
        : exp_doc expdoclist                           { $1 `appOL` $2 }
        | {- empty -}                                  { nilOL }

exp_doc :: { OrdList (LIE GhcPs) }
        : docsection    { unitOL (sL1 $1 (case (unLoc $1) of (n, doc) -> IEGroup noExt n doc)) }
        | docnamed      { unitOL (sL1 $1 (IEDocNamed noExt ((fst . unLoc) $1))) }
        | docnext       { unitOL (sL1 $1 (IEDoc noExt (unLoc $1))) }


   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export  :: { OrdList (LIE GhcPs) }
        : qcname_ext export_subspec  {% mkModuleImpExp $1 (snd $ unLoc $2)
                                          >>= \ie -> amsu (sLL $1 $> ie) (fst $ unLoc $2) }
        |  'module' modid            {% amsu (sLL $1 $> (IEModuleContents noExt $2))
                                             [mj AnnModule $1] }
        |  'pattern' qcon            {% amsu (sLL $1 $> (IEVar noExt (sLL $1 $> (IEPattern $2))))
                                             [mj AnnPattern $1] }

export_subspec :: { Located ([AddAnn],ImpExpSubSpec) }
        : {- empty -}             { sL0 ([],ImpExpAbs) }
        | '(' qcnames ')'         {% mkImpExpSubSpec (reverse (snd $2))
                                      >>= \(as,ie) -> return $ sLL $1 $>
                                            (as ++ [mop $1,mcp $3] ++ fst $2, ie) }


qcnames :: { ([AddAnn], [Located ImpExpQcSpec]) }
  : {- empty -}                   { ([],[]) }
  | qcnames1                      { $1 }

qcnames1 :: { ([AddAnn], [Located ImpExpQcSpec]) }     -- A reversed list
        :  qcnames1 ',' qcname_ext_w_wildcard  {% case (head (snd $1)) of
                                                    l@(dL->L _ ImpExpQcWildcard) ->
                                                       return ([mj AnnComma $2, mj AnnDotdot l]
                                                               ,(snd (unLoc $3)  : snd $1))
                                                    l -> (ams (head (snd $1)) [mj AnnComma $2] >>
                                                          return (fst $1 ++ fst (unLoc $3),
                                                                  snd (unLoc $3) : snd $1)) }


        -- Annotations re-added in mkImpExpSubSpec
        |  qcname_ext_w_wildcard                   { (fst (unLoc $1),[snd (unLoc $1)]) }

-- Variable, data constructor or wildcard
-- or tagged type constructor
qcname_ext_w_wildcard :: { Located ([AddAnn], Located ImpExpQcSpec) }
        :  qcname_ext               { sL1 $1 ([],$1) }
        |  '..'                     { sL1 $1 ([mj AnnDotdot $1], sL1 $1 ImpExpQcWildcard)  }

qcname_ext :: { Located ImpExpQcSpec }
        :  qcname                   { sL1 $1 (ImpExpQcName $1) }
        |  'type' oqtycon           {% do { n <- mkTypeImpExp $2
                                          ; ams (sLL $1 $> (ImpExpQcType n))
                                                [mj AnnType $1] } }

qcname  :: { Located RdrName }  -- Variable or type constructor
        :  qvar                 { $1 } -- Things which look like functions
                                       -- Note: This includes record selectors but
                                       -- also (-.->), see #11432
        |  oqtycon_no_varcon    { $1 } -- see Note [Type constructors in export list]

-----------------------------------------------------------------------------
-- Import Declarations

-- importdecls and topdecls must contain at least one declaration;
-- top handles the fact that these may be optional.

-- One or more semicolons
semis1  :: { [AddAnn] }
semis1  : semis1 ';'  { mj AnnSemi $2 : $1 }
        | ';'         { [mj AnnSemi $1] }

-- Zero or more semicolons
semis   :: { [AddAnn] }
semis   : semis ';'   { mj AnnSemi $2 : $1 }
        | {- empty -} { [] }

-- No trailing semicolons, non-empty
importdecls :: { [LImportDecl GhcPs] }
importdecls
        : importdecls_semi importdecl
                                { $2 : $1 }

-- May have trailing semicolons, can be empty
importdecls_semi :: { [LImportDecl GhcPs] }
importdecls_semi
        : importdecls_semi importdecl semis1
                                {% ams $2 $3 >> return ($2 : $1) }
        | {- empty -}           { [] }

importdecl :: { LImportDecl GhcPs }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
                {% ams (cL (comb4 $1 $6 (snd $7) $8) $
                  ImportDecl { ideclExt = noExt
                             , ideclSourceSrc = snd $ fst $2
                             , ideclName = $6, ideclPkgQual = snd $5
                             , ideclSource = snd $2, ideclSafe = snd $3
                             , ideclQualified = snd $4, ideclImplicit = False
                             , ideclAs = unLoc (snd $7)
                             , ideclHiding = unLoc $8 })
                   ((mj AnnImport $1 : (fst $ fst $2) ++ fst $3 ++ fst $4
                                    ++ fst $5 ++ fst $7)) }

maybe_src :: { (([AddAnn],SourceText),IsBootInterface) }
        : '{-# SOURCE' '#-}'        { (([mo $1,mc $2],getSOURCE_PRAGs $1)
                                      ,True) }
        | {- empty -}               { (([],NoSourceText),False) }

maybe_safe :: { ([AddAnn],Bool) }
        : 'safe'                                { ([mj AnnSafe $1],True) }
        | {- empty -}                           { ([],False) }

maybe_pkg :: { ([AddAnn],Maybe StringLiteral) }
        : STRING  {% let pkgFS = getSTRING $1 in
                     if looksLikePackageName (unpackFS pkgFS)
                        then return ([mj AnnPackageName $1], Just (StringLiteral (getSTRINGs $1) pkgFS))
                        else parseErrorSDoc (getLoc $1) $ vcat [
                             text "parse error" <> colon <+> quotes (ppr pkgFS),
                             text "Version number or non-alphanumeric" <+>
                             text "character in package name"] }
        | {- empty -}                           { ([],Nothing) }

optqualified :: { ([AddAnn],Bool) }
        : 'qualified'                           { ([mj AnnQualified $1],True)  }
        | {- empty -}                           { ([],False) }

maybeas :: { ([AddAnn],Located (Maybe (Located ModuleName))) }
        : 'as' modid                           { ([mj AnnAs $1]
                                                 ,sLL $1 $> (Just $2)) }
        | {- empty -}                          { ([],noLoc Nothing) }

maybeimpspec :: { Located (Maybe (Bool, Located [LIE GhcPs])) }
        : impspec                  {% let (b, ie) = unLoc $1 in
                                       checkImportSpec ie
                                        >>= \checkedIe ->
                                          return (cL (gl $1) (Just (b, checkedIe)))  }
        | {- empty -}              { noLoc Nothing }

impspec :: { Located (Bool, Located [LIE GhcPs]) }
        :  '(' exportlist ')'               {% ams (sLL $1 $> (False,
                                                      sLL $1 $> $ fromOL $2))
                                                   [mop $1,mcp $3] }
        |  'hiding' '(' exportlist ')'      {% ams (sLL $1 $> (True,
                                                      sLL $1 $> $ fromOL $3))
                                               [mj AnnHiding $1,mop $2,mcp $4] }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec    :: { Located (SourceText,Int) }
        : {- empty -}           { noLoc (NoSourceText,9) }
        | INTEGER
                 { sL1 $1 (getINTEGERs $1,fromInteger (il_value (getINTEGER $1))) }

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

-- No trailing semicolons, non-empty
topdecls :: { OrdList (LHsDecl GhcPs) }
        : topdecls_semi topdecl        { $1 `snocOL` $2 }

-- May have trailing semicolons, can be empty
topdecls_semi :: { OrdList (LHsDecl GhcPs) }
        : topdecls_semi topdecl semis1 {% ams $2 $3 >> return ($1 `snocOL` $2) }
        | {- empty -}                  { nilOL }

topdecl :: { LHsDecl GhcPs }
        : cl_decl                               { sL1 $1 (TyClD noExt (unLoc $1)) }
        | ty_decl                               { sL1 $1 (TyClD noExt (unLoc $1)) }
        | inst_decl                             { sL1 $1 (InstD noExt (unLoc $1)) }
        | stand_alone_deriving                  { sLL $1 $> (DerivD noExt (unLoc $1)) }
        | role_annot                            { sL1 $1 (RoleAnnotD noExt (unLoc $1)) }
        | 'default' '(' comma_types0 ')'    {% ams (sLL $1 $> (DefD noExt (DefaultDecl noExt $3)))
                                                         [mj AnnDefault $1
                                                         ,mop $2,mcp $4] }
        | 'foreign' fdecl          {% ams (sLL $1 $> (snd $ unLoc $2))
                                           (mj AnnForeign $1:(fst $ unLoc $2)) }
        | '{-# DEPRECATED' deprecations '#-}'   {% ams (sLL $1 $> $ WarningD noExt (Warnings noExt (getDEPRECATED_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# WARNING' warnings '#-}'          {% ams (sLL $1 $> $ WarningD noExt (Warnings noExt (getWARNING_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# RULES' rules '#-}'               {% ams (sLL $1 $> $ RuleD noExt (HsRules noExt (getRULES_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | annotation { $1 }
        | decl_no_th                            { $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if
        -- it had a $(..) wrapped around it
        | infixexp_top                          { sLL $1 $> $ mkSpliceDecl $1 }

-- Type classes
--
cl_decl :: { LTyClDecl GhcPs }
        : 'class' tycl_hdr fds where_cls
                {% amms (mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 (snd $ unLoc $4))
                        (mj AnnClass $1:(fst $ unLoc $3)++(fst $ unLoc $4)) }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl GhcPs }
           -- ordinary type synonyms
        : 'type' type '=' ktypedoc
                -- Note ktypedoc, not sigtype, on the right of '='
                -- We allow an explicit for-all but we don't insert one
                -- in   type Foo a = (b,b)
                -- Instead we just say b is out of scope
                --
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkTySynonym (comb2 $1 $4) $2 $4)
                        [mj AnnType $1,mj AnnEqual $3] }

           -- type family declarations
        | 'type' 'family' type opt_tyfam_kind_sig opt_injective_info
                          where_type_family
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkFamDecl (comb4 $1 $3 $4 $5) (snd $ unLoc $6) $3
                                   (snd $ unLoc $4) (snd $ unLoc $5))
                        (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)
                           ++ (fst $ unLoc $5) ++ (fst $ unLoc $6)) }

          -- ordinary data type or newtype declaration
        | data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
                {% amms (mkTyData (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                           Nothing (reverse (snd $ unLoc $4))
                                   (fmap reverse $5))
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                        ((fst $ unLoc $1):(fst $ unLoc $4)) }

          -- ordinary GADT declaration
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% amms (mkTyData (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2 $3
                            (snd $ unLoc $4) (snd $ unLoc $5)
                            (fmap reverse $6) )
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                    ((fst $ unLoc $1):(fst $ unLoc $4)++(fst $ unLoc $5)) }

          -- data/newtype family
        | 'data' 'family' type opt_datafam_kind_sig
                {% amms (mkFamDecl (comb3 $1 $2 $4) DataFamily $3
                                   (snd $ unLoc $4) Nothing)
                        (mj AnnData $1:mj AnnFamily $2:(fst $ unLoc $4)) }

inst_decl :: { LInstDecl GhcPs }
        : 'instance' overlap_pragma inst_type where_inst
       {% do { (binds, sigs, _, ats, adts, _) <- cvBindsAndSigs (snd $ unLoc $4)
             ; let cid = ClsInstDecl { cid_ext = noExt
                                     , cid_poly_ty = $3, cid_binds = binds
                                     , cid_sigs = mkClassOpSigs sigs
                                     , cid_tyfam_insts = ats
                                     , cid_overlap_mode = $2
                                     , cid_datafam_insts = adts }
             ; ams (cL (comb3 $1 (hsSigType $3) $4) (ClsInstD { cid_d_ext = noExt, cid_inst = cid }))
                   (mj AnnInstance $1 : (fst $ unLoc $4)) } }

           -- type instance declarations
        | 'type' 'instance' ty_fam_inst_eqn
                {% ams $3 (fst $ unLoc $3)
                >> amms (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3))
                    (mj AnnType $1:mj AnnInstance $2:(fst $ unLoc $3)) }

          -- data/newtype instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr_inst constrs
                          maybe_derivings
            {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 $4
                                      Nothing (reverse (snd  $ unLoc $5))
                                              (fmap reverse $6))
                    ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)) }

          -- GADT instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr_inst opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3 $4
                                   (snd $ unLoc $5) (snd $ unLoc $6)
                                   (fmap reverse $7))
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

deriv_strategy_no_via :: { LDerivStrategy GhcPs }
  : 'stock'                     {% ams (sL1 $1 StockStrategy)
                                       [mj AnnStock $1] }
  | 'anyclass'                  {% ams (sL1 $1 AnyclassStrategy)
                                       [mj AnnAnyclass $1] }
  | 'newtype'                   {% ams (sL1 $1 NewtypeStrategy)
                                       [mj AnnNewtype $1] }

deriv_strategy_via :: { LDerivStrategy GhcPs }
  : 'via' type              {% ams (sLL $1 $> (ViaStrategy (mkLHsSigType $2)))
                                            [mj AnnVia $1] }

deriv_standalone_strategy :: { Maybe (LDerivStrategy GhcPs) }
  : 'stock'                     {% ajs (Just (sL1 $1 StockStrategy))
                                       [mj AnnStock $1] }
  | 'anyclass'                  {% ajs (Just (sL1 $1 AnyclassStrategy))
                                       [mj AnnAnyclass $1] }
  | 'newtype'                   {% ajs (Just (sL1 $1 NewtypeStrategy))
                                       [mj AnnNewtype $1] }
  | deriv_strategy_via          { Just $1 }
  | {- empty -}                 { Nothing }

-- Injective type families

opt_injective_info :: { Located ([AddAnn], Maybe (LInjectivityAnn GhcPs)) }
        : {- empty -}               { noLoc ([], Nothing) }
        | '|' injectivity_cond      { sLL $1 $> ([mj AnnVbar $1]
                                                , Just ($2)) }

injectivity_cond :: { LInjectivityAnn GhcPs }
        : tyvarid '->' inj_varids
           {% ams (sLL $1 $> (InjectivityAnn $1 (reverse (unLoc $3))))
                  [mu AnnRarrow $2] }

inj_varids :: { Located [Located RdrName] }
        : inj_varids tyvarid  { sLL $1 $> ($2 : unLoc $1) }
        | tyvarid             { sLL $1 $> [$1]            }

-- Closed type families

where_type_family :: { Located ([AddAnn],FamilyInfo GhcPs) }
        : {- empty -}                      { noLoc ([],OpenTypeFamily) }
        | 'where' ty_fam_inst_eqn_list
               { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                    ,ClosedTypeFamily (fmap reverse $ snd $ unLoc $2)) }

ty_fam_inst_eqn_list :: { Located ([AddAnn],Maybe [LTyFamInstEqn GhcPs]) }
        :     '{' ty_fam_inst_eqns '}'     { sLL $1 $> ([moc $1,mcc $3]
                                                ,Just (unLoc $2)) }
        | vocurly ty_fam_inst_eqns close   { let (dL->L loc _) = $2 in
                                             cL loc ([],Just (unLoc $2)) }
        |     '{' '..' '}'                 { sLL $1 $> ([moc $1,mj AnnDotdot $2
                                                 ,mcc $3],Nothing) }
        | vocurly '..' close               { let (dL->L loc _) = $2 in
                                             cL loc ([mj AnnDotdot $2],Nothing) }

ty_fam_inst_eqns :: { Located [LTyFamInstEqn GhcPs] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
                                      {% let (dL->L loc (anns, eqn)) = $3 in
                                         asl (unLoc $1) $2 (cL loc eqn)
                                         >> ams $3 anns
                                         >> return (sLL $1 $> (cL loc eqn : unLoc $1)) }
        | ty_fam_inst_eqns ';'        {% addAnnotation (gl $1) AnnSemi (gl $2)
                                         >> return (sLL $1 $>  (unLoc $1)) }
        | ty_fam_inst_eqn             {% let (dL->L loc (anns, eqn)) = $1 in
                                         ams $1 anns
                                         >> return (sLL $1 $> [cL loc eqn]) }
        | {- empty -}                 { noLoc [] }

ty_fam_inst_eqn :: { Located ([AddAnn],TyFamInstEqn GhcPs) }
        : 'forall' tv_bndrs '.' type '=' ktype
              {% do { hintExplicitForall (getLoc $1)
                    ; (eqn,ann) <- mkTyFamInstEqn (Just $2) $4 $6
                    ; ams (sLL $4 $> (mj AnnEqual $5:ann, eqn))
                          [mu AnnForall $1, mj AnnDot $3]  } }
        | type '=' ktype
              {% do { (eqn,ann) <- mkTyFamInstEqn Nothing $1 $3
                    ; return (sLL $1 $> (mj AnnEqual $2:ann, eqn))  } }
              -- Note the use of type for the head; this allows
              -- infix type constructors and type patterns

-- Associated type family declarations
--
-- * They have a different syntax than on the toplevel (no family special
--   identifier).
--
-- * They also need to be separate from instances; otherwise, data family
--   declarations without a kind signature cause parsing conflicts with empty
--   data declarations.
--
at_decl_cls :: { LHsDecl GhcPs }
        :  -- data family declarations, with optional 'family' keyword
          'data' opt_family type opt_datafam_kind_sig
                {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4) DataFamily $3
                                                  (snd $ unLoc $4) Nothing))
                        (mj AnnData $1:$2++(fst $ unLoc $4)) }

           -- type family declarations, with optional 'family' keyword
           -- (can't use opt_instance because you get shift/reduce errors
        | 'type' type opt_at_kind_inj_sig
               {% amms (liftM mkTyClD
                        (mkFamDecl (comb3 $1 $2 $3) OpenTypeFamily $2
                                   (fst . snd $ unLoc $3)
                                   (snd . snd $ unLoc $3)))
                       (mj AnnType $1:(fst $ unLoc $3)) }
        | 'type' 'family' type opt_at_kind_inj_sig
               {% amms (liftM mkTyClD
                        (mkFamDecl (comb3 $1 $3 $4) OpenTypeFamily $3
                                   (fst . snd $ unLoc $4)
                                   (snd . snd $ unLoc $4)))
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

opt_instance :: { [AddAnn] }
              : {- empty -} { [] }
              | 'instance'  { [mj AnnInstance $1] }

-- Associated type instances
--
at_decl_inst :: { LInstDecl GhcPs }
           -- type instance declarations, with optional 'instance' keyword
        : 'type' opt_instance ty_fam_inst_eqn
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
                {% ams $3 (fst $ unLoc $3) >>
                   amms (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3))
                        (mj AnnType $1:$2++(fst $ unLoc $3)) }

        -- data/newtype instance declaration, with optional 'instance' keyword
        -- (can't use opt_instance because you get reduce/reduce errors)
        | data_or_newtype capi_ctype tycl_hdr_inst constrs maybe_derivings
               {% amms (mkDataFamInst (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                                    Nothing (reverse (snd $ unLoc $4))
                                            (fmap reverse $5))
                       ((fst $ unLoc $1):(fst $ unLoc $4)) }

        | data_or_newtype 'instance' capi_ctype tycl_hdr_inst constrs maybe_derivings
               {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 $4
                                    Nothing (reverse (snd $ unLoc $5))
                                            (fmap reverse $6))
                       ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)) }

        -- GADT instance declaration, with optional 'instance' keyword
        -- (can't use opt_instance because you get reduce/reduce errors)
        | data_or_newtype capi_ctype tycl_hdr_inst opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
                {% amms (mkDataFamInst (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2
                                $3 (snd $ unLoc $4) (snd $ unLoc $5)
                                (fmap reverse $6))
                        ((fst $ unLoc $1):(fst $ unLoc $4)++(fst $ unLoc $5)) }

        | data_or_newtype 'instance' capi_ctype tycl_hdr_inst opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
                {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3
                                $4 (snd $ unLoc $5) (snd $ unLoc $6)
                                (fmap reverse $7))
                        ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)++(fst $ unLoc $6)) }

data_or_newtype :: { Located (AddAnn, NewOrData) }
        : 'data'        { sL1 $1 (mj AnnData    $1,DataType) }
        | 'newtype'     { sL1 $1 (mj AnnNewtype $1,NewType) }

-- Family result/return kind signatures

opt_kind_sig :: { Located ([AddAnn], Maybe (LHsKind GhcPs)) }
        :               { noLoc     ([]               , Nothing) }
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], Just $2) }

opt_datafam_kind_sig :: { Located ([AddAnn], LFamilyResultSig GhcPs) }
        :               { noLoc     ([]               , noLoc (NoSig noExt)         )}
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], sLL $1 $> (KindSig noExt $2))}

opt_tyfam_kind_sig :: { Located ([AddAnn], LFamilyResultSig GhcPs) }
        :              { noLoc     ([]               , noLoc     (NoSig    noExt)   )}
        | '::' kind    { sLL $1 $> ([mu AnnDcolon $1], sLL $1 $> (KindSig  noExt $2))}
        | '='  tv_bndr { sLL $1 $> ([mj AnnEqual $1] , sLL $1 $> (TyVarSig noExt $2))}

opt_at_kind_inj_sig :: { Located ([AddAnn], ( LFamilyResultSig GhcPs
                                            , Maybe (LInjectivityAnn GhcPs)))}
        :            { noLoc ([], (noLoc (NoSig noExt), Nothing)) }
        | '::' kind  { sLL $1 $> ( [mu AnnDcolon $1]
                                 , (sLL $2 $> (KindSig noExt $2), Nothing)) }
        | '='  tv_bndr '|' injectivity_cond
                { sLL $1 $> ([mj AnnEqual $1, mj AnnVbar $3]
                            , (sLL $1 $2 (TyVarSig noExt $2), Just $4))}

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--      T a b
--      Eq a => T a
--      (Eq a, Ord b) => T a b
--      T Int [a]                       -- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (Maybe (LHsContext GhcPs), LHsType GhcPs) }
        : context '=>' type         {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                       >> (return (sLL $1 $> (Just $1, $3)))
                                    }
        | type                      { sL1 $1 (Nothing, $1) }

tycl_hdr_inst :: { Located (Maybe (LHsContext GhcPs), Maybe [LHsTyVarBndr GhcPs], LHsType GhcPs) }
        : 'forall' tv_bndrs '.' context '=>' type   {% hintExplicitForall (getLoc $1)
                                                       >> (addAnnotation (gl $4) (toUnicodeAnn AnnDarrow $5) (gl $5)
                                                           >> ams (sLL $1 $> $ (Just $4, Just $2, $6))
                                                                  [mu AnnForall $1, mj AnnDot $3])
                                                    }
        | 'forall' tv_bndrs '.' type   {% hintExplicitForall (getLoc $1)
                                          >> ams (sLL $1 $> $ (Nothing, Just $2, $4))
                                                 [mu AnnForall $1, mj AnnDot $3]
                                       }
        | context '=>' type         {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                       >> (return (sLL $1 $> (Just $1, Nothing, $3)))
                                    }
        | type                      { sL1 $1 (Nothing, Nothing, $1) }


capi_ctype :: { Maybe (Located CType) }
capi_ctype : '{-# CTYPE' STRING STRING '#-}'
                       {% ajs (Just (sLL $1 $> (CType (getCTYPEs $1) (Just (Header (getSTRINGs $2) (getSTRING $2)))
                                        (getSTRINGs $3,getSTRING $3))))
                              [mo $1,mj AnnHeader $2,mj AnnVal $3,mc $4] }

           | '{-# CTYPE'        STRING '#-}'
                       {% ajs (Just (sLL $1 $> (CType (getCTYPEs $1) Nothing  (getSTRINGs $2, getSTRING $2))))
                              [mo $1,mj AnnVal $2,mc $3] }

           |           { Nothing }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl GhcPs }
  : 'deriving' deriv_standalone_strategy 'instance' overlap_pragma inst_type
                {% do { let { err = text "in the stand-alone deriving instance"
                                    <> colon <+> quotes (ppr $5) }
                      ; ams (sLL $1 (hsSigType $>)
                                 (DerivDecl noExt (mkHsWildCardBndrs $5) $2 $4))
                            [mj AnnDeriving $1, mj AnnInstance $3] } }

-----------------------------------------------------------------------------
-- Role annotations

role_annot :: { LRoleAnnotDecl GhcPs }
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
pattern_synonym_decl :: { LHsDecl GhcPs }
        : 'pattern' pattern_synonym_lhs '=' pat
         {%      let (name, args,as ) = $2 in
                 ams (sLL $1 $> . ValD noExt $ mkPatSynBind name args $4
                                                    ImplicitBidirectional)
               (as ++ [mj AnnPattern $1, mj AnnEqual $3])
         }

        | 'pattern' pattern_synonym_lhs '<-' pat
         {%    let (name, args, as) = $2 in
               ams (sLL $1 $> . ValD noExt $ mkPatSynBind name args $4 Unidirectional)
               (as ++ [mj AnnPattern $1,mu AnnLarrow $3]) }

        | 'pattern' pattern_synonym_lhs '<-' pat where_decls
            {% do { let (name, args, as) = $2
                  ; mg <- mkPatSynMatchGroup name (snd $ unLoc $5)
                  ; ams (sLL $1 $> . ValD noExt $
                           mkPatSynBind name args $4 (ExplicitBidirectional mg))
                       (as ++ ((mj AnnPattern $1:mu AnnLarrow $3:(fst $ unLoc $5))) )
                   }}

pattern_synonym_lhs :: { (Located RdrName, HsPatSynDetails (Located RdrName), [AddAnn]) }
        : con vars0 { ($1, PrefixCon $2, []) }
        | varid conop varid { ($2, InfixCon $1 $3, []) }
        | con '{' cvars1 '}' { ($1, RecCon $3, [moc $2, mcc $4] ) }

vars0 :: { [Located RdrName] }
        : {- empty -}                 { [] }
        | varid vars0                 { $1 : $2 }

cvars1 :: { [RecordPatSynField (Located RdrName)] }
       : var                          { [RecordPatSynField $1 $1] }
       | var ',' cvars1               {% addAnnotation (getLoc $1) AnnComma (getLoc $2) >>
                                         return ((RecordPatSynField $1 $1) : $3 )}

where_decls :: { Located ([AddAnn]
                         , Located (OrdList (LHsDecl GhcPs))) }
        : 'where' '{' decls '}'       { sLL $1 $> ((mj AnnWhere $1:moc $2
                                           :mcc $4:(fst $ unLoc $3)),sL1 $3 (snd $ unLoc $3)) }
        | 'where' vocurly decls close { cL (comb2 $1 $3) ((mj AnnWhere $1:(fst $ unLoc $3))
                                          ,sL1 $3 (snd $ unLoc $3)) }

pattern_synonym_sig :: { LSig GhcPs }
        : 'pattern' con_list '::' sigtypedoc
                   {% ams (sLL $1 $> $ PatSynSig noExt (unLoc $2) (mkLHsSigType $4))
                          [mj AnnPattern $1, mu AnnDcolon $3] }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { LHsDecl GhcPs }
decl_cls  : at_decl_cls                 { $1 }
          | decl                        { $1 }

          -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtypedoc
                    {% do { v <- checkValSigLhs $2
                          ; let err = text "in default signature" <> colon <+>
                                      quotes (ppr $2)
                          ; ams (sLL $1 $> $ SigD noExt $ ClassOpSig noExt True [v] $ mkLHsSigType $4)
                                [mj AnnDefault $1,mu AnnDcolon $3] } }

decls_cls :: { Located ([AddAnn],OrdList (LHsDecl GhcPs)) }  -- Reversed
          : decls_cls ';' decl_cls      {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                    , unitOL $3))
                                             else ams (lastOL (snd $ unLoc $1)) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (fst $ unLoc $1
                                                                ,(snd $ unLoc $1) `appOL` unitOL $3)) }
          | decls_cls ';'               {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                                   ,snd $ unLoc $1))
                                             else ams (lastOL (snd $ unLoc $1)) [mj AnnSemi $2]
                                           >> return (sLL $1 $>  (unLoc $1)) }
          | decl_cls                    { sL1 $1 ([], unitOL $1) }
          | {- empty -}                 { noLoc ([],nilOL) }

decllist_cls
        :: { Located ([AddAnn]
                     , OrdList (LHsDecl GhcPs)) }      -- Reversed
        : '{'         decls_cls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        |     vocurly decls_cls close   { $2 }

-- Class body
--
where_cls :: { Located ([AddAnn]
                       ,(OrdList (LHsDecl GhcPs))) }    -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_cls          { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl GhcPs)) }
decl_inst  : at_decl_inst               { sLL $1 $> (unitOL (sL1 $1 (InstD noExt (unLoc $1)))) }
           | decl                       { sLL $1 $> (unitOL $1) }

decls_inst :: { Located ([AddAnn],OrdList (LHsDecl GhcPs)) }   -- Reversed
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
                     , OrdList (LHsDecl GhcPs)) }      -- Reversed
        : '{'         decls_inst '}'    { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2),snd $ unLoc $2) }
        |     vocurly decls_inst close  { cL (gl $2) (unLoc $2) }

-- Instance body
--
where_inst :: { Located ([AddAnn]
                        , OrdList (LHsDecl GhcPs)) }   -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_inst         { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,(snd $ unLoc $2)) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in binding groups other than classes and instances
--
decls   :: { Located ([AddAnn],OrdList (LHsDecl GhcPs)) }
        : decls ';' decl    {% if isNilOL (snd $ unLoc $1)
                                 then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                        , unitOL $3))
                                 else do ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (
                                          let { this = unitOL $3;
                                                rest = snd $ unLoc $1;
                                                these = rest `appOL` this }
                                          in rest `seq` this `seq` these `seq`
                                             (sLL $1 $> (fst $ unLoc $1,these))) }
        | decls ';'          {% if isNilOL (snd $ unLoc $1)
                                  then return (sLL $1 $> ((mj AnnSemi $2:(fst $ unLoc $1)
                                                          ,snd $ unLoc $1)))
                                  else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (unLoc $1)) }
        | decl                          { sL1 $1 ([], unitOL $1) }
        | {- empty -}                   { noLoc ([],nilOL) }

decllist :: { Located ([AddAnn],Located (OrdList (LHsDecl GhcPs))) }
        : '{'            decls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                                   ,sL1 $2 $ snd $ unLoc $2) }
        |     vocurly    decls close   { cL (gl $2) (fst $ unLoc $2,sL1 $2 $ snd $ unLoc $2) }

-- Binding groups other than those of class and instance declarations
--
binds   ::  { Located ([AddAnn],Located (HsLocalBinds GhcPs)) }
                                         -- May have implicit parameters
                                                -- No type declarations
        : decllist          {% do { val_binds <- cvBindGroup (unLoc $ snd $ unLoc $1)
                                  ; return (sL1 $1 (fst $ unLoc $1
                                                    ,sL1 $1 $ HsValBinds noExt val_binds)) } }

        | '{'            dbinds '}'     { sLL $1 $> ([moc $1,mcc $3]
                                             ,sL1 $2 $ HsIPBinds noExt (IPBinds noExt (reverse $ unLoc $2))) }

        |     vocurly    dbinds close   { cL (getLoc $2) ([]
                                            ,sL1 $2 $ HsIPBinds noExt (IPBinds noExt (reverse $ unLoc $2))) }


wherebinds :: { Located ([AddAnn],Located (HsLocalBinds GhcPs)) }
                                                -- May have implicit parameters
                                                -- No type declarations
        : 'where' binds                 { sLL $1 $> (mj AnnWhere $1 : (fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],noLoc emptyLocalBinds) }


-----------------------------------------------------------------------------
-- Transformation Rules

rules   :: { OrdList (LRuleDecl GhcPs) }
        :  rules ';' rule              {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `snocOL` $3) }
        |  rules ';'                   {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        |  rule                        { unitOL $1 }
        |  {- empty -}                 { nilOL }

rule    :: { LRuleDecl GhcPs }
        : STRING rule_activation rule_foralls infixexp '=' exp
         {%ams (sLL $1 $> $ HsRule { rd_ext = noExt
                                   , rd_name = cL (gl $1) (getSTRINGs $1, getSTRING $1)
                                   , rd_act = (snd $2) `orElse` AlwaysActive
                                   , rd_tyvs = sndOf3 $3, rd_tmvs = thdOf3 $3
                                   , rd_lhs = $4, rd_rhs = $6 })
               (mj AnnEqual $5 : (fst $2) ++ (fstOf3 $3)) }

-- Rules can be specified to be NeverActive, unlike inline/specialize pragmas
rule_activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | rule_explicit_activation              { (fst $1,Just (snd $1)) }

rule_explicit_activation :: { ([AddAnn]
                              ,Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mos $1,mj AnnVal $2,mcs $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' '~' INTEGER ']'   { ([mos $1,mj AnnTilde $2,mj AnnVal $3,mcs $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }
        | '[' '~' ']'           { ([mos $1,mj AnnTilde $2,mcs $3]
                                  ,NeverActive) }

rule_foralls :: { ([AddAnn], Maybe [LHsTyVarBndr GhcPs], [LRuleBndr GhcPs]) }
        : 'forall' rule_vars '.' 'forall' rule_vars '.'    {% let tyvs = mkRuleTyVarBndrs $2
                                                              in hintExplicitForall (getLoc $1)
                                                              >> checkRuleTyVarBndrNames (mkRuleTyVarBndrs $2)
                                                              >> return ([mu AnnForall $1,mj AnnDot $3,
                                                                          mu AnnForall $4,mj AnnDot $6],
                                                                         Just (mkRuleTyVarBndrs $2), mkRuleBndrs $5) }
        | 'forall' rule_vars '.'                           { ([mu AnnForall $1,mj AnnDot $3],
                                                              Nothing, mkRuleBndrs $2) }
        | {- empty -}                                      { ([], Nothing, []) }

rule_vars :: { [LRuleTyTmVar] }
        : rule_var rule_vars                    { $1 : $2 }
        | {- empty -}                           { [] }

rule_var :: { LRuleTyTmVar }
        : varid                         { sLL $1 $> (RuleTyTmVar $1 Nothing) }
        | '(' varid '::' ctype ')'      {% ams (sLL $1 $> (RuleTyTmVar $2 (Just $4)))
                                               [mop $1,mu AnnDcolon $3,mcp $5] }

{- Note [Parsing explicit foralls in Rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We really want the above definition of rule_foralls to be:

  rule_foralls : 'forall' tv_bndrs '.' 'forall' rule_vars '.'
               | 'forall' rule_vars '.'
               | {- empty -}

where rule_vars (term variables) can be named "forall", "family", or "role",
but tv_vars (type variables) cannot be. However, such a definition results
in a reduce/reduce conflict. For example, when parsing:
> {-# RULE "name" forall a ... #-}
before the '...' it is impossible to determine whether we should be in the
first or second case of the above.

This is resolved by using rule_vars (which is more general) for both, and
ensuring that type-level quantified variables do not have the names "forall",
"family", or "role" in the function 'checkRuleTyVarBndrNames' in RdrHsSyn.hs
Thus, whenever the definition of tyvarid (used for tv_bndrs) is changed relative
to varid (used for rule_vars), 'checkRuleTyVarBndrNames' must be updated.
-}

-----------------------------------------------------------------------------
-- Warnings and deprecations (c.f. rules)

warnings :: { OrdList (LWarnDecl GhcPs) }
        : warnings ';' warning         {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | warnings ';'                 {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | warning                      { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
warning :: { OrdList (LWarnDecl GhcPs) }
        : namelist strings
                {% amsu (sLL $1 $> (Warning noExt (unLoc $1) (WarningTxt (noLoc NoSourceText) $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

deprecations :: { OrdList (LWarnDecl GhcPs) }
        : deprecations ';' deprecation
                                       {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | deprecations ';'             {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | deprecation                  { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LWarnDecl GhcPs) }
        : namelist strings
             {% amsu (sLL $1 $> $ (Warning noExt (unLoc $1) (DeprecatedTxt (noLoc NoSourceText) $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

strings :: { Located ([AddAnn],[Located StringLiteral]) }
    : STRING { sL1 $1 ([],[cL (gl $1) (getStringLiteral $1)]) }
    | '[' stringlist ']' { sLL $1 $> $ ([mos $1,mcs $3],fromOL (unLoc $2)) }

stringlist :: { Located (OrdList (Located StringLiteral)) }
    : stringlist ',' STRING {% addAnnotation (oll $ unLoc $1) AnnComma (gl $2) >>
                               return (sLL $1 $> (unLoc $1 `snocOL`
                                                  (cL (gl $3) (getStringLiteral $3)))) }
    | STRING                { sLL $1 $> (unitOL (cL (gl $1) (getStringLiteral $1))) }
    | {- empty -}           { noLoc nilOL }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl GhcPs }
    : '{-# ANN' name_var aexp '#-}'      {% ams (sLL $1 $> (AnnD noExt $ HsAnnotation noExt
                                            (getANN_PRAGs $1)
                                            (ValueAnnProvenance $2) $3))
                                            [mo $1,mc $4] }

    | '{-# ANN' 'type' tycon aexp '#-}'  {% ams (sLL $1 $> (AnnD noExt $ HsAnnotation noExt
                                            (getANN_PRAGs $1)
                                            (TypeAnnProvenance $3) $4))
                                            [mo $1,mj AnnType $2,mc $5] }

    | '{-# ANN' 'module' aexp '#-}'      {% ams (sLL $1 $> (AnnD noExt $ HsAnnotation noExt
                                                (getANN_PRAGs $1)
                                                 ModuleAnnProvenance $3))
                                                [mo $1,mj AnnModule $2,mc $4] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { Located ([AddAnn],HsDecl GhcPs) }
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
                    ,(Located StringLiteral, Located RdrName, LHsSigType GhcPs)) }
       : STRING var '::' sigtypedoc     { sLL $1 $> ([mu AnnDcolon $3]
                                             ,(cL (getLoc $1)
                                                    (getStringLiteral $1), $2, mkLHsSigType $4)) }
       |        var '::' sigtypedoc     { sLL $1 $> ([mu AnnDcolon $2]
                                             ,(noLoc (StringLiteral NoSourceText nilFS), $1, mkLHsSigType $3)) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { ([AddAnn], Maybe (LHsType GhcPs)) }
        : {- empty -}                   { ([],Nothing) }
        | '::' sigtype                  { ([mu AnnDcolon $1],Just $2) }

opt_tyconsig :: { ([AddAnn], Maybe (Located RdrName)) }
             : {- empty -}              { ([], Nothing) }
             | '::' gtycon              { ([mu AnnDcolon $1], Just $2) }

sigtype :: { LHsType GhcPs }
        : ctype                            { $1 }

sigtypedoc :: { LHsType GhcPs }
        : ctypedoc                         { $1 }


sig_vars :: { Located [Located RdrName] }    -- Returned in reversed order
         : sig_vars ',' var           {% addAnnotation (gl $ head $ unLoc $1)
                                                       AnnComma (gl $2)
                                         >> return (sLL $1 $> ($3 : unLoc $1)) }
         | var                        { sL1 $1 [$1] }

sigtypes1 :: { (OrdList (LHsSigType GhcPs)) }
   : sigtype                 { unitOL (mkLHsSigType $1) }
   | sigtype ',' sigtypes1   {% addAnnotation (gl $1) AnnComma (gl $2)
                                >> return (unitOL (mkLHsSigType $1) `appOL` $3) }

-----------------------------------------------------------------------------
-- Types

unpackedness :: { Located ([AddAnn], SourceText, SrcUnpackedness) }
        : '{-# UNPACK' '#-}'   { sLL $1 $> ([mo $1, mc $2], getUNPACK_PRAGs $1, SrcUnpack) }
        | '{-# NOUNPACK' '#-}' { sLL $1 $> ([mo $1, mc $2], getNOUNPACK_PRAGs $1, SrcNoUnpack) }

-- A ktype/ktypedoc is a ctype/ctypedoc, possibly with a kind annotation
ktype :: { LHsType GhcPs }
        : ctype                { $1 }
        | ctype '::' kind      {% ams (sLL $1 $> $ HsKindSig noExt $1 $3)
                                      [mu AnnDcolon $2] }

ktypedoc :: { LHsType GhcPs }
         : ctypedoc            { $1 }
         | ctypedoc '::' kind  {% ams (sLL $1 $> $ HsKindSig noExt $1 $3)
                                      [mu AnnDcolon $2] }

-- A ctype is a for-all type
ctype   :: { LHsType GhcPs }
        : 'forall' tv_bndrs '.' ctype   {% hintExplicitForall (getLoc $1) >>
                                           ams (sLL $1 $> $
                                                HsForAllTy { hst_bndrs = $2
                                                           , hst_xforall = noExt
                                                           , hst_body = $4 })
                                               [mu AnnForall $1, mj AnnDot $3] }
        | context '=>' ctype          {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                         >> return (sLL $1 $> $
                                            HsQualTy { hst_ctxt = $1
                                                     , hst_xqual = noExt
                                                     , hst_body = $3 }) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy noExt $1 $3))
                                             [mu AnnDcolon $2] }
        | type                        { $1 }

-- Note [ctype and ctypedoc]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It would have been nice to simplify the grammar by unifying `ctype` and
-- ctypedoc` into one production, allowing comments on types everywhere (and
-- rejecting them after parsing, where necessary).  This is however not possible
-- since it leads to ambiguity. The reason is the support for comments on record
-- fields:
--         data R = R { field :: Int -- ^ comment on the field }
-- If we allow comments on types here, it's not clear if the comment applies
-- to 'field' or to 'Int'. So we must use `ctype` to describe the type.

ctypedoc :: { LHsType GhcPs }
        : 'forall' tv_bndrs '.' ctypedoc {% hintExplicitForall (getLoc $1) >>
                                            ams (sLL $1 $> $
                                                 HsForAllTy { hst_bndrs = $2
                                                            , hst_xforall = noExt
                                                            , hst_body = $4 })
                                                [mu AnnForall $1,mj AnnDot $3] }
        | context '=>' ctypedoc       {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                         >> return (sLL $1 $> $
                                            HsQualTy { hst_ctxt = $1
                                                     , hst_xqual = noExt
                                                     , hst_body = $3 }) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy noExt $1 $3))
                                             [mu AnnDcolon $2] }
        | typedoc                     { $1 }

----------------------
-- Notes for 'context'
-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--      (Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>

context :: { LHsContext GhcPs }
        :  btype                        {% do { (anns,ctx) <- checkContext $1
                                                ; if null (unLoc ctx)
                                                   then addAnnotation (gl $1) AnnUnit (gl $1)
                                                   else return ()
                                                ; ams ctx anns
                                                } }

-- See Note [Constr variatons of non-terminals]
constr_context :: { LHsContext GhcPs }
        :  constr_btype                 {% do { (anns,ctx) <- checkContext $1
                                                ; if null (unLoc ctx)
                                                   then addAnnotation (gl $1) AnnUnit (gl $1)
                                                   else return ()
                                                ; ams ctx anns
                                                } }

{- Note [GADT decl discards annotations]
~~~~~~~~~~~~~~~~~~~~~
The type production for

    btype `->`         ctypedoc
    btype docprev `->` ctypedoc

add the AnnRarrow annotation twice, in different places.

This is because if the type is processed as usual, it belongs on the annotations
for the type as a whole.

But if the type is passed to mkGadtDecl, it discards the top level SrcSpan, and
the top-level annotation will be disconnected. Hence for this specific case it
is connected to the first type too.
-}

type :: { LHsType GhcPs }
        : btype                        { $1 }
        | btype '->' ctype             {% ams $1 [mu AnnRarrow $2] -- See note [GADT decl discards annotations]
                                       >> ams (sLL $1 $> $ HsFunTy noExt $1 $3)
                                              [mu AnnRarrow $2] }


typedoc :: { LHsType GhcPs }
        : btype                          { $1 }
        | btype docprev                  { sLL $1 $> $ HsDocTy noExt $1 $2 }
        | docnext btype                  { sLL $1 $> $ HsDocTy noExt $2 $1 }
        | btype '->'     ctypedoc        {% ams $1 [mu AnnRarrow $2] -- See note [GADT decl discards annotations]
                                         >> ams (sLL $1 $> $ HsFunTy noExt $1 $3)
                                                [mu AnnRarrow $2] }
        | btype docprev '->' ctypedoc    {% ams $1 [mu AnnRarrow $3] -- See note [GADT decl discards annotations]
                                         >> ams (sLL $1 $> $
                                                 HsFunTy noExt (cL (comb2 $1 $2)
                                                            (HsDocTy noExt $1 $2))
                                                         $4)
                                                [mu AnnRarrow $3] }
        | docnext btype '->' ctypedoc    {% ams $2 [mu AnnRarrow $3] -- See note [GADT decl discards annotations]
                                         >> ams (sLL $1 $> $
                                                 HsFunTy noExt (cL (comb2 $1 $2)
                                                            (HsDocTy noExt $2 $1))
                                                         $4)
                                                [mu AnnRarrow $3] }

-- See Note [Constr variatons of non-terminals]
constr_btype :: { LHsType GhcPs }
        : constr_tyapps                 {% mergeOps (unLoc $1) }

-- See Note [Constr variatons of non-terminals]
constr_tyapps :: { Located [Located TyEl] } -- NB: This list is reversed
        : constr_tyapp                  { sL1 $1 [$1] }
        | constr_tyapps constr_tyapp    { sLL $1 $> $ $2 : (unLoc $1) }

-- See Note [Constr variatons of non-terminals]
constr_tyapp :: { Located TyEl }
        : tyapp                         { $1 }
        | docprev                       { sL1 $1 $ TyElDocPrev (unLoc $1) }

btype :: { LHsType GhcPs }
        : tyapps                        {% mergeOps $1 }

tyapps :: { [Located TyEl] } -- NB: This list is reversed
        : tyapp                         { [$1] }
        | tyapps tyapp                  { $2 : $1 }

tyapp :: { Located TyEl }
        : atype                         { sL1 $1 $ TyElOpd (unLoc $1) }
        | qtyconop                      { sL1 $1 $ TyElOpr (unLoc $1) }
        | tyvarop                       { sL1 $1 $ TyElOpr (unLoc $1) }
        | SIMPLEQUOTE qconop            {% ams (sLL $1 $> $ TyElOpr (unLoc $2))
                                               [mj AnnSimpleQuote $1,mj AnnVal $2] }
        | SIMPLEQUOTE varop             {% ams (sLL $1 $> $ TyElOpr (unLoc $2))
                                               [mj AnnSimpleQuote $1,mj AnnVal $2] }
        | '~'                           { sL1 $1 TyElTilde }
        | '!'                           { sL1 $1 TyElBang }
        | unpackedness                  { sL1 $1 $ TyElUnpackedness (unLoc $1) }

atype :: { LHsType GhcPs }
        : ntgtycon                       { sL1 $1 (HsTyVar noExt NotPromoted $1) }      -- Not including unit tuples
        | tyvar                          { sL1 $1 (HsTyVar noExt NotPromoted $1) }      -- (See Note [Unit tuples])
        | '*'                            {% do { warnStarIsType (getLoc $1)
                                               ; return $ sL1 $1 (HsStarTy noExt (isUnicode $1)) } }
        | '{' fielddecls '}'             {% amms (checkRecordSyntax
                                                    (sLL $1 $> $ HsRecTy noExt $2))
                                                        -- Constructor sigs only
                                                 [moc $1,mcc $3] }
        | '(' ')'                        {% ams (sLL $1 $> $ HsTupleTy noExt
                                                    HsBoxedOrConstraintTuple [])
                                                [mop $1,mcp $2] }
        | '(' ktype ',' comma_types1 ')' {% addAnnotation (gl $2) AnnComma
                                                          (gl $3) >>
                                            ams (sLL $1 $> $ HsTupleTy noExt

                                             HsBoxedOrConstraintTuple ($2 : $4))
                                                [mop $1,mcp $5] }
        | '(#' '#)'                   {% ams (sLL $1 $> $ HsTupleTy noExt HsUnboxedTuple [])
                                             [mo $1,mc $2] }
        | '(#' comma_types1 '#)'      {% ams (sLL $1 $> $ HsTupleTy noExt HsUnboxedTuple $2)
                                             [mo $1,mc $3] }
        | '(#' bar_types2 '#)'        {% ams (sLL $1 $> $ HsSumTy noExt $2)
                                             [mo $1,mc $3] }
        | '[' ktype ']'               {% ams (sLL $1 $> $ HsListTy  noExt $2) [mos $1,mcs $3] }
        | '(' ktype ')'               {% ams (sLL $1 $> $ HsParTy   noExt $2) [mop $1,mcp $3] }
        | quasiquote                  { sL1 $1 (HsSpliceTy noExt (unLoc $1) ) }
        | '$(' exp ')'                {% ams (sLL $1 $> $ mkHsSpliceTy HasParens $2)
                                             [mj AnnOpenPE $1,mj AnnCloseP $3] }
        | TH_ID_SPLICE                {%ams (sLL $1 $> $ mkHsSpliceTy HasDollar $ sL1 $1 $ HsVar noExt $
                                             (sL1 $1 (mkUnqual varName (getTH_ID_SPLICE $1))))
                                             [mj AnnThIdSplice $1] }
                                      -- see Note [Promotion] for the followings
        | SIMPLEQUOTE qcon_nowiredlist {% ams (sLL $1 $> $ HsTyVar noExt IsPromoted $2) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  '(' ktype ',' comma_types1 ')'
                             {% addAnnotation (gl $3) AnnComma (gl $4) >>
                                ams (sLL $1 $> $ HsExplicitTupleTy noExt ($3 : $5))
                                    [mj AnnSimpleQuote $1,mop $2,mcp $6] }
        | SIMPLEQUOTE  '[' comma_types0 ']'     {% ams (sLL $1 $> $ HsExplicitListTy noExt IsPromoted $3)
                                                       [mj AnnSimpleQuote $1,mos $2,mcs $4] }
        | SIMPLEQUOTE var                       {% ams (sLL $1 $> $ HsTyVar noExt IsPromoted $2)
                                                       [mj AnnSimpleQuote $1,mj AnnName $2] }

        -- Two or more [ty, ty, ty] must be a promoted list type, just as
        -- if you had written '[ty, ty, ty]
        -- (One means a list type, zero means the list type constructor,
        -- so you have to quote those.)
        | '[' ktype ',' comma_types1 ']'  {% addAnnotation (gl $2) AnnComma
                                                           (gl $3) >>
                                             ams (sLL $1 $> $ HsExplicitListTy noExt NotPromoted ($2 : $4))
                                                 [mos $1,mcs $5] }
        | INTEGER              { sLL $1 $> $ HsTyLit noExt $ HsNumTy (getINTEGERs $1)
                                                           (il_value (getINTEGER $1)) }
        | STRING               { sLL $1 $> $ HsTyLit noExt $ HsStrTy (getSTRINGs $1)
                                                                     (getSTRING  $1) }
        | '_'                  { sL1 $1 $ mkAnonWildCardTy }

-- An inst_type is what occurs in the head of an instance decl
--      e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type for convenience.
inst_type :: { LHsSigType GhcPs }
        : sigtype                       { mkLHsSigType $1 }

deriv_types :: { [LHsSigType GhcPs] }
        : typedoc                       { [mkLHsSigType $1] }

        | typedoc ',' deriv_types       {% addAnnotation (gl $1) AnnComma (gl $2)
                                           >> return (mkLHsSigType $1 : $3) }

comma_types0  :: { [LHsType GhcPs] }  -- Zero or more:  ty,ty,ty
        : comma_types1                  { $1 }
        | {- empty -}                   { [] }

comma_types1    :: { [LHsType GhcPs] }  -- One or more:  ty,ty,ty
        : ktype                        { [$1] }
        | ktype  ',' comma_types1      {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

bar_types2    :: { [LHsType GhcPs] }  -- Two or more:  ty|ty|ty
        : ktype  '|' ktype             {% addAnnotation (gl $1) AnnVbar (gl $2)
                                          >> return [$1,$3] }
        | ktype  '|' bar_types2        {% addAnnotation (gl $1) AnnVbar (gl $2)
                                          >> return ($1 : $3) }

tv_bndrs :: { [LHsTyVarBndr GhcPs] }
         : tv_bndr tv_bndrs             { $1 : $2 }
         | {- empty -}                  { [] }

tv_bndr :: { LHsTyVarBndr GhcPs }
        : tyvar                         { sL1 $1 (UserTyVar noExt $1) }
        | '(' tyvar '::' kind ')'       {% ams (sLL $1 $>  (KindedTyVar noExt $2 $4))
                                               [mop $1,mu AnnDcolon $3
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
        : varids0 '->' varids0  {% ams (cL (comb3 $1 $2 $3)
                                       (reverse (unLoc $1), reverse (unLoc $3)))
                                       [mu AnnRarrow $2] }

varids0 :: { Located [Located RdrName] }
        : {- empty -}                   { noLoc [] }
        | varids0 tyvar                 { sLL $1 $> ($2 : unLoc $1) }

-----------------------------------------------------------------------------
-- Kinds

kind :: { LHsKind GhcPs }
        : ctype                  { $1 }

{- Note [Promotion]
   ~~~~~~~~~~~~~~~~

- Syntax of promoted qualified names
We write 'Nat.Zero instead of Nat.'Zero when dealing with qualified
names. Moreover ticks are only allowed in types, not in kinds, for a
few reasons:
  1. we don't need quotes since we cannot define names in kinds
  2. if one day we merge types and kinds, tick would mean look in DataName
  3. we don't have a kind namespace anyway

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
                          ,[LConDecl GhcPs]) } -- Returned in order

        : 'where' '{'        gadt_constrs '}'    {% checkEmptyGADTs $
                                                      cL (comb2 $1 $3)
                                                        ([mj AnnWhere $1
                                                         ,moc $2
                                                         ,mcc $4]
                                                        , unLoc $3) }
        | 'where' vocurly    gadt_constrs close  {% checkEmptyGADTs $
                                                      cL (comb2 $1 $3)
                                                        ([mj AnnWhere $1]
                                                        , unLoc $3) }
        | {- empty -}                            { noLoc ([],[]) }

gadt_constrs :: { Located [LConDecl GhcPs] }
        : gadt_constr_with_doc ';' gadt_constrs
                  {% addAnnotation (gl $1) AnnSemi (gl $2)
                     >> return (cL (comb2 $1 $3) ($1 : unLoc $3)) }
        | gadt_constr_with_doc          { cL (gl $1) [$1] }
        | {- empty -}                   { noLoc [] }

-- We allow the following forms:
--      C :: Eq a => a -> T a
--      C :: forall a. Eq a => !a -> T a
--      D { x,y :: a } :: T a
--      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr_with_doc :: { LConDecl GhcPs }
gadt_constr_with_doc
        : maybe_docnext ';' gadt_constr
                {% return $ addConDoc $3 $1 }
        | gadt_constr
                {% return $1 }

gadt_constr :: { LConDecl GhcPs }
    -- see Note [Difference in parsing GADT and data constructors]
    -- Returns a list because of:   C,D :: ty
        : con_list '::' sigtypedoc
                {% let (gadt,anns) = mkGadtDecl (unLoc $1) $3
                   in ams (sLL $1 $> gadt)
                       (mu AnnDcolon $2:anns) }

{- Note [Difference in parsing GADT and data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GADT constructors have simpler syntax than usual data constructors:
in GADTs, types cannot occur to the left of '::', so they cannot be mixed
with constructor names (see Note [Parsing data constructors is hard]).

Due to simplified syntax, GADT constructor names (left-hand side of '::')
use simpler grammar production than usual data constructor names. As a
consequence, GADT constructor names are resticted (names like '(*)' are
allowed in usual data constructors, but not in GADTs).
-}

constrs :: { Located ([AddAnn],[LConDecl GhcPs]) }
        : maybe_docnext '=' constrs1    { cL (comb2 $2 $3) ([mj AnnEqual $2]
                                                     ,addConDocs (unLoc $3) $1)}

constrs1 :: { Located [LConDecl GhcPs] }
        : constrs1 maybe_docnext '|' maybe_docprev constr
            {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $3)
               >> return (sLL $1 $> (addConDoc $5 $2 : addConDocFirst (unLoc $1) $4)) }
        | constr                                          { sL1 $1 [$1] }

{- Note [Constr variatons of non-terminals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In record declarations we assume that 'ctype' used to parse the type will not
consume the trailing docprev:

  data R = R { field :: Int -- ^ comment on the field }

In 'R' we expect the comment to apply to the entire field, not to 'Int'. The
same issue is detailed in Note [ctype and ctypedoc].

So, we do not want 'ctype'  to consume 'docprev', therefore
    we do not want 'btype'  to consume 'docprev', therefore
    we do not want 'tyapps' to consume 'docprev'.

At the same time, when parsing a 'constr', we do want to consume 'docprev':

  data T = C Int  -- ^ comment on Int
             Bool -- ^ comment on Bool

So, we do want 'constr_stuff' to consume 'docprev'.

The problem arises because the clauses in 'constr' have the following
structure:

  (a)  context '=>' constr_stuff   (e.g.  data T a = Ord a => C a)
  (b)               constr_stuff   (e.g.  data T a =          C a)

and to avoid a reduce/reduce conflict, 'context' and 'constr_stuff' must be
compatible. And for 'context' to be compatible with 'constr_stuff', it must
consume 'docprev'.

So, we want 'context'  to consume 'docprev', therefore
    we want 'btype'    to consume 'docprev', therefore
    we want 'tyapps'   to consume 'docprev'.

Our requirements end up conflicting: for parsing record types, we want 'tyapps'
to leave 'docprev' alone, but for parsing constructors, we want it to consume
'docprev'.

As the result, we maintain two parallel hierarchies of non-terminals that
either consume 'docprev' or not:

  tyapps      constr_tyapps
  btype       constr_btype
  context     constr_context
  ...

They must be kept identical except for their treatment of 'docprev'.

-}

constr :: { LConDecl GhcPs }
        : maybe_docnext forall constr_context '=>' constr_stuff
                {% ams (let (con,details,doc_prev) = unLoc $5 in
                  addConDoc (cL (comb4 $2 $3 $4 $5) (mkConDeclH98 con
                                                       (snd $ unLoc $2)
                                                       (Just $3)
                                                       details))
                            ($1 `mplus` doc_prev))
                        (mu AnnDarrow $4:(fst $ unLoc $2)) }
        | maybe_docnext forall constr_stuff
                {% ams ( let (con,details,doc_prev) = unLoc $3 in
                  addConDoc (cL (comb2 $2 $3) (mkConDeclH98 con
                                                      (snd $ unLoc $2)
                                                      Nothing   -- No context
                                                      details))
                            ($1 `mplus` doc_prev))
                       (fst $ unLoc $2) }

forall :: { Located ([AddAnn], Maybe [LHsTyVarBndr GhcPs]) }
        : 'forall' tv_bndrs '.'       { sLL $1 $> ([mu AnnForall $1,mj AnnDot $3], Just $2) }
        | {- empty -}                 { noLoc ([], Nothing) }

constr_stuff :: { Located (Located RdrName, HsConDeclDetails GhcPs, Maybe LHsDocString) }
        : constr_tyapps                    {% do { c <- mergeDataCon (unLoc $1)
                                                 ; return $ sL1 $1 c } }

fielddecls :: { [LConDeclField GhcPs] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [LConDeclField GhcPs] }
        : fielddecl maybe_docnext ',' maybe_docprev fielddecls1
            {% addAnnotation (gl $1) AnnComma (gl $3) >>
               return ((addFieldDoc $1 $4) : addFieldDocs $5 $2) }
        | fielddecl   { [$1] }

fielddecl :: { LConDeclField GhcPs }
                                              -- A list because of   f,g :: Int
        : maybe_docnext sig_vars '::' ctype maybe_docprev
            {% ams (cL (comb2 $2 $4)
                      (ConDeclField noExt (reverse (map (\ln@(dL->L l n) -> cL l $ FieldOcc noExt ln) (unLoc $2))) $4 ($1 `mplus` $5)))
                   [mu AnnDcolon $3] }

-- Reversed!
maybe_derivings :: { HsDeriving GhcPs }
        : {- empty -}             { noLoc [] }
        | derivings               { $1 }

-- A list of one or more deriving clauses at the end of a datatype
derivings :: { HsDeriving GhcPs }
        : derivings deriving      { sLL $1 $> $ $2 : unLoc $1 }
        | deriving                { sLL $1 $> [$1] }

-- The outer Located is just to allow the caller to
-- know the rightmost extremity of the 'deriving' clause
deriving :: { LHsDerivingClause GhcPs }
        : 'deriving' deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in ams (cL full_loc $ HsDerivingClause noExt Nothing $2)
                        [mj AnnDeriving $1] }

        | 'deriving' deriv_strategy_no_via deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in ams (cL full_loc $ HsDerivingClause noExt (Just $2) $3)
                        [mj AnnDeriving $1] }

        | 'deriving' deriv_clause_types deriv_strategy_via
              {% let { full_loc = comb2 $1 $> }
                 in ams (cL full_loc $ HsDerivingClause noExt (Just $3) $2)
                        [mj AnnDeriving $1] }

deriv_clause_types :: { Located [LHsSigType GhcPs] }
        : qtycondoc           { sL1 $1 [mkLHsSigType $1] }
        | '(' ')'             {% ams (sLL $1 $> [])
                                     [mop $1,mcp $2] }
        | '(' deriv_types ')' {% ams (sLL $1 $> $2)
                                     [mop $1,mcp $3] }
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

docdecl :: { LHsDecl GhcPs }
        : docdecld { sL1 $1 (DocD noExt (unLoc $1)) }

docdecld :: { LDocDecl }
        : docnext                               { sL1 $1 (DocCommentNext (unLoc $1)) }
        | docprev                               { sL1 $1 (DocCommentPrev (unLoc $1)) }
        | docnamed                              { sL1 $1 (case (unLoc $1) of (n, doc) -> DocCommentNamed n doc) }
        | docsection                            { sL1 $1 (case (unLoc $1) of (n, doc) -> DocGroup n doc) }

decl_no_th :: { LHsDecl GhcPs }
        : sigdecl               { $1 }

        | '!' aexp rhs          {% do { let { e = sLL $1 $2 (SectionR noExt (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)
                                            ; l = comb2 $1 $> };
                                        (ann, r) <- checkValDef empty SrcStrict e Nothing $3 ;
                                        hintBangPat (comb2 $1 $2) (unLoc e) ;
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                        case r of {
                                          (FunBind _ n _ _ _) ->
                                                amsL l [mj AnnFunId n] >> return () ;
                                          (PatBind _ (dL->L l _) _rhs _) ->
                                                amsL l [] >> return () } ;

                                        _ <- amsL l (ann ++ fst (unLoc $3) ++ [mj AnnBang $1]) ;
                                        return $! (sL l $ ValD noExt r) } }

        | infixexp_top opt_sig rhs  {% do { (ann,r) <- checkValDef empty NoSrcStrict $1 (snd $2) $3;
                                        let { l = comb2 $1 $> };
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                        case r of {
                                          (FunBind _ n _ _ _) ->
                                                amsL l (mj AnnFunId n:(fst $2)) >> return () ;
                                          (PatBind _ (dL->L lh _lhs) _rhs _) ->
                                                amsL lh (fst $2) >> return () } ;
                                        _ <- amsL l (ann ++ (fst $ unLoc $3));
                                        return $! (sL l $ ValD noExt r) } }
        | pattern_synonym_decl  { $1 }
        | docdecl               { $1 }

decl    :: { LHsDecl GhcPs }
        : decl_no_th            { $1 }

        -- Why do we only allow naked declaration splices in top-level
        -- declarations and not here? Short answer: because readFail009
        -- fails terribly with a panic in cvBindsAndSigs otherwise.
        | splice_exp            { sLL $1 $> $ mkSpliceDecl $1 }

rhs     :: { Located ([AddAnn],GRHSs GhcPs (LHsExpr GhcPs)) }
        : '=' exp wherebinds    { sL (comb3 $1 $2 $3)
                                    ((mj AnnEqual $1 : (fst $ unLoc $3))
                                    ,GRHSs noExt (unguardedRHS (comb3 $1 $2 $3) $2)
                                   (snd $ unLoc $3)) }
        | gdrhs wherebinds      { sLL $1 $>  (fst $ unLoc $2
                                    ,GRHSs noExt (reverse (unLoc $1))
                                                    (snd $ unLoc $2)) }

gdrhs :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : gdrhs gdrh            { sLL $1 $> ($2 : unLoc $1) }
        | gdrh                  { sL1 $1 [$1] }

gdrh :: { LGRHS GhcPs (LHsExpr GhcPs) }
        : '|' guardquals '=' exp  {% ams (sL (comb2 $1 $>) $ GRHS noExt (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnEqual $3] }

sigdecl :: { LHsDecl GhcPs }
        :
        -- See Note [Declaration/signature overlap] for why we need infixexp here
          infixexp_top '::' sigtypedoc
                        {% do { v <- checkValSigLhs $1
                              ; _ <- amsL (comb2 $1 $>) [mu AnnDcolon $2]
                              ; return (sLL $1 $> $ SigD noExt $
                                  TypeSig noExt [v] (mkLHsSigWcType $3))} }

        | var ',' sig_vars '::' sigtypedoc
           {% do { let sig = TypeSig noExt ($1 : reverse (unLoc $3))
                                     (mkLHsSigWcType $5)
                 ; addAnnotation (gl $1) AnnComma (gl $2)
                 ; ams ( sLL $1 $> $ SigD noExt sig )
                       [mu AnnDcolon $4] } }

        | infix prec ops
              {% checkPrecP $2 $3 >>
                 ams (sLL $1 $> $ SigD noExt
                        (FixSig noExt (FixitySig noExt (fromOL $ unLoc $3)
                                (Fixity (fst $ unLoc $2) (snd $ unLoc $2) (unLoc $1)))))
                     [mj AnnInfix $1,mj AnnVal $2] }

        | pattern_synonym_sig   { sLL $1 $> . SigD noExt . unLoc $ $1 }

        | '{-# COMPLETE' con_list opt_tyconsig  '#-}'
                {% let (dcolon, tc) = $3
                   in ams
                       (sLL $1 $>
                         (SigD noExt (CompleteMatchSig noExt (getCOMPLETE_PRAGs $1) $2 tc)))
                    ([ mo $1 ] ++ dcolon ++ [mc $4]) }

        -- This rule is for both INLINE and INLINABLE pragmas
        | '{-# INLINE' activation qvar '#-}'
                {% ams ((sLL $1 $> $ SigD noExt (InlineSig noExt $3
                            (mkInlinePragma (getINLINE_PRAGs $1) (getINLINE $1)
                                            (snd $2)))))
                       ((mo $1:fst $2) ++ [mc $4]) }

        | '{-# SCC' qvar '#-}'
          {% ams (sLL $1 $> (SigD noExt (SCCFunSig noExt (getSCC_PRAGs $1) $2 Nothing)))
                 [mo $1, mc $3] }

        | '{-# SCC' qvar STRING '#-}'
          {% do { scc <- getSCC $3
                ; let str_lit = StringLiteral (getSTRINGs $3) scc
                ; ams (sLL $1 $> (SigD noExt (SCCFunSig noExt (getSCC_PRAGs $1) $2 (Just ( sL1 $3 str_lit)))))
                      [mo $1, mc $4] } }

        | '{-# SPECIALISE' activation qvar '::' sigtypes1 '#-}'
             {% ams (
                 let inl_prag = mkInlinePragma (getSPEC_PRAGs $1)
                                             (NoUserInline, FunLike) (snd $2)
                  in sLL $1 $> $ SigD noExt (SpecSig noExt $3 (fromOL $5) inl_prag))
                    (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
             {% ams (sLL $1 $> $ SigD noExt (SpecSig noExt $3 (fromOL $5)
                               (mkInlinePragma (getSPEC_INLINE_PRAGs $1)
                                               (getSPEC_INLINE $1) (snd $2))))
                       (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE' 'instance' inst_type '#-}'
                {% ams (sLL $1 $>
                                  $ SigD noExt (SpecInstSig noExt (getSPEC_PRAGs $1) $3))
                       [mo $1,mj AnnInstance $2,mc $4] }

        -- A minimal complete definition
        | '{-# MINIMAL' name_boolformula_opt '#-}'
            {% ams (sLL $1 $> $ SigD noExt (MinimalSig noExt (getMINIMAL_PRAGs $1) $2))
                   [mo $1,mc $3] }

activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | explicit_activation                   { (fst $1,Just (snd $1)) }

explicit_activation :: { ([AddAnn],Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mj AnnOpenS $1,mj AnnVal $2,mj AnnCloseS $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' '~' INTEGER ']'   { ([mj AnnOpenS $1,mj AnnTilde $2,mj AnnVal $3
                                                 ,mj AnnCloseS $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsSplice GhcPs) }
        : TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in sL1 $1 (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }
        | TH_QQUASIQUOTE  { let { loc = getLoc $1
                                ; ITqQuasiQuote (qual, quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkQual varName (qual, quoter) }
                            in sL (getLoc $1) (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }

exp   :: { LHsExpr GhcPs }
        : infixexp '::' sigtype {% ams (sLL $1 $> $ ExprWithTySig noExt $1 (mkLHsSigWcType $3))
                                       [mu AnnDcolon $2] }
        | infixexp '-<' exp     {% ams (sLL $1 $> $ HsArrApp noExt $1 $3
                                                        HsFirstOrderApp True)
                                       [mu Annlarrowtail $2] }
        | infixexp '>-' exp     {% ams (sLL $1 $> $ HsArrApp noExt $3 $1
                                                      HsFirstOrderApp False)
                                       [mu Annrarrowtail $2] }
        | infixexp '-<<' exp    {% ams (sLL $1 $> $ HsArrApp noExt $1 $3
                                                      HsHigherOrderApp True)
                                       [mu AnnLarrowtail $2] }
        | infixexp '>>-' exp    {% ams (sLL $1 $> $ HsArrApp noExt $3 $1
                                                      HsHigherOrderApp False)
                                       [mu AnnRarrowtail $2] }
        | infixexp              { $1 }

infixexp :: { LHsExpr GhcPs }
        : exp10 { $1 }
        | infixexp qop exp10  {% ams (sLL $1 $> (OpApp noExt $1 $2 $3))
                                     [mj AnnVal $2] }
                 -- AnnVal annotation for NPlusKPat, which discards the operator

infixexp_top :: { LHsExpr GhcPs }
        : exp10_top               { $1 }
        | infixexp_top qop exp10_top
                                  {% do { when (srcSpanEnd (getLoc $2)
                                            == srcSpanStart (getLoc $3)
                                            && checkIfBang $2) $
                                            warnSpaceAfterBang (comb2 $2 $3);
                                          ams (sLL $1 $> (OpApp noExt $1 $2 $3))
                                               [mj AnnVal $2]
                                        }
                                  }


exp10_top :: { LHsExpr GhcPs }
        : '-' fexp                      {% ams (sLL $1 $> $ NegApp noExt $2 noSyntaxExpr)
                                               [mj AnnMinus $1] }


        | hpc_annot exp        {% ams (sLL $1 $> $ HsTickPragma noExt (snd $ fst $ fst $ unLoc $1)
                                                                (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2)
                                      (fst $ fst $ fst $ unLoc $1) }

        | '{-# CORE' STRING '#-}' exp  {% ams (sLL $1 $> $ HsCoreAnn noExt (getCORE_PRAGs $1) (getStringLiteral $2) $4)
                                              [mo $1,mj AnnVal $2
                                              ,mc $3] }
                                          -- hdaume: core annotation
        | fexp                         { $1 }

exp10 :: { LHsExpr GhcPs }
        : exp10_top            { $1 }
        | scc_annot exp        {% ams (sLL $1 $> $ HsSCC noExt (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2)
                                      (fst $ fst $ unLoc $1) }

optSemi :: { ([Located a],Bool) }
        : ';'         { ([$1],True) }
        | {- empty -} { ([],False) }

scc_annot :: { Located (([AddAnn],SourceText),StringLiteral) }
        : '{-# SCC' STRING '#-}'      {% do scc <- getSCC $2
                                            ; return $ sLL $1 $>
                                               (([mo $1,mj AnnValStr $2
                                                ,mc $3],getSCC_PRAGs $1),(StringLiteral (getSTRINGs $2) scc)) }
        | '{-# SCC' VARID  '#-}'      { sLL $1 $> (([mo $1,mj AnnVal $2
                                         ,mc $3],getSCC_PRAGs $1)
                                        ,(StringLiteral NoSourceText (getVARID $2))) }

hpc_annot :: { Located ( (([AddAnn],SourceText),(StringLiteral,(Int,Int),(Int,Int))),
                         ((SourceText,SourceText),(SourceText,SourceText))
                       ) }
      : '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
                                      { sLL $1 $> $ ((([mo $1,mj AnnVal $2
                                              ,mj AnnVal $3,mj AnnColon $4
                                              ,mj AnnVal $5,mj AnnMinus $6
                                              ,mj AnnVal $7,mj AnnColon $8
                                              ,mj AnnVal $9,mc $10],
                                                getGENERATED_PRAGs $1)
                                              ,((getStringLiteral $2)
                                               ,( fromInteger $ il_value $ getINTEGER $3
                                                , fromInteger $ il_value $ getINTEGER $5
                                                )
                                               ,( fromInteger $ il_value $ getINTEGER $7
                                                , fromInteger $ il_value $ getINTEGER $9
                                                )
                                               ))
                                             , (( getINTEGERs $3
                                                , getINTEGERs $5
                                                )
                                               ,( getINTEGERs $7
                                                , getINTEGERs $9
                                                )))
                                         }

fexp    :: { LHsExpr GhcPs }
        : fexp aexp                  {% checkBlockArguments $1 >> checkBlockArguments $2 >>
                                        return (sLL $1 $> $ (HsApp noExt $1 $2)) }
        | fexp TYPEAPP atype         {% checkBlockArguments $1 >>
                                        ams (sLL $1 $> $ HsAppType noExt $1 (mkHsWildCardBndrs $3))
                                            [mj AnnAt $2] }
        | 'static' aexp              {% ams (sLL $1 $> $ HsStatic noExt $2)
                                            [mj AnnStatic $1] }
        | aexp                       { $1 }

aexp    :: { LHsExpr GhcPs }
        : qvar '@' aexp         {% ams (sLL $1 $> $ EAsPat noExt $1 $3) [mj AnnAt $2] }
            -- If you change the parsing, make sure to understand
            -- Note [Lexing type applications] in Lexer.x

        | '~' aexp              {% ams (sLL $1 $> $ ELazyPat noExt $2) [mj AnnTilde $1] }

        | '\\' apat apats '->' exp
                   {% ams (sLL $1 $> $ HsLam noExt (mkMatchGroup FromSource
                            [sLL $1 $> $ Match { m_ext = noExt
                                               , m_ctxt = LambdaExpr
                                               , m_pats = $2:$3
                                               , m_grhss = unguardedGRHSs $5 }]))
                          [mj AnnLam $1, mu AnnRarrow $4] }
        | 'let' binds 'in' exp          {% ams (sLL $1 $> $ HsLet noExt (snd $ unLoc $2) $4)
                                               (mj AnnLet $1:mj AnnIn $3
                                                 :(fst $ unLoc $2)) }
        | '\\' 'lcase' altslist
            {% ams (sLL $1 $> $ HsLamCase noExt
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
                                           ams (sLL $1 $> $ HsMultiIf noExt
                                                     (reverse $ snd $ unLoc $2))
                                               (mj AnnIf $1:(fst $ unLoc $2)) }
        | 'case' exp 'of' altslist      {% ams (cL (comb3 $1 $3 $4) $
                                                   HsCase noExt $2 (mkMatchGroup
                                                   FromSource (snd $ unLoc $4)))
                                               (mj AnnCase $1:mj AnnOf $3
                                                  :(fst $ unLoc $4)) }
        | 'do' stmtlist              {% ams (cL (comb2 $1 $2)
                                               (mkHsDo DoExpr (snd $ unLoc $2)))
                                               (mj AnnDo $1:(fst $ unLoc $2)) }
        | 'mdo' stmtlist            {% ams (cL (comb2 $1 $2)
                                              (mkHsDo MDoExpr (snd $ unLoc $2)))
                                           (mj AnnMdo $1:(fst $ unLoc $2)) }
        | 'proc' aexp '->' exp
                       {% checkPattern empty $2 >>= \ p ->
                           checkCommand $4 >>= \ cmd ->
                           ams (sLL $1 $> $ HsProc noExt p (sLL $1 $> $ HsCmdTop noExt cmd))
                                            -- TODO: is LL right here?
                               [mj AnnProc $1,mu AnnRarrow $3] }

        | aexp1                 { $1 }

aexp1   :: { LHsExpr GhcPs }
        : aexp1 '{' fbinds '}' {% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4)
                                                                   (snd $3)
                                     ; _ <- amsL (comb2 $1 $>) (moc $2:mcc $4:(fst $3))
                                     ; checkRecordSyntax (sLL $1 $> r) }}
        | aexp2                { $1 }

aexp2   :: { LHsExpr GhcPs }
        : qvar                          { sL1 $1 (HsVar noExt   $! $1) }
        | qcon                          { sL1 $1 (HsVar noExt   $! $1) }
        | ipvar                         { sL1 $1 (HsIPVar noExt $! unLoc $1) }
        | overloaded_label              { sL1 $1 (HsOverLabel noExt Nothing $! unLoc $1) }
        | literal                       { sL1 $1 (HsLit noExt  $! unLoc $1) }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--      | STRING    { sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRINGs $1)
--                                       (getSTRING $1) noExt) }
        | INTEGER   { sL (getLoc $1) (HsOverLit noExt $! mkHsIntegral   (getINTEGER $1) ) }
        | RATIONAL  { sL (getLoc $1) (HsOverLit noExt $! mkHsFractional (getRATIONAL $1) ) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
        | '(' texp ')'                  {% ams (sLL $1 $> (HsPar noExt $2)) [mop $1,mcp $3] }
        | '(' tup_exprs ')'             {% do { e <- mkSumOrTuple Boxed (comb2 $1 $3) (snd $2)
                                              ; ams (sLL $1 $> e) ((mop $1:fst $2) ++ [mcp $3]) } }

        | '(#' texp '#)'                {% ams (sLL $1 $> (ExplicitTuple noExt [cL (gl $2)
                                                         (Present noExt $2)] Unboxed))
                                               [mo $1,mc $3] }
        | '(#' tup_exprs '#)'           {% do { e <- mkSumOrTuple Unboxed (comb2 $1 $3) (snd $2)
                                              ; ams (sLL $1 $> e) ((mo $1:fst $2) ++ [mc $3]) } }

        | '[' list ']'      {% ams (sLL $1 $> (snd $2)) (mos $1:mcs $3:(fst $2)) }
        | '_'               { sL1 $1 $ EWildPat noExt }

        -- Template Haskell Extension
        | splice_exp            { $1 }

        | SIMPLEQUOTE  qvar     {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  qcon     {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE tyvar     {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt False (unLoc $2))) [mj AnnThTyQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE gtycon    {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt False (unLoc $2))) [mj AnnThTyQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE {- nothing -} {% reportEmptyDoubleQuotes (getLoc $1) }
        | '[|' exp '|]'       {% ams (sLL $1 $> $ HsBracket noExt (ExpBr noExt $2))
                                      (if (hasE $1) then [mj AnnOpenE $1, mu AnnCloseQ $3]
                                                    else [mu AnnOpenEQ $1,mu AnnCloseQ $3]) }
        | '[||' exp '||]'     {% ams (sLL $1 $> $ HsBracket noExt (TExpBr noExt $2))
                                      (if (hasE $1) then [mj AnnOpenE $1,mc $3] else [mo $1,mc $3]) }
        | '[t|' ktype '|]'    {% ams (sLL $1 $> $ HsBracket noExt (TypBr noExt $2)) [mo $1,mu AnnCloseQ $3] }
        | '[p|' infixexp '|]' {% checkPattern empty $2 >>= \p ->
                                      ams (sLL $1 $> $ HsBracket noExt (PatBr noExt p))
                                          [mo $1,mu AnnCloseQ $3] }
        | '[d|' cvtopbody '|]' {% ams (sLL $1 $> $ HsBracket noExt (DecBrL noExt (snd $2)))
                                      (mo $1:mu AnnCloseQ $3:fst $2) }
        | quasiquote          { sL1 $1 (HsSpliceE noExt (unLoc $1)) }

        -- arrow notation extension
        | '(|' aexp2 cmdargs '|)'  {% ams (sLL $1 $> $ HsArrForm noExt $2
                                                           Nothing (reverse $3))
                                          [mu AnnOpenB $1,mu AnnCloseB $4] }

splice_exp :: { LHsExpr GhcPs }
        : TH_ID_SPLICE          {% ams (sL1 $1 $ mkHsSpliceE HasDollar
                                        (sL1 $1 $ HsVar noExt (sL1 $1 (mkUnqual varName
                                                           (getTH_ID_SPLICE $1)))))
                                       [mj AnnThIdSplice $1] }
        | '$(' exp ')'          {% ams (sLL $1 $> $ mkHsSpliceE HasParens $2)
                                       [mj AnnOpenPE $1,mj AnnCloseP $3] }
        | TH_ID_TY_SPLICE       {% ams (sL1 $1 $ mkHsSpliceTE HasDollar
                                        (sL1 $1 $ HsVar noExt (sL1 $1 (mkUnqual varName
                                                        (getTH_ID_TY_SPLICE $1)))))
                                       [mj AnnThIdTySplice $1] }
        | '$$(' exp ')'         {% ams (sLL $1 $> $ mkHsSpliceTE HasParens $2)
                                       [mj AnnOpenPTE $1,mj AnnCloseP $3] }

cmdargs :: { [LHsCmdTop GhcPs] }
        : cmdargs acmd                  { $2 : $1 }
        | {- empty -}                   { [] }

acmd    :: { LHsCmdTop GhcPs }
        : aexp2                 {% checkCommand $1 >>= \ cmd ->
                                    return (sL1 $1 $ HsCmdTop noExt cmd) }

cvtopbody :: { ([AddAnn],[LHsDecl GhcPs]) }
        :  '{'            cvtopdecls0 '}'      { ([mj AnnOpenC $1
                                                  ,mj AnnCloseC $3],$2) }
        |      vocurly    cvtopdecls0 close    { ([],$2) }

cvtopdecls0 :: { [LHsDecl GhcPs] }
        : topdecls_semi         { cvTopDecls $1 }
        | topdecls              { cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Tuple expressions

-- "texp" is short for tuple expressions:
-- things that can appear unparenthesized as long as they're
-- inside parens or delimitted by commas
texp :: { LHsExpr GhcPs }
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
        | infixexp qop        { sLL $1 $> $ SectionL noExt $1 $2 }
        | qopm infixexp       { sLL $1 $> $ SectionR noExt $1 $2 }

       -- View patterns get parenthesized above
        | exp '->' texp   {% ams (sLL $1 $> $ EViewPat noExt $1 $3) [mu AnnRarrow $2] }

-- Always at least one comma or bar.
tup_exprs :: { ([AddAnn],SumOrTuple) }
           : texp commas_tup_tail
                          {% do { addAnnotation (gl $1) AnnComma (fst $2)
                                ; return ([],Tuple ((sL1 $1 (Present noExt $1)) : snd $2)) } }

           | texp bars    { (mvbars (fst $2), Sum 1  (snd $2 + 1) $1) }

           | commas tup_tail
                {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (fst $1)
                      ; return
                           ([],Tuple (map (\l -> cL l missingTupArg) (fst $1) ++ $2)) } }

           | bars texp bars0
                { (mvbars (fst $1) ++ mvbars (fst $3), Sum (snd $1 + 1) (snd $1 + snd $3 + 1) $2) }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { (SrcSpan,[LHsTupArg GhcPs]) }
commas_tup_tail : commas tup_tail
       {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (tail $ fst $1)
             ; return (
            (head $ fst $1
            ,(map (\l -> cL l missingTupArg) (tail $ fst $1)) ++ $2)) } }

-- Always follows a comma
tup_tail :: { [LHsTupArg GhcPs] }
          : texp commas_tup_tail {% addAnnotation (gl $1) AnnComma (fst $2) >>
                                    return ((cL (gl $1) (Present noExt $1)) : snd $2) }
          | texp                 { [cL (gl $1) (Present noExt $1)] }
          | {- empty -}          { [noLoc missingTupArg] }

-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.
list :: { ([AddAnn],HsExpr GhcPs) }
        : texp    { ([],ExplicitList noExt Nothing [$1]) }
        | lexps   { ([],ExplicitList noExt Nothing (reverse (unLoc $1))) }
        | texp '..'             { ([mj AnnDotdot $2],
                                      ArithSeq noExt Nothing (From $1)) }
        | texp ',' exp '..'     { ([mj AnnComma $2,mj AnnDotdot $4],
                                  ArithSeq noExt Nothing
                                                             (FromThen $1 $3)) }
        | texp '..' exp         { ([mj AnnDotdot $2],
                                   ArithSeq noExt Nothing
                                                               (FromTo $1 $3)) }
        | texp ',' exp '..' exp { ([mj AnnComma $2,mj AnnDotdot $4],
                                    ArithSeq noExt Nothing
                                                (FromThenTo $1 $3 $5)) }
        | texp '|' flattenedpquals
             {% checkMonadComp >>= \ ctxt ->
                return ([mj AnnVbar $2],
                        mkHsComp ctxt (unLoc $3) $1) }

lexps :: { Located [LHsExpr GhcPs] }
        : lexps ',' texp          {% addAnnotation (gl $ head $ unLoc $1)
                                                            AnnComma (gl $2) >>
                                      return (sLL $1 $> (((:) $! $3) $! unLoc $1)) }
        | texp ',' texp            {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                      return (sLL $1 $> [$3,$1]) }

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : pquals   { case (unLoc $1) of
                    [qs] -> sL1 $1 qs
                    -- We just had one thing in our "parallel" list so
                    -- we simply return that thing directly

                    qss -> sL1 $1 [sL1 $1 $ ParStmt noExt [ParStmtBlock noExt qs [] noSyntaxExpr |
                                            qs <- qss]
                                            noExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt GhcPs (LHsExpr GhcPs)]] }
    : squals '|' pquals
                     {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $2) >>
                        return (sLL $1 $> (reverse (unLoc $1) : unLoc $3)) }
    | squals         { cL (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }   -- In reverse order, because the last
                                        -- one can "grab" the earlier ones
    : squals ',' transformqual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                amsL (comb2 $1 $>) (fst $ unLoc $3) >>
                return (sLL $1 $> [sLL $1 $> ((snd $ unLoc $3) (reverse (unLoc $1)))]) }
    | squals ',' qual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                return (sLL $1 $> ($3 : unLoc $1)) }
    | transformqual        {% ams $1 (fst $ unLoc $1) >>
                              return (sLL $1 $> [cL (getLoc $1) ((snd $ unLoc $1) [])]) }
    | qual                                { sL1 $1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { sLL $1 $> ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { sL1 $1 [$2] }

-- It is possible to enable bracketing (associating) qualifier lists
-- by uncommenting the lines with {| |} above. Due to a lack of
-- consensus on the syntax, this feature is not being used until we
-- get user demand.

transformqual :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)] -> Stmt GhcPs (LHsExpr GhcPs)) }
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
-- Guards

guardquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : guardquals1           { cL (getLoc $1) (reverse (unLoc $1)) }

guardquals1 :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : guardquals1 ',' qual  {% addAnnotation (gl $ head $ unLoc $1) AnnComma
                                             (gl $2) >>
                               return (sLL $1 $> ($3 : unLoc $1)) }
    | qual                  { sL1 $1 [$1] }

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
        : '{'            alts '}'  { sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                               ,(reverse (snd $ unLoc $2))) }
        |     vocurly    alts  close { cL (getLoc $2) (fst $ unLoc $2
                                        ,(reverse (snd $ unLoc $2))) }
        | '{'                 '}'    { sLL $1 $> ([moc $1,mcc $2],[]) }
        |     vocurly          close { noLoc ([],[]) }

alts    :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
        : alts1                    { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
        | ';' alts                 { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2))
                                               ,snd $ unLoc $2) }

alts1   :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
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

alt     :: { LMatch GhcPs (LHsExpr GhcPs) }
           : pat alt_rhs  {%ams (sLL $1 $> (Match { m_ext = noExt
                                                  , m_ctxt = CaseAlt
                                                  , m_pats = [$1]
                                                  , m_grhss = snd $ unLoc $2 }))
                                      (fst $ unLoc $2)}

alt_rhs :: { Located ([AddAnn],GRHSs GhcPs (LHsExpr GhcPs)) }
        : ralt wherebinds           { sLL $1 $> (fst $ unLoc $2,
                                            GRHSs noExt (unLoc $1) (snd $ unLoc $2)) }

ralt :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : '->' exp            {% ams (sLL $1 $> (unguardedRHS (comb2 $1 $2) $2))
                                     [mu AnnRarrow $1] }
        | gdpats              { sL1 $1 (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : gdpats gdpat                  { sLL $1 $> ($2 : unLoc $1) }
        | gdpat                         { sL1 $1 [$1] }

-- layout for MultiWayIf doesn't begin with an open brace, because it's hard to
-- generate the open brace in addition to the vertical bar in the lexer, and
-- we don't need it.
ifgdpats :: { Located ([AddAnn],[LGRHS GhcPs (LHsExpr GhcPs)]) }
         : '{' gdpats '}'                 { sLL $1 $> ([moc $1,mcc $3],unLoc $2)  }
         |     gdpats close               { sL1 $1 ([],unLoc $1) }

gdpat   :: { LGRHS GhcPs (LHsExpr GhcPs) }
        : '|' guardquals '->' exp
                                  {% ams (sL (comb2 $1 $>) $ GRHS noExt (unLoc $2) $4)
                                         [mj AnnVbar $1,mu AnnRarrow $3] }

-- 'pat' recognises a pattern, including one with a bang at the top
--      e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat GhcPs }
pat     :  exp          {% checkPattern empty $1 }
        | '!' aexp      {% amms (checkPattern empty (sLL $1 $> (SectionR noExt
                                                     (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)))
                                [mj AnnBang $1] }

bindpat :: { LPat GhcPs }
bindpat :  exp            {% checkPattern
                                (text "Possibly caused by a missing 'do'?") $1 }
        | '!' aexp        {% amms (checkPattern
                                     (text "Possibly caused by a missing 'do'?")
                                     (sLL $1 $> (SectionR noExt (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)))
                                  [mj AnnBang $1] }

apat   :: { LPat GhcPs }
apat    : aexp                  {% checkPattern empty $1 }
        | '!' aexp              {% amms (checkPattern empty
                                            (sLL $1 $> (SectionR noExt
                                                (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)))
                                        [mj AnnBang $1] }

apats  :: { [LPat GhcPs] }
        : apat apats            { $1 : $2 }
        | {- empty -}           { [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)]) }
        : '{'           stmts '}'       { sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                             ,(reverse $ snd $ unLoc $2)) } -- AZ:performance of reverse?
        |     vocurly   stmts close     { cL (gl $2) (fst $ unLoc $2
                                                    ,reverse $ snd $ unLoc $2) }

--      do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead

stmts :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)]) }
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
maybe_stmt :: { Maybe (LStmt GhcPs (LHsExpr GhcPs)) }
        : stmt                          { Just $1 }
        | {- nothing -}                 { Nothing }

stmt  :: { LStmt GhcPs (LHsExpr GhcPs) }
        : qual                          { $1 }
        | 'rec' stmtlist                {% ams (sLL $1 $> $ mkRecStmt (snd $ unLoc $2))
                                               (mj AnnRec $1:(fst $ unLoc $2)) }

qual  :: { LStmt GhcPs (LHsExpr GhcPs) }
    : bindpat '<-' exp                  {% ams (sLL $1 $> $ mkBindStmt $1 $3)
                                               [mu AnnLarrow $2] }
    | exp                               { sL1 $1 $ mkBodyStmt $1 }
    | 'let' binds                       {% ams (sLL $1 $>$ LetStmt noExt (snd $ unLoc $2))
                                               (mj AnnLet $1:(fst $ unLoc $2)) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds  :: { ([AddAnn],([LHsRecField GhcPs (LHsExpr GhcPs)], Bool)) }
        : fbinds1                       { $1 }
        | {- empty -}                   { ([],([], False)) }

fbinds1 :: { ([AddAnn],([LHsRecField GhcPs (LHsExpr GhcPs)], Bool)) }
        : fbind ',' fbinds1
                {% addAnnotation (gl $1) AnnComma (gl $2) >>
                   return (case $3 of (ma,(flds, dd)) -> (ma,($1 : flds, dd))) }
        | fbind                         { ([],([$1], False)) }
        | '..'                          { ([mj AnnDotdot $1],([],   True)) }

fbind   :: { LHsRecField GhcPs (LHsExpr GhcPs) }
        : qvar '=' texp {% ams  (sLL $1 $> $ HsRecField (sL1 $1 $ mkFieldOcc $1) $3 False)
                                [mj AnnEqual $2] }
                        -- RHS is a 'texp', allowing view patterns (Trac #6038)
                        -- and, incidentally, sections.  Eg
                        -- f (R { x = show -> s }) = ...

        | qvar          { sLL $1 $> $ HsRecField (sL1 $1 $ mkFieldOcc $1) placeHolderPunRhs True }
                        -- In the punning case, use a place-holder
                        -- The renamer fills in the final value

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds  :: { Located [LIPBind GhcPs] }
        : dbinds ';' dbind
                      {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (let { this = $3; rest = unLoc $1 }
                              in rest `seq` this `seq` sLL $1 $> (this : rest)) }
        | dbinds ';'  {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (sLL $1 $> (unLoc $1)) }
        | dbind                        { let this = $1 in this `seq` sL1 $1 [this] }
--      | {- empty -}                  { [] }

dbind   :: { LIPBind GhcPs }
dbind   : ipvar '=' exp                {% ams (sLL $1 $> (IPBind noExt (Left $1) $3))
                                              [mj AnnEqual $2] }

ipvar   :: { Located HsIPName }
        : IPDUPVARID            { sL1 $1 (HsIPName (getIPDUPVARID $1)) }

-----------------------------------------------------------------------------
-- Overloaded labels

overloaded_label :: { Located FastString }
        : LABELVARID          { sL1 $1 (getLABELVARID $1) }

-----------------------------------------------------------------------------
-- Warnings and deprecations

name_boolformula_opt :: { LBooleanFormula (Located RdrName) }
        : name_boolformula          { $1 }
        | {- empty -}               { noLoc mkTrue }

name_boolformula :: { LBooleanFormula (Located RdrName) }
        : name_boolformula_and                      { $1 }
        | name_boolformula_and '|' name_boolformula
                           {% aa $1 (AnnVbar, $2)
                              >> return (sLL $1 $> (Or [$1,$3])) }

name_boolformula_and :: { LBooleanFormula (Located RdrName) }
        : name_boolformula_and_list
                  { sLL (head $1) (last $1) (And ($1)) }

name_boolformula_and_list :: { [LBooleanFormula (Located RdrName)] }
        : name_boolformula_atom                               { [$1] }
        | name_boolformula_atom ',' name_boolformula_and_list
            {% aa $1 (AnnComma, $2) >> return ($1 : $3) }

name_boolformula_atom :: { LBooleanFormula (Located RdrName) }
        : '(' name_boolformula ')'  {% ams (sLL $1 $> (Parens $2)) [mop $1,mcp $3] }
        | name_var                  { sL1 $1 (Var $1) }

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
        | '(' commas ')'        {% ams (sLL $1 $> $ tupleDataCon Boxed (snd $2 + 1))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' '#)'             {% ams (sLL $1 $> $ unboxedUnitDataCon) [mo $1,mc $2] }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ tupleDataCon Unboxed (snd $2 + 1))
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
        | '(' commas ')'        {% ams (sLL $1 $> $ getRdrName (tupleTyCon Boxed
                                                        (snd $2 + 1)))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ getRdrName (tupleTyCon Unboxed
                                                        (snd $2 + 1)))
                                       (mo $1:mc $3:(mcommas (fst $2))) }
        | '(' '->' ')'          {% ams (sLL $1 $> $ getRdrName funTyCon)
                                       [mop $1,mu AnnRarrow $2,mcp $3] }
        | '[' ']'               {% ams (sLL $1 $> $ listTyCon_RDR) [mos $1,mcs $2] }

oqtycon :: { Located RdrName }  -- An "ordinary" qualified tycon;
                                -- These can appear in export lists
        : qtycon                        { $1 }
        | '(' qtyconsym ')'             {% ams (sLL $1 $> (unLoc $2))
                                               [mop $1,mj AnnVal $2,mcp $3] }
        | '(' '~' ')'                   {% ams (sLL $1 $> $ eqTyCon_RDR)
                                               [mop $1,mj AnnVal $2,mcp $3] }

oqtycon_no_varcon :: { Located RdrName }  -- Type constructor which cannot be mistaken
                                          -- for variable constructor in export lists
                                          -- see Note [Type constructors in export list]
        :  qtycon            { $1 }
        | '(' QCONSYM ')'    {% let { name :: Located RdrName
                                    ; name = sL1 $2 $! mkQual tcClsName (getQCONSYM $2) }
                                in ams (sLL $1 $> (unLoc name)) [mop $1,mj AnnVal name,mcp $3] }
        | '(' CONSYM ')'     {% let { name :: Located RdrName
                                    ; name = sL1 $2 $! mkUnqual tcClsName (getCONSYM $2) }
                                in ams (sLL $1 $> (unLoc name)) [mop $1,mj AnnVal name,mcp $3] }
        | '(' ':' ')'        {% let { name :: Located RdrName
                                    ; name = sL1 $2 $! consDataCon_RDR }
                                in ams (sLL $1 $> (unLoc name)) [mop $1,mj AnnVal name,mcp $3] }
        | '(' '~' ')'        {% ams (sLL $1 $> $ eqTyCon_RDR) [mop $1,mj AnnTilde $2,mcp $3] }

{- Note [Type constructors in export list]
~~~~~~~~~~~~~~~~~~~~~
Mixing type constructors and data constructors in export lists introduces
ambiguity in grammar: e.g. (*) may be both a type constructor and a function.

-XExplicitNamespaces allows to disambiguate by explicitly prefixing type
constructors with 'type' keyword.

This ambiguity causes reduce/reduce conflicts in parser, which are always
resolved in favour of data constructors. To get rid of conflicts we demand
that ambiguous type constructors (those, which are formed by the same
productions as variable constructors) are always prefixed with 'type' keyword.
Unambiguous type constructors may occur both with or without 'type' keyword.

Note that in the parser we still parse data constructors as type
constructors. As such, they still end up in the type constructor namespace
until after renaming when we resolve the proper namespace for each exported
child.
-}

qtyconop :: { Located RdrName } -- Qualified or unqualified
        : qtyconsym                     { $1 }
        | '`' qtycon '`'                {% ams (sLL $1 $> (unLoc $2))
                                               [mj AnnBackquote $1,mj AnnVal $2
                                               ,mj AnnBackquote $3] }

qtycon :: { Located RdrName }   -- Qualified or unqualified
        : QCONID            { sL1 $1 $! mkQual tcClsName (getQCONID $1) }
        | tycon             { $1 }

qtycondoc :: { LHsType GhcPs } -- Qualified or unqualified
        : qtycon            { sL1 $1                           (HsTyVar noExt NotPromoted $1)      }
        | qtycon docprev    { sLL $1 $> (HsDocTy noExt (sL1 $1 (HsTyVar noExt NotPromoted $1)) $2) }

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
        | ':'                   { sL1 $1 $! consDataCon_RDR }
        | '-'                   { sL1 $1 $! mkUnqual tcClsName (fsLit "-") }


-----------------------------------------------------------------------------
-- Operators

op      :: { Located RdrName }   -- used in infix decls
        : varop                 { $1 }
        | conop                 { $1 }
        | '->'                  { sL1 $1 $ getRdrName funTyCon }
        | '~'                   { sL1 $1 $ eqTyCon_RDR }

varop   :: { Located RdrName }
        : varsym                { $1 }
        | '`' varid '`'         {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qop     :: { LHsExpr GhcPs }   -- used in sections
        : qvarop                { sL1 $1 $ HsVar noExt $1 }
        | qconop                { sL1 $1 $ HsVar noExt $1 }
        | hole_op               { $1 }

qopm    :: { LHsExpr GhcPs }   -- used in sections
        : qvaropm               { sL1 $1 $ HsVar noExt $1 }
        | qconop                { sL1 $1 $ HsVar noExt $1 }
        | hole_op               { $1 }

hole_op :: { LHsExpr GhcPs }   -- used in sections
hole_op : '`' '_' '`'           {% ams (sLL $1 $> $ EWildPat noExt)
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

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
        | '.'                   {% hintExplicitForall' (getLoc $1) }

tyvarid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual tvName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual tvName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual tvName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual tvName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual tvName (fsLit "interruptible") }
        -- If this changes relative to varid, update 'checkRuleTyVarBndrNames' in RdrHsSyn.hs
        -- See Note [Parsing explicit foralls in Rules]

-----------------------------------------------------------------------------
-- Variables

var     :: { Located RdrName }
        : varid                 { $1 }
        | '(' varsym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }

 -- Lexing type applications depends subtly on what characters can possibly
 -- end a qvar. Currently (June 2015), only $idchars and ")" can end a qvar.
 -- If you're changing this, please see Note [Lexing type applications] in
 -- Lexer.x.
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

-- Note that 'role' and 'family' get lexed separately regardless of
-- the use of extensions. However, because they are listed here,
-- this is OK and they can be used as normal varids.
-- See Note [Lexing type pseudo-keywords] in Lexer.x
varid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual varName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual varName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual varName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual varName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual varName (fsLit "interruptible")}
        | 'forall'         { sL1 $1 $! mkUnqual varName (fsLit "forall") }
        | 'family'         { sL1 $1 $! mkUnqual varName (fsLit "family") }
        | 'role'           { sL1 $1 $! mkUnqual varName (fsLit "role") }
        -- If this changes relative to tyvarid, update 'checkRuleTyVarBndrNames' in RdrHsSyn.hs
        -- See Note [Parsing explicit foralls in Rules]

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
-- except 'unsafe', 'interruptible', 'forall', 'family', 'role', 'stock', and
-- 'anyclass', whose treatment differs depending on context
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
        | 'stock'               { sL1 $1 (fsLit "stock") }
        | 'anyclass'            { sL1 $1 (fsLit "anyclass") }
        | 'via'                 { sL1 $1 (fsLit "via") }
        | 'unit'                { sL1 $1 (fsLit "unit") }
        | 'dependency'          { sL1 $1 (fsLit "dependency") }
        | 'signature'           { sL1 $1 (fsLit "signature") }

special_sym :: { Located FastString }
special_sym : '!'       {% ams (sL1 $1 (fsLit "!")) [mj AnnBang $1] }
            | '.'       { sL1 $1 (fsLit ".") }
            | '*'       { sL1 $1 (fsLit (if isUnicode $1 then "\x2605" else "*")) }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }   -- Qualified or unqualified
        : conid              { $1 }
        | QCONID             { sL1 $1 $! mkQual dataName (getQCONID $1) }

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

literal :: { Located (HsLit GhcPs) }
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
        | PRIMFLOAT         { sL1 $1 $ HsFloatPrim  noExt $ getPRIMFLOAT $1 }
        | PRIMDOUBLE        { sL1 $1 $ HsDoublePrim noExt $ getPRIMDOUBLE $1 }

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

bars0 :: { ([SrcSpan],Int) }     -- Zero or more bars
        : bars                   { $1 }
        |                        { ([], 0) }

bars :: { ([SrcSpan],Int) }     -- One or more bars
        : bars '|'               { ((fst $1)++[gl $2],snd $1 + 1) }
        | '|'                    { ([gl $1],1) }

-----------------------------------------------------------------------------
-- Documentation comments

docnext :: { LHsDocString }
  : DOCNEXT {% return (sL1 $1 (mkHsDocString (getDOCNEXT $1))) }

docprev :: { LHsDocString }
  : DOCPREV {% return (sL1 $1 (mkHsDocString (getDOCPREV $1))) }

docnamed :: { Located (String, HsDocString) }
  : DOCNAMED {%
      let string = getDOCNAMED $1
          (name, rest) = break isSpace string
      in return (sL1 $1 (name, mkHsDocString rest)) }

docsection :: { Located (Int, HsDocString) }
  : DOCSECTION {% let (n, doc) = getDOCSECTION $1 in
        return (sL1 $1 (n, mkHsDocString doc)) }

moduleheader :: { Maybe LHsDocString }
        : DOCNEXT {% let string = getDOCNEXT $1 in
                     return (Just (sL1 $1 (mkHsDocString string))) }

maybe_docprev :: { Maybe LHsDocString }
        : docprev                       { Just $1 }
        | {- empty -}                   { Nothing }

maybe_docnext :: { Maybe LHsDocString }
        : docnext                       { Just $1 }
        | {- empty -}                   { Nothing }

{
happyError :: P a
happyError = srcParseFail

getVARID        (dL->L _ (ITvarid    x)) = x
getCONID        (dL->L _ (ITconid    x)) = x
getVARSYM       (dL->L _ (ITvarsym   x)) = x
getCONSYM       (dL->L _ (ITconsym   x)) = x
getQVARID       (dL->L _ (ITqvarid   x)) = x
getQCONID       (dL->L _ (ITqconid   x)) = x
getQVARSYM      (dL->L _ (ITqvarsym  x)) = x
getQCONSYM      (dL->L _ (ITqconsym  x)) = x
getIPDUPVARID   (dL->L _ (ITdupipvarid   x)) = x
getLABELVARID   (dL->L _ (ITlabelvarid   x)) = x
getCHAR         (dL->L _ (ITchar   _ x)) = x
getSTRING       (dL->L _ (ITstring _ x)) = x
getINTEGER      (dL->L _ (ITinteger x))  = x
getRATIONAL     (dL->L _ (ITrational x)) = x
getPRIMCHAR     (dL->L _ (ITprimchar _ x)) = x
getPRIMSTRING   (dL->L _ (ITprimstring _ x)) = x
getPRIMINTEGER  (dL->L _ (ITprimint  _ x)) = x
getPRIMWORD     (dL->L _ (ITprimword _ x)) = x
getPRIMFLOAT    (dL->L _ (ITprimfloat x)) = x
getPRIMDOUBLE   (dL->L _ (ITprimdouble x)) = x
getTH_ID_SPLICE (dL->L _ (ITidEscape x)) = x
getTH_ID_TY_SPLICE (dL->L _ (ITidTyEscape x)) = x
getINLINE       (dL->L _ (ITinline_prag _ inl conl)) = (inl,conl)
getSPEC_INLINE  (dL->L _ (ITspec_inline_prag _ True))  = (Inline,  FunLike)
getSPEC_INLINE  (dL->L _ (ITspec_inline_prag _ False)) = (NoInline,FunLike)
getCOMPLETE_PRAGs (dL->L _ (ITcomplete_prag x)) = x

getDOCNEXT (dL->L _ (ITdocCommentNext x)) = x
getDOCPREV (dL->L _ (ITdocCommentPrev x)) = x
getDOCNAMED (dL->L _ (ITdocCommentNamed x)) = x
getDOCSECTION (dL->L _ (ITdocSection n x)) = (n, x)

getINTEGERs     (dL->L _ (ITinteger (IL src _ _))) = src
getCHARs        (dL->L _ (ITchar       src _)) = src
getSTRINGs      (dL->L _ (ITstring     src _)) = src
getPRIMCHARs    (dL->L _ (ITprimchar   src _)) = src
getPRIMSTRINGs  (dL->L _ (ITprimstring src _)) = src
getPRIMINTEGERs (dL->L _ (ITprimint    src _)) = src
getPRIMWORDs    (dL->L _ (ITprimword   src _)) = src

-- See Note [Pragma source text] in BasicTypes for the following
getINLINE_PRAGs       (dL->L _ (ITinline_prag       src _ _)) = src
getSPEC_PRAGs         (dL->L _ (ITspec_prag         src))     = src
getSPEC_INLINE_PRAGs  (dL->L _ (ITspec_inline_prag  src _))   = src
getSOURCE_PRAGs       (dL->L _ (ITsource_prag       src)) = src
getRULES_PRAGs        (dL->L _ (ITrules_prag        src)) = src
getWARNING_PRAGs      (dL->L _ (ITwarning_prag      src)) = src
getDEPRECATED_PRAGs   (dL->L _ (ITdeprecated_prag   src)) = src
getSCC_PRAGs          (dL->L _ (ITscc_prag          src)) = src
getGENERATED_PRAGs    (dL->L _ (ITgenerated_prag    src)) = src
getCORE_PRAGs         (dL->L _ (ITcore_prag         src)) = src
getUNPACK_PRAGs       (dL->L _ (ITunpack_prag       src)) = src
getNOUNPACK_PRAGs     (dL->L _ (ITnounpack_prag     src)) = src
getANN_PRAGs          (dL->L _ (ITann_prag          src)) = src
getMINIMAL_PRAGs      (dL->L _ (ITminimal_prag      src)) = src
getOVERLAPPABLE_PRAGs (dL->L _ (IToverlappable_prag src)) = src
getOVERLAPPING_PRAGs  (dL->L _ (IToverlapping_prag  src)) = src
getOVERLAPS_PRAGs     (dL->L _ (IToverlaps_prag     src)) = src
getINCOHERENT_PRAGs   (dL->L _ (ITincoherent_prag   src)) = src
getCTYPEs             (dL->L _ (ITctype             src)) = src

getStringLiteral l = StringLiteral (getSTRINGs l) (getSTRING l)

isUnicode :: Located Token -> Bool
isUnicode (dL->L _ (ITforall         iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITdarrow         iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITdcolon         iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITlarrow         iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITrarrow         iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITlarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITrarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITLarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITRarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (IToparenbar      iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITcparenbar      iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITopenExpQuote _ iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITcloseQuote     iu)) = iu == UnicodeSyntax
isUnicode (dL->L _ (ITstar           iu)) = iu == UnicodeSyntax
isUnicode _                           = False

hasE :: Located Token -> Bool
hasE (dL->L _ (ITopenExpQuote HasE _)) = True
hasE (dL->L _ (ITopenTExpQuote HasE))  = True
hasE _                             = False

getSCC :: Located Token -> P FastString
getSCC lt = do let s = getSTRING lt
                   err = "Spaces are not allowed in SCCs"
               -- We probably actually want to be more restrictive than this
               if ' ' `elem` unpackFS s
                   then failSpanMsgP (getLoc lt) (text err)
                   else return s

-- Utilities for combining source spans
comb2 :: (HasSrcSpan a , HasSrcSpan b) => a -> b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

comb3 :: (HasSrcSpan a , HasSrcSpan b , HasSrcSpan c) =>
         a -> b -> c -> SrcSpan
comb3 a b c = a `seq` b `seq` c `seq`
    combineSrcSpans (getLoc a) (combineSrcSpans (getLoc b) (getLoc c))

comb4 :: (HasSrcSpan a , HasSrcSpan b , HasSrcSpan c , HasSrcSpan d) =>
         a -> b -> c -> d -> SrcSpan
comb4 a b c d = a `seq` b `seq` c `seq` d `seq`
    (combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
                combineSrcSpans (getLoc c) (getLoc d))

-- strict constructor version:
{-# INLINE sL #-}
sL :: HasSrcSpan a => SrcSpan -> SrcSpanLess a -> a
sL span a = span `seq` a `seq` cL span a

-- See Note [Adding location info] for how these utility functions are used

-- replaced last 3 CPP macros in this file
{-# INLINE sL0 #-}
sL0 :: HasSrcSpan a => SrcSpanLess a -> a
sL0 = cL noSrcSpan       -- #define L0   L noSrcSpan

{-# INLINE sL1 #-}
sL1 :: (HasSrcSpan a , HasSrcSpan b) => a -> SrcSpanLess b -> b
sL1 x = sL (getLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sLL #-}
sLL :: (HasSrcSpan a , HasSrcSpan b , HasSrcSpan c) =>
       a -> b -> SrcSpanLess c -> c
sLL x y = sL (comb2 x y) -- #define LL   sL (comb2 $1 $>)

{- Note [Adding location info]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

-}

-- Make a source location for the file.  We're a bit lazy here and just
-- make a point SrcSpan at line 1, column 0.  Strictly speaking we should
-- try to find the span of the whole file (ToDo).
fileSrcSpan :: P SrcSpan
fileSrcSpan = do
  l <- getRealSrcLoc;
  let loc = mkSrcLoc (srcLocFile l) 1 1;
  return (mkSrcSpan loc loc)

-- Hint about the MultiWayIf extension
hintMultiWayIf :: SrcSpan -> P ()
hintMultiWayIf span = do
  mwiEnabled <- liftM ((LangExt.MultiWayIf `extopt`) . options) getPState
  unless mwiEnabled $ parseErrorSDoc span $
    text "Multi-way if-expressions need MultiWayIf turned on"

-- Hint about if usage for beginners
hintIf :: SrcSpan -> String -> P (LHsExpr GhcPs)
hintIf span msg = do
  mwiEnabled <- liftM ((LangExt.MultiWayIf `extopt`) . options) getPState
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

-- Hint about explicit-forall, assuming UnicodeSyntax is off
hintExplicitForall' :: SrcSpan -> P (Located RdrName)
hintExplicitForall' span = do
    forall    <- extension explicitForallEnabled
    let illegalDot = "Illegal symbol '.' in type"
    if forall
      then parseErrorSDoc span $ vcat
        [ text illegalDot
        , text "Perhaps you meant to write 'forall <tvs>. <type>'?"
        ]
      else parseErrorSDoc span $ vcat
        [ text illegalDot
        , text "Perhaps you intended to use RankNTypes or a similar language"
        , text "extension to enable explicit-forall syntax: forall <tvs>. <type>"
        ]

checkIfBang :: LHsExpr GhcPs -> Bool
checkIfBang (dL->L _ (HsVar _ (dL->L _ op))) = op == bang_RDR
checkIfBang _ = False

-- | Warn about missing space after bang
warnSpaceAfterBang :: SrcSpan -> P ()
warnSpaceAfterBang span = do
    bang_on <- extension bangPatEnabled
    unless bang_on $
      addWarning Opt_WarnSpaceAfterBang span msg
    where
      msg = text "Did you forget to enable BangPatterns?" $$
            text "If you mean to bind (!) then perhaps you want" $$
            text "to add a space after the bang for clarity."

-- When two single quotes don't followed by tyvar or gtycon, we report the
-- error as empty character literal, or TH quote that missing proper type
-- variable or constructor. See Trac #13450.
reportEmptyDoubleQuotes :: SrcSpan -> P (Located (HsExpr GhcPs))
reportEmptyDoubleQuotes span = do
    thEnabled <- liftM ((LangExt.TemplateHaskellQuotes `extopt`) . options) getPState
    if thEnabled
      then parseErrorSDoc span $ vcat
        [ text "Parser error on `''`"
        , text "Character literals may not be empty"
        , text "Or perhaps you intended to use quotation syntax of TemplateHaskell,"
        , text "but the type variable or constructor is missing"
        ]
      else parseErrorSDoc span $ vcat
        [ text "Parser error on `''`"
        , text "Character literals may not be empty"
        ]

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
-- of the keyword itself
mj :: HasSrcSpan e => AnnKeywordId -> e -> AddAnn
mj a l s = addAnnotation s a (gl l)

mjL :: AnnKeywordId -> SrcSpan -> AddAnn
mjL a l s = addAnnotation s a l



-- |Construct an AddAnn from the annotation keyword and the Located Token. If
-- the token has a unicode equivalent and this has been used, provide the
-- unicode variant of the annotation.
mu :: AnnKeywordId -> Located Token -> AddAnn
mu a lt@(dL->L l t) = (\s -> addAnnotation s (toUnicodeAnn a lt) l)

-- | If the 'Token' is using its unicode variant return the unicode variant of
--   the annotation
toUnicodeAnn :: AnnKeywordId -> Located Token -> AnnKeywordId
toUnicodeAnn a t = if isUnicode t then unicodeAnn a else a

gl :: HasSrcSpan a => a -> SrcSpan
gl = getLoc

-- |Add an annotation to the located element, and return the located
-- element as a pass through
aa :: (HasSrcSpan a , HasSrcSpan c) => a -> (AnnKeywordId, c) -> P a
aa a@(dL->L l _) (b,s) = addAnnotation l b (gl s) >> return a

-- |Add an annotation to a located element resulting from a monadic action
am :: (HasSrcSpan a , HasSrcSpan b) => P a -> (AnnKeywordId, b) -> P a
am a (b,s) = do
  av@(dL->L l _) <- a
  addAnnotation l b (gl s)
  return av

-- | Add a list of AddAnns to the given AST element.  For example,
-- the parsing rule for @let@ looks like:
--
-- @
--      | 'let' binds 'in' exp    {% ams (sLL $1 $> $ HsLet (snd $ unLoc $2) $4)
--                                       (mj AnnLet $1:mj AnnIn $3
--                                         :(fst $ unLoc $2)) }
-- @
--
-- This adds an AnnLet annotation for @let@, an AnnIn for @in@, as well
-- as any annotations that may arise in the binds. This will include open
-- and closing braces if they are used to delimit the let expressions.
--
ams :: Located a -> [AddAnn] -> P (Located a)
ams a@(dL->L l _) bs = addAnnsAt l bs >> return a

amsL :: SrcSpan -> [AddAnn] -> P ()
amsL sp bs = addAnnsAt sp bs >> return ()

-- |Add all [AddAnn] to an AST element wrapped in a Just
ajs a@(Just (dL->L l _)) bs = addAnnsAt l bs >> return a

-- |Add a list of AddAnns to the given AST element, where the AST element is the
--  result of a monadic action
amms :: HasSrcSpan a => P a -> [AddAnn] -> P a
amms a bs = do { av@(dL->L l _) <- a
               ; addAnnsAt l bs
               ; return av }

-- |Add a list of AddAnns to the AST element, and return the element as a
--  OrdList
amsu :: HasSrcSpan a => a -> [AddAnn] -> P (OrdList a)
amsu a@(dL->L l _) bs = addAnnsAt l bs >> return (unitOL a)

-- |Synonyms for AddAnn versions of AnnOpen and AnnClose
mo,mc :: Located Token -> AddAnn
mo ll = mj AnnOpen ll
mc ll = mj AnnClose ll

moc,mcc :: Located Token -> AddAnn
moc ll = mj AnnOpenC ll
mcc ll = mj AnnCloseC ll

mop,mcp :: Located Token -> AddAnn
mop ll = mj AnnOpenP ll
mcp ll = mj AnnCloseP ll

mos,mcs :: Located Token -> AddAnn
mos ll = mj AnnOpenS ll
mcs ll = mj AnnCloseS ll

-- |Given a list of the locations of commas, provide a [AddAnn] with an AnnComma
--  entry for each SrcSpan
mcommas :: [SrcSpan] -> [AddAnn]
mcommas ss = map (mjL AnnCommaTuple) ss

-- |Given a list of the locations of '|'s, provide a [AddAnn] with an AnnVbar
--  entry for each SrcSpan
mvbars :: [SrcSpan] -> [AddAnn]
mvbars ss = map (mjL AnnVbar) ss

-- |Get the location of the last element of a OrdList, or noSrcSpan
oll :: HasSrcSpan a => OrdList a -> SrcSpan
oll l =
  if isNilOL l then noSrcSpan
               else getLoc (lastOL l)

-- |Add a semicolon annotation in the right place in a list. If the
-- leading list is empty, add it to the tail
asl :: (HasSrcSpan a , HasSrcSpan b) => [a] -> b -> a -> P()
asl [] (dL->L ls _) (dL->L l _) = addAnnotation l          AnnSemi ls
asl (x:_xs) (dL->L ls _) _x = addAnnotation (getLoc x) AnnSemi ls
}
