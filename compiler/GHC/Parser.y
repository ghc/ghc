--                                                              -*-haskell-*-
-- ---------------------------------------------------------------------------
-- (c) The University of Glasgow 1997-2003
---
-- The GHC grammar.
--
-- Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-- ---------------------------------------------------------------------------
{
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | This module provides the generated Happy parser for Haskell. It exports
-- a number of parsers which may be used in any library that uses the GHC API.
-- A common usage pattern is to initialize the parser state with a given string
-- and then parse that string:
--
-- @
--     runParser :: ParserOpts -> String -> P a -> ParseResult a
--     runParser opts str parser = unP parser parseState
--     where
--       filename = "\<interactive\>"
--       location = mkRealSrcLoc (mkFastString filename) 1 1
--       buffer = stringToStringBuffer str
--       parseState = initParserState opts buffer location
-- @
module GHC.Parser
   ( parseModule, parseSignature, parseImport, parseStatement, parseBackpack
   , parseDeclaration, parseExpression, parsePattern
   , parseTypeSignature
   , parseStmt, parseIdentifier
   , parseType, parseHeader
   , parseModuleNoHaddock
   )
where

-- base
import Control.Monad    ( unless, liftM, when, (<=<) )
import GHC.Exts
import Data.Maybe       ( maybeToList )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import qualified Prelude -- for happy-generated code

import GHC.Hs

import GHC.Driver.Backpack.Syntax

import GHC.Unit.Info
import GHC.Unit.Module
import GHC.Unit.Module.Warnings

import GHC.Data.OrdList
import GHC.Data.BooleanFormula ( BooleanFormula(..), LBooleanFormula, mkTrue )
import GHC.Data.FastString
import GHC.Data.Maybe          ( orElse )

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Misc          ( looksLikePackageName, fstOf3, sndOf3, thdOf3 )
import GHC.Utils.Panic
import GHC.Prelude
import qualified GHC.Data.Strict as Strict

import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence ( varName, dataName, tcClsName, tvName, occNameFS, mkVarOccFS)
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Error ( GhcHint(..) )
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.SourceFile
import GHC.Types.SourceText
import GHC.Types.PkgQual

import GHC.Core.Type    ( Specificity(..) )
import GHC.Core.Class   ( FunDep )
import GHC.Core.DataCon ( DataCon, dataConName )

import GHC.Parser.PostProcess
import GHC.Parser.PostProcess.Haddock
import GHC.Parser.Lexer
import GHC.Parser.HaddockLex
import GHC.Parser.Annotation
import GHC.Parser.Errors.Types
import GHC.Parser.Errors.Ppr ()

import GHC.Builtin.Types ( unitTyCon, unitDataCon, sumTyCon,
                           tupleTyCon, tupleDataCon, nilDataCon,
                           unboxedUnitTyCon, unboxedUnitDataCon,
                           listTyCon_RDR, consDataCon_RDR,
                           unrestrictedFunTyCon )

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import qualified Data.Semigroup as Semi
}

%expect 0 -- shift/reduce conflicts

{- Note [shift/reduce conflicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'happy' tool turns this grammar into an efficient parser that follows the
shift-reduce parsing model. There's a parse stack that contains items parsed so
far (both terminals and non-terminals). Every next token produced by the lexer
results in one of two actions:

  SHIFT:    push the token onto the parse stack

  REDUCE:   pop a few items off the parse stack and combine them
            with a function (reduction rule)

However, sometimes it's unclear which of the two actions to take.
Consider this code example:

    if x then y else f z

There are two ways to parse it:

    (if x then y else f) z
    if x then y else (f z)

How is this determined? At some point, the parser gets to the following state:

  parse stack:  'if' exp 'then' exp 'else' "f"
  next token:   "z"

Scenario A (simplified):

  1. REDUCE, parse stack: 'if' exp 'then' exp 'else' exp
             next token:  "z"
        (Note that "f" reduced to exp here)

  2. REDUCE, parse stack: exp
             next token:  "z"

  3. SHIFT,  parse stack: exp "z"
             next token:  ...

  4. REDUCE, parse stack: exp
             next token:  ...

  This way we get:  (if x then y else f) z

Scenario B (simplified):

  1. SHIFT,  parse stack: 'if' exp 'then' exp 'else' "f" "z"
             next token:  ...

  2. REDUCE, parse stack: 'if' exp 'then' exp 'else' exp
             next token:  ...

  3. REDUCE, parse stack: exp
             next token:  ...

  This way we get:  if x then y else (f z)

The end result is determined by the chosen action. When Happy detects this, it
reports a shift/reduce conflict. At the top of the file, we have the following
directive:

  %expect 0

It means that we expect no unresolved shift/reduce conflicts in this grammar.
If you modify the grammar and get shift/reduce conflicts, follow the steps
below to resolve them.

STEP ONE
  is to figure out what causes the conflict.
  That's where the -i flag comes in handy:

      happy -agc --strict compiler/GHC/Parser.y -idetailed-info

  By analysing the output of this command, in a new file `detailed-info`, you
  can figure out which reduction rule causes the issue. At the top of the
  generated report, you will see a line like this:

      state 147 contains 67 shift/reduce conflicts.

  Scroll down to section State 147 (in your case it could be a different
  state). The start of the section lists the reduction rules that can fire
  and shows their context:

        exp10 -> fexp .                 (rule 492)
        fexp -> fexp . aexp             (rule 498)
        fexp -> fexp . PREFIX_AT atype  (rule 499)

  And then, for every token, it tells you the parsing action:

        ']'            reduce using rule 492
        '::'           reduce using rule 492
        '('            shift, and enter state 178
        QVARID         shift, and enter state 44
        DO             shift, and enter state 182
        ...

  But if you look closer, some of these tokens also have another parsing action
  in parentheses:

        QVARID    shift, and enter state 44
                   (reduce using rule 492)

  That's how you know rule 492 is causing trouble.
  Scroll back to the top to see what this rule is:

        ----------------------------------
        Grammar
        ----------------------------------
        ...
        ...
        exp10 -> fexp                (492)
        optSemi -> ';'               (493)
        ...
        ...

  Hence the shift/reduce conflict is caused by this parser production:

        exp10 :: { ECP }
                : '-' fexp    { ... }
                | fexp        { ... }    -- problematic rule

STEP TWO
  is to mark the problematic rule with the %shift pragma. This signals to
  'happy' that any shift/reduce conflicts involving this rule must be resolved
  in favor of a shift. There's currently no dedicated pragma to resolve in
  favor of the reduce.

STEP THREE
  is to add a dedicated Note for this specific conflict, as is done for all
  other conflicts below.
-}

{- Note [%shift: rule_activation -> {- empty -}]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    rule -> STRING . rule_activation rule_foralls infixexp '=' exp

Example:
    {-# RULES "name" [0] f = rhs #-}

Ambiguity:
    If we reduced, then we'd get an empty activation rule, and [0] would be
    parsed as part of the left-hand side expression.

    We shift, so [0] is parsed as an activation rule.
-}

{- Note [%shift: rule_foralls -> {- empty -}]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    rule -> STRING rule_activation . rule_foralls infixexp '=' exp

Example:
    {-# RULES "name" forall a1. lhs = rhs #-}

Ambiguity:
    If we reduced, then we would get an empty rule_foralls; the 'forall', being
    a valid term-level identifier, would be parsed as part of the left-hand
    side expression.

    We shift, so the 'forall' is parsed as part of rule_foralls.
-}

{- Note [%shift: type -> btype]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    context -> btype .
    type -> btype .
    type -> btype . '->' ctype
    type -> btype . '->.' ctype

Example:
    a :: Maybe Integer -> Bool

Ambiguity:
    If we reduced, we would get:   (a :: Maybe Integer) -> Bool
    We shift to get this instead:  a :: (Maybe Integer -> Bool)
-}

{- Note [%shift: infixtype -> ftype]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    infixtype -> ftype .
    infixtype -> ftype . tyop infixtype
    ftype -> ftype . tyarg
    ftype -> ftype . PREFIX_AT tyarg

Example:
    a :: Maybe Integer

Ambiguity:
    If we reduced, we would get:    (a :: Maybe) Integer
    We shift to get this instead:   a :: (Maybe Integer)
-}

{- Note [%shift: atype -> tyvar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    atype -> tyvar .
    tv_bndr_no_braces -> '(' tyvar . '::' kind ')'

Example:
    class C a where type D a = (a :: Type ...

Ambiguity:
    If we reduced, we could specify a default for an associated type like this:

      class C a where type D a
                      type D a = (a :: Type)

    But we shift in order to allow injectivity signatures like this:

      class C a where type D a = (r :: Type) | r -> a
-}

{- Note [%shift: exp -> infixexp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    exp -> infixexp . '::' sigtype
    exp -> infixexp . '-<' exp
    exp -> infixexp . '>-' exp
    exp -> infixexp . '-<<' exp
    exp -> infixexp . '>>-' exp
    exp -> infixexp .
    infixexp -> infixexp . qop exp10p

Examples:
    1) if x then y else z -< e
    2) if x then y else z :: T
    3) if x then y else z + 1   -- (NB: '+' is in VARSYM)

Ambiguity:
    If we reduced, we would get:

      1) (if x then y else z) -< e
      2) (if x then y else z) :: T
      3) (if x then y else z) + 1

    We shift to get this instead:

      1) if x then y else (z -< e)
      2) if x then y else (z :: T)
      3) if x then y else (z + 1)
-}

{- Note [%shift: exp10 -> '-' fexp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    exp10 -> '-' fexp .
    fexp -> fexp . aexp
    fexp -> fexp . PREFIX_AT atype

Examples & Ambiguity:
    Same as in Note [%shift: exp10 -> fexp],
    but with a '-' in front.
-}

{- Note [%shift: exp10 -> fexp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    exp10 -> fexp .
    fexp -> fexp . aexp
    fexp -> fexp . PREFIX_AT atype

Examples:
    1) if x then y else f z
    2) if x then y else f @z

Ambiguity:
    If we reduced, we would get:

      1) (if x then y else f) z
      2) (if x then y else f) @z

    We shift to get this instead:

      1) if x then y else (f z)
      2) if x then y else (f @z)
-}

{- Note [%shift: aexp2 -> ipvar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    aexp2 -> ipvar .
    dbind -> ipvar . '=' exp

Example:
    let ?x = ...

Ambiguity:
    If we reduced, ?x would be parsed as the LHS of a normal binding,
    eventually producing an error.

    We shift, so it is parsed as the LHS of an implicit binding.
-}

{- Note [%shift: aexp2 -> TH_TY_QUOTE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    aexp2 -> TH_TY_QUOTE . tyvar
    aexp2 -> TH_TY_QUOTE . gtycon
    aexp2 -> TH_TY_QUOTE .

Examples:
    1) x = ''
    2) x = ''a
    3) x = ''T

Ambiguity:
    If we reduced, the '' would result in reportEmptyDoubleQuotes even when
    followed by a type variable or a type constructor. But the only reason
    this reduction rule exists is to improve error messages.

    Naturally, we shift instead, so that ''a and ''T work as expected.
-}

{- Note [%shift: tup_tail -> {- empty -}]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    tup_exprs -> commas . tup_tail
    sysdcon_nolist -> '(' commas . ')'
    sysdcon_nolist -> '(#' commas . '#)'
    commas -> commas . ','

Example:
    (,,)

Ambiguity:
    A tuple section with no components is indistinguishable from the Haskell98
    data constructor for a tuple.

    If we reduced, (,,) would be parsed as a tuple section.
    We shift, so (,,) is parsed as a data constructor.

    This is preferable because we want to accept (,,) without -XTupleSections.
    See also Note [ExplicitTuple] in GHC.Hs.Expr.
-}

{- Note [%shift: qtyconop -> qtyconsym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    oqtycon -> '(' qtyconsym . ')'
    qtyconop -> qtyconsym .

Example:
    foo :: (:%)

Ambiguity:
    If we reduced, (:%) would be parsed as a parenthesized infix type
    expression without arguments, resulting in the 'failOpFewArgs' error.

    We shift, so it is parsed as a type constructor.
-}

{- Note [%shift: special_id -> 'group']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    transformqual -> 'then' 'group' . 'using' exp
    transformqual -> 'then' 'group' . 'by' exp 'using' exp
    special_id -> 'group' .

Example:
    [ ... | then group by dept using groupWith
          , then take 5 ]

Ambiguity:
    If we reduced, 'group' would be parsed as a term-level identifier, just as
    'take' in the other clause.

    We shift, so it is parsed as part of the 'group by' clause introduced by
    the -XTransformListComp extension.
-}

{- Note [%shift: activation -> {- empty -}]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    sigdecl -> '{-# INLINE' . activation qvarcon '#-}'
    activation -> {- empty -}
    activation -> explicit_activation

Example:

    {-# INLINE [0] Something #-}

Ambiguity:
    We don't know whether the '[' is the start of the activation or the beginning
    of the [] data constructor.
    We parse this as having '[0]' activation for inlining 'Something', rather than
    empty activation and inlining '[0] Something'.
-}

{- Note [Parser API Annotations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A lot of the productions are now cluttered with calls to
aa,am,acs,acsA etc.

These are helper functions to make sure that the locations of the
various keywords such as do / let / in are captured for use by tools
that want to do source to source conversions, such as refactorers or
structured editors.

The helper functions are defined at the bottom of this file.

See
  https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations and
  https://gitlab.haskell.org/ghc/ghc/wikis/ghc-ast-annotations
for some background.

-}

{- Note [Parsing lists]
~~~~~~~~~~~~~~~~~~~~~~~
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
sequences. See:
https://haskell-happy.readthedocs.io/en/latest/using.html#parsing-sequences
for more guidance.

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
-}

%token
 '_'            { L _ ITunderscore }            -- Haskell keywords
 'as'           { L _ ITas }
 'case'         { L _ ITcase }
 'class'        { L _ ITclass }
 'data'         { L _ ITdata }
 'default'      { L _ ITdefault }
 'deriving'     { L _ ITderiving }
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
 '{-# OPAQUE'             { L _ (ITopaque_prag _) }
 '{-# SPECIALISE'         { L _ (ITspec_prag _) }
 '{-# SPECIALISE_INLINE'  { L _ (ITspec_inline_prag _ _) }
 '{-# SOURCE'             { L _ (ITsource_prag _) }
 '{-# RULES'              { L _ (ITrules_prag _) }
 '{-# SCC'                { L _ (ITscc_prag _)}
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
 'lcases'       { L _ ITlcases }
 '|'            { L _ ITvbar }
 '<-'           { L _ (ITlarrow _) }
 '->'           { L _ (ITrarrow _) }
 '->.'          { L _ ITlolly }
 TIGHT_INFIX_AT { L _ ITat }
 '=>'           { L _ (ITdarrow _) }
 '-'            { L _ ITminus }
 PREFIX_TILDE   { L _ ITtilde }
 PREFIX_BANG    { L _ ITbang }
 PREFIX_MINUS   { L _ ITprefixminus }
 '*'            { L _ (ITstar _) }
 '-<'           { L _ (ITlarrowtail _) }            -- for arrow notation
 '>-'           { L _ (ITrarrowtail _) }            -- for arrow notation
 '-<<'          { L _ (ITLarrowtail _) }            -- for arrow notation
 '>>-'          { L _ (ITRarrowtail _) }            -- for arrow notation
 '.'            { L _ ITdot }
 PREFIX_PROJ    { L _ (ITproj True) }               -- RecordDotSyntax
 TIGHT_INFIX_PROJ { L _ (ITproj False) }            -- RecordDotSyntax
 PREFIX_AT      { L _ ITtypeApp }
 PREFIX_PERCENT { L _ ITpercent }                   -- for linear types

 '{'            { L _ ITocurly }                        -- special symbols
 '}'            { L _ ITccurly }
 vocurly        { L _ ITvocurly } -- virtual open curly (from layout)
 vccurly        { L _ ITvccurly } -- virtual close curly (from layout)
 '['            { L _ ITobrack }
 ']'            { L _ ITcbrack }
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


 -- QualifiedDo
 DO             { L _ (ITdo  _) }
 MDO            { L _ (ITmdo _) }

 IPDUPVARID     { L _ (ITdupipvarid   _) }              -- GHC extension
 LABELVARID     { L _ (ITlabelvarid _ _) }

 CHAR           { L _ (ITchar   _ _) }
 STRING         { L _ (ITstring _ _) }
 INTEGER        { L _ (ITinteger _) }
 RATIONAL       { L _ (ITrational _) }

 PRIMCHAR       { L _ (ITprimchar   _ _) }
 PRIMSTRING     { L _ (ITprimstring _ _) }
 PRIMINTEGER    { L _ (ITprimint    _ _) }
 PRIMWORD       { L _ (ITprimword   _ _) }
 PRIMINTEGER8   { L _ (ITprimint8   _ _) }
 PRIMINTEGER16  { L _ (ITprimint16  _ _) }
 PRIMINTEGER32  { L _ (ITprimint32  _ _) }
 PRIMINTEGER64  { L _ (ITprimint64  _ _) }
 PRIMWORD8      { L _ (ITprimword8  _ _) }
 PRIMWORD16     { L _ (ITprimword16 _ _) }
 PRIMWORD32     { L _ (ITprimword32 _ _) }
 PRIMWORD64     { L _ (ITprimword64 _ _) }
 PRIMFLOAT      { L _ (ITprimfloat  _) }
 PRIMDOUBLE     { L _ (ITprimdouble _) }

-- Template Haskell
'[|'            { L _ (ITopenExpQuote _ _) }
'[p|'           { L _ ITopenPatQuote  }
'[t|'           { L _ ITopenTypQuote  }
'[d|'           { L _ ITopenDecQuote  }
'|]'            { L _ (ITcloseQuote _) }
'[||'           { L _ (ITopenTExpQuote _) }
'||]'           { L _ ITcloseTExpQuote  }
PREFIX_DOLLAR   { L _ ITdollar }
PREFIX_DOLLAR_DOLLAR { L _ ITdollardollar }
TH_TY_QUOTE     { L _ ITtyQuote       }      -- ''T
TH_QUASIQUOTE   { L _ (ITquasiQuote _) }
TH_QQUASIQUOTE  { L _ (ITqQuasiQuote _) }

%monad { P } { >>= } { return }
%lexer { (lexer True) } { L _ ITeof }
  -- Replace 'lexer' above with 'lexerDbg'
  -- to dump the tokens fed to the parser.
%tokentype { (Located Token) }

-- Exported parsers
%name parseModuleNoHaddock module
%name parseSignatureNoHaddock signature
%name parseImport importdecl
%name parseStatement e_stmt
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
identifier :: { LocatedN RdrName }
        : qvar                          { $1 }
        | qcon                          { $1 }
        | qvarop                        { $1 }
        | qconop                        { $1 }
    | '(' '->' ')'      {% amsr (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                                (NameAnnRArrow (isUnicode $2) (Just $ glAA $1) (glAA $2) (Just $ glAA $3) []) }
    | '->'              {% amsr (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                                (NameAnnRArrow (isUnicode $1) Nothing (glAA $1) Nothing []) }

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
        : modid '=' moduleid { sLL $1 $> $ (reLoc $1, $3) }
        | modid VARSYM modid VARSYM { sLL $1 $> $ (reLoc $1, sLL $2 $> $ HsModuleVar (reLoc $3)) }

moduleid :: { LHsModuleId PackageName }
          : VARSYM modid VARSYM { sLL $1 $> $ HsModuleVar (reLoc $2) }
          | unitid ':' modid    { sLL $1 $> $ HsModuleId $1 (reLoc $3) }

pkgname :: { Located PackageName }
        : STRING     { sL1 $1 $ PackageName (getSTRING $1) }
        | litpkgname { sL1 $1 $ PackageName (unLoc $1) }

litpkgname_segment :: { Located FastString }
        : VARID  { sL1 $1 $ getVARID $1 }
        | CONID  { sL1 $1 $ getCONID $1 }
        | special_id { $1 }

-- Parse a minus sign regardless of whether -XLexicalNegation is turned on or off.
-- See Note [Minus tokens] in GHC.Parser.Lexer
HYPHEN :: { [AddEpAnn] }
      : '-'          { [mj AnnMinus $1 ] }
      | PREFIX_MINUS { [mj AnnMinus $1 ] }
      | VARSYM  {% if (getVARSYM $1 == fsLit "-")
                   then return [mj AnnMinus $1]
                   else do { addError $ mkPlainErrorMsgEnvelope (getLoc $1) $ PsErrExpectedHyphen
                           ; return [] } }


litpkgname :: { Located FastString }
        : litpkgname_segment { $1 }
        -- a bit of a hack, means p - b is parsed same as p-b, enough for now.
        | litpkgname_segment HYPHEN litpkgname  { sLL $1 $> $ concatFS [unLoc $1, fsLit "-", (unLoc $3)] }

mayberns :: { Maybe [LRenaming] }
        : {- empty -} { Nothing }
        | '(' rns ')' { Just (fromOL $2) }

rns :: { OrdList LRenaming }
        : rns ',' rn { $1 `appOL` unitOL $3 }
        | rns ','    { $1 }
        | rn         { unitOL $1 }

rn :: { LRenaming }
        : modid 'as' modid { sLL $1 $> $ Renaming (reLoc $1) (Just (reLoc $3)) }
        | modid            { sL1 $1    $ Renaming (reLoc $1) Nothing }

unitbody :: { OrdList (LHsUnitDecl PackageName) }
        : '{'     unitdecls '}'   { $2 }
        | vocurly unitdecls close { $2 }

unitdecls :: { OrdList (LHsUnitDecl PackageName) }
        : unitdecls ';' unitdecl { $1 `appOL` unitOL $3 }
        | unitdecls ';'         { $1 }
        | unitdecl              { unitOL $1 }

unitdecl :: { LHsUnitDecl PackageName }
        : 'module' maybe_src modid maybe_warning_pragma maybeexports 'where' body
             -- XXX not accurate
             { sL1 $1 $ DeclD
                 (case snd $2 of
                   NotBoot -> HsSrcFile
                   IsBoot  -> HsBootFile)
                 (reLoc $3)
                 (sL1 $1 (HsModule (XModulePs noAnn (thdOf3 $7) $4 Nothing) (Just $3) $5 (fst $ sndOf3 $7) (snd $ sndOf3 $7))) }
        | 'signature' modid maybe_warning_pragma maybeexports 'where' body
             { sL1 $1 $ DeclD
                 HsigFile
                 (reLoc $2)
                 (sL1 $1 (HsModule (XModulePs noAnn (thdOf3 $6) $3 Nothing) (Just $2) $4 (fst $ sndOf3 $6) (snd $ sndOf3 $6))) }
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
       : 'signature' modid maybe_warning_pragma maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                acs loc (\loc cs-> (L loc (HsModule (XModulePs
                                               (EpAnn (spanAsAnchor loc) (AnnsModule [mj AnnSignature $1, mj AnnWhere $5] (fstOf3 $6) [] Nothing) cs)
                                               (thdOf3 $6) $3 Nothing)
                                            (Just $2) $4 (fst $ sndOf3 $6)
                                            (snd $ sndOf3 $6)))
                    ) }

module :: { Located (HsModule GhcPs) }
       : 'module' modid maybe_warning_pragma maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                acsFinal (\cs eof -> (L loc (HsModule (XModulePs
                                                     (EpAnn (spanAsAnchor loc) (AnnsModule [mj AnnModule $1, mj AnnWhere $5] (fstOf3 $6) [] eof) cs)
                                                     (thdOf3 $6) $3 Nothing)
                                                  (Just $2) $4 (fst $ sndOf3 $6)
                                                  (snd $ sndOf3 $6))
                    )) }
        | body2
                {% fileSrcSpan >>= \ loc ->
                   acsFinal (\cs eof -> (L loc (HsModule (XModulePs
                                                        (EpAnn (spanAsAnchor loc) (AnnsModule [] (fstOf3 $1) [] eof) cs)
                                                        (thdOf3 $1) Nothing Nothing)
                                                     Nothing Nothing
                                                     (fst $ sndOf3 $1) (snd $ sndOf3 $1)))) }

missing_module_keyword :: { () }
        : {- empty -}                           {% pushModuleContext }

implicit_top :: { () }
        : {- empty -}                           {% pushModuleContext }

body    :: { ([TrailingAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])
             ,EpLayout) }
        :  '{'            top '}'      { (fst $2, snd $2, epExplicitBraces $1 $3) }
        |      vocurly    top close    { (fst $2, snd $2, EpVirtualBraces (getVOCURLY $1)) }

body2   :: { ([TrailingAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])
             ,EpLayout) }
        :  '{' top '}'                          { (fst $2, snd $2, epExplicitBraces $1 $3) }
        |  missing_module_keyword top close     { ([], snd $2, EpVirtualBraces leftmostColumn) }


top     :: { ([TrailingAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        : semis top1                            { (reverse $1, $2) }

top1    :: { ([LImportDecl GhcPs], [LHsDecl GhcPs]) }
        : importdecls_semi topdecls_cs_semi        { (reverse $1, cvTopDecls $2) }
        | importdecls_semi topdecls_cs             { (reverse $1, cvTopDecls $2) }
        | importdecls                              { (reverse $1, []) }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header  :: { Located (HsModule GhcPs) }
        : 'module' modid maybe_warning_pragma maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   acs loc (\loc cs -> (L loc (HsModule (XModulePs
                                                   (EpAnn (spanAsAnchor loc) (AnnsModule [mj AnnModule $1,mj AnnWhere $5] [] [] Nothing) cs)
                                                   EpNoLayout $3 Nothing)
                                                (Just $2) $4 $6 []
                          ))) }
        | 'signature' modid maybe_warning_pragma maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   acs loc (\loc cs -> (L loc (HsModule (XModulePs
                                                   (EpAnn (spanAsAnchor loc) (AnnsModule [mj AnnModule $1,mj AnnWhere $5] [] [] Nothing) cs)
                                                   EpNoLayout $3 Nothing)
                                                (Just $2) $4 $6 []
                          ))) }
        | header_body2
                {% fileSrcSpan >>= \ loc ->
                   return (L loc (HsModule (XModulePs noAnn EpNoLayout Nothing Nothing) Nothing Nothing $1 [])) }

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

maybeexports :: { (Maybe (LocatedL [LIE GhcPs])) }
        :  '(' exportlist ')'       {% fmap Just $ amsr (sLL $1 $> (fromOL $ snd $2))
                                        (AnnList Nothing (Just $ mop $1) (Just $ mcp $3) (fst $2) []) }
        |  {- empty -}              { Nothing }

exportlist :: { ([AddEpAnn], OrdList (LIE GhcPs)) }
        : exportlist1     { ([], $1) }
        | {- empty -}     { ([], nilOL) }

        -- trailing comma:
        | exportlist1 ',' {% case $1 of
                               SnocOL hs t -> do
                                 t' <- addTrailingCommaA t (gl $2)
                                 return ([], snocOL hs t')}
        | ','             { ([mj AnnComma $1], nilOL) }

exportlist1 :: { OrdList (LIE GhcPs) }
        : exportlist1 ',' export_cs
                          {% let ls = $1
                             in if isNilOL ls
                                  then return (ls `appOL` $3)
                                  else case ls of
                                         SnocOL hs t -> do
                                           t' <- addTrailingCommaA t (gl $2)
                                           return (snocOL hs t' `appOL` $3)}
        | export_cs       { $1 }


export_cs :: { OrdList (LIE GhcPs) }
export_cs : export {% return (unitOL $1) }

   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export  :: { LIE GhcPs }
        : maybe_warning_pragma qcname_ext export_subspec {% do { let { span = (maybe comb2 comb3 $1) $2 $> }
                                                          ; impExp <- mkModuleImpExp $1 (fst $ unLoc $3) $2 (snd $ unLoc $3)
                                                          ; return $ reLoc $ sL span $ impExp } }
        | maybe_warning_pragma 'module' modid            {% do { let { span = (maybe comb2 comb3 $1) $2 $>
                                                                     ; anchor = (maybe glR (\loc -> spanAsAnchor . comb2 loc) $1) $2 }
                                                          ; locImpExp <- return (sL span (IEModuleContents ($1, [mj AnnModule $2]) $3))
                                                          ; return $ reLoc $ locImpExp } }
        | maybe_warning_pragma 'pattern' qcon            { let span = (maybe comb2 comb3 $1) $2 $>
                                                           in reLoc $ sL span $ IEVar $1 (sLLa $2 $> (IEPattern (glAA $2) $3)) Nothing }

export_subspec :: { Located ([AddEpAnn],ImpExpSubSpec) }
        : {- empty -}             { sL0 ([],ImpExpAbs) }
        | '(' qcnames ')'         {% mkImpExpSubSpec (reverse (snd $2))
                                      >>= \(as,ie) -> return $ sLL $1 $>
                                            (as ++ [mop $1,mcp $3] ++ fst $2, ie) }

qcnames :: { ([AddEpAnn], [LocatedA ImpExpQcSpec]) }
  : {- empty -}                   { ([],[]) }
  | qcnames1                      { $1 }

qcnames1 :: { ([AddEpAnn], [LocatedA ImpExpQcSpec]) }     -- A reversed list
        :  qcnames1 ',' qcname_ext_w_wildcard  {% case (snd $1) of
                                                    (l@(L la ImpExpQcWildcard):t) ->
                                                       do { l' <- addTrailingCommaA l (gl $2)
                                                          ; return ([mj AnnDotdot (reLoc l),
                                                                     mj AnnComma $2]
                                                                   ,(snd (unLoc $3)  : l' : t)) }
                                                    (l:t) ->
                                                       do { l' <- addTrailingCommaA l (gl $2)
                                                          ; return (fst $1 ++ fst (unLoc $3)
                                                                   , snd (unLoc $3) : l' : t)} }

        -- Annotations re-added in mkImpExpSubSpec
        |  qcname_ext_w_wildcard                   { (fst (unLoc $1),[snd (unLoc $1)]) }

-- Variable, data constructor or wildcard
-- or tagged type constructor
qcname_ext_w_wildcard :: { Located ([AddEpAnn], LocatedA ImpExpQcSpec) }
        :  qcname_ext               { sL1 $1 ([],$1) }
        |  '..'                     { sL1 $1 ([mj AnnDotdot $1], sL1a $1 ImpExpQcWildcard)  }

qcname_ext :: { LocatedA ImpExpQcSpec }
        :  qcname                   { sL1a $1 (ImpExpQcName $1) }
        |  'type' oqtycon           {% do { n <- mkTypeImpExp $2
                                          ; return $ sLLa $1 $> (ImpExpQcType (glAA $1) n) }}

qcname  :: { LocatedN RdrName }  -- Variable or type constructor
        :  qvar                 { $1 } -- Things which look like functions
                                       -- Note: This includes record selectors but
                                       -- also (-.->), see #11432
        |  oqtycon_no_varcon    { $1 } -- see Note [Type constructors in export list]

-----------------------------------------------------------------------------
-- Import Declarations

-- importdecls and topdecls must contain at least one declaration;
-- top handles the fact that these may be optional.

-- One or more semicolons
semis1  :: { Located [TrailingAnn] }
semis1  : semis1 ';'  { if isZeroWidthSpan (gl $2) then (sL1 $1 $ unLoc $1) else (sLL $1 $> $ AddSemiAnn (glAA $2) : (unLoc $1)) }
        | ';'         { case msemi $1 of
                          [] -> noLoc []
                          ms -> sL1 $1 $ ms }

-- Zero or more semicolons
semis   :: { [TrailingAnn] }
semis   : semis ';'   { if isZeroWidthSpan (gl $2) then $1 else (AddSemiAnn (glAA $2) : $1) }
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
                                {% do { i <- amsAl $2 (comb2 $2 $3) (reverse $ unLoc $3)
                                      ; return (i : $1)} }
        | {- empty -}           { [] }

importdecl :: { LImportDecl GhcPs }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid optqualified maybeas maybeimpspec
                {% do {
                  ; let { ; mPreQual = unLoc $4
                          ; mPostQual = unLoc $7 }
                  ; checkImportDecl mPreQual mPostQual
                  ; let anns
                         = EpAnnImportDecl
                             { importDeclAnnImport    = glAA $1
                             , importDeclAnnPragma    = fst $ fst $2
                             , importDeclAnnSafe      = fst $3
                             , importDeclAnnQualified = fst $ importDeclQualifiedStyle mPreQual mPostQual
                             , importDeclAnnPackage   = fst $5
                             , importDeclAnnAs        = fst $8
                             }
                  ; let loc = (comb5 $1 $6 $7 (snd $8) $9);
                  ; fmap reLoc $ acs loc (\loc cs -> L loc $
                      ImportDecl { ideclExt = XImportDeclPass (EpAnn (spanAsAnchor loc) anns cs) (snd $ fst $2) False
                                  , ideclName = $6, ideclPkgQual = snd $5
                                  , ideclSource = snd $2, ideclSafe = snd $3
                                  , ideclQualified = snd $ importDeclQualifiedStyle mPreQual mPostQual
                                  , ideclAs = unLoc (snd $8)
                                  , ideclImportList = unLoc $9 })
                  }
                }


maybe_src :: { ((Maybe (EpaLocation,EpaLocation),SourceText),IsBootInterface) }
        : '{-# SOURCE' '#-}'        { ((Just (glAA $1,glAA $2),getSOURCE_PRAGs $1)
                                      , IsBoot) }
        | {- empty -}               { ((Nothing,NoSourceText),NotBoot) }

maybe_safe :: { (Maybe EpaLocation,Bool) }
        : 'safe'                                { (Just (glAA $1),True) }
        | {- empty -}                           { (Nothing,      False) }

maybe_pkg :: { (Maybe EpaLocation, RawPkgQual) }
        : STRING  {% do { let { pkgFS = getSTRING $1 }
                        ; unless (looksLikePackageName (unpackFS pkgFS)) $
                             addError $ mkPlainErrorMsgEnvelope (getLoc $1) $
                               (PsErrInvalidPackageName pkgFS)
                        ; return (Just (glAA $1), RawPkgQual (StringLiteral (getSTRINGs $1) pkgFS Nothing)) } }
        | {- empty -}                           { (Nothing,NoRawPkgQual) }

optqualified :: { Located (Maybe EpaLocation) }
        : 'qualified'                           { sL1 $1 (Just (glAA $1)) }
        | {- empty -}                           { noLoc Nothing }

maybeas :: { (Maybe EpaLocation,Located (Maybe (LocatedA ModuleName))) }
        : 'as' modid                           { (Just (glAA $1)
                                                 ,sLL $1 $> (Just $2)) }
        | {- empty -}                          { (Nothing,noLoc Nothing) }

maybeimpspec :: { Located (Maybe (ImportListInterpretation, LocatedL [LIE GhcPs])) }
        : impspec                  {% let (b, ie) = unLoc $1 in
                                       checkImportSpec ie
                                        >>= \checkedIe ->
                                          return (L (gl $1) (Just (b, checkedIe)))  }
        | {- empty -}              { noLoc Nothing }

impspec :: { Located (ImportListInterpretation, LocatedL [LIE GhcPs]) }
        :  '(' importlist ')'               {% do { es <- amsr (sLL $1 $> $ fromOL $ snd $2)
                                                               (AnnList Nothing (Just $ mop $1) (Just $ mcp $3) (fst $2) [])
                                                  ; return $ sLL $1 $> (Exactly, es)} }
        |  'hiding' '(' importlist ')'      {% do { es <- amsr (sLL $1 $> $ fromOL $ snd $3)
                                                               (AnnList Nothing (Just $ mop $2) (Just $ mcp $4) (mj AnnHiding $1:fst $3) [])
                                                  ; return $ sLL $1 $> (EverythingBut, es)} }

importlist :: { ([AddEpAnn], OrdList (LIE GhcPs)) }
        : importlist1     { ([], $1) }
        | {- empty -}     { ([], nilOL) }

        -- trailing comma:
        | importlist1 ',' {% case $1 of
                               SnocOL hs t -> do
                                 t' <- addTrailingCommaA t (gl $2)
                                 return ([], snocOL hs t')}
        | ','             { ([mj AnnComma $1], nilOL) }

importlist1 :: { OrdList (LIE GhcPs) }
        : importlist1 ',' import
                          {% let ls = $1
                             in if isNilOL ls
                                  then return (ls `appOL` $3)
                                  else case ls of
                                         SnocOL hs t -> do
                                           t' <- addTrailingCommaA t (gl $2)
                                           return (snocOL hs t' `appOL` $3)}
        | import          { $1 }

import  :: { OrdList (LIE GhcPs) }
        : qcname_ext export_subspec {% fmap (unitOL . reLoc . (sLL $1 $>)) $ mkModuleImpExp Nothing (fst $ unLoc $2) $1 (snd $ unLoc $2) }
        | 'module' modid            {% fmap (unitOL . reLoc) $ return (sLL $1 $> (IEModuleContents (Nothing, [mj AnnModule $1]) $2)) }
        | 'pattern' qcon            { unitOL $ reLoc $ sLL $1 $> $ IEVar Nothing (sLLa $1 $> (IEPattern (glAA $1) $2)) Nothing }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec    :: { Maybe (Located (SourceText,Int)) }
        : {- empty -}           { Nothing }
        | INTEGER
                 { Just (sL1 $1 (getINTEGERs $1,fromInteger (il_value (getINTEGER $1)))) }

infix   :: { Located FixityDirection }
        : 'infix'                               { sL1 $1 InfixN  }
        | 'infixl'                              { sL1 $1 InfixL  }
        | 'infixr'                              { sL1 $1 InfixR }

ops     :: { Located (OrdList (LocatedN RdrName)) }
        : ops ',' op       {% case (unLoc $1) of
                                SnocOL hs t -> do
                                  t' <- addTrailingCommaN t (gl $2)
                                  return (sLL $1 $> (snocOL hs t' `appOL` unitOL $3)) }
        | op               { sL1 $1 (unitOL $1) }

-----------------------------------------------------------------------------
-- Top-Level Declarations

-- No trailing semicolons, non-empty
topdecls :: { OrdList (LHsDecl GhcPs) }
        : topdecls_semi topdecl        { $1 `snocOL` $2 }

-- May have trailing semicolons, can be empty
topdecls_semi :: { OrdList (LHsDecl GhcPs) }
        : topdecls_semi topdecl semis1 {% do { t <- amsAl $2 (comb2 $2 $3) (reverse $ unLoc $3)
                                             ; return ($1 `snocOL` t) }}
        | {- empty -}                  { nilOL }


-----------------------------------------------------------------------------
-- Each topdecl accumulates prior comments
-- No trailing semicolons, non-empty
topdecls_cs :: { OrdList (LHsDecl GhcPs) }
        : topdecls_cs_semi topdecl_cs        { $1 `snocOL` $2 }

-- May have trailing semicolons, can be empty
topdecls_cs_semi :: { OrdList (LHsDecl GhcPs) }
        : topdecls_cs_semi topdecl_cs semis1 {% do { t <- amsAl $2 (comb2 $2 $3) (reverse $ unLoc $3)
                                                   ; return ($1 `snocOL` t) }}
        | {- empty -}                  { nilOL }

-- Each topdecl accumulates prior comments
topdecl_cs :: { LHsDecl GhcPs }
topdecl_cs : topdecl {% commentsPA $1 }

-----------------------------------------------------------------------------
topdecl :: { LHsDecl GhcPs }
        : cl_decl                               { L (getLoc $1) (TyClD noExtField (unLoc $1)) }
        | ty_decl                               { L (getLoc $1) (TyClD noExtField (unLoc $1)) }
        | standalone_kind_sig                   { L (getLoc $1) (KindSigD noExtField (unLoc $1)) }
        | inst_decl                             { L (getLoc $1) (InstD noExtField (unLoc $1)) }
        | stand_alone_deriving                  { L (getLoc $1) (DerivD noExtField (unLoc $1)) }
        | role_annot                            { L (getLoc $1) (RoleAnnotD noExtField (unLoc $1)) }
        | 'default' '(' comma_types0 ')'        {% amsA' (sLL $1 $>
                                                    (DefD noExtField (DefaultDecl [mj AnnDefault $1,mop $2,mcp $4] $3))) }
        | 'foreign' fdecl                       {% amsA' (sLL $1 $> ((snd $ unLoc $2) (mj AnnForeign $1:(fst $ unLoc $2)))) }
        | '{-# DEPRECATED' deprecations '#-}'   {% amsA' (sLL $1 $> $ WarningD noExtField (Warnings ([mo $1,mc $3], (getDEPRECATED_PRAGs $1)) (fromOL $2))) }
        | '{-# WARNING' warnings '#-}'          {% amsA' (sLL $1 $> $ WarningD noExtField (Warnings ([mo $1,mc $3], (getWARNING_PRAGs $1)) (fromOL $2))) }
        | '{-# RULES' rules '#-}'               {% amsA' (sLL $1 $> $ RuleD noExtField (HsRules ([mo $1,mc $3], (getRULES_PRAGs $1)) (reverse $2))) }
        | annotation { $1 }
        | decl_no_th                            { $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if
        -- it had a $(..) wrapped around it
        | infixexp                              {% runPV (unECP $1) >>= \ $1 ->
                                                       commentsPA $ mkSpliceDecl $1 }

-- Type classes
--
cl_decl :: { LTyClDecl GhcPs }
        : 'class' tycl_hdr fds where_cls
                {% (mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 (sndOf3 $ unLoc $4) (thdOf3 $ unLoc $4))
                        (mj AnnClass $1:(fst $ unLoc $3)++(fstOf3 $ unLoc $4)) }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl GhcPs }
           -- ordinary type synonyms
        : 'type' type '=' ktype
                -- Note ktype, not sigtype, on the right of '='
                -- We allow an explicit for-all but we don't insert one
                -- in   type Foo a = (b,b)
                -- Instead we just say b is out of scope
                --
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% mkTySynonym (comb2 $1 $4) $2 $4 [mj AnnType $1,mj AnnEqual $3] }

           -- type family declarations
        | 'type' 'family' type opt_tyfam_kind_sig opt_injective_info
                          where_type_family
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% mkFamDecl (comb5 $1 $3 $4 $5 $6) (snd $ unLoc $6) TopLevel $3
                                   (snd $ unLoc $4) (snd $ unLoc $5)
                           (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)
                           ++ (fst $ unLoc $5) ++ (fst $ unLoc $6))  }

          -- ordinary data type or newtype declaration
        | type_data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
                {% mkTyData (comb4 $1 $3 $4 $5) (sndOf3 $ unLoc $1) (thdOf3 $ unLoc $1) $2 $3
                           Nothing (reverse (snd $ unLoc $4))
                                   (fmap reverse $5)
                           ((fstOf3 $ unLoc $1)++(fst $ unLoc $4)) }
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty

          -- ordinary GADT declaration
        | type_data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% mkTyData (comb4 $1 $3 $5 $6) (sndOf3 $ unLoc $1) (thdOf3 $ unLoc $1) $2 $3
                            (snd $ unLoc $4) (snd $ unLoc $5)
                            (fmap reverse $6)
                            ((fstOf3 $ unLoc $1)++(fst $ unLoc $4)++(fst $ unLoc $5)) }
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty

          -- data/newtype family
        | 'data' 'family' type opt_datafam_kind_sig
                {% mkFamDecl (comb4 $1 $2 $3 $4) DataFamily TopLevel $3
                                   (snd $ unLoc $4) Nothing
                          (mj AnnData $1:mj AnnFamily $2:(fst $ unLoc $4)) }

-- standalone kind signature
standalone_kind_sig :: { LStandaloneKindSig GhcPs }
  : 'type' sks_vars '::' sigktype
      {% mkStandaloneKindSig (comb2 $1 $4) (L (gl $2) $ unLoc $2) $4
               [mj AnnType $1,mu AnnDcolon $3]}

-- See also: sig_vars
sks_vars :: { Located [LocatedN RdrName] }  -- Returned in reverse order
  : sks_vars ',' oqtycon
      {% case unLoc $1 of
           (h:t) -> do
             h' <- addTrailingCommaN h (gl $2)
             return (sLL $1 $> ($3 : h' : t)) }
  | oqtycon { sL1 $1 [$1] }

inst_decl :: { LInstDecl GhcPs }
        : 'instance' maybe_warning_pragma overlap_pragma inst_type where_inst
       {% do { (binds, sigs, _, ats, adts, _) <- cvBindsAndSigs (snd $ unLoc $5)
             ; let anns = (mj AnnInstance $1 : (fst $ unLoc $5))
             ; let cid = ClsInstDecl
                                  { cid_ext = ($2, anns, NoAnnSortKey)
                                  , cid_poly_ty = $4, cid_binds = binds
                                  , cid_sigs = mkClassOpSigs sigs
                                  , cid_tyfam_insts = ats
                                  , cid_overlap_mode = $3
                                  , cid_datafam_insts = adts }
             ; amsA' (L (comb3 $1 $4 $5)
                             (ClsInstD { cid_d_ext = noExtField, cid_inst = cid }))
                   } }

           -- type instance declarations
        | 'type' 'instance' ty_fam_inst_eqn
                {% mkTyFamInst (comb2 $1 $3) (unLoc $3)
                        (mj AnnType $1:mj AnnInstance $2:[]) }

          -- data/newtype instance declaration
        | data_or_newtype 'instance' capi_ctype datafam_inst_hdr constrs
                          maybe_derivings
            {% mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 (unLoc $4)
                                      Nothing (reverse (snd  $ unLoc $5))
                                              (fmap reverse $6)
                      ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)) }

          -- GADT instance declaration
        | data_or_newtype 'instance' capi_ctype datafam_inst_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3 (unLoc $4)
                                   (snd $ unLoc $5) (snd $ unLoc $6)
                                   (fmap reverse $7)
                     ((fst $ unLoc $1):mj AnnInstance $2
                       :(fst $ unLoc $5)++(fst $ unLoc $6)) }

overlap_pragma :: { Maybe (LocatedP OverlapMode) }
  : '{-# OVERLAPPABLE'    '#-}' {% fmap Just $ amsr (sLL $1 $> (Overlappable (getOVERLAPPABLE_PRAGs $1)))
                                       (AnnPragma (mo $1) (mc $2) []) }
  | '{-# OVERLAPPING'     '#-}' {% fmap Just $ amsr (sLL $1 $> (Overlapping (getOVERLAPPING_PRAGs $1)))
                                       (AnnPragma (mo $1) (mc $2) []) }
  | '{-# OVERLAPS'        '#-}' {% fmap Just $ amsr (sLL $1 $> (Overlaps (getOVERLAPS_PRAGs $1)))
                                       (AnnPragma (mo $1) (mc $2) []) }
  | '{-# INCOHERENT'      '#-}' {% fmap Just $ amsr (sLL $1 $> (Incoherent (getINCOHERENT_PRAGs $1)))
                                       (AnnPragma (mo $1) (mc $2) []) }
  | {- empty -}                 { Nothing }

deriv_strategy_no_via :: { LDerivStrategy GhcPs }
  : 'stock'                     {% amsA' (sL1 $1 (StockStrategy [mj AnnStock $1])) }
  | 'anyclass'                  {% amsA' (sL1 $1 (AnyclassStrategy [mj AnnAnyclass $1])) }
  | 'newtype'                   {% amsA' (sL1 $1 (NewtypeStrategy [mj AnnNewtype $1])) }

deriv_strategy_via :: { LDerivStrategy GhcPs }
  : 'via' sigktype          {% amsA' (sLL $1 $> (ViaStrategy (XViaStrategyPs [mj AnnVia $1] $2))) }

deriv_standalone_strategy :: { Maybe (LDerivStrategy GhcPs) }
  : 'stock'                     {% fmap Just $ amsA' (sL1 $1 (StockStrategy [mj AnnStock $1])) }
  | 'anyclass'                  {% fmap Just $ amsA' (sL1 $1 (AnyclassStrategy [mj AnnAnyclass $1])) }
  | 'newtype'                   {% fmap Just $ amsA' (sL1 $1 (NewtypeStrategy [mj AnnNewtype $1])) }
  | deriv_strategy_via          { Just $1 }
  | {- empty -}                 { Nothing }

-- Injective type families

opt_injective_info :: { Located ([AddEpAnn], Maybe (LInjectivityAnn GhcPs)) }
        : {- empty -}               { noLoc ([], Nothing) }
        | '|' injectivity_cond      { sLL $1 $> ([mj AnnVbar $1]
                                                , Just ($2)) }

injectivity_cond :: { LInjectivityAnn GhcPs }
        : tyvarid '->' inj_varids
           {% amsA' (sLL $1 $> (InjectivityAnn [mu AnnRarrow $2] $1 (reverse (unLoc $3)))) }

inj_varids :: { Located [LocatedN RdrName] }
        : inj_varids tyvarid  { sLL $1 $> ($2 : unLoc $1) }
        | tyvarid             { sL1  $1 [$1]               }

-- Closed type families

where_type_family :: { Located ([AddEpAnn],FamilyInfo GhcPs) }
        : {- empty -}                      { noLoc ([],OpenTypeFamily) }
        | 'where' ty_fam_inst_eqn_list
               { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                    ,ClosedTypeFamily (fmap reverse $ snd $ unLoc $2)) }

ty_fam_inst_eqn_list :: { Located ([AddEpAnn],Maybe [LTyFamInstEqn GhcPs]) }
        :     '{' ty_fam_inst_eqns '}'     { sLL $1 $> ([moc $1,mcc $3]
                                                ,Just (unLoc $2)) }
        | vocurly ty_fam_inst_eqns close   { let (L loc _) = $2 in
                                             L loc ([],Just (unLoc $2)) }
        |     '{' '..' '}'                 { sLL $1 $> ([moc $1,mj AnnDotdot $2
                                                 ,mcc $3],Nothing) }
        | vocurly '..' close               { let (L loc _) = $2 in
                                             L loc ([mj AnnDotdot $2],Nothing) }

ty_fam_inst_eqns :: { Located [LTyFamInstEqn GhcPs] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
                                      {% let (L loc eqn) = $3 in
                                         case unLoc $1 of
                                           [] -> return (sLL $1 $> (L loc eqn : unLoc $1))
                                           (h:t) -> do
                                             h' <- addTrailingSemiA h (gl $2)
                                             return (sLL $1 $> ($3 : h' : t)) }
        | ty_fam_inst_eqns ';'        {% case unLoc $1 of
                                           [] -> return (sLZ $1 $> (unLoc $1))
                                           (h:t) -> do
                                             h' <- addTrailingSemiA h (gl $2)
                                             return (sLZ $1 $>  (h':t)) }
        | ty_fam_inst_eqn             { sLL $1 $> [$1] }
        | {- empty -}                 { noLoc [] }

ty_fam_inst_eqn :: { LTyFamInstEqn GhcPs }
        : 'forall' tv_bndrs '.' type '=' ktype
              {% do { hintExplicitForall $1
                    ; tvbs <- fromSpecTyVarBndrs $2
                    ; let loc = comb2 $1 $>
                    ; !cs <- getCommentsFor loc
                    ; mkTyFamInstEqn loc (mkHsOuterExplicit (EpAnn (glEE $1 $3) (mu AnnForall $1, mj AnnDot $3) cs) tvbs) $4 $6 [mj AnnEqual $5] }}
        | type '=' ktype
              {% mkTyFamInstEqn (comb2 $1 $>) mkHsOuterImplicit $1 $3 (mj AnnEqual $2:[]) }
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
                {% liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4) DataFamily NotTopLevel $3
                                                  (snd $ unLoc $4) Nothing
                        (mj AnnData $1:$2++(fst $ unLoc $4))) }

           -- type family declarations, with optional 'family' keyword
           -- (can't use opt_instance because you get shift/reduce errors
        | 'type' type opt_at_kind_inj_sig
               {% liftM mkTyClD
                        (mkFamDecl (comb3 $1 $2 $3) OpenTypeFamily NotTopLevel $2
                                   (fst . snd $ unLoc $3)
                                   (snd . snd $ unLoc $3)
                         (mj AnnType $1:(fst $ unLoc $3)) )}
        | 'type' 'family' type opt_at_kind_inj_sig
               {% liftM mkTyClD
                        (mkFamDecl (comb3 $1 $3 $4) OpenTypeFamily NotTopLevel $3
                                   (fst . snd $ unLoc $4)
                                   (snd . snd $ unLoc $4)
                         (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)))}

           -- default type instances, with optional 'instance' keyword
        | 'type' ty_fam_inst_eqn
                {% liftM mkInstD (mkTyFamInst (comb2 $1 $2) (unLoc $2)
                          [mj AnnType $1]) }
        | 'type' 'instance' ty_fam_inst_eqn
                {% liftM mkInstD (mkTyFamInst (comb2 $1 $3) (unLoc $3)
                              (mj AnnType $1:mj AnnInstance $2:[]) )}

opt_family   :: { [AddEpAnn] }
              : {- empty -}   { [] }
              | 'family'      { [mj AnnFamily $1] }

opt_instance :: { [AddEpAnn] }
              : {- empty -} { [] }
              | 'instance'  { [mj AnnInstance $1] }

-- Associated type instances
--
at_decl_inst :: { LInstDecl GhcPs }
           -- type instance declarations, with optional 'instance' keyword
        : 'type' opt_instance ty_fam_inst_eqn
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
                {% mkTyFamInst (comb2 $1 $3) (unLoc $3)
                          (mj AnnType $1:$2) }

        -- data/newtype instance declaration, with optional 'instance' keyword
        | data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
               {% mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 (unLoc $4)
                                    Nothing (reverse (snd $ unLoc $5))
                                            (fmap reverse $6)
                        ((fst $ unLoc $1):$2++(fst $ unLoc $5)) }

        -- GADT instance declaration, with optional 'instance' keyword
        | data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
                {% mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3
                                (unLoc $4) (snd $ unLoc $5) (snd $ unLoc $6)
                                (fmap reverse $7)
                        ((fst $ unLoc $1):$2++(fst $ unLoc $5)++(fst $ unLoc $6)) }

type_data_or_newtype :: { Located ([AddEpAnn], Bool, NewOrData) }
        : 'data'        { sL1 $1 ([mj AnnData    $1],            False,DataType) }
        | 'newtype'     { sL1 $1 ([mj AnnNewtype $1],            False,NewType) }
        | 'type' 'data' { sL1 $1 ([mj AnnType $1, mj AnnData $2],True ,DataType) }

data_or_newtype :: { Located (AddEpAnn, NewOrData) }
        : 'data'        { sL1 $1 (mj AnnData    $1,DataType) }
        | 'newtype'     { sL1 $1 (mj AnnNewtype $1,NewType) }

-- Family result/return kind signatures

opt_kind_sig :: { Located ([AddEpAnn], Maybe (LHsKind GhcPs)) }
        :               { noLoc     ([]               , Nothing) }
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], Just $2) }

opt_datafam_kind_sig :: { Located ([AddEpAnn], LFamilyResultSig GhcPs) }
        :               { noLoc     ([]               , noLocA (NoSig noExtField)         )}
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], sLLa $1 $> (KindSig noExtField $2))}

opt_tyfam_kind_sig :: { Located ([AddEpAnn], LFamilyResultSig GhcPs) }
        :              { noLoc     ([]               , noLocA     (NoSig    noExtField)   )}
        | '::' kind    { sLL $1 $> ([mu AnnDcolon $1], sLLa $1 $> (KindSig  noExtField $2))}
        | '='  tv_bndr {% do { tvb <- fromSpecTyVarBndr $2
                             ; return $ sLL $1 $> ([mj AnnEqual $1], sLLa $1 $> (TyVarSig noExtField tvb))} }

opt_at_kind_inj_sig :: { Located ([AddEpAnn], ( LFamilyResultSig GhcPs
                                            , Maybe (LInjectivityAnn GhcPs)))}
        :            { noLoc ([], (noLocA (NoSig noExtField), Nothing)) }
        | '::' kind  { sLL $1 $> ( [mu AnnDcolon $1]
                                 , (sL1a $> (KindSig noExtField $2), Nothing)) }
        | '='  tv_bndr_no_braces '|' injectivity_cond
                {% do { tvb <- fromSpecTyVarBndr $2
                      ; return $ sLL $1 $> ([mj AnnEqual $1, mj AnnVbar $3]
                                           , (sLLa $1 $2 (TyVarSig noExtField tvb), Just $4))} }

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--      T a b
--      Eq a => T a
--      (Eq a, Ord b) => T a b
--      T Int [a]                       -- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (Maybe (LHsContext GhcPs), LHsType GhcPs) }
        : context '=>' type         {% acs (comb2 $1 $>) (\loc cs -> (L loc (Just (addTrailingDarrowC $1 $2 cs), $3))) }
        | type                      { sL1 $1 (Nothing, $1) }

datafam_inst_hdr :: { Located (Maybe (LHsContext GhcPs), HsOuterFamEqnTyVarBndrs GhcPs, LHsType GhcPs) }
        : 'forall' tv_bndrs '.' context '=>' type   {% hintExplicitForall $1
                                                       >> fromSpecTyVarBndrs $2
                                                         >>= \tvbs ->
                                                             (acs (comb2 $1 $>) (\loc cs -> (L loc
                                                                                  (Just ( addTrailingDarrowC $4 $5 cs)
                                                                                        , mkHsOuterExplicit (EpAnn (glEE $1 $3) (mu AnnForall $1, mj AnnDot $3) emptyComments) tvbs, $6))))
                                                    }
        | 'forall' tv_bndrs '.' type   {% do { hintExplicitForall $1
                                             ; tvbs <- fromSpecTyVarBndrs $2
                                             ; let loc = comb2 $1 $>
                                             ; !cs <- getCommentsFor loc
                                             ; return (sL loc (Nothing, mkHsOuterExplicit (EpAnn (glEE $1 $3) (mu AnnForall $1, mj AnnDot $3) cs) tvbs, $4))
                                       } }
        | context '=>' type         {% acs (comb2 $1 $>) (\loc cs -> (L loc (Just (addTrailingDarrowC $1 $2 cs), mkHsOuterImplicit, $3))) }
        | type                      { sL1 $1 (Nothing, mkHsOuterImplicit, $1) }


capi_ctype :: { Maybe (LocatedP CType) }
capi_ctype : '{-# CTYPE' STRING STRING '#-}'
                       {% fmap Just $ amsr (sLL $1 $> (CType (getCTYPEs $1) (Just (Header (getSTRINGs $2) (getSTRING $2)))
                                        (getSTRINGs $3,getSTRING $3)))
                              (AnnPragma (mo $1) (mc $4) [mj AnnHeader $2,mj AnnVal $3]) }

           | '{-# CTYPE'        STRING '#-}'
                       {% fmap Just $ amsr (sLL $1 $> (CType (getCTYPEs $1) Nothing (getSTRINGs $2, getSTRING $2)))
                              (AnnPragma (mo $1) (mc $3) [mj AnnVal $2]) }

           |           { Nothing }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl GhcPs }
  : 'deriving' deriv_standalone_strategy 'instance' maybe_warning_pragma overlap_pragma inst_type
                {% do { let { err = text "in the stand-alone deriving instance"
                                    <> colon <+> quotes (ppr $6) }
                      ; amsA' (sLL $1 $>
                                 (DerivDecl ($4, [mj AnnDeriving $1, mj AnnInstance $3]) (mkHsWildCardBndrs $6) $2 $5)) }}

-----------------------------------------------------------------------------
-- Role annotations

role_annot :: { LRoleAnnotDecl GhcPs }
role_annot : 'type' 'role' oqtycon maybe_roles
          {% mkRoleAnnotDecl (comb3 $1 $4 $3) $3 (reverse (unLoc $4))
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
         {%      let (name, args, as ) = $2 in
                 amsA' (sLL $1 $> . ValD noExtField $ mkPatSynBind name args $4
                                                    ImplicitBidirectional
                      (as ++ [mj AnnPattern $1, mj AnnEqual $3])) }

        | 'pattern' pattern_synonym_lhs '<-' pat
         {%    let (name, args, as) = $2 in
               amsA' (sLL $1 $> . ValD noExtField $ mkPatSynBind name args $4 Unidirectional
                       (as ++ [mj AnnPattern $1,mu AnnLarrow $3])) }

        | 'pattern' pattern_synonym_lhs '<-' pat where_decls
            {% do { let (name, args, as) = $2
                  ; mg <- mkPatSynMatchGroup name $5
                  ; amsA' (sLL $1 $> . ValD noExtField $
                           mkPatSynBind name args $4 (ExplicitBidirectional mg)
                            (as ++ [mj AnnPattern $1,mu AnnLarrow $3]))
                   }}

pattern_synonym_lhs :: { (LocatedN RdrName, HsPatSynDetails GhcPs, [AddEpAnn]) }
        : con vars0 { ($1, PrefixCon noTypeArgs $2, []) }
        | varid conop varid { ($2, InfixCon $1 $3, []) }
        | con '{' cvars1 '}' { ($1, RecCon $3, [moc $2, mcc $4] ) }

vars0 :: { [LocatedN RdrName] }
        : {- empty -}                 { [] }
        | varid vars0                 { $1 : $2 }

cvars1 :: { [RecordPatSynField GhcPs] }
       : var                          { [RecordPatSynField (mkFieldOcc $1) $1] }
       | var ',' cvars1               {% do { h <- addTrailingCommaN $1 (gl $2)
                                            ; return ((RecordPatSynField (mkFieldOcc h) h) : $3 )}}

where_decls :: { LocatedL (OrdList (LHsDecl GhcPs)) }
        : 'where' '{' decls '}'       {% amsr (sLL $1 $> (snd $ unLoc $3))
                                              (AnnList (Just $ glR $3) (Just $ moc $2) (Just $ mcc $4) (mj AnnWhere $1: (fst $ unLoc $3)) []) }
        | 'where' vocurly decls close {% amsr (sLL $1 $3 (snd $ unLoc $3))
                                              (AnnList (Just $ glR $3) Nothing Nothing (mj AnnWhere $1: (fst $ unLoc $3)) []) }

pattern_synonym_sig :: { LSig GhcPs }
        : 'pattern' con_list '::' sigtype
                   {% amsA' (sLL $1 $>
                                $ PatSynSig (AnnSig (mu AnnDcolon $3) [mj AnnPattern $1])
                                  (toList $ unLoc $2) $4) }

qvarcon :: { LocatedN RdrName }
        : qvar                          { $1 }
        | qcon                          { $1 }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { LHsDecl GhcPs }
decl_cls  : at_decl_cls                 { $1 }
          | decl                        { $1 }

          -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtype
                    {% runPV (unECP $2) >>= \ $2 ->
                       do { v <- checkValSigLhs $2
                          ; let err = text "in default signature" <> colon <+>
                                      quotes (ppr $2)
                          ; amsA' (sLL $1 $> $ SigD noExtField $ ClassOpSig (AnnSig (mu AnnDcolon $3) [mj AnnDefault $1]) True [v] $4) }}

decls_cls :: { Located ([AddEpAnn],OrdList (LHsDecl GhcPs)) }  -- Reversed
          : decls_cls ';' decl_cls      {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> ((fst $ unLoc $1) ++ (mz AnnSemi $2)
                                                                    , unitOL $3))
                                            else case (snd $ unLoc $1) of
                                              SnocOL hs t -> do
                                                 t' <- addTrailingSemiA t (gl $2)
                                                 return (sLL $1 $> (fst $ unLoc $1
                                                                , snocOL hs t' `appOL` unitOL $3)) }
          | decls_cls ';'               {% if isNilOL (snd $ unLoc $1)
                                             then return (sLZ $1 $> ( (fst $ unLoc $1) ++ (mz AnnSemi $2)
                                                                                   ,snd $ unLoc $1))
                                             else case (snd $ unLoc $1) of
                                               SnocOL hs t -> do
                                                  t' <- addTrailingSemiA t (gl $2)
                                                  return (sLZ $1 $> (fst $ unLoc $1
                                                                 , snocOL hs t')) }
          | decl_cls                    { sL1 $1 ([], unitOL $1) }
          | {- empty -}                 { noLoc ([],nilOL) }

decllist_cls
        :: { Located ([AddEpAnn]
                     , OrdList (LHsDecl GhcPs)
                     , EpLayout) }      -- Reversed
        : '{'         decls_cls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                             ,snd $ unLoc $2, epExplicitBraces $1 $3) }
        |     vocurly decls_cls close   { let { L l (anns, decls) = $2 }
                                           in L l (anns, decls, EpVirtualBraces (getVOCURLY $1)) }

-- Class body
--
where_cls :: { Located ([AddEpAnn]
                       ,(OrdList (LHsDecl GhcPs))    -- Reversed
                       ,EpLayout) }
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_cls          { sLL $1 $> (mj AnnWhere $1:(fstOf3 $ unLoc $2)
                                             ,sndOf3 $ unLoc $2,thdOf3 $ unLoc $2) }
        | {- empty -}                   { noLoc ([],nilOL,EpNoLayout) }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl GhcPs)) }
decl_inst  : at_decl_inst               { sL1 $1 (unitOL (sL1a $1 (InstD noExtField (unLoc $1)))) }
           | decl                       { sL1 $1 (unitOL $1) }

decls_inst :: { Located ([AddEpAnn],OrdList (LHsDecl GhcPs)) }   -- Reversed
           : decls_inst ';' decl_inst   {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> ((fst $ unLoc $1) ++ (mz AnnSemi $2)
                                                                    , unLoc $3))
                                             else case (snd $ unLoc $1) of
                                               SnocOL hs t -> do
                                                  t' <- addTrailingSemiA t (gl $2)
                                                  return (sLL $1 $> (fst $ unLoc $1
                                                                 , snocOL hs t' `appOL` unLoc $3)) }
           | decls_inst ';'             {% if isNilOL (snd $ unLoc $1)
                                             then return (sLZ $1 $> ((fst $ unLoc $1) ++ (mz AnnSemi $2)
                                                                                   ,snd $ unLoc $1))
                                             else case (snd $ unLoc $1) of
                                               SnocOL hs t -> do
                                                  t' <- addTrailingSemiA t (gl $2)
                                                  return (sLZ $1 $> (fst $ unLoc $1
                                                                 , snocOL hs t')) }
           | decl_inst                  { sL1 $1 ([],unLoc $1) }
           | {- empty -}                { noLoc ([],nilOL) }

decllist_inst
        :: { Located ([AddEpAnn]
                     , OrdList (LHsDecl GhcPs)) }      -- Reversed
        : '{'         decls_inst '}'    { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2),snd $ unLoc $2) }
        |     vocurly decls_inst close  { L (gl $2) (unLoc $2) }

-- Instance body
--
where_inst :: { Located ([AddEpAnn]
                        , OrdList (LHsDecl GhcPs)) }   -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_inst         { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,(snd $ unLoc $2)) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in binding groups other than classes and instances
--
decls   :: { Located ([AddEpAnn], OrdList (LHsDecl GhcPs)) }
        : decls ';' decl    {% if isNilOL (snd $ unLoc $1)
                                 then return (sLL $1 $> ((fst $ unLoc $1) ++ (msemiA $2)
                                                        , unitOL $3))
                                 else case (snd $ unLoc $1) of
                                   SnocOL hs t -> do
                                      t' <- addTrailingSemiA t (gl $2)
                                      let { this = unitOL $3;
                                            rest = snocOL hs t';
                                            these = rest `appOL` this }
                                      return (rest `seq` this `seq` these `seq`
                                                 (sLL $1 $> (fst $ unLoc $1, these))) }
        | decls ';'          {% if isNilOL (snd $ unLoc $1)
                                  then return (sLZ $1 $> (((fst $ unLoc $1) ++ (msemiA $2)
                                                          ,snd $ unLoc $1)))
                                  else case (snd $ unLoc $1) of
                                    SnocOL hs t -> do
                                       t' <- addTrailingSemiA t (gl $2)
                                       return (sLZ $1 $> (fst $ unLoc $1
                                                      , snocOL hs t')) }
        | decl                          { sL1 $1 ([], unitOL $1) }
        | {- empty -}                   { noLoc ([],nilOL) }

decllist :: { Located (AnnList,Located (OrdList (LHsDecl GhcPs))) }
        : '{'            decls '}'     { sLL $1 $> (AnnList (Just $ glR $2) (Just $ moc $1) (Just $ mcc $3)  (fst $ unLoc $2) []
                                                   ,sL1 $2 $ snd $ unLoc $2) }
        |     vocurly    decls close   { L (gl $2) (AnnList (Just $ glR $2) Nothing Nothing (fst $ unLoc $2) []
                                                   ,sL1 $2 $ snd $ unLoc $2) }

-- Binding groups other than those of class and instance declarations
--
binds   ::  { Located (HsLocalBinds GhcPs) }
                                         -- May have implicit parameters
                                                -- No type declarations
        : decllist          {% do { val_binds <- cvBindGroup (unLoc $ snd $ unLoc $1)
                                  ; !cs <- getCommentsFor (gl $1)
                                  ; return (sL1 $1 $ HsValBinds (fixValbindsAnn $ EpAnn (glR $1) (fst $ unLoc $1) cs) val_binds)} }

        | '{'            dbinds '}'     {% acs (comb3 $1 $2 $3) (\loc cs -> (L loc
                                             $ HsIPBinds (EpAnn (spanAsAnchor (comb3 $1 $2 $3)) (AnnList (Just$ glR $2) (Just $ moc $1) (Just $ mcc $3) [] []) cs) (IPBinds noExtField (reverse $ unLoc $2)))) }

        |     vocurly    dbinds close   {% acs (gl $2) (\loc cs -> (L loc
                                             $ HsIPBinds (EpAnn (glR $1) (AnnList (Just $ glR $2) Nothing Nothing [] []) cs) (IPBinds noExtField (reverse $ unLoc $2)))) }


wherebinds :: { Maybe (Located (HsLocalBinds GhcPs, Maybe EpAnnComments )) }
                                                -- May have implicit parameters
                                                -- No type declarations
        : 'where' binds                 {% do { r <- acs (comb2 $1 $>) (\loc cs ->
                                                (L loc (annBinds (mj AnnWhere $1) cs (unLoc $2))))
                                              ; return $ Just r} }
        | {- empty -}                   { Nothing }

-----------------------------------------------------------------------------
-- Transformation Rules

rules   :: { [LRuleDecl GhcPs] } -- Reversed
        :  rules ';' rule              {% case $1 of
                                            [] -> return ($3:$1)
                                            (h:t) -> do
                                              h' <- addTrailingSemiA h (gl $2)
                                              return ($3:h':t) }
        |  rules ';'                   {% case $1 of
                                            [] -> return $1
                                            (h:t) -> do
                                              h' <- addTrailingSemiA h (gl $2)
                                              return (h':t) }
        |  rule                        { [$1] }
        |  {- empty -}                 { [] }

rule    :: { LRuleDecl GhcPs }
        : STRING rule_activation rule_foralls infixexp '=' exp
         {%runPV (unECP $4) >>= \ $4 ->
           runPV (unECP $6) >>= \ $6 ->
           amsA' (sLL $1 $> $ HsRule
                                   { rd_ext = (((fstOf3 $3) (mj AnnEqual $5 : (fst $2))), getSTRINGs $1)
                                   , rd_name = L (noAnnSrcSpan $ gl $1) (getSTRING $1)
                                   , rd_act = (snd $2) `orElse` AlwaysActive
                                   , rd_tyvs = sndOf3 $3, rd_tmvs = thdOf3 $3
                                   , rd_lhs = $4, rd_rhs = $6 }) }

-- Rules can be specified to be NeverActive, unlike inline/specialize pragmas
rule_activation :: { ([AddEpAnn],Maybe Activation) }
        -- See Note [%shift: rule_activation -> {- empty -}]
        : {- empty -} %shift                    { ([],Nothing) }
        | rule_explicit_activation              { (fst $1,Just (snd $1)) }

-- This production is used to parse the tilde syntax in pragmas such as
--   * {-# INLINE[~2] ... #-}
--   * {-# SPECIALISE [~ 001] ... #-}
--   * {-# RULES ... [~0] ... g #-}
-- Note that it can be written either
--   without a space [~1]  (the PREFIX_TILDE case), or
--   with    a space [~ 1] (the VARSYM case).
-- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
rule_activation_marker :: { [AddEpAnn] }
      : PREFIX_TILDE { [mj AnnTilde $1] }
      | VARSYM  {% if (getVARSYM $1 == fsLit "~")
                   then return [mj AnnTilde $1]
                   else do { addError $ mkPlainErrorMsgEnvelope (getLoc $1) $
                               PsErrInvalidRuleActivationMarker
                           ; return [] } }

rule_explicit_activation :: { ([AddEpAnn]
                              ,Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mos $1,mj AnnVal $2,mcs $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' rule_activation_marker INTEGER ']'
                                { ($2++[mos $1,mj AnnVal $3,mcs $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }
        | '[' rule_activation_marker ']'
                                { ($2++[mos $1,mcs $3]
                                  ,NeverActive) }

rule_foralls :: { ([AddEpAnn] -> HsRuleAnn, Maybe [LHsTyVarBndr () GhcPs], [LRuleBndr GhcPs]) }
        : 'forall' rule_vars '.' 'forall' rule_vars '.'    {% let tyvs = mkRuleTyVarBndrs $2
                                                              in hintExplicitForall $1
                                                              >> checkRuleTyVarBndrNames (mkRuleTyVarBndrs $2)
                                                              >> return (\anns -> HsRuleAnn
                                                                          (Just (mu AnnForall $1,mj AnnDot $3))
                                                                          (Just (mu AnnForall $4,mj AnnDot $6))
                                                                          anns,
                                                                         Just (mkRuleTyVarBndrs $2), mkRuleBndrs $5) }
        | 'forall' rule_vars '.'                           { (\anns -> HsRuleAnn Nothing (Just (mu AnnForall $1,mj AnnDot $3)) anns,
                                                              Nothing, mkRuleBndrs $2) }
        -- See Note [%shift: rule_foralls -> {- empty -}]
        | {- empty -}            %shift                    { (\anns -> HsRuleAnn Nothing Nothing anns, Nothing, []) }

rule_vars :: { [LRuleTyTmVar] }
        : rule_var rule_vars                    { $1 : $2 }
        | {- empty -}                           { [] }

rule_var :: { LRuleTyTmVar }
        : varid                         { sL1a $1 (RuleTyTmVar noAnn $1 Nothing) }
        | '(' varid '::' ctype ')'      {% amsA' (sLL $1 $> (RuleTyTmVar [mop $1,mu AnnDcolon $3,mcp $5] $2 (Just $4))) }

{- Note [Parsing explicit foralls in Rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We really want the above definition of rule_foralls to be:

  rule_foralls : 'forall' tv_bndrs '.' 'forall' rule_vars '.'
               | 'forall' rule_vars '.'
               | {- empty -}

where rule_vars (term variables) can be named "family" or "role",
but tv_vars (type variables) cannot be. However, such a definition results
in a reduce/reduce conflict. For example, when parsing:
> {-# RULE "name" forall a ... #-}
before the '...' it is impossible to determine whether we should be in the
first or second case of the above.

This is resolved by using rule_vars (which is more general) for both, and
ensuring that type-level quantified variables do not have the names "forall",
"family", or "role" in the function 'checkRuleTyVarBndrNames' in
GHC.Parser.PostProcess.
Thus, whenever the definition of tyvarid (used for tv_bndrs) is changed relative
to varid (used for rule_vars), 'checkRuleTyVarBndrNames' must be updated.
-}

-----------------------------------------------------------------------------
-- Warnings and deprecations (c.f. rules)

maybe_warning_pragma :: { Maybe (LWarningTxt GhcPs) }
        : '{-# DEPRECATED' strings '#-}'
                            {% fmap Just $ amsr (sLL $1 $> $ DeprecatedTxt (getDEPRECATED_PRAGs $1) (map stringLiteralToHsDocWst $ snd $ unLoc $2))
                                (AnnPragma (mo $1) (mc $3) (fst $ unLoc $2)) }
        | '{-# WARNING' warning_category strings '#-}'
                            {% fmap Just $ amsr (sLL $1 $> $ WarningTxt $2 (getWARNING_PRAGs $1) (map stringLiteralToHsDocWst $ snd $ unLoc $3))
                                (AnnPragma (mo $1) (mc $4) (fst $ unLoc $3))}
        |  {- empty -}      { Nothing }

warning_category :: { Maybe (LocatedE InWarningCategory) }
        : 'in' STRING                  { Just (reLoc $ sLL $1 $> $ InWarningCategory (epTok $1) (getSTRINGs $2)
                                                                    (reLoc $ sL1 $2 $ mkWarningCategory (getSTRING $2))) }
        | {- empty -}                  { Nothing }

warnings :: { OrdList (LWarnDecl GhcPs) }
        : warnings ';' warning         {% if isNilOL $1
                                           then return ($1 `appOL` $3)
                                           else case $1 of
                                             SnocOL hs t -> do
                                              t' <- addTrailingSemiA t (gl $2)
                                              return (snocOL hs t' `appOL` $3) }
        | warnings ';'                 {% if isNilOL $1
                                           then return $1
                                           else case $1 of
                                             SnocOL hs t -> do
                                              t' <- addTrailingSemiA t (gl $2)
                                              return (snocOL hs t') }
        | warning                      { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
warning :: { OrdList (LWarnDecl GhcPs) }
        : warning_category namespace_spec namelist strings
                {% fmap unitOL $ amsA' (L (comb4 $1 $2 $3 $4)
                     (Warning (unLoc $2, fst $ unLoc $4) (unLoc $3)
                              (WarningTxt $1 NoSourceText $ map stringLiteralToHsDocWst $ snd $ unLoc $4))) }

namespace_spec :: { Located NamespaceSpecifier }
  : 'type'      { sL1 $1 $ TypeNamespaceSpecifier (epTok $1) }
  | 'data'      { sL1 $1 $ DataNamespaceSpecifier (epTok $1) }
  | {- empty -} { sL0    $ NoNamespaceSpecifier }

deprecations :: { OrdList (LWarnDecl GhcPs) }
        : deprecations ';' deprecation
                                       {% if isNilOL $1
                                           then return ($1 `appOL` $3)
                                           else case $1 of
                                             SnocOL hs t -> do
                                              t' <- addTrailingSemiA t (gl $2)
                                              return (snocOL hs t' `appOL` $3) }
        | deprecations ';'             {% if isNilOL $1
                                           then return $1
                                           else case $1 of
                                             SnocOL hs t -> do
                                              t' <- addTrailingSemiA t (gl $2)
                                              return (snocOL hs t') }
        | deprecation                  { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LWarnDecl GhcPs) }
        : namespace_spec namelist strings
             {% fmap unitOL $ amsA' (sL (comb3 $1 $2 $>) $ (Warning (unLoc $1, fst $ unLoc $3) (unLoc $2)
                                          (DeprecatedTxt NoSourceText $ map stringLiteralToHsDocWst $ snd $ unLoc $3))) }

strings :: { Located ([AddEpAnn],[Located StringLiteral]) }
    : STRING { sL1 $1 ([],[L (gl $1) (getStringLiteral $1)]) }
    | '[' stringlist ']' { sLL $1 $> $ ([mos $1,mcs $3],fromOL (unLoc $2)) }

stringlist :: { Located (OrdList (Located StringLiteral)) }
    : stringlist ',' STRING {% if isNilOL (unLoc $1)
                                then return (sLL $1 $> (unLoc $1 `snocOL`
                                                  (L (gl $3) (getStringLiteral $3))))
                                else case (unLoc $1) of
                                   SnocOL hs t -> do
                                     let { t' = addTrailingCommaS t (glAA $2) }
                                     return (sLL $1 $> (snocOL hs t' `snocOL`
                                                  (L (gl $3) (getStringLiteral $3))))

}
    | STRING                { sLL $1 $> (unitOL (L (gl $1) (getStringLiteral $1))) }
    | {- empty -}           { noLoc nilOL }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl GhcPs }
    : '{-# ANN' name_var aexp '#-}'      {% runPV (unECP $3) >>= \ $3 ->
                                            amsA' (sLL $1 $> (AnnD noExtField $ HsAnnotation
                                            (AnnPragma (mo $1) (mc $4) [],
                                            (getANN_PRAGs $1))
                                            (ValueAnnProvenance $2) $3)) }

    | '{-# ANN' 'type' otycon aexp '#-}' {% runPV (unECP $4) >>= \ $4 ->
                                            amsA' (sLL $1 $> (AnnD noExtField $ HsAnnotation
                                            (AnnPragma (mo $1) (mc $5) [mj AnnType $2],
                                            (getANN_PRAGs $1))
                                            (TypeAnnProvenance $3) $4)) }

    | '{-# ANN' 'module' aexp '#-}'      {% runPV (unECP $3) >>= \ $3 ->
                                            amsA' (sLL $1 $> (AnnD noExtField $ HsAnnotation
                                                (AnnPragma (mo $1) (mc $4) [mj AnnModule $2],
                                                (getANN_PRAGs $1))
                                                 ModuleAnnProvenance $3)) }

-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { Located ([AddEpAnn], [AddEpAnn] -> HsDecl GhcPs) }
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

fspec :: { Located ([AddEpAnn]
                    ,(Located StringLiteral, LocatedN RdrName, LHsSigType GhcPs)) }
       : STRING var '::' sigtype        { sLL $1 $> ([mu AnnDcolon $3]
                                             ,(L (getLoc $1)
                                                    (getStringLiteral $1), $2, $4)) }
       |        var '::' sigtype        { sLL $1 $> ([mu AnnDcolon $2]
                                             ,(noLoc (StringLiteral NoSourceText nilFS Nothing), $1, $3)) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { Maybe (AddEpAnn, LHsType GhcPs) }
        : {- empty -}                   { Nothing }
        | '::' ctype                    { Just (mu AnnDcolon $1, $2) }

opt_tyconsig :: { ([AddEpAnn], Maybe (LocatedN RdrName)) }
             : {- empty -}              { ([], Nothing) }
             | '::' gtycon              { ([mu AnnDcolon $1], Just $2) }

-- Like ktype, but for types that obey the forall-or-nothing rule.
-- See Note [forall-or-nothing rule] in GHC.Hs.Type.
sigktype :: { LHsSigType GhcPs }
        : sigtype              { $1 }
        | ctype '::' kind      {% amsA' (sLL $1 $> $ mkHsImplicitSigType $
                                         sLLa $1 $> $ HsKindSig [mu AnnDcolon $2] $1 $3) }

-- Like ctype, but for types that obey the forall-or-nothing rule.
-- See Note [forall-or-nothing rule] in GHC.Hs.Type. To avoid duplicating the
-- logic in ctype here, we simply reuse the ctype production and perform
-- surgery on the LHsType it returns to turn it into an LHsSigType.
sigtype :: { LHsSigType GhcPs }
        : ctype                            { hsTypeToHsSigType $1 }

sig_vars :: { Located [LocatedN RdrName] }    -- Returned in reversed order
         : sig_vars ',' var           {% case unLoc $1 of
                                           [] -> return (sLL $1 $> ($3 : unLoc $1))
                                           (h:t) -> do
                                             h' <- addTrailingCommaN h (gl $2)
                                             return (sLL $1 $> ($3 : h' : t)) }
         | var                        { sL1 $1 [$1] }

sigtypes1 :: { OrdList (LHsSigType GhcPs) }
   : sigtype                 { unitOL $1 }
   | sigtype ',' sigtypes1   {% do { st <- addTrailingCommaA $1 (gl $2)
                                   ; return $ unitOL st `appOL` $3 } }
-----------------------------------------------------------------------------
-- Types

unpackedness :: { Located UnpackednessPragma }
        : '{-# UNPACK' '#-}'   { sLL $1 $> (UnpackednessPragma [mo $1, mc $2] (getUNPACK_PRAGs $1) SrcUnpack) }
        | '{-# NOUNPACK' '#-}' { sLL $1 $> (UnpackednessPragma [mo $1, mc $2] (getNOUNPACK_PRAGs $1) SrcNoUnpack) }

forall_telescope :: { Located (HsForAllTelescope GhcPs) }
        : 'forall' tv_bndrs '.'  {% do { hintExplicitForall $1
                                       ; acs (comb2 $1 $>) (\loc cs -> (L loc $
                                           mkHsForAllInvisTele (EpAnn (glEE $1 $>) (mu AnnForall $1,mu AnnDot $3) cs) $2 )) }}
        | 'forall' tv_bndrs '->' {% do { hintExplicitForall $1
                                       ; req_tvbs <- fromSpecTyVarBndrs $2
                                       ; acs (comb2 $1 $>) (\loc cs -> (L loc $
                                           mkHsForAllVisTele (EpAnn (glEE $1 $>) (mu AnnForall $1,mu AnnRarrow $3) cs) req_tvbs )) }}

-- A ktype is a ctype, possibly with a kind annotation
ktype :: { LHsType GhcPs }
        : ctype                { $1 }
        | ctype '::' kind      {% amsA' (sLL $1 $> $ HsKindSig [mu AnnDcolon $2] $1 $3) }

-- A ctype is a for-all type
ctype   :: { LHsType GhcPs }
        : forall_telescope ctype      { sLLa $1 $> $
                                              HsForAllTy { hst_tele = unLoc $1
                                                         , hst_xforall = noExtField
                                                         , hst_body = $2 } }
        | context '=>' ctype          {% acsA (comb2 $1 $>) (\loc cs -> (L loc $
                                            HsQualTy { hst_ctxt = addTrailingDarrowC $1 $2 cs
                                                     , hst_xqual = NoExtField
                                                     , hst_body = $3 })) }

        | ipvar '::' ctype            {% amsA' (sLL $1 $> (HsIParamTy [mu AnnDcolon $2] (reLoc $1) $3)) }
        | type                        { $1 }

----------------------
-- Notes for 'context'
-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--      (Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>

context :: { LHsContext GhcPs }
        :  btype                        {% checkContext $1 }

{- Note [GADT decl discards annotations]
~~~~~~~~~~~~~~~~~~~~~
The type production for

    btype `->` ctype

add the AnnRarrow annotation twice, in different places.

This is because if the type is processed as usual, it belongs on the annotations
for the type as a whole.

But if the type is passed to mkGadtDecl, it discards the top level SrcSpan, and
the top-level annotation will be disconnected. Hence for this specific case it
is connected to the first type too.
-}

type :: { LHsType GhcPs }
        -- See Note [%shift: type -> btype]
        : btype %shift                 { $1 }
        | btype '->' ctype             {% amsA' (sLL $1 $>
                                            $ HsFunTy noExtField (HsUnrestrictedArrow (epUniTok $2)) $1 $3) }

        | btype mult '->' ctype        {% hintLinear (getLoc $2)
                                       >> let arr = (unLoc $2) (epUniTok $3)
                                          in amsA' (sLL $1 $> $ HsFunTy noExtField arr $1 $4) }

        | btype '->.' ctype            {% hintLinear (getLoc $2) >>
                                          amsA' (sLL $1 $> $ HsFunTy noExtField (HsLinearArrow (EpLolly (epTok $2))) $1 $3) }
                                              -- [mu AnnLollyU $2] }

mult :: { Located (EpUniToken "->" "\8594" -> HsArrow GhcPs) }
        : PREFIX_PERCENT atype          { sLL $1 $> (mkMultTy (epTok $1) $2) }

btype :: { LHsType GhcPs }
        : infixtype                     {% runPV $1 }

infixtype :: { forall b. DisambTD b => PV (LocatedA b) }
        -- See Note [%shift: infixtype -> ftype]
        : ftype %shift                  { $1 }
        | ftype tyop infixtype          { $1 >>= \ $1 ->
                                          $3 >>= \ $3 ->
                                          do { let (op, prom) = $2
                                             ; when (looksLikeMult $1 op $3) $ hintLinear (getLocA op)
                                             ; mkHsOpTyPV prom $1 op $3 } }
        | unpackedness infixtype        { $2 >>= \ $2 ->
                                          mkUnpackednessPV $1 $2 }

ftype :: { forall b. DisambTD b => PV (LocatedA b) }
        : atype                         { mkHsAppTyHeadPV $1 }
        | tyop                          { failOpFewArgs (fst $1) }
        | ftype tyarg                   { $1 >>= \ $1 ->
                                          mkHsAppTyPV $1 $2 }
        | ftype PREFIX_AT atype         { $1 >>= \ $1 ->
                                          mkHsAppKindTyPV $1 (epTok $2) $3 }

tyarg :: { LHsType GhcPs }
        : atype                         { $1 }
        | unpackedness atype            {% addUnpackednessP $1 $2 }

tyop :: { (LocatedN RdrName, PromotionFlag) }
        : qtyconop                      { ($1, NotPromoted) }
        | tyvarop                       { ($1, NotPromoted) }
        | SIMPLEQUOTE qconop            {% do { op <- amsr (sLL $1 $> (unLoc $2))
                                                           (NameAnnQuote (glAA $1) (gl $2) [])
                                              ; return (op, IsPromoted) } }
        | SIMPLEQUOTE varop             {% do { op <- amsr (sLL $1 $> (unLoc $2))
                                                           (NameAnnQuote (glAA $1) (gl $2) [])
                                              ; return (op, IsPromoted) } }

atype :: { LHsType GhcPs }
        : ntgtycon                       {% amsA' (sL1 $1 (HsTyVar [] NotPromoted $1)) }      -- Not including unit tuples
        -- See Note [%shift: atype -> tyvar]
        | tyvar %shift                   {% amsA' (sL1 $1 (HsTyVar [] NotPromoted $1)) }      -- (See Note [Unit tuples])
        | '*'                            {% do { warnStarIsType (getLoc $1)
                                               ; return $ sL1a $1 (HsStarTy noExtField (isUnicode $1)) } }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | PREFIX_TILDE atype             {% amsA' (sLL $1 $> (mkBangTy [mj AnnTilde $1] SrcLazy $2)) }
        | PREFIX_BANG  atype             {% amsA' (sLL $1 $> (mkBangTy [mj AnnBang $1] SrcStrict $2)) }

        | '{' fielddecls '}'             {% do { decls <- amsA' (sLL $1 $> $ HsRecTy (AnnList (listAsAnchorM $2) (Just $ moc $1) (Just $ mcc $3) [] []) $2)
                                               ; checkRecordSyntax decls }}
                                                        -- Constructor sigs only

        -- List and tuple syntax whose interpretation depends on the extension ListTuplePuns.
        | '(' ')'                        {% amsA' . sLL $1 $> =<< (mkTupleSyntaxTy (glR $1) [] (glR $>)) }
        | '(' ktype ',' comma_types1 ')' {% do { h <- addTrailingCommaA $2 (gl $3)
                                               ; amsA' . sLL $1 $> =<< (mkTupleSyntaxTy (glR $1) (h : $4) (glR $>)) }}
        | '(#' '#)'                   {% do { requireLTPuns PEP_TupleSyntaxType $1 $>
                                            ; amsA' (sLL $1 $> $ HsTupleTy (AnnParen AnnParensHash (glAA $1) (glAA $2)) HsUnboxedTuple []) } }
        | '(#' comma_types1 '#)'      {% do { requireLTPuns PEP_TupleSyntaxType $1 $>
                                            ; amsA' (sLL $1 $> $ HsTupleTy (AnnParen AnnParensHash (glAA $1) (glAA $3)) HsUnboxedTuple $2) } }
        | '(#' bar_types2 '#)'        {% do { requireLTPuns PEP_SumSyntaxType $1 $>
                                      ; amsA' (sLL $1 $> $ HsSumTy (AnnParen AnnParensHash (glAA $1) (glAA $3)) $2) } }
        | '[' ktype ']'               {% amsA' . sLL $1 $> =<< (mkListSyntaxTy1 (glR $1) $2 (glR $3)) }
        | '(' ktype ')'               {% amsA' (sLL $1 $> $ HsParTy  (AnnParen AnnParens       (glAA $1) (glAA $3)) $2) }
                                      -- see Note [Promotion] for the followings
        | SIMPLEQUOTE '(' ')'         {% do { requireLTPuns PEP_QuoteDisambiguation $1 $>
                                            ; amsA' (sLL $1 $> $ HsExplicitTupleTy [mj AnnSimpleQuote $1,mop $2,mcp $3] []) }}
        | SIMPLEQUOTE gen_qcon {% amsA' (sLL $1 $> $ HsTyVar [mj AnnSimpleQuote $1,mjN AnnName $2] IsPromoted $2) }
        | SIMPLEQUOTE sysdcon_nolist {% do { requireLTPuns PEP_QuoteDisambiguation $1 (reLoc $>)
                                           ; amsA' (sLL $1 $> $ HsTyVar [mj AnnSimpleQuote $1,mjN AnnName $2] IsPromoted (L (getLoc $2) $ nameRdrName (dataConName (unLoc $2)))) }}
        | SIMPLEQUOTE  '(' ktype ',' comma_types1 ')'
                             {% do { requireLTPuns PEP_QuoteDisambiguation $1 $>
                                   ; h <- addTrailingCommaA $3 (gl $4)
                                   ; amsA' (sLL $1 $> $ HsExplicitTupleTy [mj AnnSimpleQuote $1,mop $2,mcp $6] (h : $5)) }}
        | '[' ']'               {% withCombinedComments $1 $> (mkListSyntaxTy0 (glR $1) (glR $2)) }
        | SIMPLEQUOTE  '[' comma_types0 ']'     {% do { requireLTPuns PEP_QuoteDisambiguation $1 $>
                                                      ; amsA' (sLL $1 $> $ HsExplicitListTy [mj AnnSimpleQuote $1,mos $2,mcs $4] IsPromoted $3) }}
        | SIMPLEQUOTE var                       {% amsA' (sLL $1 $> $ HsTyVar [mj AnnSimpleQuote $1,mjN AnnName $2] IsPromoted $2) }

        | quasiquote                  { mapLocA (HsSpliceTy noExtField) $1 }
        | splice_untyped              { mapLocA (HsSpliceTy noExtField) $1 }

        -- Two or more [ty, ty, ty] must be a promoted list type, just as
        -- if you had written '[ty, ty, ty]
        -- (One means a list type, zero means the list type constructor,
        -- so you have to quote those.)
        | '[' ktype ',' comma_types1 ']'  {% do { h <- addTrailingCommaA $2 (gl $3)
                                                ; amsA' (sLL $1 $> $ HsExplicitListTy [mos $1,mcs $5] NotPromoted (h:$4)) }}
        | INTEGER              { sLLa $1 $> $ HsTyLit noExtField $ HsNumTy (getINTEGERs $1)
                                                           (il_value (getINTEGER $1)) }
        | CHAR                 { sLLa $1 $> $ HsTyLit noExtField $ HsCharTy (getCHARs $1)
                                                                        (getCHAR $1) }
        | STRING               { sLLa $1 $> $ HsTyLit noExtField $ HsStrTy (getSTRINGs $1)
                                                                     (getSTRING  $1) }
        | '_'                  { sL1a $1 $ mkAnonWildCardTy }
        -- Type variables are never exported, so `M.tyvar` will be rejected by the renamer.
        -- We let it pass the parser because the renamer can generate a better error message.
        | QVARID                      {% let qname = mkQual tvName (getQVARID $1)
                                         in  amsA' (sL1 $1 (HsTyVar [] NotPromoted (sL1n $1 $ qname)))}

-- An inst_type is what occurs in the head of an instance decl
--      e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type for convenience.
inst_type :: { LHsSigType GhcPs }
        : sigtype                       { $1 }

deriv_types :: { [LHsSigType GhcPs] }
        : sigktype                      { [$1] }

        | sigktype ',' deriv_types      {% do { h <- addTrailingCommaA $1 (gl $2)
                                           ; return (h : $3) } }

comma_types0  :: { [LHsType GhcPs] }  -- Zero or more:  ty,ty,ty
        : comma_types1                  { $1 }
        | {- empty -}                   { [] }

comma_types1    :: { [LHsType GhcPs] }  -- One or more:  ty,ty,ty
        : ktype                        { [$1] }
        | ktype  ',' comma_types1      {% do { h <- addTrailingCommaA $1 (gl $2)
                                             ; return (h : $3) }}

bar_types2    :: { [LHsType GhcPs] }  -- Two or more:  ty|ty|ty
        : ktype  '|' ktype             {% do { h <- addTrailingVbarA $1 (gl $2)
                                             ; return [h,$3] }}
        | ktype  '|' bar_types2        {% do { h <- addTrailingVbarA $1 (gl $2)
                                             ; return (h : $3) }}

tv_bndrs :: { [LHsTyVarBndr Specificity GhcPs] }
         : tv_bndr tv_bndrs             { $1 : $2 }
         | {- empty -}                  { [] }

tv_bndr :: { LHsTyVarBndr Specificity GhcPs }
        : tv_bndr_no_braces             { $1 }
        | '{' tyvar '}'                 {% amsA' (sLL $1 $> (UserTyVar   [moc $1, mcc $3] InferredSpec $2)) }
        | '{' tyvar '::' kind '}'       {% amsA' (sLL $1 $> (KindedTyVar [moc $1,mu AnnDcolon $3 ,mcc $5] InferredSpec $2 $4)) }

tv_bndr_no_braces :: { LHsTyVarBndr Specificity GhcPs }
        : tyvar                         {% amsA' (sL1 $1    (UserTyVar   [] SpecifiedSpec $1)) }
        | '(' tyvar '::' kind ')'       {% amsA' (sLL $1 $> (KindedTyVar [mop $1,mu AnnDcolon $3 ,mcp $5] SpecifiedSpec $2 $4)) }

fds :: { Located ([AddEpAnn],[LHsFunDep GhcPs]) }
        : {- empty -}                   { noLoc ([],[]) }
        | '|' fds1                      { (sLL $1 $> ([mj AnnVbar $1]
                                                 ,reverse (unLoc $2))) }

fds1 :: { Located [LHsFunDep GhcPs] }
        : fds1 ',' fd   {%
                           do { let (h:t) = unLoc $1 -- Safe from fds1 rules
                              ; h' <- addTrailingCommaA h (gl $2)
                              ; return (sLL $1 $> ($3 : h' : t)) }}
        | fd            { sL1 $1 [$1] }

fd :: { LHsFunDep GhcPs }
        : varids0 '->' varids0  {% amsA' (L (comb3 $1 $2 $3)
                                       (FunDep [mu AnnRarrow $2]
                                               (reverse (unLoc $1))
                                               (reverse (unLoc $3)))) }

varids0 :: { Located [LocatedN RdrName] }
        : {- empty -}                   { noLoc [] }
        | varids0 tyvar                 { sLL $1 $> ($2 : (unLoc $1)) }

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
change its namespace to DataName, see Note [Demotion] in GHC.Types.Names.OccName).
And both become a HsTyVar ("Zero", DataName) after the renamer.

- ListTuplePuns
When this extension is disabled, ticked constructors for lists and tuples are
not accepted, while the unticked variants are unconditionally parsed as data
constructors.

-}


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located ([AddEpAnn]
                          ,[LConDecl GhcPs]) } -- Returned in order

        : 'where' '{'        gadt_constrs '}'    {% checkEmptyGADTs $
                                                      L (comb2 $1 $4)
                                                        ([mj AnnWhere $1
                                                         ,moc $2
                                                         ,mcc $4]
                                                        , unLoc $3) }
        | 'where' vocurly    gadt_constrs close  {% checkEmptyGADTs $
                                                      L (comb2 $1 $3)
                                                        ([mj AnnWhere $1]
                                                        , unLoc $3) }
        | {- empty -}                            { noLoc ([],[]) }

gadt_constrs :: { Located [LConDecl GhcPs] }
        : gadt_constr ';' gadt_constrs
                  {% do { h <- addTrailingSemiA $1 (gl $2)
                        ; return (L (comb2 $1 $3) (h : unLoc $3)) }}
        | gadt_constr                   { L (glA $1) [$1] }
        | {- empty -}                   { noLoc [] }

-- We allow the following forms:
--      C :: Eq a => a -> T a
--      C :: forall a. Eq a => !a -> T a
--      D { x,y :: a } :: T a
--      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { LConDecl GhcPs }
    -- see Note [Difference in parsing GADT and data constructors]
    -- Returns a list because of:   C,D :: ty
    -- TODO:AZ capture the optSemi. Why leading?
        : optSemi con_list '::' sigtype
                {% mkGadtDecl (comb2 $2 $>) (unLoc $2) (epUniTok $3) $4 }

{- Note [Difference in parsing GADT and data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GADT constructors have simpler syntax than usual data constructors:
in GADTs, types cannot occur to the left of '::', so they cannot be mixed
with constructor names (see Note [Parsing data constructors is hard]).

Due to simplified syntax, GADT constructor names (left-hand side of '::')
use simpler grammar production than usual data constructor names. As a
consequence, GADT constructor names are restricted (names like '(*)' are
allowed in usual data constructors, but not in GADTs).
-}

constrs :: { Located ([AddEpAnn],[LConDecl GhcPs]) }
        : '=' constrs1    { sLL $1 $2 ([mj AnnEqual $1],unLoc $2)}

constrs1 :: { Located [LConDecl GhcPs] }
        : constrs1 '|' constr
            {% do { let (h:t) = unLoc $1
                  ; h' <- addTrailingVbarA h (gl $2)
                  ; return (sLL $1 $> ($3 : h' : t)) }}
        | constr                         { sL1 $1 [$1] }

constr :: { LConDecl GhcPs }
        : forall context '=>' constr_stuff
                {% amsA' (let (con,details) = unLoc $4 in
                  (L (comb4 $1 $2 $3 $4) (mkConDeclH98
                                                       (mu AnnDarrow $3:(fst $ unLoc $1))
                                                       con
                                                       (snd $ unLoc $1)
                                                       (Just $2)
                                                       details))) }
        | forall constr_stuff
                {% amsA' (let (con,details) = unLoc $2 in
                  (L (comb2 $1 $2) (mkConDeclH98 (fst $ unLoc $1)
                                                      con
                                                      (snd $ unLoc $1)
                                                      Nothing   -- No context
                                                      details))) }

forall :: { Located ([AddEpAnn], Maybe [LHsTyVarBndr Specificity GhcPs]) }
        : 'forall' tv_bndrs '.'       { sLL $1 $> ([mu AnnForall $1,mj AnnDot $3], Just $2) }
        | {- empty -}                 { noLoc ([], Nothing) }

constr_stuff :: { Located (LocatedN RdrName, HsConDeclH98Details GhcPs) }
        : infixtype       {% fmap (reLoc. (fmap (\b -> (dataConBuilderCon b,
                                                        dataConBuilderDetails b))))
                                  (runPV $1) }
        | '(#' usum_constr '#)' {% let (t, tag, arity) = $2 in pure (sLL $1 $3 $ mkUnboxedSumCon t tag arity)}

usum_constr :: { (LHsType GhcPs, Int, Int) } -- constructor for the data decls SumN#
         : ktype bars { ($1, 1, (snd $2 + 1)) }
         | bars ktype bars0 { ($2, snd $1 + 1, snd $1 + snd $3 + 1) }

fielddecls :: { [LConDeclField GhcPs] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [LConDeclField GhcPs] }
        : fielddecl ',' fielddecls1
            {% do { h <- addTrailingCommaA $1 (gl $2)
                  ; return (h : $3) }}
        | fielddecl   { [$1] }

fielddecl :: { LConDeclField GhcPs }
                                              -- A list because of   f,g :: Int
        : sig_vars '::' ctype
            {% amsA' (L (comb2 $1 $3)
                      (ConDeclField [mu AnnDcolon $2]
                                    (reverse (map (\ln@(L l n)
                                               -> L (fromTrailingN l) $ FieldOcc noExtField (L (noTrailingN l) n)) (unLoc $1))) $3 Nothing))}

-- Reversed!
maybe_derivings :: { Located (HsDeriving GhcPs) }
        : {- empty -}             { noLoc [] }
        | derivings               { $1 }

-- A list of one or more deriving clauses at the end of a datatype
derivings :: { Located (HsDeriving GhcPs) }
        : derivings deriving      { sLL $1 $> ($2 : unLoc $1) } -- AZ: order?
        | deriving                { sL1 $> [$1] }

-- The outer Located is just to allow the caller to
-- know the rightmost extremity of the 'deriving' clause
deriving :: { LHsDerivingClause GhcPs }
        : 'deriving' deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in amsA' (L full_loc $ HsDerivingClause [mj AnnDeriving $1] Nothing $2) }

        | 'deriving' deriv_strategy_no_via deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in amsA' (L full_loc $ HsDerivingClause [mj AnnDeriving $1] (Just $2) $3) }

        | 'deriving' deriv_clause_types deriv_strategy_via
              {% let { full_loc = comb2 $1 $> }
                 in amsA' (L full_loc $ HsDerivingClause [mj AnnDeriving $1] (Just $3) $2) }

deriv_clause_types :: { LDerivClauseTys GhcPs }
        : qtycon              { let { tc = sL1a $1 $ mkHsImplicitSigType $
                                           sL1a $1 $ HsTyVar noAnn NotPromoted $1 } in
                                sL1a $1 (DctSingle noExtField tc) }
        | '(' ')'             {% amsr (sLL $1 $> (DctMulti noExtField []))
                                      (AnnContext Nothing [glAA $1] [glAA $2]) }
        | '(' deriv_types ')' {% amsr (sLL $1 $> (DctMulti noExtField $2))
                                      (AnnContext Nothing [glAA $1] [glAA $3])}

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

decl_no_th :: { LHsDecl GhcPs }
        : sigdecl               { $1 }

        | infixexp     opt_sig rhs  {% runPV (unECP $1) >>= \ $1 ->
                                       do { let { l = comb2 $1 $> }
                                          ; r <- checkValDef l $1 (HsNoMultAnn noExtField, $2) $3;
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                          ; !cs <- getCommentsFor l
                                          ; return $! (sL (commentsA l cs) $ ValD noExtField r) } }
        | PREFIX_PERCENT atype infixexp     opt_sig rhs  {% runPV (unECP $3) >>= \ $3 ->
                                       do { let { l = comb2 $1 $> }
                                          ; r <- checkValDef l $3 (mkMultAnn (epTok $1) $2, $4) $5;
                                        -- parses bindings of the form %p x or
                                        -- %p x :: sig
                                        --
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                          ; !cs <- getCommentsFor l
                                          ; return $! (sL (commentsA l cs) $ ValD noExtField r) } }
        | pattern_synonym_decl  { $1 }

decl    :: { LHsDecl GhcPs }
        : decl_no_th            { $1 }

        -- Why do we only allow naked declaration splices in top-level
        -- declarations and not here? Short answer: because readFail009
        -- fails terribly with a panic in cvBindsAndSigs otherwise.
        | splice_exp            { mkSpliceDecl $1 }

rhs     :: { Located (GRHSs GhcPs (LHsExpr GhcPs)) }
        : '=' exp wherebinds    {% runPV (unECP $2) >>= \ $2 ->
                                  do { let L l (bs, csw) = adaptWhereBinds $3
                                     ; let loc = (comb3 $1 $2 (L l bs))
                                     ; let locg = (comb2 $1 $2)
                                     ; acs loc (\loc cs ->
                                       sL loc (GRHSs csw (unguardedRHS (EpAnn (spanAsAnchor locg) (GrhsAnn Nothing (mj AnnEqual $1)) cs) locg $2)
                                                      bs)) } }
        | gdrhs wherebinds      {% do { let {L l (bs, csw) = adaptWhereBinds $2}
                                      ; acs (comb2 $1 (L l bs)) (\loc cs -> L loc
                                                (GRHSs (cs Semi.<> csw) (reverse (unLoc $1)) bs)) }}

gdrhs :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : gdrhs gdrh            { sLL $1 $> ($2 : unLoc $1) }
        | gdrh                  { sL1 $1 [$1] }

gdrh :: { LGRHS GhcPs (LHsExpr GhcPs) }
        : '|' guardquals '=' exp  {% runPV (unECP $4) >>= \ $4 ->
                                     acsA (comb2 $1 $>) (\loc cs -> L loc $ GRHS (EpAnn (glEE $1 $>) (GrhsAnn (Just $ glAA $1) (mj AnnEqual $3)) cs) (unLoc $2) $4) }

sigdecl :: { LHsDecl GhcPs }
        :
        -- See Note [Declaration/signature overlap] for why we need infixexp here
          infixexp     '::' sigtype
                        {% do { $1 <- runPV (unECP $1)
                              ; v <- checkValSigLhs $1
                              ; amsA' (sLL $1 $> $ SigD noExtField $
                                  TypeSig (AnnSig (mu AnnDcolon $2) []) [v] (mkHsWildCardBndrs $3))} }

        | var ',' sig_vars '::' sigtype
           {% do { v <- addTrailingCommaN $1 (gl $2)
                 ; let sig = TypeSig (AnnSig (mu AnnDcolon $4) []) (v : reverse (unLoc $3))
                                      (mkHsWildCardBndrs $5)
                 ; amsA' (sLL $1 $> $ SigD noExtField sig ) }}

        | infix prec namespace_spec ops
             {% do { mbPrecAnn <- traverse (\l2 -> do { checkPrecP l2 $4
                                                      ; pure (mj AnnVal l2) })
                                       $2
                   ; let (fixText, fixPrec) = case $2 of
                                                -- If an explicit precedence isn't supplied,
                                                -- it defaults to maxPrecedence
                                                Nothing -> (NoSourceText, maxPrecedence)
                                                Just l2 -> (fst $ unLoc l2, snd $ unLoc l2)
                   ; amsA' (sLL $1 $> $ SigD noExtField
                            (FixSig (mj AnnInfix $1 : maybeToList mbPrecAnn) (FixitySig (unLoc $3) (fromOL $ unLoc $4)
                                    (Fixity fixText fixPrec (unLoc $1)))))
                   }}

        | pattern_synonym_sig   { sL1a $1 . SigD noExtField . unLoc $ $1 }

        | '{-# COMPLETE' qcon_list opt_tyconsig  '#-}'
                {% let (dcolon, tc) = $3
                   in amsA' (sLL $1 $>
                         (SigD noExtField (CompleteMatchSig ([ mo $1 ] ++ dcolon ++ [mc $4], (getCOMPLETE_PRAGs $1)) $2 tc))) }

        -- This rule is for both INLINE and INLINABLE pragmas
        | '{-# INLINE' activation qvarcon '#-}'
                {% amsA' (sLL $1 $> $ SigD noExtField (InlineSig ((mo $1:fst $2) ++ [mc $4]) $3
                            (mkInlinePragma (getINLINE_PRAGs $1) (getINLINE $1)
                                            (snd $2)))) }
        | '{-# OPAQUE' qvar '#-}'
                {% amsA' (sLL $1 $> $ SigD noExtField (InlineSig [mo $1, mc $3] $2
                            (mkOpaquePragma (getOPAQUE_PRAGs $1)))) }
        | '{-# SCC' qvar '#-}'
          {% amsA' (sLL $1 $> (SigD noExtField (SCCFunSig ([mo $1, mc $3], (getSCC_PRAGs $1)) $2 Nothing))) }

        | '{-# SCC' qvar STRING '#-}'
          {% do { scc <- getSCC $3
                ; let str_lit = StringLiteral (getSTRINGs $3) scc Nothing
                ; amsA' (sLL $1 $> (SigD noExtField (SCCFunSig ([mo $1, mc $4], (getSCC_PRAGs $1)) $2 (Just ( sL1a $3 str_lit))))) }}

        | '{-# SPECIALISE' activation qvar '::' sigtypes1 '#-}'
             {% amsA' (
                 let inl_prag = mkInlinePragma (getSPEC_PRAGs $1)
                                             (NoUserInlinePrag, FunLike) (snd $2)
                  in sLL $1 $> $ SigD noExtField (SpecSig (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) $3 (fromOL $5) inl_prag)) }

        | '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
             {% amsA' (sLL $1 $> $ SigD noExtField (SpecSig (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) $3 (fromOL $5)
                               (mkInlinePragma (getSPEC_INLINE_PRAGs $1)
                                               (getSPEC_INLINE $1) (snd $2)))) }

        | '{-# SPECIALISE' 'instance' inst_type '#-}'
                {% amsA' (sLL $1 $> $ SigD noExtField (SpecInstSig ([mo $1,mj AnnInstance $2,mc $4], (getSPEC_PRAGs $1)) $3)) }

        -- A minimal complete definition
        | '{-# MINIMAL' name_boolformula_opt '#-}'
            {% amsA' (sLL $1 $> $ SigD noExtField (MinimalSig ([mo $1,mc $3], (getMINIMAL_PRAGs $1)) $2)) }

activation :: { ([AddEpAnn],Maybe Activation) }
        -- See Note [%shift: activation -> {- empty -}]
        : {- empty -} %shift                    { ([],Nothing) }
        | explicit_activation                   { (fst $1,Just (snd $1)) }

explicit_activation :: { ([AddEpAnn],Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mj AnnOpenS $1,mj AnnVal $2,mj AnnCloseS $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' rule_activation_marker INTEGER ']'
                                { ($2++[mj AnnOpenS $1,mj AnnVal $3,mj AnnCloseS $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsUntypedSplice GhcPs) }
        : TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in sL1 $1 (HsQuasiQuote noExtField quoterId (L (noAnnSrcSpan (mkSrcSpanPs quoteSpan)) quote)) }
        | TH_QQUASIQUOTE  { let { loc = getLoc $1
                                ; ITqQuasiQuote (qual, quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkQual varName (qual, quoter) }
                            in sL1 $1 (HsQuasiQuote noExtField quoterId (L (noAnnSrcSpan (mkSrcSpanPs quoteSpan)) quote)) }

exp   :: { ECP }
        : infixexp '::' ctype
                                { ECP $
                                   unECP $1 >>= \ $1 ->
                                   rejectPragmaPV $1 >>
                                   mkHsTySigPV (noAnnSrcSpan $ comb2 $1 $>) $1 $3
                                          [(mu AnnDcolon $2)] }
        | infixexp '-<' exp     {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   amsA' (sLL $1 $> $ HsCmdArrApp (mu Annlarrowtail $2) $1 $3
                                                        HsFirstOrderApp True) }
        | infixexp '>-' exp     {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   amsA' (sLL $1 $> $ HsCmdArrApp (mu Annrarrowtail $2) $3 $1
                                                      HsFirstOrderApp False) }
        | infixexp '-<<' exp    {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   amsA' (sLL $1 $> $ HsCmdArrApp (mu AnnLarrowtail $2) $1 $3
                                                      HsHigherOrderApp True) }
        | infixexp '>>-' exp    {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   amsA' (sLL $1 $> $ HsCmdArrApp (mu AnnRarrowtail $2) $3 $1
                                                      HsHigherOrderApp False) }
        -- See Note [%shift: exp -> infixexp]
        | infixexp %shift       { $1 }
        | exp_prag(exp)         { $1 } -- See Note [Pragmas and operator fixity]

        -- Embed types into expressions and patterns for required type arguments
        | 'type' atype
                {% do { requireExplicitNamespaces (getLoc $1)
                      ; return $ ECP $ mkHsEmbTyPV (comb2 $1 $>) (epTok $1) $2 } }

infixexp :: { ECP }
        : exp10 { $1 }
        | infixexp qop exp10p    -- See Note [Pragmas and operator fixity]
                               { ECP $
                                 superInfixOp $
                                 $2 >>= \ $2 ->
                                 unECP $1 >>= \ $1 ->
                                 unECP $3 >>= \ $3 ->
                                 rejectPragmaPV $1 >>
                                 (mkHsOpAppPV (comb2 $1 $3) $1 $2 $3) }
                 -- AnnVal annotation for NPlusKPat, which discards the operator

exp10p :: { ECP }
  : exp10            { $1 }
  | exp_prag(exp10p) { $1 } -- See Note [Pragmas and operator fixity]

exp_prag(e) :: { ECP }
  : prag_e e  -- See Note [Pragmas and operator fixity]
      {% runPV (unECP $2) >>= \ $2 ->
         fmap ecpFromExp $
         amsA' $ (sLL $1 $> $ HsPragE noExtField (unLoc $1) $2) }

exp10 :: { ECP }
        -- See Note [%shift: exp10 -> '-' fexp]
        : '-' fexp %shift               { ECP $
                                           unECP $2 >>= \ $2 ->
                                           mkHsNegAppPV (comb2 $1 $>) $2
                                                 [mj AnnMinus $1] }
        -- See Note [%shift: exp10 -> fexp]
        | fexp %shift                  { $1 }

optSemi :: { (Maybe EpaLocation,Bool) }
        : ';'         { (msemim $1,True) }
        | {- empty -} { (Nothing,False) }

{- Note [Pragmas and operator fixity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'prag_e' is an expression pragma, such as {-# SCC ... #-}.

It must be used with care, or else #15730 happens. Consider this infix
expression:

         1 / 2 / 2

There are two ways to parse it:

    1.   (1 / 2) / 2   =  0.25
    2.   1 / (2 / 2)   =  1.0

Due to the fixity of the (/) operator (assuming it comes from Prelude),
option 1 is the correct parse. However, in the past GHC's parser used to get
confused by the SCC annotation when it occurred in the middle of an infix
expression:

         1 / {-# SCC ann #-} 2 / 2    -- used to get parsed as option 2

There are several ways to address this issue, see GHC Proposal #176 for a
detailed exposition:

  https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0176-scc-parsing.rst

The accepted fix is to disallow pragmas that occur within infix expressions.
Infix expressions are assembled out of 'exp10', so 'exp10' must not accept
pragmas. Instead, we accept them in exactly two places:

* at the start of an expression or a parenthesized subexpression:

    f = {-# SCC ann #-} 1 / 2 / 2          -- at the start of the expression
    g = 5 + ({-# SCC ann #-} 1 / 2 / 2)    -- at the start of a parenthesized subexpression

* immediately after the last operator:

    f = 1 / 2 / {-# SCC ann #-} 2

In both cases, the parse does not depend on operator fixity. The second case
may sound unnecessary, but it's actually needed to support a common idiom:

    f $ {-# SCC ann $-} ...

-}
prag_e :: { Located (HsPragE GhcPs) }
      : '{-# SCC' STRING '#-}'      {% do { scc <- getSCC $2
                                          ; return (sLL $1 $>
                                             (HsPragSCC
                                                (AnnPragma (mo $1) (mc $3) [mj AnnValStr $2],
                                                (getSCC_PRAGs $1))
                                                (StringLiteral (getSTRINGs $2) scc Nothing)))} }
      | '{-# SCC' VARID  '#-}'      { sLL $1 $>
                                             (HsPragSCC
                                               (AnnPragma (mo $1) (mc $3) [mj AnnVal $2],
                                               (getSCC_PRAGs $1))
                                               (StringLiteral NoSourceText (getVARID $2) Nothing)) }

fexp    :: { ECP }
        : fexp aexp                  { ECP $
                                          superFunArg $
                                          unECP $1 >>= \ $1 ->
                                          unECP $2 >>= \ $2 ->
                                          spanWithComments (comb2 $1 $>) >>= \l ->
                                          mkHsAppPV l $1 $2 }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | fexp PREFIX_AT atype       { ECP $
                                        unECP $1 >>= \ $1 ->
                                        mkHsAppTypePV (noAnnSrcSpan $ comb2 $1 $>) $1 (epTok $2) $3 }

        | 'static' aexp              {% runPV (unECP $2) >>= \ $2 ->
                                        fmap ecpFromExp $
                                        amsA' (sLL $1 $> $ HsStatic [mj AnnStatic $1] $2) }

        | aexp                       { $1 }

aexp    :: { ECP }
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        : qvar TIGHT_INFIX_AT aexp
                                { ECP $
                                   unECP $3 >>= \ $3 ->
                                     mkHsAsPatPV (comb2 $1 $>) $1 (epTok $2) $3 }


        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | PREFIX_TILDE aexp     { ECP $
                                   unECP $2 >>= \ $2 ->
                                   mkHsLazyPatPV (comb2 $1 $>) $2 [mj AnnTilde $1] }
        | PREFIX_BANG aexp      { ECP $
                                   unECP $2 >>= \ $2 ->
                                   mkHsBangPatPV (comb2 $1 $>) $2 [mj AnnBang $1] }
        | PREFIX_MINUS aexp     { ECP $
                                   unECP $2 >>= \ $2 ->
                                   mkHsNegAppPV (comb2 $1 $>) $2 [mj AnnMinus $1] }
        | 'let' binds 'in' exp          {  ECP $
                                           unECP $4 >>= \ $4 ->
                                           mkHsLetPV (comb2 $1 $>) (epTok $1) (unLoc $2) (epTok $3) $4 }
        | '\\' argpats '->' exp { ECP $
                      unECP $4 >>= \ $4 ->
                      mkHsLamPV (comb2 $1 $>) LamSingle
                            (sLLl $1 $>
                            [sLLa $1 $>
                                         $ Match { m_ext = []
                                                 , m_ctxt = LamAlt LamSingle
                                                 , m_pats = $2
                                                 , m_grhss = unguardedGRHSs (comb2 $3 $4) $4 (EpAnn (glR $3) (GrhsAnn Nothing (mu AnnRarrow $3)) emptyComments) }])
                            [mj AnnLam $1] }
        | '\\' 'lcase' altslist(pats1)
            {  ECP $ $3 >>= \ $3 ->
                 mkHsLamPV (comb2 $1 $>) LamCase $3 [mj AnnLam $1,mj AnnCase $2] }
        | '\\' 'lcases' altslist(argpats)
            {  ECP $ $3 >>= \ $3 ->
                 mkHsLamPV (comb2 $1 $>) LamCases $3 [mj AnnLam $1,mj AnnCases $2] }
        | 'if' exp optSemi 'then' exp optSemi 'else' exp
                         {% runPV (unECP $2) >>= \ ($2 :: LHsExpr GhcPs) ->
                            return $ ECP $
                              unECP $5 >>= \ $5 ->
                              unECP $8 >>= \ $8 ->
                              mkHsIfPV (comb2 $1 $>) $2 (snd $3) $5 (snd $6) $8
                                    (AnnsIf
                                      { aiIf = glAA $1
                                      , aiThen = glAA $4
                                      , aiElse = glAA $7
                                      , aiThenSemi = fst $3
                                      , aiElseSemi = fst $6})}

        | 'if' ifgdpats                 {% hintMultiWayIf (getLoc $1) >>= \_ ->
                                           fmap ecpFromExp $
                                           amsA' (sLL $1 $> $ HsMultiIf (mj AnnIf $1:(fst $ unLoc $2))
                                                     (reverse $ snd $ unLoc $2)) }
        | 'case' exp 'of' altslist(pats1) {% runPV (unECP $2) >>= \ ($2 :: LHsExpr GhcPs) ->
                                             return $ ECP $
                                               $4 >>= \ $4 ->
                                               mkHsCasePV (comb3 $1 $3 $4) $2 $4
                                                    (EpAnnHsCase (glAA $1) (glAA $3) []) }
        -- QualifiedDo.
        | DO  stmtlist               {% do
                                      hintQualifiedDo $1
                                      return $ ECP $
                                        $2 >>= \ $2 ->
                                        mkHsDoPV (comb2 $1 $2)
                                                 (fmap mkModuleNameFS (getDO $1))
                                                 $2
                                                 (AnnList (Just $ glR $2) Nothing Nothing [mj AnnDo $1] []) }
        | MDO stmtlist             {% hintQualifiedDo $1 >> runPV $2 >>= \ $2 ->
                                       fmap ecpFromExp $
                                       amsA' (L (comb2 $1 $2)
                                              (mkHsDoAnns (MDoExpr $
                                                          fmap mkModuleNameFS (getMDO $1))
                                                          $2
                                              (AnnList (Just $ glR $2) Nothing Nothing [mj AnnMdo $1] []) )) }
        | 'proc' aexp '->' exp
                       {% (checkPattern <=< runPV) (unECP $2) >>= \ p ->
                           runPV (unECP $4) >>= \ $4@cmd ->
                           fmap ecpFromExp $
                           amsA' (sLL $1 $> $ HsProc [mj AnnProc $1,mu AnnRarrow $3] p (sLLa $1 $> $ HsCmdTop noExtField cmd)) }

        | aexp1                 { $1 }

aexp1   :: { ECP }
        : aexp1 '{' fbinds '}' { ECP $
                                   getBit OverloadedRecordUpdateBit >>= \ overloaded ->
                                   unECP $1 >>= \ $1 ->
                                   $3 >>= \ $3 ->
                                   mkHsRecordPV overloaded (comb2 $1 $>) (comb2 $2 $4) $1 $3
                                        [moc $2,mcc $4]
                               }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | aexp1 TIGHT_INFIX_PROJ field
            {% runPV (unECP $1) >>= \ $1 ->
               fmap ecpFromExp $ amsA' (
                 let fl = sLLa $2 $> (DotFieldOcc (AnnFieldLabel (Just $ glAA $2)) $3) in
               sLL $1 $> $ mkRdrGetField $1 fl)  }



        | aexp2                { $1 }

aexp2   :: { ECP }
        : qvar                          { ECP $ mkHsVarPV $! $1 }
        | qcon                          { ECP $ mkHsVarPV $! $1 }
        -- See Note [%shift: aexp2 -> ipvar]
        | ipvar %shift                  {% fmap ecpFromExp
                                           (ams1 $1 (HsIPVar NoExtField $! unLoc $1)) }
        | overloaded_label              {% fmap ecpFromExp
                                           (ams1 $1 (HsOverLabel NoExtField (fst $! unLoc $1) (snd $! unLoc $1))) }
        | literal                       { ECP $ mkHsLitPV $! $1 }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -XOverloadedStrings is on.
--      | STRING    { sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRINGs $1)
--                                       (getSTRING $1) noExtField) }
        | INTEGER   { ECP $ mkHsOverLitPV (sL1a $1 $ mkHsIntegral   (getINTEGER  $1)) }
        | RATIONAL  { ECP $ mkHsOverLitPV (sL1a $1 $ mkHsFractional (getRATIONAL $1)) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
        | '(' texp ')'                  { ECP $
                                           unECP $2 >>= \ $2 ->
                                           mkHsParPV (comb2 $1 $>) (epTok $1) $2 (epTok $3) }
        | '(' tup_exprs ')'             { ECP $
                                           $2 >>= \ $2 ->
                                           mkSumOrTuplePV (noAnnSrcSpan $ comb2 $1 $>) Boxed $2
                                                [mop $1,mcp $3]}

        -- This case is only possible when 'OverloadedRecordDotBit' is enabled.
        | '(' projection ')'            { ECP $
                                            amsA' (sLL $1 $> $ mkRdrProjection (NE.reverse (unLoc $2)) (AnnProjection (glAA $1) (glAA $3)) )
                                            >>= ecpFromExp'
                                        }

        | '(#' texp '#)'                { ECP $
                                           unECP $2 >>= \ $2 ->
                                           mkSumOrTuplePV (noAnnSrcSpan $ comb2 $1 $>) Unboxed (Tuple [Right $2])
                                                 [moh $1,mch $3] }
        | '(#' tup_exprs '#)'           { ECP $
                                           $2 >>= \ $2 ->
                                           mkSumOrTuplePV (noAnnSrcSpan $ comb2 $1 $>) Unboxed $2
                                                [moh $1,mch $3] }

        | '[' list ']'      { ECP $ $2 (comb2 $1 $>) (mos $1,mcs $3) }
        | '_'               { ECP $ mkHsWildCardPV (getLoc $1) }

        -- Template Haskell Extension
        | splice_untyped { ECP $ mkHsSplicePV $1 }
        | splice_typed   { ecpFromExp $ fmap (uncurry HsTypedSplice) (reLoc $1) }

        | SIMPLEQUOTE  qvar     {% fmap ecpFromExp $ amsA' (sLL $1 $> $ HsUntypedBracket [mj AnnSimpleQuote $1] (VarBr noExtField True  $2)) }
        | SIMPLEQUOTE  qcon     {% fmap ecpFromExp $ amsA' (sLL $1 $> $ HsUntypedBracket [mj AnnSimpleQuote $1] (VarBr noExtField True  $2)) }
        | TH_TY_QUOTE tyvar     {% fmap ecpFromExp $ amsA' (sLL $1 $> $ HsUntypedBracket [mj AnnThTyQuote $1  ] (VarBr noExtField False $2)) }
        | TH_TY_QUOTE gtycon    {% fmap ecpFromExp $ amsA' (sLL $1 $> $ HsUntypedBracket [mj AnnThTyQuote $1  ] (VarBr noExtField False $2)) }
        -- See Note [%shift: aexp2 -> TH_TY_QUOTE]
        | TH_TY_QUOTE %shift    {% reportEmptyDoubleQuotes (getLoc $1) }
        | '[|' exp '|]'       {% runPV (unECP $2) >>= \ $2 ->
                                 fmap ecpFromExp $
                                 amsA' (sLL $1 $> $ HsUntypedBracket (if (hasE $1) then [mj AnnOpenE $1, mu AnnCloseQ $3]
                                                                                         else [mu AnnOpenEQ $1,mu AnnCloseQ $3]) (ExpBr noExtField $2)) }
        | '[||' exp '||]'     {% runPV (unECP $2) >>= \ $2 ->
                                 fmap ecpFromExp $
                                 amsA' (sLL $1 $> $ HsTypedBracket (if (hasE $1) then [mj AnnOpenE $1,mc $3] else [mo $1,mc $3]) $2) }
        | '[t|' ktype '|]'    {% fmap ecpFromExp $
                                 amsA' (sLL $1 $> $ HsUntypedBracket [mo $1,mu AnnCloseQ $3] (TypBr noExtField $2)) }
        | '[p|' infixexp '|]' {% (checkPattern <=< runPV) (unECP $2) >>= \p ->
                                      fmap ecpFromExp $
                                      amsA' (sLL $1 $> $ HsUntypedBracket [mo $1,mu AnnCloseQ $3] (PatBr noExtField p)) }
        | '[d|' cvtopbody '|]' {% fmap ecpFromExp $
                                  amsA' (sLL $1 $> $ HsUntypedBracket (mo $1:mu AnnCloseQ $3:fst $2) (DecBrL noExtField (snd $2))) }
        | quasiquote          { ECP $ mkHsSplicePV $1 }

        -- arrow notation extension
        | '(|' aexp cmdargs '|)'  {% runPV (unECP $2) >>= \ $2 ->
                                      fmap ecpFromCmd $
                                      amsA' (sLL $1 $> $ HsCmdArrForm (AnnList (glRM $1) (Just $ mu AnnOpenB $1) (Just $ mu AnnCloseB $4) [] []) $2 Prefix
                                                           Nothing (reverse $3)) }

projection :: { Located (NonEmpty (LocatedAn NoEpAnns (DotFieldOcc GhcPs))) }
projection
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parsing.Lexer
        : projection TIGHT_INFIX_PROJ field
                             { sLL $1 $> ((sLLa $2 $> $ DotFieldOcc (AnnFieldLabel (Just $ glAA $2)) $3) `NE.cons` unLoc $1) }
        | PREFIX_PROJ field  { sLL $1 $> ((sLLa $1 $> $ DotFieldOcc (AnnFieldLabel (Just $ glAA $1)) $2) :| [])}

splice_exp :: { LHsExpr GhcPs }
        : splice_untyped { fmap (HsUntypedSplice noExtField) (reLoc $1) }
        | splice_typed   { fmap (uncurry HsTypedSplice) (reLoc $1) }

splice_untyped :: { Located (HsUntypedSplice GhcPs) }
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        : PREFIX_DOLLAR aexp2   {% runPV (unECP $2) >>= \ $2 ->
                                   return (sLL $1 $> $ HsUntypedSpliceExpr [mj AnnDollar $1] $2) }

splice_typed :: { Located ([AddEpAnn], LHsExpr GhcPs) }
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        : PREFIX_DOLLAR_DOLLAR aexp2
                                {% runPV (unECP $2) >>= \ $2 ->
                                   return (sLL $1 $> $ ([mj AnnDollarDollar $1], $2)) }

cmdargs :: { [LHsCmdTop GhcPs] }
        : cmdargs acmd                  { $2 : $1 }
        | {- empty -}                   { [] }

acmd    :: { LHsCmdTop GhcPs }
        : aexp                  {% runPV (unECP $1) >>= \ (cmd :: LHsCmd GhcPs) ->
                                   runPV (checkCmdBlockArguments cmd) >>= \ _ ->
                                   return (sL1a cmd $ HsCmdTop noExtField cmd) }

cvtopbody :: { ([AddEpAnn],[LHsDecl GhcPs]) }
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
-- inside parens or delimited by commas
texp :: { ECP }
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
        | infixexp qop
                             {% runPV (unECP $1) >>= \ $1 ->
                                runPV (rejectPragmaPV $1) >>
                                runPV $2 >>= \ $2 ->
                                return $ ecpFromExp $
                                sLLa $1 $> $ SectionL noExtField $1 (n2l $2) }
        | qopm infixexp      { ECP $
                                superInfixOp $
                                unECP $2 >>= \ $2 ->
                                $1 >>= \ $1 ->
                                mkHsSectionR_PV (comb2 $1 $>) (n2l $1) $2 }

       -- View patterns get parenthesized above
        | exp '->' texp   { ECP $
                             unECP $1 >>= \ $1 ->
                             unECP $3 >>= \ $3 ->
                             mkHsViewPatPV (comb2 $1 $>) $1 $3 [mu AnnRarrow $2] }

-- Always at least one comma or bar.
-- Though this can parse just commas (without any expressions), it won't
-- in practice, because (,,,) is parsed as a name. See Note [ExplicitTuple]
-- in GHC.Hs.Expr.
tup_exprs :: { forall b. DisambECP b => PV (SumOrTuple b) }
           : texp commas_tup_tail
                           { unECP $1 >>= \ $1 ->
                             $2 >>= \ $2 ->
                             do { t <- amsA $1 [AddCommaAnn (srcSpan2e $ fst $2)]
                                ; return (Tuple (Right t : snd $2)) } }
           | commas tup_tail
                 { $2 >>= \ $2 ->
                   do { let {cos = map (\ll -> (Left (EpAnn (spanAsAnchor ll) True emptyComments))) (fst $1) }
                      ; return (Tuple (cos ++ $2)) } }

           | texp bars   { unECP $1 >>= \ $1 -> return $
                            (Sum 1  (snd $2 + 1) $1 [] (map srcSpan2e $ fst $2)) }

           | bars texp bars0
                { unECP $2 >>= \ $2 -> return $
                  (Sum (snd $1 + 1) (snd $1 + snd $3 + 1) $2
                    (map srcSpan2e $ fst $1)
                    (map srcSpan2e $ fst $3)) }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { forall b. DisambECP b => PV (SrcSpan,[Either (EpAnn Bool) (LocatedA b)]) }
commas_tup_tail : commas tup_tail
        { $2 >>= \ $2 ->
          do { let {cos = map (\l -> (Left (EpAnn (spanAsAnchor l) True emptyComments))) (tail $ fst $1) }
             ; return ((head $ fst $1, cos ++ $2)) } }

-- Always follows a comma
tup_tail :: { forall b. DisambECP b => PV [Either (EpAnn Bool) (LocatedA b)] }
          : texp commas_tup_tail { unECP $1 >>= \ $1 ->
                                   $2 >>= \ $2 ->
                                   do { t <- amsA $1 [AddCommaAnn (srcSpan2e $ fst $2)]
                                      ; return (Right t : snd $2) } }
          | texp                 { unECP $1 >>= \ $1 ->
                                   return [Right $1] }
          -- See Note [%shift: tup_tail -> {- empty -}]
          | {- empty -} %shift   { return [Left noAnn] }

-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.
-- Never empty.
list :: { forall b. DisambECP b => SrcSpan -> (AddEpAnn, AddEpAnn) -> PV (LocatedA b) }
        : texp    { \loc (ao,ac) -> unECP $1 >>= \ $1 ->
                            mkHsExplicitListPV loc [$1] (AnnList Nothing (Just ao) (Just ac) [] []) }
        | lexps   { \loc (ao,ac) -> $1 >>= \ $1 ->
                            mkHsExplicitListPV loc (reverse $1) (AnnList Nothing (Just ao) (Just ac) [] []) }
        | texp '..'  { \loc (ao,ac) -> unECP $1 >>= \ $1 ->
                                  amsA' (L loc $ ArithSeq [ao,mj AnnDotdot $2,ac] Nothing (From $1))
                                      >>= ecpFromExp' }
        | texp ',' exp '..' { \loc (ao,ac) ->
                                   unECP $1 >>= \ $1 ->
                                   unECP $3 >>= \ $3 ->
                                   amsA' (L loc $ ArithSeq [ao,mj AnnComma $2,mj AnnDotdot $4,ac] Nothing (FromThen $1 $3))
                                       >>= ecpFromExp' }
        | texp '..' exp  { \loc (ao,ac) ->
                                   unECP $1 >>= \ $1 ->
                                   unECP $3 >>= \ $3 ->
                                   amsA' (L loc $ ArithSeq [ao,mj AnnDotdot $2,ac] Nothing (FromTo $1 $3))
                                       >>= ecpFromExp' }
        | texp ',' exp '..' exp { \loc (ao,ac) ->
                                   unECP $1 >>= \ $1 ->
                                   unECP $3 >>= \ $3 ->
                                   unECP $5 >>= \ $5 ->
                                   amsA' (L loc $ ArithSeq [ao,mj AnnComma $2,mj AnnDotdot $4,ac] Nothing (FromThenTo $1 $3 $5))
                                       >>= ecpFromExp' }
        | texp '|' flattenedpquals
             { \loc (ao,ac) ->
                checkMonadComp >>= \ ctxt ->
                unECP $1 >>= \ $1 -> do { t <- addTrailingVbarA $1 (gl $2)
                ; amsA' (L loc $ mkHsCompAnns ctxt (unLoc $3) t (AnnList Nothing (Just ao) (Just ac) [] []))
                    >>= ecpFromExp' } }

lexps :: { forall b. DisambECP b => PV [LocatedA b] }
        : lexps ',' texp           { $1 >>= \ $1 ->
                                     unECP $3 >>= \ $3 ->
                                     case $1 of
                                       (h:t) -> do
                                         h' <- addTrailingCommaA h (gl $2)
                                         return (((:) $! $3) $! (h':t)) }
        | texp ',' texp             { unECP $1 >>= \ $1 ->
                                      unECP $3 >>= \ $3 ->
                                      do { h <- addTrailingCommaA $1 (gl $2)
                                         ; return [$3,h] }}

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : pquals   { case (unLoc $1) of
                    [qs] -> sL1 $1 qs
                    -- We just had one thing in our "parallel" list so
                    -- we simply return that thing directly

                    qss -> sL1 $1 [sL1a $1 $ ParStmt noExtField [ParStmtBlock noExtField qs [] noSyntaxExpr |
                                            qs <- qss]
                                            noExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt GhcPs (LHsExpr GhcPs)]] }
    : squals '|' pquals
                     {% case unLoc $1 of
                          (h:t) -> do
                            h' <- addTrailingVbarA h (gl $2)
                            return (sLL $1 $> (reverse (h':t) : unLoc $3)) }
    | squals         { L (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }   -- In reverse order, because the last
                                        -- one can "grab" the earlier ones
    : squals ',' transformqual
             {% case unLoc $1 of
                  (h:t) -> do
                    h' <- addTrailingCommaA h (gl $2)
                    return (sLL $1 $> [sLLa $1 $> ((unLoc $3) (reverse (h':t)))]) }
    | squals ',' qual
             {% runPV $3 >>= \ $3 ->
                case unLoc $1 of
                  (h:t) -> do
                    h' <- addTrailingCommaA h (gl $2)
                    return (sLL $1 $> ($3 : (h':t))) }
    | transformqual        { sLL $1 $> [L (getLocAnn $1) ((unLoc $1) [])] }
    | qual                               {% runPV $1 >>= \ $1 ->
                                            return $ sL1 $1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { sLL $1 $> ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { sL1 $1 [$2] }

-- It is possible to enable bracketing (associating) qualifier lists
-- by uncommenting the lines with {| |} above. Due to a lack of
-- consensus on the syntax, this feature is not being used until we
-- get user demand.

transformqual :: { Located ([LStmt GhcPs (LHsExpr GhcPs)] -> Stmt GhcPs (LHsExpr GhcPs)) }
                        -- Function is applied to a list of stmts *in order*
    : 'then' exp              {% runPV (unECP $2) >>= \ $2 ->
                                 return (
                                 sLL $1 $> (\ss -> (mkTransformStmt [mj AnnThen $1] ss $2))) }
    | 'then' exp 'by' exp     {% runPV (unECP $2) >>= \ $2 ->
                                 runPV (unECP $4) >>= \ $4 ->
                                 return (sLL $1 $> (\ss -> (mkTransformByStmt [mj AnnThen $1,mj AnnBy $3] ss $2 $4))) }
    | 'then' 'group' 'using' exp
            {% runPV (unECP $4) >>= \ $4 ->
               return (sLL $1 $> (\ss -> (mkGroupUsingStmt [mj AnnThen $1,mj AnnGroup $2,mj AnnUsing $3] ss $4))) }

    | 'then' 'group' 'by' exp 'using' exp
            {% runPV (unECP $4) >>= \ $4 ->
               runPV (unECP $6) >>= \ $6 ->
               return (sLL $1 $> (\ss -> (mkGroupByUsingStmt [mj AnnThen $1,mj AnnGroup $2,mj AnnBy $3,mj AnnUsing $5] ss $4 $6))) }

-- Note that 'group' is a special_id, which means that you can enable
-- TransformListComp while still using Data.List.group. However, this
-- introduces a shift/reduce conflict. Happy chooses to resolve the conflict
-- in by choosing the "group by" variant, which is what we want.

-----------------------------------------------------------------------------
-- Guards

guardquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : guardquals1           { L (getLoc $1) (reverse (unLoc $1)) }

guardquals1 :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : guardquals1 ',' qual  {% runPV $3 >>= \ $3 ->
                               case unLoc $1 of
                                 (h:t) -> do
                                   h' <- addTrailingCommaA h (gl $2)
                                   return (sLL $1 $> ($3 : (h':t))) }
    | qual                  {% runPV $1 >>= \ $1 ->
                               return $ sL1 $1 [$1] }

-----------------------------------------------------------------------------
-- Case alternatives

altslist(PATS) :: { forall b. DisambECP b => PV (LocatedL [LMatch GhcPs (LocatedA b)]) }
        : '{'        alts(PATS) '}'    { $2 >>= \ $2 -> amsr
                                           (sLL $1 $> (reverse (snd $ unLoc $2)))
                                           (AnnList (Just $ glR $2) (Just $ moc $1) (Just $ mcc $3) (fst $ unLoc $2) []) }
        | vocurly    alts(PATS)  close { $2 >>= \ $2 -> amsr
                                           (L (getLoc $2) (reverse (snd $ unLoc $2)))
                                           (AnnList (Just $ glR $2) Nothing Nothing (fst $ unLoc $2) []) }
        | '{'              '}'   { amsr (sLL $1 $> []) (AnnList Nothing (Just $ moc $1) (Just $ mcc $2) [] []) }
        | vocurly          close { return $ noLocA [] }

alts(PATS) :: { forall b. DisambECP b => PV (Located ([AddEpAnn],[LMatch GhcPs (LocatedA b)])) }
        : alts1(PATS)              { $1 >>= \ $1 -> return $
                                     sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
        | ';' alts(PATS)           { $2 >>= \ $2 -> return $
                                     sLL $1 $> (((mz AnnSemi $1) ++ (fst $ unLoc $2) )
                                               ,snd $ unLoc $2) }

alts1(PATS) :: { forall b. DisambECP b => PV (Located ([AddEpAnn],[LMatch GhcPs (LocatedA b)])) }
        : alts1(PATS) ';' alt(PATS) { $1 >>= \ $1 ->
                                        $3 >>= \ $3 ->
                                          case snd $ unLoc $1 of
                                            [] -> return (sLL $1 $> ((fst $ unLoc $1) ++ (mz AnnSemi $2)
                                                                            ,[$3]))
                                            (h:t) -> do
                                              h' <- addTrailingSemiA h (gl $2)
                                              return (sLL $1 $> (fst $ unLoc $1,$3 : h' : t)) }
        | alts1(PATS) ';'           {  $1 >>= \ $1 ->
                                         case snd $ unLoc $1 of
                                           [] -> return (sLZ $1 $> ((fst $ unLoc $1) ++ (mz AnnSemi $2)
                                                                           ,[]))
                                           (h:t) -> do
                                             h' <- addTrailingSemiA h (gl $2)
                                             return (sLZ $1 $> (fst $ unLoc $1, h' : t)) }
        | alt(PATS)                 { $1 >>= \ $1 -> return $ sL1 $1 ([],[$1]) }

alt(PATS) :: { forall b. DisambECP b => PV (LMatch GhcPs (LocatedA b)) }
        : PATS alt_rhs { $2 >>= \ $2 ->
                         acsA (sLLAsl $1 $> ()) (\loc cs -> L (locA loc)
                                         (Match { m_ext = []
                                                , m_ctxt = CaseAlt -- for \case and \cases, this will be changed during post-processing
                                                , m_pats = $1
                                                , m_grhss = unLoc $2 }))}

alt_rhs :: { forall b. DisambECP b => PV (Located (GRHSs GhcPs (LocatedA b))) }
        : ralt wherebinds           { $1 >>= \alt ->
                                      do { let {L l (bs, csw) = adaptWhereBinds $2}
                                         ; acs (comb2 alt (L l bs)) (\loc cs -> L loc (GRHSs (cs Semi.<> csw) (unLoc alt) bs)) }}

ralt :: { forall b. DisambECP b => PV (Located [LGRHS GhcPs (LocatedA b)]) }
        : '->' exp            { unECP $2 >>= \ $2 ->
                                acs (comb2 $1 $>) (\loc cs -> L loc (unguardedRHS (EpAnn (spanAsAnchor $ comb2 $1 $2) (GrhsAnn Nothing (mu AnnRarrow $1)) cs) (comb2 $1 $2) $2)) }
        | gdpats              { $1 >>= \gdpats ->
                                return $ sL1 gdpats (reverse (unLoc gdpats)) }

gdpats :: { forall b. DisambECP b => PV (Located [LGRHS GhcPs (LocatedA b)]) }
        : gdpats gdpat { $1 >>= \gdpats ->
                         $2 >>= \gdpat ->
                         return $ sLL gdpats gdpat (gdpat : unLoc gdpats) }
        | gdpat        { $1 >>= \gdpat -> return $ sL1 gdpat [gdpat] }

-- layout for MultiWayIf doesn't begin with an open brace, because it's hard to
-- generate the open brace in addition to the vertical bar in the lexer, and
-- we don't need it.
ifgdpats :: { Located ([AddEpAnn],[LGRHS GhcPs (LHsExpr GhcPs)]) }
         : '{' gdpats '}'                 {% runPV $2 >>= \ $2 ->
                                             return $ sLL $1 $> ([moc $1,mcc $3],unLoc $2)  }
         |     gdpats close               {% runPV $1 >>= \ $1 ->
                                             return $ sL1 $1 ([],unLoc $1) }

gdpat   :: { forall b. DisambECP b => PV (LGRHS GhcPs (LocatedA b)) }
        : '|' guardquals '->' exp
                                   { unECP $4 >>= \ $4 ->
                                     acsA (comb2 $1 $>) (\loc cs -> sL loc $ GRHS (EpAnn (glEE $1 $>) (GrhsAnn (Just $ glAA $1) (mu AnnRarrow $3)) cs) (unLoc $2) $4) }

-- 'pat' recognises a pattern, including one with a bang at the top
--      e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat GhcPs }
pat     :  exp          {% (checkPattern <=< runPV) (unECP $1) }

-- 'pats1' does the same thing as 'pat', but returns it as a singleton
-- list so that it can be used with a parameterized production rule
--
-- It is used only for parsing patterns in `\case` and `case of`
pats1   :: { [LPat GhcPs] }
pats1   : pat { [ $1 ] }

bindpat :: { LPat GhcPs }
bindpat :  exp            {% -- See Note [Parser-Validator Details] in GHC.Parser.PostProcess
                             checkPattern_details incompleteDoBlock
                                              (unECP $1) }

argpat   :: { LPat GhcPs }
argpat    : apat                  { $1 }
          | PREFIX_AT atype       { sLLa $1 $> (InvisPat (epTok $1) (mkHsTyPat $2)) }

argpats :: { [LPat GhcPs] }
          : argpat argpats            { $1 : $2 }
          | {- empty -}               { [] }


apat   :: { LPat GhcPs }
apat    : aexp                  {% (checkPattern <=< runPV) (unECP $1) }

apats  :: { [LPat GhcPs] }
        : apat apats            { $1 : $2 }
        | {- empty -}           { [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { forall b. DisambECP b => PV (LocatedL [LocatedA (Stmt GhcPs (LocatedA b))]) }
        : '{'           stmts '}'       { $2 >>= \ $2 ->
                                          amsr (sLL $1 $> (reverse $ snd $ unLoc $2)) (AnnList (stmtsAnchor $2) (Just $ moc $1) (Just $ mcc $3) (fromOL $ fst $ unLoc $2) []) }
        |     vocurly   stmts close     { $2 >>= \ $2 -> amsr
                                          (L (stmtsLoc $2) (reverse $ snd $ unLoc $2)) (AnnList (stmtsAnchor $2) Nothing Nothing (fromOL $ fst $ unLoc $2) []) }

--      do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead

stmts :: { forall b. DisambECP b => PV (Located (OrdList AddEpAnn,[LStmt GhcPs (LocatedA b)])) }
        : stmts ';' stmt  { $1 >>= \ $1 ->
                            $3 >>= \ ($3 :: LStmt GhcPs (LocatedA b)) ->
                            case (snd $ unLoc $1) of
                              [] -> return (sLL $1 $> ((fst $ unLoc $1) `snocOL` (mj AnnSemi $2)
                                                     ,$3   : (snd $ unLoc $1)))
                              (h:t) -> do
                               { h' <- addTrailingSemiA h (gl $2)
                               ; return $ sLL $1 $> (fst $ unLoc $1,$3 :(h':t)) }}

        | stmts ';'     {  $1 >>= \ $1 ->
                           case (snd $ unLoc $1) of
                             [] -> return (sLZ $1 $> ((fst $ unLoc $1) `snocOL` (mj AnnSemi $2),snd $ unLoc $1))
                             (h:t) -> do
                               { h' <- addTrailingSemiA h (gl $2)
                               ; return $ sL1 $1 (fst $ unLoc $1,h':t) }}
        | stmt                   { $1 >>= \ $1 ->
                                   return $ sL1 $1 (nilOL,[$1]) }
        | {- empty -}            { return $ noLoc (nilOL,[]) }


-- For typing stmts at the GHCi prompt, where
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt GhcPs (LHsExpr GhcPs)) }
        : stmt                          {% fmap Just (runPV $1) }
        | {- nothing -}                 { Nothing }

-- For GHC API.
e_stmt :: { LStmt GhcPs (LHsExpr GhcPs) }
        : stmt                          {% runPV $1 }

stmt  :: { forall b. DisambECP b => PV (LStmt GhcPs (LocatedA b)) }
        : qual                          { $1 }
        | 'rec' stmtlist                {  $2 >>= \ $2 ->
                                           amsA' (sLL $1 $> $ mkRecStmt (hsDoAnn $1 $2 AnnRec) $2) }

qual  :: { forall b. DisambECP b => PV (LStmt GhcPs (LocatedA b)) }
    : bindpat '<-' exp                   { unECP $3 >>= \ $3 ->
                                           amsA' (sLL $1 $> $ mkPsBindStmt [mu AnnLarrow $2] $1 $3) }
    | exp                                { unECP $1 >>= \ $1 ->
                                           return $ sL1a $1 $ mkBodyStmt $1 }
    | 'let' binds                        { amsA' (sLL $1 $> $ mkLetStmt [mj AnnLet $1] (unLoc $2)) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds  :: { forall b. DisambECP b => PV ([Fbind b], Maybe SrcSpan) }
        : fbinds1                       { $1 }
        | {- empty -}                   { return ([], Nothing) }

fbinds1 :: { forall b. DisambECP b => PV ([Fbind b], Maybe SrcSpan) }
        : fbind ',' fbinds1
                 { $1 >>= \ $1 ->
                   $3 >>= \ $3 -> do
                   h <- addTrailingCommaFBind $1 (gl $2)
                   return (case $3 of (flds, dd) -> (h : flds, dd)) }
        | fbind                         { $1 >>= \ $1 ->
                                          return ([$1], Nothing) }
        | '..'                          { return ([],   Just (getLoc $1)) }

fbind   :: { forall b. DisambECP b => PV (Fbind b) }
        : qvar '=' texp  { unECP $3 >>= \ $3 ->
                           fmap Left $ amsA' (sLL $1 $> $ HsFieldBind [mj AnnEqual $2] (sL1a $1 $ mkFieldOcc $1) $3 False) }
                        -- RHS is a 'texp', allowing view patterns (#6038)
                        -- and, incidentally, sections.  Eg
                        -- f (R { x = show -> s }) = ...

        | qvar          { placeHolderPunRhs >>= \rhs ->
                          fmap Left $ amsA' (sL1 $1 $ HsFieldBind [] (sL1a $1 $ mkFieldOcc $1) rhs True) }
                        -- In the punning case, use a place-holder
                        -- The renamer fills in the final value

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        -- AZ: need to pull out the let block into a helper
        | field TIGHT_INFIX_PROJ fieldToUpdate '=' texp
                        { do
                            let top = sL1a $1 $ DotFieldOcc noAnn $1
                                ((L lf (DotFieldOcc _ f)):t) = reverse (unLoc $3)
                                lf' = comb2 $2 (L lf ())
                                fields = top : L (noAnnSrcSpan lf') (DotFieldOcc (AnnFieldLabel (Just $ glAA $2))  f) : t
                                final = last fields
                                l = comb2 $1 $3
                                isPun = False
                            $5 <- unECP $5
                            fmap Right $ mkHsProjUpdatePV (comb2 $1 $5) (L l fields) $5 isPun
                                            [mj AnnEqual $4]
                        }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        -- AZ: need to pull out the let block into a helper
        | field TIGHT_INFIX_PROJ fieldToUpdate
                        { do
                            let top =  sL1a $1 $ DotFieldOcc noAnn $1
                                ((L lf (DotFieldOcc _ f)):t) = reverse (unLoc $3)
                                lf' = comb2 $2 (L lf ())
                                fields = top : L (noAnnSrcSpan lf') (DotFieldOcc (AnnFieldLabel (Just $ glAA $2)) f) : t
                                final = last fields
                                l = comb2 $1 $3
                                isPun = True
                            var <- mkHsVarPV (L (noAnnSrcSpan $ getLocA final) (mkRdrUnqual . mkVarOccFS . field_label . unLoc . dfoLabel . unLoc $ final))
                            fmap Right $ mkHsProjUpdatePV l (L l fields) var isPun []
                        }

fieldToUpdate :: { Located [LocatedAn NoEpAnns (DotFieldOcc GhcPs)] }
fieldToUpdate
        -- See Note [Whitespace-sensitive operator parsing] in Lexer.x
        : fieldToUpdate TIGHT_INFIX_PROJ field   { sLL $1 $> ((sLLa $2 $> (DotFieldOcc (AnnFieldLabel $ Just $ glAA $2) $3)) : unLoc $1) }
        | field       { sL1 $1 [sL1a $1 (DotFieldOcc (AnnFieldLabel Nothing) $1)] }

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds  :: { Located [LIPBind GhcPs] } -- reversed
        : dbinds ';' dbind
                      {% case unLoc $1 of
                           (h:t) -> do
                             h' <- addTrailingSemiA h (gl $2)
                             return (let { this = $3; rest = h':t }
                                in rest `seq` this `seq` sLL $1 $> (this : rest)) }
        | dbinds ';'  {% case unLoc $1 of
                           (h:t) -> do
                             h' <- addTrailingSemiA h (gl $2)
                             return (sLZ $1 $> (h':t)) }
        | dbind                        { let this = $1 in this `seq` (sL1 $1 [this]) }
--      | {- empty -}                  { [] }

dbind   :: { LIPBind GhcPs }
dbind   : ipvar '=' exp                {% runPV (unECP $3) >>= \ $3 ->
                                          amsA' (sLL $1 $> (IPBind [mj AnnEqual $2] (reLoc $1) $3)) }

ipvar   :: { Located HsIPName }
        : IPDUPVARID            { sL1 $1 (HsIPName (getIPDUPVARID $1)) }

-----------------------------------------------------------------------------
-- Overloaded labels

overloaded_label :: { Located (SourceText, FastString) }
        : LABELVARID          { sL1 $1 (getLABELVARIDs $1, getLABELVARID $1) }

-----------------------------------------------------------------------------
-- Warnings and deprecations

name_boolformula_opt :: { LBooleanFormula (LocatedN RdrName) }
        : name_boolformula          { $1 }
        | {- empty -}               { noLocA mkTrue }

name_boolformula :: { LBooleanFormula (LocatedN RdrName) }
        : name_boolformula_and                      { $1 }
        | name_boolformula_and '|' name_boolformula
                           {% do { h <- addTrailingVbarL $1 (gl $2)
                                 ; return (sLLa $1 $> (Or [h,$3])) } }

name_boolformula_and :: { LBooleanFormula (LocatedN RdrName) }
        : name_boolformula_and_list
                  { sLLa (head $1) (last $1) (And ($1)) }

name_boolformula_and_list :: { [LBooleanFormula (LocatedN RdrName)] }
        : name_boolformula_atom                               { [$1] }
        | name_boolformula_atom ',' name_boolformula_and_list
            {% do { h <- addTrailingCommaL $1 (gl $2)
                  ; return (h : $3) } }

name_boolformula_atom :: { LBooleanFormula (LocatedN RdrName) }
        : '(' name_boolformula ')'  {% amsr (sLL $1 $> (Parens $2))
                                      (AnnList Nothing (Just (mop $1)) (Just (mcp $3)) [] []) }
        | name_var                  { sL1a $1 (Var $1) }

namelist :: { Located [LocatedN RdrName] }
namelist : name_var              { sL1 $1 [$1] }
         | name_var ',' namelist {% do { h <- addTrailingCommaN $1 (gl $2)
                                       ; return (sLL $1 $> (h : unLoc $3)) }}

name_var :: { LocatedN RdrName }
name_var : var { $1 }
         | con { $1 }

-----------------------------------------
-- Data constructors
-- There are two different productions here as lifted list constructors
-- are parsed differently.

qcon :: { LocatedN RdrName }
  : gen_qcon              { $1}
  | sysdcon               { L (getLoc $1) $ nameRdrName (dataConName (unLoc $1)) }

gen_qcon :: { LocatedN RdrName }
  : qconid                { $1 }
  | '(' qconsym ')'       {% amsr (sLL $1 $> (unLoc $2))
                                  (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }

con     :: { LocatedN RdrName }
        : conid                 { $1 }
        | '(' consym ')'        {% amsr (sLL $1 $> (unLoc $2))
                                        (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }
        | sysdcon               { L (getLoc $1) $ nameRdrName (dataConName (unLoc $1)) }

con_list :: { Located (NonEmpty (LocatedN RdrName)) }
con_list : con                  { sL1 $1 (pure $1) }
         | con ',' con_list     {% sLL $1 $> . (:| toList (unLoc $3)) <\$> addTrailingCommaN $1 (gl $2) }

qcon_list :: { [LocatedN RdrName] }
qcon_list : qcon                  { [$1] }
          | qcon ',' qcon_list    {% do { h <- addTrailingCommaN $1 (gl $2)
                                        ; return (h : $3) }}

-- See Note [ExplicitTuple] in GHC.Hs.Expr
sysdcon_nolist :: { LocatedN DataCon }  -- Wired in data constructors
        : '(' commas ')'        {% amsr (sLL $1 $> $ tupleDataCon Boxed (snd $2 + 1))
                                       (NameAnnCommas NameParens (glAA $1) (map srcSpan2e (fst $2)) (glAA $3) []) }
        | '(#' '#)'             {% amsr (sLL $1 $> $ unboxedUnitDataCon) (NameAnnOnly NameParensHash (glAA $1) (glAA $2) []) }
        | '(#' commas '#)'      {% amsr (sLL $1 $> $ tupleDataCon Unboxed (snd $2 + 1))
                                       (NameAnnCommas NameParensHash (glAA $1) (map srcSpan2e (fst $2)) (glAA $3) []) }

-- See Note [Empty lists] in GHC.Hs.Expr
sysdcon :: { LocatedN DataCon }
        : sysdcon_nolist                 { $1 }
        | '(' ')'               {% amsr (sLL $1 $> unitDataCon) (NameAnnOnly NameParens (glAA $1) (glAA $2) []) }
        |  '[' ']'               {% amsr (sLL $1 $> nilDataCon) (NameAnnOnly NameSquare (glAA $1) (glAA $2) []) }

conop :: { LocatedN RdrName }
        : consym                { $1 }
        | '`' conid '`'         {% amsr (sLL $1 $> (unLoc $2))
                                          (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

qconop :: { LocatedN RdrName }
        : qconsym               { $1 }
        | '`' qconid '`'        {% amsr (sLL $1 $> (unLoc $2))
                                          (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

----------------------------------------------------------------------------
-- Type constructors


-- See Note [Unit tuples] in GHC.Hs.Type for the distinction
-- between gtycon and ntgtycon
gtycon :: { LocatedN RdrName }  -- A "general" qualified tycon, including unit tuples
        : ntgtycon                     { $1 }
        | '(' ')'                      {% amsr (sLL $1 $> $ getRdrName unitTyCon)
                                                (NameAnnOnly NameParens (glAA $1) (glAA $2) []) }
        | '(#' '#)'                    {% amsr (sLL $1 $> $ getRdrName unboxedUnitTyCon)
                                                (NameAnnOnly NameParensHash (glAA $1) (glAA $2) []) }
        | '[' ']'               {% amsr (sLL $1 $> $ listTyCon_RDR)
                                      (NameAnnOnly NameSquare (glAA $1) (glAA $2) []) }

ntgtycon :: { LocatedN RdrName }  -- A "general" qualified tycon, excluding unit tuples
        : oqtycon               { $1 }
        | '(' commas ')'        {% do { n <- mkTupleSyntaxTycon Boxed (snd $2 + 1)
                                      ; amsr (sLL $1 $> n) (NameAnnCommas NameParens (glAA $1) (map srcSpan2e (fst $2)) (glAA $3) []) }}
        | '(#' commas '#)'      {% do { n <- mkTupleSyntaxTycon Unboxed (snd $2 + 1)
                                      ; amsr (sLL $1 $> n) (NameAnnCommas NameParensHash (glAA $1) (map srcSpan2e (fst $2)) (glAA $3) []) }}
        | '(#' bars '#)'        {% do { requireLTPuns PEP_SumSyntaxType $1 $>
                                      ; amsr (sLL $1 $> $ (getRdrName (sumTyCon (snd $2 + 1))))
                                       (NameAnnBars NameParensHash (glAA $1) (map srcSpan2e (fst $2)) (glAA $3) []) } }
        | '(' '->' ')'          {% amsr (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                                       (NameAnnRArrow (isUnicode $2) (Just $ glAA $1) (glAA $2) (Just $ glAA $3) []) }

oqtycon :: { LocatedN RdrName }  -- An "ordinary" qualified tycon;
                                -- These can appear in export lists
        : qtycon                        { $1 }
        | '(' qtyconsym ')'             {% amsr (sLL $1 $> (unLoc $2))
                                                  (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }

oqtycon_no_varcon :: { LocatedN RdrName }  -- Type constructor which cannot be mistaken
                                          -- for variable constructor in export lists
                                          -- see Note [Type constructors in export list]
        :  qtycon            { $1 }
        | '(' QCONSYM ')'    {% let { name :: Located RdrName
                                    ; name = sL1 $2 $! mkQual tcClsName (getQCONSYM $2) }
                                in amsr (sLL $1 $> (unLoc name)) (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }
        | '(' CONSYM ')'     {% let { name :: Located RdrName
                                    ; name = sL1 $2 $! mkUnqual tcClsName (getCONSYM $2) }
                                in amsr (sLL $1 $> (unLoc name)) (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }
        | '(' ':' ')'        {% let { name :: Located RdrName
                                    ; name = sL1 $2 $! consDataCon_RDR }
                                in amsr (sLL $1 $> (unLoc name)) (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }

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

qtyconop :: { LocatedN RdrName } -- Qualified or unqualified
        -- See Note [%shift: qtyconop -> qtyconsym]
        : qtyconsym %shift              { $1 }
        | '`' qtycon '`'                {% amsr (sLL $1 $> (unLoc $2))
                                                (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

qtycon :: { LocatedN RdrName }   -- Qualified or unqualified
        : QCONID            { sL1n $1 $! mkQual tcClsName (getQCONID $1) }
        | tycon             { $1 }

tycon   :: { LocatedN RdrName }  -- Unqualified
        : CONID                   { sL1n $1 $! mkUnqual tcClsName (getCONID $1) }

qtyconsym :: { LocatedN RdrName }
        : QCONSYM            { sL1n $1 $! mkQual tcClsName (getQCONSYM $1) }
        | QVARSYM            { sL1n $1 $! mkQual tcClsName (getQVARSYM $1) }
        | tyconsym           { $1 }

tyconsym :: { LocatedN RdrName }
        : CONSYM                { sL1n $1 $! mkUnqual tcClsName (getCONSYM $1) }
        | VARSYM                { sL1n $1 $! mkUnqual tcClsName (getVARSYM $1) }
        | ':'                   { sL1n $1 $! consDataCon_RDR }
        | '-'                   { sL1n $1 $! mkUnqual tcClsName (fsLit "-") }
        | '.'                   { sL1n $1 $! mkUnqual tcClsName (fsLit ".") }

-- An "ordinary" unqualified tycon. See `oqtycon` for the qualified version.
-- These can appear in `ANN type` declarations (#19374).
otycon :: { LocatedN RdrName }
        : tycon                 { $1 }
        | '(' tyconsym ')'      {% amsr (sLL $1 $> (unLoc $2))
                                        (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }

-----------------------------------------------------------------------------
-- Operators

op      :: { LocatedN RdrName }   -- used in infix decls
        : varop                 { $1 }
        | conop                 { $1 }
        | '->'                  {% amsr (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                                     (NameAnnRArrow (isUnicode $1) Nothing (glAA $1) Nothing []) }

varop   :: { LocatedN RdrName }
        : varsym                { $1 }
        | '`' varid '`'         {% amsr (sLL $1 $> (unLoc $2))
                                           (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

qop     :: { forall b. DisambInfixOp b => PV (LocatedN b) }   -- used in sections
        : qvarop                { mkHsVarOpPV $1 }
        | qconop                { mkHsConOpPV $1 }
        | hole_op               { mkHsInfixHolePV $1 }

qopm    :: { forall b. DisambInfixOp b => PV (LocatedN b) }   -- used in sections
        : qvaropm               { mkHsVarOpPV $1 }
        | qconop                { mkHsConOpPV $1 }
        | hole_op               { mkHsInfixHolePV $1 }

hole_op :: { LocatedN (HsExpr GhcPs) }   -- used in sections
hole_op : '`' '_' '`'           { sLLa $1 $> (hsHoleExpr (Just $ EpAnnUnboundVar (glAA $1, glAA $3) (glAA $2))) }

qvarop :: { LocatedN RdrName }
        : qvarsym               { $1 }
        | '`' qvarid '`'        {% amsr (sLL $1 $> (unLoc $2))
                                           (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

qvaropm :: { LocatedN RdrName }
        : qvarsym_no_minus      { $1 }
        | '`' qvarid '`'        {% amsr (sLL $1 $> (unLoc $2))
                                           (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

-----------------------------------------------------------------------------
-- Type variables

tyvar   :: { LocatedN RdrName }
tyvar   : tyvarid               { $1 }

tyvarop :: { LocatedN RdrName }
tyvarop : '`' tyvarid '`'       {% amsr (sLL $1 $> (unLoc $2))
                                           (NameAnn NameBackquotes (glAA $1) (glAA $2) (glAA $3) []) }

tyvarid :: { LocatedN RdrName }
        : VARID            { sL1n $1 $! mkUnqual tvName (getVARID $1) }
        | special_id       { sL1n $1 $! mkUnqual tvName (unLoc $1) }
        | 'unsafe'         { sL1n $1 $! mkUnqual tvName (fsLit "unsafe") }
        | 'safe'           { sL1n $1 $! mkUnqual tvName (fsLit "safe") }
        | 'interruptible'  { sL1n $1 $! mkUnqual tvName (fsLit "interruptible") }
        -- If this changes relative to varid, update 'checkRuleTyVarBndrNames'
        -- in GHC.Parser.PostProcess
        -- See Note [Parsing explicit foralls in Rules]

-----------------------------------------------------------------------------
-- Variables

var     :: { LocatedN RdrName }
        : varid                 { $1 }
        | '(' varsym ')'        {% amsr (sLL $1 $> (unLoc $2))
                                   (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }

qvar    :: { LocatedN RdrName }
        : qvarid                { $1 }
        | '(' varsym ')'        {% amsr (sLL $1 $> (unLoc $2))
                                   (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }
        | '(' qvarsym1 ')'      {% amsr (sLL $1 $> (unLoc $2))
                                   (NameAnn NameParens (glAA $1) (glAA $2) (glAA $3) []) }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

field :: { LocatedN FieldLabelString  }
      : varid { fmap (FieldLabelString . occNameFS . rdrNameOcc) $1 }

qvarid :: { LocatedN RdrName }
        : varid               { $1 }
        | QVARID              { sL1n $1 $! mkQual varName (getQVARID $1) }

-- Note that 'role' and 'family' get lexed separately regardless of
-- the use of extensions. However, because they are listed here,
-- this is OK and they can be used as normal varids.
-- See Note [Lexing type pseudo-keywords] in GHC.Parser.Lexer
varid :: { LocatedN RdrName }
        : VARID            { sL1n $1 $! mkUnqual varName (getVARID $1) }
        | special_id       { sL1n $1 $! mkUnqual varName (unLoc $1) }
        | 'unsafe'         { sL1n $1 $! mkUnqual varName (fsLit "unsafe") }
        | 'safe'           { sL1n $1 $! mkUnqual varName (fsLit "safe") }
        | 'interruptible'  { sL1n $1 $! mkUnqual varName (fsLit "interruptible")}
        | 'family'         { sL1n $1 $! mkUnqual varName (fsLit "family") }
        | 'role'           { sL1n $1 $! mkUnqual varName (fsLit "role") }
        -- If this changes relative to tyvarid, update 'checkRuleTyVarBndrNames'
        -- in GHC.Parser.PostProcess
        -- See Note [Parsing explicit foralls in Rules]

qvarsym :: { LocatedN RdrName }
        : varsym                { $1 }
        | qvarsym1              { $1 }

qvarsym_no_minus :: { LocatedN RdrName }
        : varsym_no_minus       { $1 }
        | qvarsym1              { $1 }

qvarsym1 :: { LocatedN RdrName }
qvarsym1 : QVARSYM              { sL1n $1 $ mkQual varName (getQVARSYM $1) }

varsym :: { LocatedN RdrName }
        : varsym_no_minus       { $1 }
        | '-'                   { sL1n $1 $ mkUnqual varName (fsLit "-") }

varsym_no_minus :: { LocatedN RdrName } -- varsym not including '-'
        : VARSYM               { sL1n $1 $ mkUnqual varName (getVARSYM $1) }
        | special_sym          { sL1n $1 $ mkUnqual varName (unLoc $1) }


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
        -- See Note [%shift: special_id -> 'group']
        | 'group' %shift        { sL1 $1 (fsLit "group") }
        | 'stock'               { sL1 $1 (fsLit "stock") }
        | 'anyclass'            { sL1 $1 (fsLit "anyclass") }
        | 'via'                 { sL1 $1 (fsLit "via") }
        | 'unit'                { sL1 $1 (fsLit "unit") }
        | 'dependency'          { sL1 $1 (fsLit "dependency") }
        | 'signature'           { sL1 $1 (fsLit "signature") }

special_sym :: { Located FastString }
special_sym : '.'       { sL1 $1 (fsLit ".") }
            | '*'       { sL1 $1 (starSym (isUnicode $1)) }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { LocatedN RdrName }   -- Qualified or unqualified
        : conid              { $1 }
        | QCONID             { sL1n $1 $! mkQual dataName (getQCONID $1) }

conid   :: { LocatedN RdrName }
        : CONID                { sL1n $1 $ mkUnqual dataName (getCONID $1) }

qconsym :: { LocatedN RdrName }  -- Qualified or unqualified
        : consym               { $1 }
        | QCONSYM              { sL1n $1 $ mkQual dataName (getQCONSYM $1) }

consym :: { LocatedN RdrName }
        : CONSYM              { sL1n $1 $ mkUnqual dataName (getCONSYM $1) }

        -- ':' means only list cons
        | ':'                { sL1n $1 $ consDataCon_RDR }


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
        | PRIMINTEGER8      { sL1 $1 $ HsInt8Prim   (getPRIMINTEGER8s $1)
                                                    $ getPRIMINTEGER8 $1 }
        | PRIMINTEGER16     { sL1 $1 $ HsInt16Prim  (getPRIMINTEGER16s $1)
                                                    $ getPRIMINTEGER16 $1 }
        | PRIMINTEGER32     { sL1 $1 $ HsInt32Prim  (getPRIMINTEGER32s $1)
                                                    $ getPRIMINTEGER32 $1 }
        | PRIMINTEGER64     { sL1 $1 $ HsInt64Prim  (getPRIMINTEGER64s $1)
                                                    $ getPRIMINTEGER64 $1 }
        | PRIMWORD8         { sL1 $1 $ HsWord8Prim  (getPRIMWORD8s $1)
                                                    $ getPRIMWORD8 $1 }
        | PRIMWORD16        { sL1 $1 $ HsWord16Prim (getPRIMWORD16s $1)
                                                    $ getPRIMWORD16 $1 }
        | PRIMWORD32        { sL1 $1 $ HsWord32Prim (getPRIMWORD32s $1)
                                                    $ getPRIMWORD32 $1 }
        | PRIMWORD64        { sL1 $1 $ HsWord64Prim (getPRIMWORD64s $1)
                                                    $ getPRIMWORD64 $1 }
        | PRIMCHAR          { sL1 $1 $ HsCharPrim   (getPRIMCHARs $1)
                                                    $ getPRIMCHAR $1 }
        | PRIMSTRING        { sL1 $1 $ HsStringPrim (getPRIMSTRINGs $1)
                                                    $ getPRIMSTRING $1 }
        | PRIMFLOAT         { sL1 $1 $ HsFloatPrim  noExtField $ getPRIMFLOAT $1 }
        | PRIMDOUBLE        { sL1 $1 $ HsDoublePrim noExtField $ getPRIMDOUBLE $1 }

-----------------------------------------------------------------------------
-- Layout

{- Note [Layout and error]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The Haskell 2010 report (Section 10.3, Note 5) dictates the use of the error
token in `close`. To recall why that is necessary, consider

  f x = case x of
    True -> False
    where y = x+1

The virtual pass L inserts vocurly, semi, vccurly to return a laid-out
token stream. It must insert a vccurly before `where` to close the layout
block introduced by `of`.
But there is no good way to do so other than L becoming aware of the grammar!
Thus, L is specified to detect the ensuing parse error (implemented via
happy's `error` token) and then insert the vccurly.
Thus in effect, L is distributed between Lexer.x and Parser.y.

There are a bunch of other, less "tricky" examples:

  let x = x {- vccurly -} in x   -- could just track bracketing of
                                 -- let..in on layout stack to fix
  (case x of
   True -> False {- vccurly -})  -- ditto for surrounding delimiters such as ()

  data T = T;{- vccurly -}       -- Need insert vccurly at EOF

Many of these are not that hard to fix, but still tedious and prone to break
when the grammar changes; but the `of`/`where` example is especially gnarly,
because it demonstrates a grammatical interaction between two lexically
unrelated tokens.
-}
close :: { () }
        : vccurly               { () } -- context popped in lexer.
        | error                 {% popContext } -- See Note [Layout and error]

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid   :: { LocatedA ModuleName }
        : CONID                 { sL1a $1 $ mkModuleNameFS (getCONID $1) }
        | QCONID                { sL1a $1 $ let (mod,c) = getQCONID $1 in
                                  mkModuleNameFS
                                   (concatFS [mod, fsLit ".", c])
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

{
happyError :: P a
happyError = srcParseFail

getVARID          (L _ (ITvarid    x)) = x
getCONID          (L _ (ITconid    x)) = x
getVARSYM         (L _ (ITvarsym   x)) = x
getCONSYM         (L _ (ITconsym   x)) = x
getDO             (L _ (ITdo      x)) = x
getMDO            (L _ (ITmdo     x)) = x
getQVARID         (L _ (ITqvarid   x)) = x
getQCONID         (L _ (ITqconid   x)) = x
getQVARSYM        (L _ (ITqvarsym  x)) = x
getQCONSYM        (L _ (ITqconsym  x)) = x
getIPDUPVARID     (L _ (ITdupipvarid   x)) = x
getLABELVARID     (L _ (ITlabelvarid _ x)) = x
getCHAR           (L _ (ITchar   _ x)) = x
getSTRING         (L _ (ITstring _ x)) = x
getINTEGER        (L _ (ITinteger x))  = x
getRATIONAL       (L _ (ITrational x)) = x
getPRIMCHAR       (L _ (ITprimchar _ x)) = x
getPRIMSTRING     (L _ (ITprimstring _ x)) = x
getPRIMINTEGER    (L _ (ITprimint  _ x)) = x
getPRIMWORD       (L _ (ITprimword _ x)) = x
getPRIMINTEGER8   (L _ (ITprimint8 _ x)) = x
getPRIMINTEGER16  (L _ (ITprimint16 _ x)) = x
getPRIMINTEGER32  (L _ (ITprimint32 _ x)) = x
getPRIMINTEGER64  (L _ (ITprimint64 _ x)) = x
getPRIMWORD8      (L _ (ITprimword8 _ x)) = x
getPRIMWORD16     (L _ (ITprimword16 _ x)) = x
getPRIMWORD32     (L _ (ITprimword32 _ x)) = x
getPRIMWORD64     (L _ (ITprimword64 _ x)) = x
getPRIMFLOAT      (L _ (ITprimfloat x)) = x
getPRIMDOUBLE     (L _ (ITprimdouble x)) = x
getINLINE         (L _ (ITinline_prag _ inl conl)) = (inl,conl)
getSPEC_INLINE    (L _ (ITspec_inline_prag src True))  = (Inline src,FunLike)
getSPEC_INLINE    (L _ (ITspec_inline_prag src False)) = (NoInline src,FunLike)
getCOMPLETE_PRAGs (L _ (ITcomplete_prag x)) = x
getVOCURLY        (L (RealSrcSpan l _) ITvocurly) = srcSpanStartCol l

getINTEGERs       (L _ (ITinteger (IL src _ _))) = src
getCHARs          (L _ (ITchar       src _)) = src
getSTRINGs        (L _ (ITstring     src _)) = src
getPRIMCHARs      (L _ (ITprimchar   src _)) = src
getPRIMSTRINGs    (L _ (ITprimstring src _)) = src
getPRIMINTEGERs   (L _ (ITprimint    src _)) = src
getPRIMWORDs      (L _ (ITprimword   src _)) = src
getPRIMINTEGER8s  (L _ (ITprimint8   src _)) = src
getPRIMINTEGER16s (L _ (ITprimint16  src _)) = src
getPRIMINTEGER32s (L _ (ITprimint32  src _)) = src
getPRIMINTEGER64s (L _ (ITprimint64  src _)) = src
getPRIMWORD8s     (L _ (ITprimword8  src _)) = src
getPRIMWORD16s    (L _ (ITprimword16 src _)) = src
getPRIMWORD32s    (L _ (ITprimword32 src _)) = src
getPRIMWORD64s    (L _ (ITprimword64 src _)) = src

getLABELVARIDs    (L _ (ITlabelvarid src _)) = src

-- See Note [Pragma source text] in "GHC.Types.SourceText" for the following
getINLINE_PRAGs       (L _ (ITinline_prag       _ inl _)) = inlineSpecSource inl
getOPAQUE_PRAGs       (L _ (ITopaque_prag       src))     = src
getSPEC_PRAGs         (L _ (ITspec_prag         src))     = src
getSPEC_INLINE_PRAGs  (L _ (ITspec_inline_prag  src _))   = src
getSOURCE_PRAGs       (L _ (ITsource_prag       src)) = src
getRULES_PRAGs        (L _ (ITrules_prag        src)) = src
getWARNING_PRAGs      (L _ (ITwarning_prag      src)) = src
getDEPRECATED_PRAGs   (L _ (ITdeprecated_prag   src)) = src
getSCC_PRAGs          (L _ (ITscc_prag          src)) = src
getUNPACK_PRAGs       (L _ (ITunpack_prag       src)) = src
getNOUNPACK_PRAGs     (L _ (ITnounpack_prag     src)) = src
getANN_PRAGs          (L _ (ITann_prag          src)) = src
getMINIMAL_PRAGs      (L _ (ITminimal_prag      src)) = src
getOVERLAPPABLE_PRAGs (L _ (IToverlappable_prag src)) = src
getOVERLAPPING_PRAGs  (L _ (IToverlapping_prag  src)) = src
getOVERLAPS_PRAGs     (L _ (IToverlaps_prag     src)) = src
getINCOHERENT_PRAGs   (L _ (ITincoherent_prag   src)) = src
getCTYPEs             (L _ (ITctype             src)) = src

getStringLiteral l = StringLiteral (getSTRINGs l) (getSTRING l) Nothing

isUnicode :: Located Token -> Bool
isUnicode (L _ (ITforall         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITdarrow         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITdcolon         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITlarrow         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITrarrow         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITlarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITrarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITLarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITRarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (IToparenbar      iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITcparenbar      iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITopenExpQuote _ iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITcloseQuote     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITstar           iu)) = iu == UnicodeSyntax
isUnicode (L _ ITlolly)               = True
isUnicode _                           = False

hasE :: Located Token -> Bool
hasE (L _ (ITopenExpQuote HasE _)) = True
hasE (L _ (ITopenTExpQuote HasE))  = True
hasE _                             = False

getSCC :: Located Token -> P FastString
getSCC lt = do let s = getSTRING lt
               -- We probably actually want to be more restrictive than this
               if ' ' `elem` unpackFS s
                   then addFatalError $ mkPlainErrorMsgEnvelope (getLoc lt) $ PsErrSpaceInSCC
                   else return s

stringLiteralToHsDocWst :: Located StringLiteral -> LocatedE (WithHsDocIdentifiers StringLiteral GhcPs)
stringLiteralToHsDocWst  sl = reLoc $ lexStringLiteral parseIdentifier sl

-- Utilities for combining source spans
comb2 :: (HasLoc a, HasLoc b) => a -> b -> SrcSpan
comb2 !a !b = combineHasLocs a b

comb3 :: (HasLoc a, HasLoc b, HasLoc c) => a -> b -> c -> SrcSpan
comb3 !a !b !c = combineSrcSpans (getHasLoc a) (combineHasLocs b c)

comb4 :: (HasLoc a, HasLoc b, HasLoc c, HasLoc d) => a -> b -> c -> d -> SrcSpan
comb4 !a !b !c !d =
    combineSrcSpans (getHasLoc a) $
    combineSrcSpans (getHasLoc b) $
    combineSrcSpans (getHasLoc c) (getHasLoc d)

comb5 :: (HasLoc a, HasLoc b, HasLoc c, HasLoc d, HasLoc e) => a -> b -> c -> d -> e -> SrcSpan
comb5 !a !b !c !d !e =
    combineSrcSpans (getHasLoc a) $
    combineSrcSpans (getHasLoc b) $
    combineSrcSpans (getHasLoc c) $
    combineSrcSpans (getHasLoc d) (getHasLoc e)

-- strict constructor version:
{-# INLINE sL #-}
sL :: l -> a -> GenLocated l a
sL !loc !a = L loc a

-- See Note [Adding location info] for how these utility functions are used

-- replaced last 3 CPP macros in this file
{-# INLINE sL0 #-}
sL0 :: a -> Located a
sL0 = L noSrcSpan       -- #define L0   L noSrcSpan

{-# INLINE sL1 #-}
sL1 :: HasLoc a => a -> b -> Located b
sL1 !x = sL (getHasLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sL1a #-}
sL1a :: (HasLoc a, HasAnnotation t) =>  a -> b -> GenLocated t b
sL1a !x = sL (noAnnSrcSpan $ getHasLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sL1n #-}
sL1n :: HasLoc a => a -> b -> LocatedN b
sL1n !x = L (noAnnSrcSpan $ getHasLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sLL #-}
sLL :: (HasLoc a, HasLoc b) => a -> b -> c -> Located c
sLL !x !y = sL (comb2 x y) -- #define LL   sL (comb2 $1 $>)

{-# INLINE sLLa #-}
sLLa :: (HasLoc a, HasLoc b, NoAnn t) => a -> b -> c -> LocatedAn t c
sLLa !x !y = sL (noAnnSrcSpan $ comb2 x y) -- #define LL   sL (comb2 $1 $>)

{-# INLINE sLLl #-}
sLLl :: (HasLoc a, HasLoc b) => a -> b -> c -> LocatedL c
sLLl !x !y = sL (noAnnSrcSpan $ comb2 x y) -- #define LL   sL (comb2 $1 $>)

{-# INLINE sLLAsl #-}
sLLAsl :: (HasLoc a) => [a] -> Located b -> c -> Located c
sLLAsl [] = sL1
sLLAsl (!x:_) = sLL x

{-# INLINE sLZ #-}
sLZ :: (HasLoc a, HasLoc b) => a -> b -> c -> Located c
sLZ !x !y = if isZeroWidthSpan (getHasLoc y)
                 then sL (getHasLoc x)
                 else sL (comb2 x y)

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

-- Hint about linear types
hintLinear :: MonadP m => SrcSpan -> m ()
hintLinear span = do
  linearEnabled <- getBit LinearTypesBit
  unless linearEnabled $ addError $ mkPlainErrorMsgEnvelope span $ PsErrLinearFunction

-- Does this look like (a %m)?
looksLikeMult :: LHsType GhcPs -> LocatedN RdrName -> LHsType GhcPs -> Bool
looksLikeMult ty1 l_op ty2
  | Unqual op_name <- unLoc l_op
  , occNameFS op_name == fsLit "%"
  , Strict.Just ty1_pos <- getBufSpan (getLocA ty1)
  , Strict.Just pct_pos <- getBufSpan (getLocA l_op)
  , Strict.Just ty2_pos <- getBufSpan (getLocA ty2)
  , bufSpanEnd ty1_pos /= bufSpanStart pct_pos
  , bufSpanEnd pct_pos == bufSpanStart ty2_pos
  = True
  | otherwise = False

-- Hint about the MultiWayIf extension
hintMultiWayIf :: SrcSpan -> P ()
hintMultiWayIf span = do
  mwiEnabled <- getBit MultiWayIfBit
  unless mwiEnabled $ addError $ mkPlainErrorMsgEnvelope span PsErrMultiWayIf

-- Hint about explicit-forall
hintExplicitForall :: Located Token -> P ()
hintExplicitForall tok = do
    explicit_forall_enabled <- getBit ExplicitForallBit
    in_rule_prag <- getBit InRulePragBit
    unless (explicit_forall_enabled || in_rule_prag) $
      addError $ mkPlainErrorMsgEnvelope (getLoc tok) $
        PsErrExplicitForall (isUnicode tok)

-- Hint about qualified-do
hintQualifiedDo :: Located Token -> P ()
hintQualifiedDo tok = do
    qualifiedDo   <- getBit QualifiedDoBit
    case maybeQDoDoc of
      Just qdoDoc | not qualifiedDo ->
        addError $ mkPlainErrorMsgEnvelope (getLoc tok) $
          (PsErrIllegalQualifiedDo qdoDoc)
      _ -> return ()
  where
    maybeQDoDoc = case unLoc tok of
      ITdo (Just m) -> Just $ ftext m <> text ".do"
      ITmdo (Just m) -> Just $ ftext m <> text ".mdo"
      t -> Nothing

-- When two single quotes don't followed by tyvar or gtycon, we report the
-- error as empty character literal, or TH quote that missing proper type
-- variable or constructor. See #13450.
reportEmptyDoubleQuotes :: SrcSpan -> P a
reportEmptyDoubleQuotes span = do
    thQuotes <- getBit ThQuotesBit
    addFatalError $ mkPlainErrorMsgEnvelope span $ PsErrEmptyDoubleQuotes thQuotes

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************

For the general principles of the following routines, see Note [exact print annotations]
in GHC.Parser.Annotation

-}

-- |Construct an AddEpAnn from the annotation keyword and the location
-- of the keyword itself
mj :: AnnKeywordId -> Located e -> AddEpAnn
mj !a !l = AddEpAnn a (srcSpan2e $ gl l)

mjN :: AnnKeywordId -> LocatedN e -> AddEpAnn
mjN !a !l = AddEpAnn a (srcSpan2e $ glA l)

-- |Construct an AddEpAnn from the annotation keyword and the location
-- of the keyword itself, provided the span is not zero width
mz :: AnnKeywordId -> Located e -> [AddEpAnn]
mz !a !l = if isZeroWidthSpan (gl l) then [] else [AddEpAnn a (srcSpan2e $ gl l)]

msemi :: Located e -> [TrailingAnn]
msemi !l = if isZeroWidthSpan (gl l) then [] else [AddSemiAnn (srcSpan2e $ gl l)]

msemiA :: Located e -> [AddEpAnn]
msemiA !l = if isZeroWidthSpan (gl l) then [] else [AddEpAnn AnnSemi (srcSpan2e $ gl l)]

msemim :: Located e -> Maybe EpaLocation
msemim !l = if isZeroWidthSpan (gl l) then Nothing else Just (srcSpan2e $ gl l)

-- |Construct an AddEpAnn from the annotation keyword and the Located Token. If
-- the token has a unicode equivalent and this has been used, provide the
-- unicode variant of the annotation.
mu :: AnnKeywordId -> Located Token -> AddEpAnn
mu !a lt@(L l t) = AddEpAnn (toUnicodeAnn a lt) (srcSpan2e l)

-- | If the 'Token' is using its unicode variant return the unicode variant of
--   the annotation
toUnicodeAnn :: AnnKeywordId -> Located Token -> AnnKeywordId
toUnicodeAnn !a !t = if isUnicode t then unicodeAnn a else a

toUnicode :: Located Token -> IsUnicodeSyntax
toUnicode t = if isUnicode t then UnicodeSyntax else NormalSyntax

-- -------------------------------------

gl :: GenLocated l a -> l
gl = getLoc

glA :: HasLoc a => a -> SrcSpan
glA = getHasLoc

glR :: HasLoc a => a -> Anchor
glR !la = EpaSpan (getHasLoc la)

glEE :: (HasLoc a, HasLoc b) => a -> b -> Anchor
glEE !x !y = spanAsAnchor $ comb2 x y

glRM :: Located a -> Maybe Anchor
glRM (L !l _) = Just $ spanAsAnchor l

glAA :: HasLoc a => a -> EpaLocation
glAA = srcSpan2e . getHasLoc

n2l :: LocatedN a -> LocatedA a
n2l (L !la !a) = L (l2l la) a

-- Called at the very end to pick up the EOF position, as well as any comments not allocated yet.
acsFinal :: (EpAnnComments -> Maybe (RealSrcSpan, RealSrcSpan) -> Located a) -> P (Located a)
acsFinal a = do
  let (L l _) = a emptyComments Nothing
  !cs <- getCommentsFor l
  csf <- getFinalCommentsFor l
  meof <- getEofPos
  let ce = case meof of
             Strict.Nothing  -> Nothing
             Strict.Just (pos `Strict.And` gap) -> Just (pos,gap)
  return (a (cs Semi.<> csf) ce)

acs :: (HasLoc l, MonadP m) => l -> (l -> EpAnnComments -> GenLocated l a) -> m (GenLocated l a)
acs !l a = do
  !cs <- getCommentsFor (locA l)
  return (a l cs)

acsA :: (HasLoc l, HasAnnotation t, MonadP m) => l -> (l -> EpAnnComments -> Located a) -> m (GenLocated t a)
acsA !l a = do
  !cs <- getCommentsFor (locA l)
  return $ reLoc (a l cs)

ams1 :: MonadP m => Located a -> b -> m (LocatedA b)
ams1 (L l a) b = do
  !cs <- getCommentsFor l
  return (L (EpAnn (spanAsAnchor l) noAnn cs) b)

amsA' :: (NoAnn t, MonadP m) => Located a -> m (GenLocated (EpAnn t) a)
amsA' (L l a) = do
  !cs <- getCommentsFor l
  return (L (EpAnn (spanAsAnchor l) noAnn cs) a)

amsA :: MonadP m => LocatedA a -> [TrailingAnn] -> m (LocatedA a)
amsA (L !l a) bs = do
  !cs <- getCommentsFor (locA l)
  return (L (addAnnsA l bs cs) a)

amsAl :: MonadP m => LocatedA a -> SrcSpan -> [TrailingAnn] -> m (LocatedA a)
amsAl (L l a) loc bs = do
  !cs <- getCommentsFor loc
  return (L (addAnnsA l bs cs) a)

amsr :: MonadP m => Located a -> an -> m (LocatedAn an a)
amsr (L l a) an = do
  !cs <- getCommentsFor l
  return (L (EpAnn (spanAsAnchor l) an cs) a)

-- |Synonyms for AddEpAnn versions of AnnOpen and AnnClose
mo,mc :: Located Token -> AddEpAnn
mo !ll = mj AnnOpen ll
mc !ll = mj AnnClose ll

moc,mcc :: Located Token -> AddEpAnn
moc !ll = mj AnnOpenC ll
mcc !ll = mj AnnCloseC ll

mop,mcp :: Located Token -> AddEpAnn
mop !ll = mj AnnOpenP ll
mcp !ll = mj AnnCloseP ll

moh,mch :: Located Token -> AddEpAnn
moh !ll = mj AnnOpenPH ll
mch !ll = mj AnnClosePH ll

mos,mcs :: Located Token -> AddEpAnn
mos !ll = mj AnnOpenS ll
mcs !ll = mj AnnCloseS ll

-- | Parse a Haskell module with Haddock comments. This is done in two steps:
--
-- * 'parseModuleNoHaddock' to build the AST
-- * 'addHaddockToModule' to insert Haddock comments into it
--
-- This and the signature module parser are the only parser entry points that
-- deal with Haddock comments. The other entry points ('parseDeclaration',
-- 'parseExpression', etc) do not insert them into the AST.
parseModule :: P (Located (HsModule GhcPs))
parseModule = parseModuleNoHaddock >>= addHaddockToModule

-- | Parse a Haskell signature module with Haddock comments. This is done in two
-- steps:
--
-- * 'parseSignatureNoHaddock' to build the AST
-- * 'addHaddockToModule' to insert Haddock comments into it
--
-- This and the module parser are the only parser entry points that deal with
-- Haddock comments. The other entry points ('parseDeclaration',
-- 'parseExpression', etc) do not insert them into the AST.
parseSignature :: P (Located (HsModule GhcPs))
parseSignature = parseSignatureNoHaddock >>= addHaddockToModule

commentsA :: (NoAnn ann) => SrcSpan -> EpAnnComments -> EpAnn ann
commentsA loc cs = EpAnn (EpaSpan loc) noAnn cs

spanWithComments :: (NoAnn ann, MonadP m) => SrcSpan -> m (EpAnn ann)
spanWithComments l = do
  !cs <- getCommentsFor l
  return (commentsA l cs)

-- | Instead of getting the *enclosed* comments, this includes the
-- *preceding* ones.  It is used at the top level to get comments
-- between top level declarations.
commentsPA :: (NoAnn ann) => LocatedAn ann a -> P (LocatedAn ann a)
commentsPA la@(L l a) = do
  !cs <- getPriorCommentsFor (getLocA la)
  return (L (addCommentsToEpAnn l cs) a)

hsDoAnn :: Located a -> LocatedAn t b -> AnnKeywordId -> AnnList
hsDoAnn (L l _) (L ll _) kw
  = AnnList (Just $ spanAsAnchor (locA ll)) Nothing Nothing [AddEpAnn kw (srcSpan2e l)] []

listAsAnchor :: [LocatedAn t a] -> Located b -> Anchor
listAsAnchor [] (L l _) = spanAsAnchor l
listAsAnchor (h:_) s = spanAsAnchor (comb2 h s)

listAsAnchorM :: [LocatedAn t a] -> Maybe Anchor
listAsAnchorM [] = Nothing
listAsAnchorM (L l _:_) =
  case locA l of
    RealSrcSpan ll _ -> Just $ realSpanAsAnchor ll
    _                -> Nothing

epTok :: Located Token -> EpToken tok
epTok (L !l _) = EpTok (EpaSpan l)

epUniTok :: Located Token -> EpUniToken tok utok
epUniTok t@(L !l _) = EpUniTok (EpaSpan l) u
  where
    u = if isUnicode t then UnicodeSyntax else NormalSyntax

epExplicitBraces :: Located Token -> Located Token -> EpLayout
epExplicitBraces !t1 !t2 = EpExplicitBraces (epTok t1) (epTok t2)

-- -------------------------------------

addTrailingCommaFBind :: MonadP m => Fbind b -> SrcSpan -> m (Fbind b)
addTrailingCommaFBind (Left b)  l = fmap Left  (addTrailingCommaA b l)
addTrailingCommaFBind (Right b) l = fmap Right (addTrailingCommaA b l)

addTrailingVbarA :: MonadP m => LocatedA a -> SrcSpan -> m (LocatedA a)
addTrailingVbarA  la span = addTrailingAnnA la span AddVbarAnn

addTrailingSemiA :: MonadP m => LocatedA a -> SrcSpan -> m (LocatedA a)
addTrailingSemiA  la span = addTrailingAnnA la span AddSemiAnn

addTrailingCommaA :: MonadP m => LocatedA a -> SrcSpan -> m (LocatedA a)
addTrailingCommaA  la span = addTrailingAnnA la span AddCommaAnn

addTrailingAnnA :: MonadP m => LocatedA a -> SrcSpan -> (EpaLocation -> TrailingAnn) -> m (LocatedA a)
addTrailingAnnA (L anns a) ss ta = do
  let cs = emptyComments
  -- AZ:TODO: generalise updating comments into an annotation
  let
    anns' = if isZeroWidthSpan ss
              then anns
              else addTrailingAnnToA (ta (srcSpan2e ss)) cs anns
  return (L anns' a)

-- -------------------------------------

addTrailingVbarL :: MonadP m => LocatedL a -> SrcSpan -> m (LocatedL a)
addTrailingVbarL  la span = addTrailingAnnL la (AddVbarAnn (srcSpan2e span))

addTrailingCommaL :: MonadP m => LocatedL a -> SrcSpan -> m (LocatedL a)
addTrailingCommaL  la span = addTrailingAnnL la (AddCommaAnn (srcSpan2e span))

addTrailingAnnL :: MonadP m => LocatedL a -> TrailingAnn -> m (LocatedL a)
addTrailingAnnL (L anns a) ta = do
  !cs <- getCommentsFor (locA anns)
  let anns' = addTrailingAnnToL ta cs anns
  return (L anns' a)

-- -------------------------------------

-- Mostly use to add AnnComma, special case it to NOP if adding a zero-width annotation
addTrailingCommaN :: MonadP m => LocatedN a -> SrcSpan -> m (LocatedN a)
addTrailingCommaN (L anns a) span = do
  let cs = emptyComments
  -- AZ:TODO: generalise updating comments into an annotation
  let anns' = if isZeroWidthSpan span
                then anns
                else addTrailingCommaToN anns (srcSpan2e span)
  return (L anns' a)

addTrailingCommaS :: Located StringLiteral -> EpaLocation -> Located StringLiteral
addTrailingCommaS (L l sl) span
    = L (widenSpan l [AddEpAnn AnnComma span]) (sl { sl_tc = Just (epaToNoCommentsLocation span) })

-- -------------------------------------

addTrailingDarrowC :: LocatedC a -> Located Token -> EpAnnComments -> LocatedC a
addTrailingDarrowC (L (EpAnn lr (AnnContext _ o c) csc) a) lt cs =
  let
    u = if (isUnicode lt) then UnicodeSyntax else NormalSyntax
  in L (EpAnn lr (AnnContext (Just (u,glAA lt)) o c) (cs Semi.<> csc)) a

-- -------------------------------------

-- We need a location for the where binds, when computing the SrcSpan
-- for the AST element using them.  Where there is a span, we return
-- it, else noLoc, which is ignored in the comb2 call.
adaptWhereBinds :: Maybe (Located (HsLocalBinds GhcPs, Maybe EpAnnComments))
                ->        Located (HsLocalBinds GhcPs,       EpAnnComments)
adaptWhereBinds Nothing = noLoc (EmptyLocalBinds noExtField, emptyComments)
adaptWhereBinds (Just (L l (b, mc))) = L l (b, maybe emptyComments id mc)

combineHasLocs :: (HasLoc a, HasLoc b) => a -> b -> SrcSpan
combineHasLocs a b = combineSrcSpans (getHasLoc a) (getHasLoc b)

fromTrailingN :: SrcSpanAnnN -> SrcSpanAnnA
fromTrailingN (EpAnn anc ann cs)
    = EpAnn anc (AnnListItem (nann_trailing ann)) cs
}
