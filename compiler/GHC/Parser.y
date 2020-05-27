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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Prelude -- for happy-generated code

import GHC.Prelude

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
import GHC.Utils.Misc          ( looksLikePackageName, fstOf3, sndOf3, thdOf3 )

import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence ( varName, dataName, tcClsName, tvName, occNameFS )
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.SourceFile
import GHC.Types.SourceText

import GHC.Core.Type    ( unrestrictedFunTyCon, Specificity(..) )
import GHC.Core.Class   ( FunDep )
import GHC.Core.DataCon ( DataCon, dataConName )

import GHC.Parser.PostProcess
import GHC.Parser.PostProcess.Haddock
import GHC.Parser.Lexer
import GHC.Parser.Annotation
import GHC.Parser.Errors

import GHC.Builtin.Types ( unitTyCon, unitDataCon, tupleTyCon, tupleDataCon, nilDataCon,
                           unboxedUnitTyCon, unboxedUnitDataCon,
                           listTyCon_RDR, consDataCon_RDR, eqTyCon_RDR)
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

{- Note [%shift: rule_foralls -> 'forall' rule_vars '.']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Context:
    rule_foralls -> 'forall' rule_vars '.' . 'forall' rule_vars '.'
    rule_foralls -> 'forall' rule_vars '.' .

Example:
    {-# RULES "name" forall a1. forall a2. lhs = rhs #-}

Ambiguity:
    Same as in Note [%shift: rule_foralls -> {- empty -}]
    but for the second 'forall'.
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
    If we reduced, (:%) would be parsed as a parenthehsized infix type
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


{- Note [Parser API Annotations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A lot of the productions are now cluttered with calls to
aa,am,ams,amms etc.

These are helper functions to make sure that the locations of the
various keywords such as do / let / in are captured for use by tools
that want to do source to source conversions, such as refactorers or
structured editors.

The helper functions are defined at the bottom of this file.

See
  https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations and
  https://gitlab.haskell.org/ghc/ghc/wikis/ghc-ast-annotations
for some background.

If you modify the parser and want to ensure that the API annotations are processed
correctly, see the README in (REPO)/utils/check-api-annotations for details on
how to set up a test using the check-api-annotations utility, and interpret the
output it generates.
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
%name parseSignature signature
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
identifier :: { Located RdrName }
        : qvar                          { $1 }
        | qcon                          { $1 }
        | qvarop                        { $1 }
        | qconop                        { $1 }
    | '(' '->' ')'      {% ams (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                               [mop $1,mu AnnRarrow $2,mcp $3] }
    | '->'              {% ams (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                               [mu AnnRarrow $1] }

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

-- Parse a minus sign regardless of whether -XLexicalNegation is turned on or off.
-- See Note [Minus tokens] in GHC.Parser.Lexer
HYPHEN :: { [AddAnn] }
      : '-'          { [mj AnnMinus $1 ] }
      | PREFIX_MINUS { [mj AnnMinus $1 ] }
      | VARSYM  {% if (getVARSYM $1 == fsLit "-")
                   then return [mj AnnMinus $1]
                   else do { addError $ PsError PsErrExpectedHyphen [] (getLoc $1)
                           ; return [] } }


litpkgname :: { Located FastString }
        : litpkgname_segment { $1 }
        -- a bit of a hack, means p - b is parsed same as p-b, enough for now.
        | litpkgname_segment HYPHEN litpkgname  { sLL $1 $> $ appendFS (unLoc $1) (consFS '-' (unLoc $3)) }

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
        : 'module' maybe_src modid maybemodwarning maybeexports 'where' body
             -- XXX not accurate
             { sL1 $1 $ DeclD
                 (case snd $2 of
                   NotBoot -> HsSrcFile
                   IsBoot  -> HsBootFile)
                 $3
                 (Just $ sL1 $1 (HsModule (thdOf3 $7) (Just $3) $5 (fst $ sndOf3 $7) (snd $ sndOf3 $7) $4 Nothing)) }
        | 'signature' modid maybemodwarning maybeexports 'where' body
             { sL1 $1 $ DeclD
                 HsigFile
                 $2
                 (Just $ sL1 $1 (HsModule (thdOf3 $6) (Just $2) $4 (fst $ sndOf3 $6) (snd $ sndOf3 $6) $3 Nothing)) }
        | 'module' maybe_src modid
             { sL1 $1 $ DeclD (case snd $2 of
                   NotBoot -> HsSrcFile
                   IsBoot  -> HsBootFile) $3 Nothing }
        | 'signature' modid
             { sL1 $1 $ DeclD HsigFile $2 Nothing }
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

signature :: { Located HsModule }
       : 'signature' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (L loc (HsModule (thdOf3 $6) (Just $2) $4 (fst $ sndOf3 $6)
                              (snd $ sndOf3 $6) $3 Nothing)
                    )
                    ([mj AnnSignature $1, mj AnnWhere $5] ++ fstOf3 $6) }

module :: { Located HsModule }
       : 'module' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (L loc (HsModule (thdOf3 $6) (Just $2) $4 (fst $ sndOf3 $6)
                              (snd $ sndOf3 $6) $3 Nothing)
                    )
                    ([mj AnnModule $1, mj AnnWhere $5] ++ fstOf3 $6) }
        | body2
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule (thdOf3 $1) Nothing Nothing
                               (fst $ sndOf3 $1) (snd $ sndOf3 $1) Nothing Nothing))
                       (fstOf3 $1) }

missing_module_keyword :: { () }
        : {- empty -}                           {% pushModuleContext }

implicit_top :: { () }
        : {- empty -}                           {% pushModuleContext }

maybemodwarning :: { Maybe (Located WarningTxt) }
    : '{-# DEPRECATED' strings '#-}'
                      {% ajs (sLL $1 $> $ DeprecatedTxt (sL1 $1 (getDEPRECATED_PRAGs $1)) (snd $ unLoc $2))
                             (mo $1:mc $3: (fst $ unLoc $2)) }
    | '{-# WARNING' strings '#-}'
                         {% ajs (sLL $1 $> $ WarningTxt (sL1 $1 (getWARNING_PRAGs $1)) (snd $ unLoc $2))
                                (mo $1:mc $3 : (fst $ unLoc $2)) }
    |  {- empty -}                  { Nothing }

body    :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])
             ,LayoutInfo) }
        :  '{'            top '}'      { (moc $1:mcc $3:(fst $2)
                                         , snd $2, ExplicitBraces) }
        |      vocurly    top close    { (fst $2, snd $2, VirtualBraces (getVOCURLY $1)) }

body2   :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])
             ,LayoutInfo) }
        :  '{' top '}'                          { (moc $1:mcc $3
                                                   :(fst $2), snd $2, ExplicitBraces) }
        |  missing_module_keyword top close     { ([],snd $2, VirtualBraces leftmostColumn) }


top     :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        : semis top1                            { ($1, $2) }

top1    :: { ([LImportDecl GhcPs], [LHsDecl GhcPs]) }
        : importdecls_semi topdecls_semi        { (reverse $1, cvTopDecls $2) }
        | importdecls_semi topdecls             { (reverse $1, cvTopDecls $2) }
        | importdecls                           { (reverse $1, []) }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header  :: { Located HsModule }
        : 'module' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule NoLayoutInfo (Just $2) $4 $6 [] $3 Nothing
                          )) [mj AnnModule $1,mj AnnWhere $5] }
        | 'signature' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule NoLayoutInfo (Just $2) $4 $6 [] $3 Nothing
                          )) [mj AnnModule $1,mj AnnWhere $5] }
        | header_body2
                {% fileSrcSpan >>= \ loc ->
                   return (L loc (HsModule NoLayoutInfo Nothing Nothing $1 [] Nothing
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
        :  '(' exportlist ')'       {% amsL (comb2 $1 $>) ([mop $1,mcp $3] ++ (fst $2)) >>
                                       return (Just (sLL $1 $> (fromOL $ snd $2))) }
        |  {- empty -}              { Nothing }

exportlist :: { ([AddAnn], OrdList (LIE GhcPs)) }
        : exportlist1     { ([], $1) }
        | {- empty -}     { ([], nilOL) }

        -- trailing comma:
        | exportlist1 ',' { ([mj AnnComma $2], $1) }
        | ','             { ([mj AnnComma $1], nilOL) }

exportlist1 :: { OrdList (LIE GhcPs) }
        : exportlist1 ',' export
                          {% (addAnnotation (oll $1) AnnComma (gl $2) ) >>
                              return ($1 `appOL` $3) }
        | export          { $1 }


   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export  :: { OrdList (LIE GhcPs) }
        : qcname_ext export_subspec  {% mkModuleImpExp $1 (snd $ unLoc $2)
                                          >>= \ie -> amsu (sLL $1 $> ie) (fst $ unLoc $2) }
        |  'module' modid            {% amsu (sLL $1 $> (IEModuleContents noExtField $2))
                                             [mj AnnModule $1] }
        |  'pattern' qcon            {% amsu (sLL $1 $> (IEVar noExtField (sLL $1 $> (IEPattern $2))))
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
                                                    l@(L _ ImpExpQcWildcard) ->
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
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid optqualified maybeas maybeimpspec
                {% do {
                  ; let { ; mPreQual = unLoc $4
                          ; mPostQual = unLoc $7 }
                  ; checkImportDecl mPreQual mPostQual
                  ; ams (L (comb5 $1 $6 $7 (snd $8) $9) $
                      ImportDecl { ideclExt = noExtField
                                  , ideclSourceSrc = snd $ fst $2
                                  , ideclName = $6, ideclPkgQual = snd $5
                                  , ideclSource = snd $2, ideclSafe = snd $3
                                  , ideclQualified = importDeclQualifiedStyle mPreQual mPostQual
                                  , ideclImplicit = False
                                  , ideclAs = unLoc (snd $8)
                                  , ideclHiding = unLoc $9 })
                         (mj AnnImport $1 : fst (fst $2) ++ fst $3 ++ fmap (mj AnnQualified) (maybeToList mPreQual)
                                          ++ fst $5 ++ fmap (mj AnnQualified) (maybeToList mPostQual) ++ fst $8)
                  }
                }


maybe_src :: { (([AddAnn],SourceText),IsBootInterface) }
        : '{-# SOURCE' '#-}'        { (([mo $1,mc $2],getSOURCE_PRAGs $1)
                                      , IsBoot) }
        | {- empty -}               { (([],NoSourceText),NotBoot) }

maybe_safe :: { ([AddAnn],Bool) }
        : 'safe'                                { ([mj AnnSafe $1],True) }
        | {- empty -}                           { ([],False) }

maybe_pkg :: { ([AddAnn],Maybe StringLiteral) }
        : STRING  {% do { let { pkgFS = getSTRING $1 }
                        ; unless (looksLikePackageName (unpackFS pkgFS)) $
                             addError $ PsError (PsErrInvalidPackageName pkgFS) [] (getLoc $1)
                        ; return ([mj AnnPackageName $1], Just (StringLiteral (getSTRINGs $1) pkgFS)) } }
        | {- empty -}                           { ([],Nothing) }

optqualified :: { Located (Maybe (Located Token)) }
        : 'qualified'                           { sL1 $1 (Just $1) }
        | {- empty -}                           { noLoc Nothing }

maybeas :: { ([AddAnn],Located (Maybe (Located ModuleName))) }
        : 'as' modid                           { ([mj AnnAs $1]
                                                 ,sLL $1 $> (Just $2)) }
        | {- empty -}                          { ([],noLoc Nothing) }

maybeimpspec :: { Located (Maybe (Bool, Located [LIE GhcPs])) }
        : impspec                  {% let (b, ie) = unLoc $1 in
                                       checkImportSpec ie
                                        >>= \checkedIe ->
                                          return (L (gl $1) (Just (b, checkedIe)))  }
        | {- empty -}              { noLoc Nothing }

impspec :: { Located (Bool, Located [LIE GhcPs]) }
        :  '(' exportlist ')'               {% ams (sLL $1 $> (False,
                                                      sLL $1 $> $ fromOL (snd $2)))
                                                   ([mop $1,mcp $3] ++ (fst $2)) }
        |  'hiding' '(' exportlist ')'      {% ams (sLL $1 $> (True,
                                                      sLL $1 $> $ fromOL (snd $3)))
                                               ([mj AnnHiding $1,mop $2,mcp $4] ++ (fst $3)) }

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
        : cl_decl                               { sL1 $1 (TyClD noExtField (unLoc $1)) }
        | ty_decl                               { sL1 $1 (TyClD noExtField (unLoc $1)) }
        | standalone_kind_sig                   { sL1 $1 (KindSigD noExtField (unLoc $1)) }
        | inst_decl                             { sL1 $1 (InstD noExtField (unLoc $1)) }
        | stand_alone_deriving                  { sLL $1 $> (DerivD noExtField (unLoc $1)) }
        | role_annot                            { sL1 $1 (RoleAnnotD noExtField (unLoc $1)) }
        | 'default' '(' comma_types0 ')'    {% ams (sLL $1 $> (DefD noExtField (DefaultDecl noExtField $3)))
                                                         [mj AnnDefault $1
                                                         ,mop $2,mcp $4] }
        | 'foreign' fdecl          {% ams (sLL $1 $> (snd $ unLoc $2))
                                           (mj AnnForeign $1:(fst $ unLoc $2)) }
        | '{-# DEPRECATED' deprecations '#-}'   {% ams (sLL $1 $> $ WarningD noExtField (Warnings noExtField (getDEPRECATED_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# WARNING' warnings '#-}'          {% ams (sLL $1 $> $ WarningD noExtField (Warnings noExtField (getWARNING_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# RULES' rules '#-}'               {% ams (sLL $1 $> $ RuleD noExtField (HsRules noExtField (getRULES_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | annotation { $1 }
        | decl_no_th                            { $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if
        -- it had a $(..) wrapped around it
        | infixexp                              {% runPV (unECP $1) >>= \ $1 ->
                                                   return $ sLL $1 $> $ mkSpliceDecl $1 }

-- Type classes
--
cl_decl :: { LTyClDecl GhcPs }
        : 'class' tycl_hdr fds where_cls
                {% amms (mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 (sndOf3 $ unLoc $4) (thdOf3 $ unLoc $4))
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

-- standalone kind signature
standalone_kind_sig :: { LStandaloneKindSig GhcPs }
  : 'type' sks_vars '::' sigktype
      {% amms (mkStandaloneKindSig (comb2 $1 $4) $2 $4)
              [mj AnnType $1,mu AnnDcolon $3] }

-- See also: sig_vars
sks_vars :: { Located [Located RdrName] }  -- Returned in reverse order
  : sks_vars ',' oqtycon
      {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
         return (sLL $1 $> ($3 : unLoc $1)) }
  | oqtycon { sL1 $1 [$1] }

inst_decl :: { LInstDecl GhcPs }
        : 'instance' overlap_pragma inst_type where_inst
       {% do { (binds, sigs, _, ats, adts, _) <- cvBindsAndSigs (snd $ unLoc $4)
             ; let cid = ClsInstDecl { cid_ext = noExtField
                                     , cid_poly_ty = $3, cid_binds = binds
                                     , cid_sigs = mkClassOpSigs sigs
                                     , cid_tyfam_insts = ats
                                     , cid_overlap_mode = $2
                                     , cid_datafam_insts = adts }
             ; ams (L (comb3 $1 $3 $4) (ClsInstD { cid_d_ext = noExtField, cid_inst = cid }))
                   (mj AnnInstance $1 : (fst $ unLoc $4)) } }

           -- type instance declarations
        | 'type' 'instance' ty_fam_inst_eqn
                {% ams $3 (fst $ unLoc $3)
                >> amms (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3))
                    (mj AnnType $1:mj AnnInstance $2:(fst $ unLoc $3)) }

          -- data/newtype instance declaration
        | data_or_newtype 'instance' capi_ctype datafam_inst_hdr constrs
                          maybe_derivings
            {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 (snd $ unLoc $4)
                                      Nothing (reverse (snd  $ unLoc $5))
                                              (fmap reverse $6))
                    ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $4)++(fst $ unLoc $5)) }

          -- GADT instance declaration
        | data_or_newtype 'instance' capi_ctype datafam_inst_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3 (snd $ unLoc $4)
                                   (snd $ unLoc $5) (snd $ unLoc $6)
                                   (fmap reverse $7))
                    ((fst $ unLoc $1):mj AnnInstance $2
                       :(fst $ unLoc $4)++(fst $ unLoc $5)++(fst $ unLoc $6)) }

overlap_pragma :: { Maybe (Located OverlapMode) }
  : '{-# OVERLAPPABLE'    '#-}' {% ajs (sLL $1 $> (Overlappable (getOVERLAPPABLE_PRAGs $1)))
                                       [mo $1,mc $2] }
  | '{-# OVERLAPPING'     '#-}' {% ajs (sLL $1 $> (Overlapping (getOVERLAPPING_PRAGs $1)))
                                       [mo $1,mc $2] }
  | '{-# OVERLAPS'        '#-}' {% ajs (sLL $1 $> (Overlaps (getOVERLAPS_PRAGs $1)))
                                       [mo $1,mc $2] }
  | '{-# INCOHERENT'      '#-}' {% ajs (sLL $1 $> (Incoherent (getINCOHERENT_PRAGs $1)))
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
  : 'via' sigktype          {% ams (sLL $1 $> (ViaStrategy $2))
                                       [mj AnnVia $1] }

deriv_standalone_strategy :: { Maybe (LDerivStrategy GhcPs) }
  : 'stock'                     {% ajs (sL1 $1 StockStrategy)
                                       [mj AnnStock $1] }
  | 'anyclass'                  {% ajs (sL1 $1 AnyclassStrategy)
                                       [mj AnnAnyclass $1] }
  | 'newtype'                   {% ajs (sL1 $1 NewtypeStrategy)
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
        | vocurly ty_fam_inst_eqns close   { let (L loc _) = $2 in
                                             L loc ([],Just (unLoc $2)) }
        |     '{' '..' '}'                 { sLL $1 $> ([moc $1,mj AnnDotdot $2
                                                 ,mcc $3],Nothing) }
        | vocurly '..' close               { let (L loc _) = $2 in
                                             L loc ([mj AnnDotdot $2],Nothing) }

ty_fam_inst_eqns :: { Located [LTyFamInstEqn GhcPs] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
                                      {% let (L loc (anns, eqn)) = $3 in
                                         asl (unLoc $1) $2 (L loc eqn)
                                         >> ams $3 anns
                                         >> return (sLL $1 $> (L loc eqn : unLoc $1)) }
        | ty_fam_inst_eqns ';'        {% addAnnotation (gl $1) AnnSemi (gl $2)
                                         >> return (sLL $1 $>  (unLoc $1)) }
        | ty_fam_inst_eqn             {% let (L loc (anns, eqn)) = $1 in
                                         ams $1 anns
                                         >> return (sLL $1 $> [L loc eqn]) }
        | {- empty -}                 { noLoc [] }

ty_fam_inst_eqn :: { Located ([AddAnn],TyFamInstEqn GhcPs) }
        : 'forall' tv_bndrs '.' type '=' ktype
              {% do { hintExplicitForall $1
                    ; tvbs <- fromSpecTyVarBndrs $2
                    ; (eqn,ann) <- mkTyFamInstEqn (mkHsOuterExplicit tvbs) $4 $6
                    ; return (sLL $1 $>
                               (mu AnnForall $1:mj AnnDot $3:mj AnnEqual $5:ann,eqn)) } }
        | type '=' ktype
              {% do { (eqn,ann) <- mkTyFamInstEqn mkHsOuterImplicit $1 $3
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
        | data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
               {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 (snd $ unLoc $4)
                                    Nothing (reverse (snd $ unLoc $5))
                                            (fmap reverse $6))
                       ((fst $ unLoc $1):$2++(fst $ unLoc $4)++(fst $ unLoc $5)) }

        -- GADT instance declaration, with optional 'instance' keyword
        | data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
                {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3
                                (snd $ unLoc $4) (snd $ unLoc $5) (snd $ unLoc $6)
                                (fmap reverse $7))
                        ((fst $ unLoc $1):$2++(fst $ unLoc $4)++(fst $ unLoc $5)++(fst $ unLoc $6)) }

data_or_newtype :: { Located (AddAnn, NewOrData) }
        : 'data'        { sL1 $1 (mj AnnData    $1,DataType) }
        | 'newtype'     { sL1 $1 (mj AnnNewtype $1,NewType) }

-- Family result/return kind signatures

opt_kind_sig :: { Located ([AddAnn], Maybe (LHsKind GhcPs)) }
        :               { noLoc     ([]               , Nothing) }
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], Just $2) }

opt_datafam_kind_sig :: { Located ([AddAnn], LFamilyResultSig GhcPs) }
        :               { noLoc     ([]               , noLoc (NoSig noExtField)         )}
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], sLL $1 $> (KindSig noExtField $2))}

opt_tyfam_kind_sig :: { Located ([AddAnn], LFamilyResultSig GhcPs) }
        :              { noLoc     ([]               , noLoc     (NoSig    noExtField)   )}
        | '::' kind    { sLL $1 $> ([mu AnnDcolon $1], sLL $1 $> (KindSig  noExtField $2))}
        | '='  tv_bndr {% do { tvb <- fromSpecTyVarBndr $2
                             ; return $ sLL $1 $> ([mj AnnEqual $1] , sLL $1 $> (TyVarSig noExtField tvb))} }

opt_at_kind_inj_sig :: { Located ([AddAnn], ( LFamilyResultSig GhcPs
                                            , Maybe (LInjectivityAnn GhcPs)))}
        :            { noLoc ([], (noLoc (NoSig noExtField), Nothing)) }
        | '::' kind  { sLL $1 $> ( [mu AnnDcolon $1]
                                 , (sLL $2 $> (KindSig noExtField $2), Nothing)) }
        | '='  tv_bndr_no_braces '|' injectivity_cond
                {% do { tvb <- fromSpecTyVarBndr $2
                      ; return $ sLL $1 $> ([mj AnnEqual $1, mj AnnVbar $3]
                                           , (sLL $1 $2 (TyVarSig noExtField tvb), Just $4))} }

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

datafam_inst_hdr :: { Located ([AddAnn],(Maybe (LHsContext GhcPs), HsOuterFamEqnTyVarBndrs GhcPs, LHsType GhcPs)) }
        : 'forall' tv_bndrs '.' context '=>' type   {% hintExplicitForall $1
                                                       >> fromSpecTyVarBndrs $2
                                                         >>= \tvbs -> (addAnnotation (gl $4) (toUnicodeAnn AnnDarrow $5) (gl $5)
                                                             >> return (sLL $1 $> ([mu AnnForall $1, mj AnnDot $3]
                                                                                  , (Just $4, mkHsOuterExplicit tvbs, $6)))
                                                          )
                                                    }
        | 'forall' tv_bndrs '.' type   {% do { hintExplicitForall $1
                                             ; tvbs <- fromSpecTyVarBndrs $2
                                             ; return (sLL $1 $> ([mu AnnForall $1, mj AnnDot $3]
                                                                 , (Nothing, mkHsOuterExplicit tvbs, $4)))
                                       } }
        | context '=>' type         {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                       >> (return (sLL $1 $>([], (Just $1, mkHsOuterImplicit, $3))))
                                    }
        | type                      { sL1 $1 ([], (Nothing, mkHsOuterImplicit, $1)) }


capi_ctype :: { Maybe (Located CType) }
capi_ctype : '{-# CTYPE' STRING STRING '#-}'
                       {% ajs (sLL $1 $> (CType (getCTYPEs $1) (Just (Header (getSTRINGs $2) (getSTRING $2)))
                                        (getSTRINGs $3,getSTRING $3)))
                              [mo $1,mj AnnHeader $2,mj AnnVal $3,mc $4] }

           | '{-# CTYPE'        STRING '#-}'
                       {% ajs (sLL $1 $> (CType (getCTYPEs $1) Nothing (getSTRINGs $2, getSTRING $2)))
                              [mo $1,mj AnnVal $2,mc $3] }

           |           { Nothing }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl GhcPs }
  : 'deriving' deriv_standalone_strategy 'instance' overlap_pragma inst_type
                {% do { let { err = text "in the stand-alone deriving instance"
                                    <> colon <+> quotes (ppr $5) }
                      ; ams (sLL $1 $>
                                 (DerivDecl noExtField (mkHsWildCardBndrs $5) $2 $4))
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
                 ams (sLL $1 $> . ValD noExtField $ mkPatSynBind name args $4
                                                    ImplicitBidirectional)
               (as ++ [mj AnnPattern $1, mj AnnEqual $3])
         }

        | 'pattern' pattern_synonym_lhs '<-' pat
         {%    let (name, args, as) = $2 in
               ams (sLL $1 $> . ValD noExtField $ mkPatSynBind name args $4 Unidirectional)
               (as ++ [mj AnnPattern $1,mu AnnLarrow $3]) }

        | 'pattern' pattern_synonym_lhs '<-' pat where_decls
            {% do { let (name, args, as) = $2
                  ; mg <- mkPatSynMatchGroup name (snd $ unLoc $5)
                  ; ams (sLL $1 $> . ValD noExtField $
                           mkPatSynBind name args $4 (ExplicitBidirectional mg))
                       (as ++ ((mj AnnPattern $1:mu AnnLarrow $3:(fst $ unLoc $5))) )
                   }}

pattern_synonym_lhs :: { (Located RdrName, HsPatSynDetails GhcPs, [AddAnn]) }
        : con vars0 { ($1, PrefixCon noTypeArgs $2, []) }
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
        | 'where' vocurly decls close { L (comb2 $1 $3) ((mj AnnWhere $1:(fst $ unLoc $3))
                                          ,sL1 $3 (snd $ unLoc $3)) }

pattern_synonym_sig :: { LSig GhcPs }
        : 'pattern' con_list '::' sigtype
                   {% ams (sLL $1 $> $ PatSynSig noExtField (unLoc $2) $4)
                          [mj AnnPattern $1, mu AnnDcolon $3] }

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
                          ; ams (sLL $1 $> $ SigD noExtField $ ClassOpSig noExtField True [v] $4)
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
                     , OrdList (LHsDecl GhcPs)
                     , LayoutInfo) }      -- Reversed
        : '{'         decls_cls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                             ,snd $ unLoc $2, ExplicitBraces) }
        |     vocurly decls_cls close   { let { L l (anns, decls) = $2 }
                                           in L l (anns, decls, VirtualBraces (getVOCURLY $1)) }

-- Class body
--
where_cls :: { Located ([AddAnn]
                       ,(OrdList (LHsDecl GhcPs))    -- Reversed
                       ,LayoutInfo) }
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_cls          { sLL $1 $> (mj AnnWhere $1:(fstOf3 $ unLoc $2)
                                             ,sndOf3 $ unLoc $2,thdOf3 $ unLoc $2) }
        | {- empty -}                   { noLoc ([],nilOL,NoLayoutInfo) }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl GhcPs)) }
decl_inst  : at_decl_inst               { sLL $1 $> (unitOL (sL1 $1 (InstD noExtField (unLoc $1)))) }
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
        |     vocurly decls_inst close  { L (gl $2) (unLoc $2) }

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
        |     vocurly    decls close   { L (gl $2) (fst $ unLoc $2,sL1 $2 $ snd $ unLoc $2) }

-- Binding groups other than those of class and instance declarations
--
binds   ::  { Located ([AddAnn],Located (HsLocalBinds GhcPs)) }
                                         -- May have implicit parameters
                                                -- No type declarations
        : decllist          {% do { val_binds <- cvBindGroup (unLoc $ snd $ unLoc $1)
                                  ; return (sL1 $1 (fst $ unLoc $1
                                                    ,sL1 $1 $ HsValBinds noExtField val_binds)) } }

        | '{'            dbinds '}'     { sLL $1 $> ([moc $1,mcc $3]
                                             ,sL1 $2 $ HsIPBinds noExtField (IPBinds noExtField (reverse $ unLoc $2))) }

        |     vocurly    dbinds close   { L (getLoc $2) ([]
                                            ,sL1 $2 $ HsIPBinds noExtField (IPBinds noExtField (reverse $ unLoc $2))) }


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
         {%runPV (unECP $4) >>= \ $4 ->
           runPV (unECP $6) >>= \ $6 ->
           ams (sLL $1 $> $ HsRule { rd_ext = noExtField
                                   , rd_name = L (gl $1) (getSTRINGs $1, getSTRING $1)
                                   , rd_act = (snd $2) `orElse` AlwaysActive
                                   , rd_tyvs = sndOf3 $3, rd_tmvs = thdOf3 $3
                                   , rd_lhs = $4, rd_rhs = $6 })
               (mj AnnEqual $5 : (fst $2) ++ (fstOf3 $3)) }

-- Rules can be specified to be NeverActive, unlike inline/specialize pragmas
rule_activation :: { ([AddAnn],Maybe Activation) }
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
rule_activation_marker :: { [AddAnn] }
      : PREFIX_TILDE { [mj AnnTilde $1] }
      | VARSYM  {% if (getVARSYM $1 == fsLit "~")
                   then return [mj AnnTilde $1]
                   else do { addError $ PsError PsErrInvalidRuleActivationMarker [] (getLoc $1)
                           ; return [] } }

rule_explicit_activation :: { ([AddAnn]
                              ,Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mos $1,mj AnnVal $2,mcs $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' rule_activation_marker INTEGER ']'
                                { ($2++[mos $1,mj AnnVal $3,mcs $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }
        | '[' rule_activation_marker ']'
                                { ($2++[mos $1,mcs $3]
                                  ,NeverActive) }

rule_foralls :: { ([AddAnn], Maybe [LHsTyVarBndr () GhcPs], [LRuleBndr GhcPs]) }
        : 'forall' rule_vars '.' 'forall' rule_vars '.'    {% let tyvs = mkRuleTyVarBndrs $2
                                                              in hintExplicitForall $1
                                                              >> checkRuleTyVarBndrNames (mkRuleTyVarBndrs $2)
                                                              >> return ([mu AnnForall $1,mj AnnDot $3,
                                                                          mu AnnForall $4,mj AnnDot $6],
                                                                         Just (mkRuleTyVarBndrs $2), mkRuleBndrs $5) }

        -- See Note [%shift: rule_foralls -> 'forall' rule_vars '.']
        | 'forall' rule_vars '.' %shift                    { ([mu AnnForall $1,mj AnnDot $3],
                                                              Nothing, mkRuleBndrs $2) }
        -- See Note [%shift: rule_foralls -> {- empty -}]
        | {- empty -}            %shift                    { ([], Nothing, []) }

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
"family", or "role" in the function 'checkRuleTyVarBndrNames' in
GHC.Parser.PostProcess.
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
                {% amsu (sLL $1 $> (Warning noExtField (unLoc $1) (WarningTxt (noLoc NoSourceText) $ snd $ unLoc $2)))
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
             {% amsu (sLL $1 $> $ (Warning noExtField (unLoc $1) (DeprecatedTxt (noLoc NoSourceText) $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

strings :: { Located ([AddAnn],[Located StringLiteral]) }
    : STRING { sL1 $1 ([],[L (gl $1) (getStringLiteral $1)]) }
    | '[' stringlist ']' { sLL $1 $> $ ([mos $1,mcs $3],fromOL (unLoc $2)) }

stringlist :: { Located (OrdList (Located StringLiteral)) }
    : stringlist ',' STRING {% addAnnotation (oll $ unLoc $1) AnnComma (gl $2) >>
                               return (sLL $1 $> (unLoc $1 `snocOL`
                                                  (L (gl $3) (getStringLiteral $3)))) }
    | STRING                { sLL $1 $> (unitOL (L (gl $1) (getStringLiteral $1))) }
    | {- empty -}           { noLoc nilOL }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl GhcPs }
    : '{-# ANN' name_var aexp '#-}'      {% runPV (unECP $3) >>= \ $3 ->
                                            ams (sLL $1 $> (AnnD noExtField $ HsAnnotation noExtField
                                            (getANN_PRAGs $1)
                                            (ValueAnnProvenance $2) $3))
                                            [mo $1,mc $4] }

    | '{-# ANN' 'type' tycon aexp '#-}'  {% runPV (unECP $4) >>= \ $4 ->
                                            ams (sLL $1 $> (AnnD noExtField $ HsAnnotation noExtField
                                            (getANN_PRAGs $1)
                                            (TypeAnnProvenance $3) $4))
                                            [mo $1,mj AnnType $2,mc $5] }

    | '{-# ANN' 'module' aexp '#-}'      {% runPV (unECP $3) >>= \ $3 ->
                                            ams (sLL $1 $> (AnnD noExtField $ HsAnnotation noExtField
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
       : STRING var '::' sigtype        { sLL $1 $> ([mu AnnDcolon $3]
                                             ,(L (getLoc $1)
                                                    (getStringLiteral $1), $2, $4)) }
       |        var '::' sigtype        { sLL $1 $> ([mu AnnDcolon $2]
                                             ,(noLoc (StringLiteral NoSourceText nilFS), $1, $3)) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { ([AddAnn], Maybe (LHsType GhcPs)) }
        : {- empty -}                   { ([],Nothing) }
        | '::' ctype                    { ([mu AnnDcolon $1],Just $2) }

opt_tyconsig :: { ([AddAnn], Maybe (Located RdrName)) }
             : {- empty -}              { ([], Nothing) }
             | '::' gtycon              { ([mu AnnDcolon $1], Just $2) }

-- Like ktype, but for types that obey the forall-or-nothing rule.
-- See Note [forall-or-nothing rule] in GHC.Hs.Type.
sigktype :: { LHsSigType GhcPs }
        : sigtype              { $1 }
        | ctype '::' kind      {% ams (sLL $1 $> $ mkHsImplicitSigType $
                                       sLL $1 $> $ HsKindSig noExtField $1 $3)
                                      [mu AnnDcolon $2] }

-- Like ctype, but for types that obey the forall-or-nothing rule.
-- See Note [forall-or-nothing rule] in GHC.Hs.Type. To avoid duplicating the
-- logic in ctype here, we simply reuse the ctype production and perform
-- surgery on the LHsType it returns to turn it into an LHsSigType.
sigtype :: { LHsSigType GhcPs }
        : ctype                            { hsTypeToHsSigType $1 }

sig_vars :: { Located [Located RdrName] }    -- Returned in reversed order
         : sig_vars ',' var           {% addAnnotation (gl $ head $ unLoc $1)
                                                       AnnComma (gl $2)
                                         >> return (sLL $1 $> ($3 : unLoc $1)) }
         | var                        { sL1 $1 [$1] }

sigtypes1 :: { (OrdList (LHsSigType GhcPs)) }
   : sigtype                 { unitOL $1 }
   | sigtype ',' sigtypes1   {% addAnnotation (gl $1) AnnComma (gl $2)
                                >> return (unitOL $1 `appOL` $3) }

-----------------------------------------------------------------------------
-- Types

unpackedness :: { Located UnpackednessPragma }
        : '{-# UNPACK' '#-}'   { sLL $1 $> (UnpackednessPragma [mo $1, mc $2] (getUNPACK_PRAGs $1) SrcUnpack) }
        | '{-# NOUNPACK' '#-}' { sLL $1 $> (UnpackednessPragma [mo $1, mc $2] (getNOUNPACK_PRAGs $1) SrcNoUnpack) }

forall_telescope :: { Located ([AddAnn], HsForAllTelescope GhcPs) }
        : 'forall' tv_bndrs '.'  {% do { hintExplicitForall $1
                                       ; pure $ sLL $1 $>
                                           ( [mu AnnForall $1, mu AnnDot $3]
                                           , mkHsForAllInvisTele $2 ) }}
        | 'forall' tv_bndrs '->' {% do { hintExplicitForall $1
                                       ; req_tvbs <- fromSpecTyVarBndrs $2
                                       ; pure $ sLL $1 $> $
                                           ( [mu AnnForall $1, mu AnnRarrow $3]
                                           , mkHsForAllVisTele req_tvbs ) }}

-- A ktype is a ctype, possibly with a kind annotation
ktype :: { LHsType GhcPs }
        : ctype                { $1 }
        | ctype '::' kind      {% ams (sLL $1 $> $ HsKindSig noExtField $1 $3)
                                      [mu AnnDcolon $2] }
-- A ctype is a for-all type
ctype   :: { LHsType GhcPs }
        : forall_telescope ctype      {% let (forall_anns, forall_tele) = unLoc $1 in
                                         ams (sLL $1 $> $
                                              HsForAllTy { hst_tele = forall_tele
                                                         , hst_xforall = noExtField
                                                         , hst_body = $2 })
                                             forall_anns }
        | context '=>' ctype          {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                         >> return (sLL $1 $> $
                                            HsQualTy { hst_ctxt = $1
                                                     , hst_xqual = noExtField
                                                     , hst_body = $3 }) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy noExtField $1 $3))
                                             [mu AnnDcolon $2] }
        | type                        { $1 }

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
        | btype '->' ctype             {% ams $1 [mu AnnRarrow $2] -- See Note [GADT decl discards annotations]
                                       >> ams (sLL $1 $> $ HsFunTy noExtField (HsUnrestrictedArrow (toUnicode $2)) $1 $3)
                                              [mu AnnRarrow $2] }

        | btype mult '->' ctype        {% hintLinear (getLoc $2)
                                       >> let (arr, ann) = (unLoc $2) (toUnicode $3)
                                          in (ams $1 [ann,mu AnnRarrow $3] -- See Note [GADT decl discards annotations]
                                             >> ams (sLL $1 $> $ HsFunTy noExtField arr $1 $4)
                                                  [ann,mu AnnRarrow $3]) }

        | btype '->.' ctype            {% hintLinear (getLoc $2)
                                       >> ams $1 [mu AnnLollyU $2] -- See Note [GADT decl discards annotations]
                                       >> ams (sLL $1 $> $ HsFunTy noExtField (HsLinearArrow UnicodeSyntax) $1 $3)
                                              [mu AnnLollyU $2] }

mult :: { Located (IsUnicodeSyntax -> (HsArrow GhcPs, AddAnn)) }
        : PREFIX_PERCENT atype          { sLL $1 $> (\u -> mkMultTy u $1 $2) }

btype :: { LHsType GhcPs }
        : infixtype                     {% runPV $1 }

infixtype :: { forall b. DisambTD b => PV (Located b) }
        -- See Note [%shift: infixtype -> ftype]
        : ftype %shift                  { $1 }
        | ftype tyop infixtype          { $1 >>= \ $1 ->
                                          $3 >>= \ $3 ->
                                          do { when (looksLikeMult $1 $2 $3) $ hintLinear (getLoc $2)
                                             ; mkHsOpTyPV $1 $2 $3 } }
        | unpackedness infixtype        { $2 >>= \ $2 ->
                                          mkUnpackednessPV $1 $2 }

ftype :: { forall b. DisambTD b => PV (Located b) }
        : atype                         { mkHsAppTyHeadPV $1 }
        | tyop                          { failOpFewArgs $1 }
        | ftype tyarg                   { $1 >>= \ $1 ->
                                          mkHsAppTyPV $1 $2 }
        | ftype PREFIX_AT atype         { $1 >>= \ $1 ->
                                          mkHsAppKindTyPV $1 (getLoc $2) $3 }

tyarg :: { LHsType GhcPs }
        : atype                         { $1 }
        | unpackedness atype            {% addUnpackednessP $1 $2 }

tyop :: { Located RdrName }
        : qtyconop                      { $1 }
        | tyvarop                       { $1 }
        | SIMPLEQUOTE qconop            {% ams (sLL $1 $> (unLoc $2))
                                               [mj AnnSimpleQuote $1,mj AnnVal $2] }
        | SIMPLEQUOTE varop             {% ams (sLL $1 $> (unLoc $2))
                                               [mj AnnSimpleQuote $1,mj AnnVal $2] }

atype :: { LHsType GhcPs }
        : ntgtycon                       { sL1 $1 (HsTyVar noExtField NotPromoted $1) }      -- Not including unit tuples
        -- See Note [%shift: atype -> tyvar]
        | tyvar %shift                   { sL1 $1 (HsTyVar noExtField NotPromoted $1) }      -- (See Note [Unit tuples])
        | '*'                            {% do { warnStarIsType (getLoc $1)
                                               ; return $ sL1 $1 (HsStarTy noExtField (isUnicode $1)) } }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | PREFIX_TILDE atype             {% ams (sLL $1 $> (mkBangTy SrcLazy $2)) [mj AnnTilde $1] }
        | PREFIX_BANG  atype             {% ams (sLL $1 $> (mkBangTy SrcStrict $2)) [mj AnnBang $1] }

        | '{' fielddecls '}'             {% amms (checkRecordSyntax
                                                    (sLL $1 $> $ HsRecTy noExtField $2))
                                                        -- Constructor sigs only
                                                 [moc $1,mcc $3] }
        | '(' ')'                        {% ams (sLL $1 $> $ HsTupleTy noExtField
                                                    HsBoxedOrConstraintTuple [])
                                                [mop $1,mcp $2] }
        | '(' ktype ',' comma_types1 ')' {% addAnnotation (gl $2) AnnComma
                                                          (gl $3) >>
                                            ams (sLL $1 $> $ HsTupleTy noExtField

                                             HsBoxedOrConstraintTuple ($2 : $4))
                                                [mop $1,mcp $5] }
        | '(#' '#)'                   {% ams (sLL $1 $> $ HsTupleTy noExtField HsUnboxedTuple [])
                                             [mo $1,mc $2] }
        | '(#' comma_types1 '#)'      {% ams (sLL $1 $> $ HsTupleTy noExtField HsUnboxedTuple $2)
                                             [mo $1,mc $3] }
        | '(#' bar_types2 '#)'        {% ams (sLL $1 $> $ HsSumTy noExtField $2)
                                             [mo $1,mc $3] }
        | '[' ktype ']'               {% ams (sLL $1 $> $ HsListTy  noExtField $2) [mos $1,mcs $3] }
        | '(' ktype ')'               {% ams (sLL $1 $> $ HsParTy   noExtField $2) [mop $1,mcp $3] }
        | quasiquote                  { mapLoc (HsSpliceTy noExtField) $1 }
        | splice_untyped              { mapLoc (HsSpliceTy noExtField) $1 }
                                      -- see Note [Promotion] for the followings
        | SIMPLEQUOTE qcon_nowiredlist {% ams (sLL $1 $> $ HsTyVar noExtField IsPromoted $2) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  '(' ktype ',' comma_types1 ')'
                             {% addAnnotation (gl $3) AnnComma (gl $4) >>
                                ams (sLL $1 $> $ HsExplicitTupleTy noExtField ($3 : $5))
                                    [mj AnnSimpleQuote $1,mop $2,mcp $6] }
        | SIMPLEQUOTE  '[' comma_types0 ']'     {% ams (sLL $1 $> $ HsExplicitListTy noExtField IsPromoted $3)
                                                       [mj AnnSimpleQuote $1,mos $2,mcs $4] }
        | SIMPLEQUOTE var                       {% ams (sLL $1 $> $ HsTyVar noExtField IsPromoted $2)
                                                       [mj AnnSimpleQuote $1,mj AnnName $2] }

        -- Two or more [ty, ty, ty] must be a promoted list type, just as
        -- if you had written '[ty, ty, ty]
        -- (One means a list type, zero means the list type constructor,
        -- so you have to quote those.)
        | '[' ktype ',' comma_types1 ']'  {% addAnnotation (gl $2) AnnComma
                                                           (gl $3) >>
                                             ams (sLL $1 $> $ HsExplicitListTy noExtField NotPromoted ($2 : $4))
                                                 [mos $1,mcs $5] }
        | INTEGER              { sLL $1 $> $ HsTyLit noExtField $ HsNumTy (getINTEGERs $1)
                                                           (il_value (getINTEGER $1)) }
        | CHAR                 { sLL $1 $> $ HsTyLit noExtField $ HsCharTy (getCHARs $1)
                                                                        (getCHAR $1) }
        | STRING               { sLL $1 $> $ HsTyLit noExtField $ HsStrTy (getSTRINGs $1)
                                                                     (getSTRING  $1) }
        | '_'                  { sL1 $1 $ mkAnonWildCardTy }

-- An inst_type is what occurs in the head of an instance decl
--      e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type for convenience.
inst_type :: { LHsSigType GhcPs }
        : sigtype                       { $1 }

deriv_types :: { [LHsSigType GhcPs] }
        : sigktype                      { [$1] }

        | sigktype ',' deriv_types      {% addAnnotation (gl $1) AnnComma (gl $2)
                                           >> return ($1 : $3) }

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

tv_bndrs :: { [LHsTyVarBndr Specificity GhcPs] }
         : tv_bndr tv_bndrs             { $1 : $2 }
         | {- empty -}                  { [] }

tv_bndr :: { LHsTyVarBndr Specificity GhcPs }
        : tv_bndr_no_braces             { $1 }
        | '{' tyvar '}'                 {% ams (sLL $1 $> (UserTyVar noExtField InferredSpec $2))
                                               [moc $1, mcc $3] }
        | '{' tyvar '::' kind '}'       {% ams (sLL $1 $> (KindedTyVar noExtField InferredSpec $2 $4))
                                               [moc $1,mu AnnDcolon $3
                                               ,mcc $5] }

tv_bndr_no_braces :: { LHsTyVarBndr Specificity GhcPs }
        : tyvar                         { sL1 $1 (UserTyVar noExtField SpecifiedSpec $1) }
        | '(' tyvar '::' kind ')'       {% ams (sLL $1 $> (KindedTyVar noExtField SpecifiedSpec $2 $4))
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
        : varids0 '->' varids0  {% ams (L (comb3 $1 $2 $3)
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
change its namespace to DataName, see Note [Demotion] in GHC.Types.Names.OccName).
And both become a HsTyVar ("Zero", DataName) after the renamer.

-}


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located ([AddAnn]
                          ,[LConDecl GhcPs]) } -- Returned in order

        : 'where' '{'        gadt_constrs '}'    {% checkEmptyGADTs $
                                                      L (comb2 $1 $3)
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
                  {% addAnnotation (gl $1) AnnSemi (gl $2)
                     >> return (L (comb2 $1 $3) ($1 : unLoc $3)) }
        | gadt_constr                   { L (gl $1) [$1] }
        | {- empty -}                   { noLoc [] }

-- We allow the following forms:
--      C :: Eq a => a -> T a
--      C :: forall a. Eq a => !a -> T a
--      D { x,y :: a } :: T a
--      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr :: { LConDecl GhcPs }
    -- see Note [Difference in parsing GADT and data constructors]
    -- Returns a list because of:   C,D :: ty
        : optSemi con_list '::' sigtype
                {% do { (decl, anns) <- mkGadtDecl (unLoc $2) $4
                      ; ams (sLL $2 $> decl)
                            (mu AnnDcolon $3:anns) } }

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

constrs :: { Located ([AddAnn],[LConDecl GhcPs]) }
        : '=' constrs1    { sLL $1 $2 ([mj AnnEqual $1],unLoc $2)}

constrs1 :: { Located [LConDecl GhcPs] }
        : constrs1 '|' constr
            {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $2)
               >> return (sLL $1 $> ($3 : unLoc $1)) }
        | constr                                          { sL1 $1 [$1] }

constr :: { LConDecl GhcPs }
        : forall context '=>' constr_stuff
                {% ams (let (con,details) = unLoc $4 in
                  (L (comb4 $1 $2 $3 $4) (mkConDeclH98 con
                                             (snd $ unLoc $1)
                                             (Just $2)
                                             details)))
                        (mu AnnDarrow $3:(fst $ unLoc $1)) }
        | forall constr_stuff
                {% ams (let (con,details) = unLoc $2 in
                  (L (comb2 $1 $2) (mkConDeclH98 con
                                            (snd $ unLoc $1)
                                            Nothing   -- No context
                                            details)))
                       (fst $ unLoc $1) }

forall :: { Located ([AddAnn], Maybe [LHsTyVarBndr Specificity GhcPs]) }
        : 'forall' tv_bndrs '.'       { sLL $1 $> ([mu AnnForall $1,mj AnnDot $3], Just $2) }
        | {- empty -}                 { noLoc ([], Nothing) }

constr_stuff :: { Located (Located RdrName, HsConDeclH98Details GhcPs) }
        : infixtype       {% fmap (mapLoc (\b -> (dataConBuilderCon b,
                                                  dataConBuilderDetails b)))
                                  (runPV $1) }

fielddecls :: { [LConDeclField GhcPs] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [LConDeclField GhcPs] }
        : fielddecl ',' fielddecls1
            {% addAnnotation (gl $1) AnnComma (gl $2) >>
               return ($1 : $3) }
        | fielddecl   { [$1] }

fielddecl :: { LConDeclField GhcPs }
                                              -- A list because of   f,g :: Int
        : sig_vars '::' ctype
            {% ams (L (comb2 $1 $3)
                      (ConDeclField noExtField (reverse (map (\ln@(L l n) -> L l $ FieldOcc noExtField ln) (unLoc $1))) $3 Nothing))
                   [mu AnnDcolon $2] }

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
                 in ams (L full_loc $ HsDerivingClause noExtField Nothing $2)
                        [mj AnnDeriving $1] }

        | 'deriving' deriv_strategy_no_via deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in ams (L full_loc $ HsDerivingClause noExtField (Just $2) $3)
                        [mj AnnDeriving $1] }

        | 'deriving' deriv_clause_types deriv_strategy_via
              {% let { full_loc = comb2 $1 $> }
                 in ams (L full_loc $ HsDerivingClause noExtField (Just $3) $2)
                        [mj AnnDeriving $1] }

deriv_clause_types :: { LDerivClauseTys GhcPs }
        : qtycon              { let { tc = sL1 $1 $ mkHsImplicitSigType $
                                           sL1 $1 $ HsTyVar noExtField NotPromoted $1 } in
                                sL1 $1 (DctSingle noExtField tc) }
        | '(' ')'             {% ams (sLL $1 $> (DctMulti noExtField []))
                                     [mop $1,mcp $2] }
        | '(' deriv_types ')' {% ams (sLL $1 $> (DctMulti noExtField $2))
                                     [mop $1,mcp $3] }

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
                                       do { (ann,r) <- checkValDef $1 (snd $2) $3;
                                        let { l = comb2 $1 $> };
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                        case r of {
                                          (FunBind _ n _ _) ->
                                                amsL l (mj AnnFunId n:(fst $2)) >> return () ;
                                          (PatBind _ (L lh _lhs) _rhs _) ->
                                                amsL lh (fst $2) >> return () } ;
                                        _ <- amsL l (ann ++ (fst $ unLoc $3));
                                        return $! (sL l $ ValD noExtField r) } }
        | pattern_synonym_decl  { $1 }

decl    :: { LHsDecl GhcPs }
        : decl_no_th            { $1 }

        -- Why do we only allow naked declaration splices in top-level
        -- declarations and not here? Short answer: because readFail009
        -- fails terribly with a panic in cvBindsAndSigs otherwise.
        | splice_exp            { sLL $1 $> $ mkSpliceDecl $1 }

rhs     :: { Located ([AddAnn],GRHSs GhcPs (LHsExpr GhcPs)) }
        : '=' exp wherebinds    {% runPV (unECP $2) >>= \ $2 -> return $
                                  sL (comb3 $1 $2 $3)
                                    ((mj AnnEqual $1 : (fst $ unLoc $3))
                                    ,GRHSs noExtField (unguardedRHS (comb3 $1 $2 $3) $2)
                                   (snd $ unLoc $3)) }
        | gdrhs wherebinds      { sLL $1 $>  (fst $ unLoc $2
                                    ,GRHSs noExtField (reverse (unLoc $1))
                                                    (snd $ unLoc $2)) }

gdrhs :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : gdrhs gdrh            { sLL $1 $> ($2 : unLoc $1) }
        | gdrh                  { sL1 $1 [$1] }

gdrh :: { LGRHS GhcPs (LHsExpr GhcPs) }
        : '|' guardquals '=' exp  {% runPV (unECP $4) >>= \ $4 ->
                                     ams (sL (comb2 $1 $>) $ GRHS noExtField (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnEqual $3] }

sigdecl :: { LHsDecl GhcPs }
        :
        -- See Note [Declaration/signature overlap] for why we need infixexp here
          infixexp     '::' sigtype
                        {% do { $1 <- runPV (unECP $1)
                              ; v <- checkValSigLhs $1
                              ; _ <- amsL (comb2 $1 $>) [mu AnnDcolon $2]
                              ; return (sLL $1 $> $ SigD noExtField $
                                  TypeSig noExtField [v] (mkHsWildCardBndrs $3))} }

        | var ',' sig_vars '::' sigtype
           {% do { let sig = TypeSig noExtField ($1 : reverse (unLoc $3))
                                     (mkHsWildCardBndrs $5)
                 ; addAnnotation (gl $1) AnnComma (gl $2)
                 ; ams ( sLL $1 $> $ SigD noExtField sig )
                       [mu AnnDcolon $4] } }

        | infix prec ops
              {% checkPrecP $2 $3 >>
                 ams (sLL $1 $> $ SigD noExtField
                        (FixSig noExtField (FixitySig noExtField (fromOL $ unLoc $3)
                                (Fixity (fst $ unLoc $2) (snd $ unLoc $2) (unLoc $1)))))
                     [mj AnnInfix $1,mj AnnVal $2] }

        | pattern_synonym_sig   { sLL $1 $> . SigD noExtField . unLoc $ $1 }

        | '{-# COMPLETE' con_list opt_tyconsig  '#-}'
                {% let (dcolon, tc) = $3
                   in ams
                       (sLL $1 $>
                         (SigD noExtField (CompleteMatchSig noExtField (getCOMPLETE_PRAGs $1) $2 tc)))
                    ([ mo $1 ] ++ dcolon ++ [mc $4]) }

        -- This rule is for both INLINE and INLINABLE pragmas
        | '{-# INLINE' activation qvar '#-}'
                {% ams ((sLL $1 $> $ SigD noExtField (InlineSig noExtField $3
                            (mkInlinePragma (getINLINE_PRAGs $1) (getINLINE $1)
                                            (snd $2)))))
                       ((mo $1:fst $2) ++ [mc $4]) }

        | '{-# SCC' qvar '#-}'
          {% ams (sLL $1 $> (SigD noExtField (SCCFunSig noExtField (getSCC_PRAGs $1) $2 Nothing)))
                 [mo $1, mc $3] }

        | '{-# SCC' qvar STRING '#-}'
          {% do { scc <- getSCC $3
                ; let str_lit = StringLiteral (getSTRINGs $3) scc
                ; ams (sLL $1 $> (SigD noExtField (SCCFunSig noExtField (getSCC_PRAGs $1) $2 (Just ( sL1 $3 str_lit)))))
                      [mo $1, mc $4] } }

        | '{-# SPECIALISE' activation qvar '::' sigtypes1 '#-}'
             {% ams (
                 let inl_prag = mkInlinePragma (getSPEC_PRAGs $1)
                                             (NoUserInlinePrag, FunLike) (snd $2)
                  in sLL $1 $> $ SigD noExtField (SpecSig noExtField $3 (fromOL $5) inl_prag))
                    (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
             {% ams (sLL $1 $> $ SigD noExtField (SpecSig noExtField $3 (fromOL $5)
                               (mkInlinePragma (getSPEC_INLINE_PRAGs $1)
                                               (getSPEC_INLINE $1) (snd $2))))
                       (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE' 'instance' inst_type '#-}'
                {% ams (sLL $1 $>
                                  $ SigD noExtField (SpecInstSig noExtField (getSPEC_PRAGs $1) $3))
                       [mo $1,mj AnnInstance $2,mc $4] }

        -- A minimal complete definition
        | '{-# MINIMAL' name_boolformula_opt '#-}'
            {% ams (sLL $1 $> $ SigD noExtField (MinimalSig noExtField (getMINIMAL_PRAGs $1) $2))
                   [mo $1,mc $3] }

activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | explicit_activation                   { (fst $1,Just (snd $1)) }

explicit_activation :: { ([AddAnn],Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mj AnnOpenS $1,mj AnnVal $2,mj AnnCloseS $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' rule_activation_marker INTEGER ']'
                                { ($2++[mj AnnOpenS $1,mj AnnVal $3,mj AnnCloseS $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsSplice GhcPs) }
        : TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in sL1 $1 (mkHsQuasiQuote quoterId (mkSrcSpanPs quoteSpan) quote) }
        | TH_QQUASIQUOTE  { let { loc = getLoc $1
                                ; ITqQuasiQuote (qual, quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkQual varName (qual, quoter) }
                            in sL (getLoc $1) (mkHsQuasiQuote quoterId (mkSrcSpanPs quoteSpan) quote) }

exp   :: { ECP }
        : infixexp '::' ctype
                                { ECP $
                                   unECP $1 >>= \ $1 ->
                                   rejectPragmaPV $1 >>
                                   amms (mkHsTySigPV (comb2 $1 $>) $1 $3)
                                       [mu AnnDcolon $2] }
        | infixexp '-<' exp     {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   ams (sLL $1 $> $ HsCmdArrApp noExtField $1 $3
                                                        HsFirstOrderApp True)
                                       [mu Annlarrowtail $2] }
        | infixexp '>-' exp     {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   ams (sLL $1 $> $ HsCmdArrApp noExtField $3 $1
                                                      HsFirstOrderApp False)
                                       [mu Annrarrowtail $2] }
        | infixexp '-<<' exp    {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   ams (sLL $1 $> $ HsCmdArrApp noExtField $1 $3
                                                      HsHigherOrderApp True)
                                       [mu AnnLarrowtail $2] }
        | infixexp '>>-' exp    {% runPV (unECP $1) >>= \ $1 ->
                                   runPV (unECP $3) >>= \ $3 ->
                                   fmap ecpFromCmd $
                                   ams (sLL $1 $> $ HsCmdArrApp noExtField $3 $1
                                                      HsHigherOrderApp False)
                                       [mu AnnRarrowtail $2] }
        -- See Note [%shift: exp -> infixexp]
        | infixexp %shift       { $1 }
        | exp_prag(exp)         { $1 } -- See Note [Pragmas and operator fixity]

infixexp :: { ECP }
        : exp10 { $1 }
        | infixexp qop exp10p    -- See Note [Pragmas and operator fixity]
                               { ECP $
                                 superInfixOp $
                                 $2 >>= \ $2 ->
                                 unECP $1 >>= \ $1 ->
                                 unECP $3 >>= \ $3 ->
                                 rejectPragmaPV $1 >>
                                 amms (mkHsOpAppPV (comb2 $1 $>) $1 $2 $3)
                                     [mj AnnVal $2] }
                 -- AnnVal annotation for NPlusKPat, which discards the operator

exp10p :: { ECP }
  : exp10            { $1 }
  | exp_prag(exp10p) { $1 } -- See Note [Pragmas and operator fixity]

exp_prag(e) :: { ECP }
  : prag_e e  -- See Note [Pragmas and operator fixity]
      {% runPV (unECP $2) >>= \ $2 ->
         fmap ecpFromExp $
         ams (sLL $1 $> $ HsPragE noExtField (snd $ unLoc $1) $2)
             (fst $ unLoc $1) }

exp10 :: { ECP }
        -- See Note [%shift: exp10 -> '-' fexp]
        : '-' fexp %shift               { ECP $
                                           unECP $2 >>= \ $2 ->
                                           amms (mkHsNegAppPV (comb2 $1 $>) $2)
                                               [mj AnnMinus $1] }
        -- See Note [%shift: exp10 -> fexp]
        | fexp %shift                  { $1 }

optSemi :: { ([Located Token],Bool) }
        : ';'         { ([$1],True) }
        | {- empty -} { ([],False) }

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
prag_e :: { Located ([AddAnn], HsPragE GhcPs) }
      : '{-# SCC' STRING '#-}'      {% do scc <- getSCC $2
                                          ; return $ sLL $1 $>
                                             ([mo $1,mj AnnValStr $2,mc $3],
                                              HsPragSCC noExtField
                                                (getSCC_PRAGs $1)
                                                (StringLiteral (getSTRINGs $2) scc)) }
      | '{-# SCC' VARID  '#-}'      { sLL $1 $> ([mo $1,mj AnnVal $2,mc $3],
                                                  HsPragSCC noExtField
                                                    (getSCC_PRAGs $1)
                                                    (StringLiteral NoSourceText (getVARID $2))) }
fexp    :: { ECP }
        : fexp aexp                  { ECP $
                                          superFunArg $
                                          unECP $1 >>= \ $1 ->
                                          unECP $2 >>= \ $2 ->
                                          mkHsAppPV (comb2 $1 $>) $1 $2 }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | fexp PREFIX_AT atype       { ECP $
                                        unECP $1 >>= \ $1 ->
                                        amms (mkHsAppTypePV (comb2 $1 $>) $1 $3) [mj AnnAt $2] }

        | 'static' aexp              {% runPV (unECP $2) >>= \ $2 ->
                                        fmap ecpFromExp $
                                        ams (sLL $1 $> $ HsStatic noExtField $2)
                                            [mj AnnStatic $1] }
        | aexp                       { $1 }

aexp    :: { ECP }
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        : qvar TIGHT_INFIX_AT aexp
                                { ECP $
                                   unECP $3 >>= \ $3 ->
                                   amms (mkHsAsPatPV (comb2 $1 $>) $1 $3) [mj AnnAt $2] }

        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        | PREFIX_TILDE aexp     { ECP $
                                   unECP $2 >>= \ $2 ->
                                   amms (mkHsLazyPatPV (comb2 $1 $>) $2) [mj AnnTilde $1] }
        | PREFIX_BANG aexp      { ECP $
                                   unECP $2 >>= \ $2 ->
                                   amms (mkHsBangPatPV (comb2 $1 $>) $2) [mj AnnBang $1] }
        | PREFIX_MINUS aexp     { ECP $
                                   unECP $2 >>= \ $2 ->
                                   amms (mkHsNegAppPV (comb2 $1 $>) $2) [mj AnnMinus $1] }

        | '\\' apat apats '->' exp
                   {  ECP $
                      unECP $5 >>= \ $5 ->
                      amms (mkHsLamPV (comb2 $1 $>) (mkMatchGroup FromSource
                            [sLL $1 $> $ Match { m_ext = noExtField
                                               , m_ctxt = LambdaExpr
                                               , m_pats = $2:$3
                                               , m_grhss = unguardedGRHSs $5 }]))
                          [mj AnnLam $1, mu AnnRarrow $4] }
        | 'let' binds 'in' exp          {  ECP $
                                           unECP $4 >>= \ $4 ->
                                           amms (mkHsLetPV (comb2 $1 $>) (snd (unLoc $2)) $4)
                                               (mj AnnLet $1:mj AnnIn $3
                                                 :(fst $ unLoc $2)) }
        | '\\' 'lcase' altslist
            {  ECP $ $3 >>= \ $3 ->
               amms (mkHsLamCasePV (comb2 $1 $>)
                                   (mkMatchGroup FromSource (snd $ unLoc $3)))
                    (mj AnnLam $1:mj AnnCase $2:(fst $ unLoc $3)) }
        | 'if' exp optSemi 'then' exp optSemi 'else' exp
                         {% runPV (unECP $2) >>= \ $2 ->
                            return $ ECP $
                              unECP $5 >>= \ $5 ->
                              unECP $8 >>= \ $8 ->
                              amms (mkHsIfPV (comb2 $1 $>) $2 (snd $3) $5 (snd $6) $8)
                                  (mj AnnIf $1:mj AnnThen $4
                                     :mj AnnElse $7
                                     :(map (\l -> mj AnnSemi l) (fst $3))
                                    ++(map (\l -> mj AnnSemi l) (fst $6))) }
        | 'if' ifgdpats                 {% hintMultiWayIf (getLoc $1) >>= \_ ->
                                           fmap ecpFromExp $
                                           ams (sLL $1 $> $ HsMultiIf noExtField
                                                     (reverse $ snd $ unLoc $2))
                                               (mj AnnIf $1:(fst $ unLoc $2)) }
        | 'case' exp 'of' altslist    {% runPV (unECP $2) >>= \ $2 ->
                                         return $ ECP $
                                           $4 >>= \ $4 ->
                                           amms (mkHsCasePV (comb3 $1 $3 $4) $2 (mkMatchGroup
                                                   FromSource (snd $ unLoc $4)))
                                               (mj AnnCase $1:mj AnnOf $3
                                                  :(fst $ unLoc $4)) }
        -- QualifiedDo.
        | DO  stmtlist               {% do
                                      hintQualifiedDo $1
                                      return $ ECP $
                                        $2 >>= \ $2 ->
                                        amms (mkHsDoPV (comb2 $1 $2)
                                                       (fmap mkModuleNameFS (getDO $1))
                                                       (mapLoc snd $2))
                                             (mj AnnDo $1:(fst $ unLoc $2)) }
        | MDO stmtlist             {% hintQualifiedDo $1 >> runPV $2 >>= \ $2 ->
                                       fmap ecpFromExp $
                                       ams (L (comb2 $1 $2)
                                              (mkHsDo (MDoExpr $
                                                        fmap mkModuleNameFS (getMDO $1))
                                                        (snd $ unLoc $2)))
                                           (mj AnnMdo $1:(fst $ unLoc $2)) }
        | 'proc' aexp '->' exp
                       {% (checkPattern <=< runPV) (unECP $2) >>= \ p ->
                           runPV (unECP $4) >>= \ $4@cmd ->
                           fmap ecpFromExp $
                           ams (sLL $1 $> $ HsProc noExtField p (sLL $1 $> $ HsCmdTop noExtField cmd))
                                            -- TODO: is LL right here?
                               [mj AnnProc $1,mu AnnRarrow $3] }

        | aexp1                 { $1 }

aexp1   :: { ECP }
        : aexp1 '{' fbinds '}' { ECP $
                                  unECP $1 >>= \ $1 ->
                                  $3 >>= \ $3 ->
                                  amms (mkHsRecordPV (comb2 $1 $>) (comb2 $2 $4) $1 (snd $3))
                                       (moc $2:mcc $4:(fst $3)) }
        | aexp2                { $1 }

aexp2   :: { ECP }
        : qvar                          { ECP $ mkHsVarPV $! $1 }
        | qcon                          { ECP $ mkHsVarPV $! $1 }
        -- See Note [%shift: aexp2 -> ipvar]
        | ipvar %shift                  { ecpFromExp $ sL1 $1 (HsIPVar noExtField $! unLoc $1) }
        | overloaded_label              { ecpFromExp $ sL1 $1 (HsOverLabel noExtField Nothing $! unLoc $1) }
        | literal                       { ECP $ mkHsLitPV $! $1 }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--      | STRING    { sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRINGs $1)
--                                       (getSTRING $1) noExtField) }
        | INTEGER   { ECP $ mkHsOverLitPV (sL1 $1 $ mkHsIntegral   (getINTEGER  $1)) }
        | RATIONAL  { ECP $ mkHsOverLitPV (sL1 $1 $ mkHsFractional (getRATIONAL $1)) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
        | '(' texp ')'                  { ECP $
                                           unECP $2 >>= \ $2 ->
                                           amms (mkHsParPV (comb2 $1 $>) $2) [mop $1,mcp $3] }
        | '(' tup_exprs ')'             { ECP $
                                           $2 >>= \ $2 ->
                                           amms (mkSumOrTuplePV (comb2 $1 $>) Boxed (snd $2))
                                                ((mop $1:fst $2) ++ [mcp $3]) }

        | '(#' texp '#)'                { ECP $
                                           unECP $2 >>= \ $2 ->
                                           amms (mkSumOrTuplePV (comb2 $1 $>) Unboxed (Tuple [L (gl $2) (Just $2)]))
                                                [mo $1,mc $3] }
        | '(#' tup_exprs '#)'           { ECP $
                                           $2 >>= \ $2 ->
                                           amms (mkSumOrTuplePV (comb2 $1 $>) Unboxed (snd $2))
                                                ((mo $1:fst $2) ++ [mc $3]) }

        | '[' list ']'      { ECP $ $2 (comb2 $1 $>) >>= \a -> ams a [mos $1,mcs $3] }
        | '_'               { ECP $ mkHsWildCardPV (getLoc $1) }

        -- Template Haskell Extension
        | splice_untyped { ECP $ mkHsSplicePV $1 }
        | splice_typed   { ecpFromExp $ mapLoc (HsSpliceE noExtField) $1 }

        | SIMPLEQUOTE  qvar     {% fmap ecpFromExp $ ams (sLL $1 $> $ HsBracket noExtField (VarBr noExtField True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  qcon     {% fmap ecpFromExp $ ams (sLL $1 $> $ HsBracket noExtField (VarBr noExtField True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE tyvar     {% fmap ecpFromExp $ ams (sLL $1 $> $ HsBracket noExtField (VarBr noExtField False (unLoc $2))) [mj AnnThTyQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE gtycon    {% fmap ecpFromExp $ ams (sLL $1 $> $ HsBracket noExtField (VarBr noExtField False (unLoc $2))) [mj AnnThTyQuote $1,mj AnnName $2] }
        -- See Note [%shift: aexp2 -> TH_TY_QUOTE]
        | TH_TY_QUOTE %shift    {% reportEmptyDoubleQuotes (getLoc $1) }
        | '[|' exp '|]'       {% runPV (unECP $2) >>= \ $2 ->
                                 fmap ecpFromExp $
                                 ams (sLL $1 $> $ HsBracket noExtField (ExpBr noExtField $2))
                                      (if (hasE $1) then [mj AnnOpenE $1, mu AnnCloseQ $3]
                                                    else [mu AnnOpenEQ $1,mu AnnCloseQ $3]) }
        | '[||' exp '||]'     {% runPV (unECP $2) >>= \ $2 ->
                                 fmap ecpFromExp $
                                 ams (sLL $1 $> $ HsBracket noExtField (TExpBr noExtField $2))
                                      (if (hasE $1) then [mj AnnOpenE $1,mc $3] else [mo $1,mc $3]) }
        | '[t|' ktype '|]'    {% fmap ecpFromExp $
                                 ams (sLL $1 $> $ HsBracket noExtField (TypBr noExtField $2)) [mo $1,mu AnnCloseQ $3] }
        | '[p|' infixexp '|]' {% (checkPattern <=< runPV) (unECP $2) >>= \p ->
                                      fmap ecpFromExp $
                                      ams (sLL $1 $> $ HsBracket noExtField (PatBr noExtField p))
                                          [mo $1,mu AnnCloseQ $3] }
        | '[d|' cvtopbody '|]' {% fmap ecpFromExp $
                                  ams (sLL $1 $> $ HsBracket noExtField (DecBrL noExtField (snd $2)))
                                      (mo $1:mu AnnCloseQ $3:fst $2) }
        | quasiquote          { ECP $ mkHsSplicePV $1 }

        -- arrow notation extension
        | '(|' aexp cmdargs '|)'  {% runPV (unECP $2) >>= \ $2 ->
                                     fmap ecpFromCmd $
                                     ams (sLL $1 $> $ HsCmdArrForm noExtField $2 Prefix
                                                          Nothing (reverse $3))
                                         [mu AnnOpenB $1,mu AnnCloseB $4] }

splice_exp :: { LHsExpr GhcPs }
        : splice_untyped { mapLoc (HsSpliceE noExtField) $1 }
        | splice_typed   { mapLoc (HsSpliceE noExtField) $1 }

splice_untyped :: { Located (HsSplice GhcPs) }
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        : PREFIX_DOLLAR aexp2   {% runPV (unECP $2) >>= \ $2 ->
                                   ams (sLL $1 $> $ mkUntypedSplice DollarSplice $2)
                                       [mj AnnDollar $1] }

splice_typed :: { Located (HsSplice GhcPs) }
        -- See Note [Whitespace-sensitive operator parsing] in GHC.Parser.Lexer
        : PREFIX_DOLLAR_DOLLAR aexp2
                                {% runPV (unECP $2) >>= \ $2 ->
                                   ams (sLL $1 $> $ mkTypedSplice DollarSplice $2)
                                       [mj AnnDollarDollar $1] }

cmdargs :: { [LHsCmdTop GhcPs] }
        : cmdargs acmd                  { $2 : $1 }
        | {- empty -}                   { [] }

acmd    :: { LHsCmdTop GhcPs }
        : aexp                  {% runPV (unECP $1) >>= \ cmd ->
                                   runPV (checkCmdBlockArguments cmd) >>= \ _ ->
                                   return (sL1 cmd $ HsCmdTop noExtField cmd) }

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
                                sLL $1 $> $ SectionL noExtField $1 $2 }
        | qopm infixexp      { ECP $
                                superInfixOp $
                                unECP $2 >>= \ $2 ->
                                $1 >>= \ $1 ->
                                mkHsSectionR_PV (comb2 $1 $>) $1 $2 }

       -- View patterns get parenthesized above
        | exp '->' texp   { ECP $
                             unECP $1 >>= \ $1 ->
                             unECP $3 >>= \ $3 ->
                             amms (mkHsViewPatPV (comb2 $1 $>) $1 $3) [mu AnnRarrow $2] }

-- Always at least one comma or bar.
-- Though this can parse just commas (without any expressions), it won't
-- in practice, because (,,,) is parsed as a name. See Note [ExplicitTuple]
-- in GHC.Hs.Expr.
tup_exprs :: { forall b. DisambECP b => PV ([AddAnn],SumOrTuple b) }
           : texp commas_tup_tail
                           { unECP $1 >>= \ $1 ->
                             $2 >>= \ $2 ->
                             do { addAnnotation (gl $1) AnnComma (fst $2)
                                ; return ([],Tuple ((sL1 $1 (Just $1)) : snd $2)) } }

           | texp bars   { unECP $1 >>= \ $1 -> return $
                            (mvbars (fst $2), Sum 1  (snd $2 + 1) $1) }

           | commas tup_tail
                 { $2 >>= \ $2 ->
                   do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (fst $1)
                      ; return
                           ([],Tuple (map (\l -> L l Nothing) (fst $1) ++ $2)) } }

           | bars texp bars0
                { unECP $2 >>= \ $2 -> return $
                  (mvbars (fst $1) ++ mvbars (fst $3), Sum (snd $1 + 1) (snd $1 + snd $3 + 1) $2) }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { forall b. DisambECP b => PV (SrcSpan,[Located (Maybe (Located b))]) }
commas_tup_tail : commas tup_tail
        { $2 >>= \ $2 ->
          do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (tail $ fst $1)
             ; return (
            (head $ fst $1
            ,(map (\l -> L l Nothing) (tail $ fst $1)) ++ $2)) } }

-- Always follows a comma
tup_tail :: { forall b. DisambECP b => PV [Located (Maybe (Located b))] }
          : texp commas_tup_tail { unECP $1 >>= \ $1 ->
                                   $2 >>= \ $2 ->
                                   addAnnotation (gl $1) AnnComma (fst $2) >>
                                   return ((L (gl $1) (Just $1)) : snd $2) }
          | texp                 { unECP $1 >>= \ $1 ->
                                   return [L (gl $1) (Just $1)] }
          -- See Note [%shift: tup_tail -> {- empty -}]
          | {- empty -} %shift   { return [noLoc Nothing] }

-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.
-- Never empty.
list :: { forall b. DisambECP b => SrcSpan -> PV (Located b) }
        : texp    { \loc -> unECP $1 >>= \ $1 ->
                            mkHsExplicitListPV loc [$1] }
        | lexps   { \loc -> $1 >>= \ $1 ->
                            mkHsExplicitListPV loc (reverse $1) }
        | texp '..'  { \loc ->    unECP $1 >>= \ $1 ->
                                  ams (L loc $ ArithSeq noExtField Nothing (From $1))
                                      [mj AnnDotdot $2]
                                      >>= ecpFromExp' }
        | texp ',' exp '..' { \loc ->
                                   unECP $1 >>= \ $1 ->
                                   unECP $3 >>= \ $3 ->
                                   ams (L loc $ ArithSeq noExtField Nothing (FromThen $1 $3))
                                       [mj AnnComma $2,mj AnnDotdot $4]
                                       >>= ecpFromExp' }
        | texp '..' exp  { \loc -> unECP $1 >>= \ $1 ->
                                   unECP $3 >>= \ $3 ->
                                   ams (L loc $ ArithSeq noExtField Nothing (FromTo $1 $3))
                                       [mj AnnDotdot $2]
                                       >>= ecpFromExp' }
        | texp ',' exp '..' exp { \loc ->
                                   unECP $1 >>= \ $1 ->
                                   unECP $3 >>= \ $3 ->
                                   unECP $5 >>= \ $5 ->
                                   ams (L loc $ ArithSeq noExtField Nothing (FromThenTo $1 $3 $5))
                                       [mj AnnComma $2,mj AnnDotdot $4]
                                       >>= ecpFromExp' }
        | texp '|' flattenedpquals
             { \loc ->
                checkMonadComp >>= \ ctxt ->
                unECP $1 >>= \ $1 ->
                ams (L loc $ mkHsComp ctxt (unLoc $3) $1)
                    [mj AnnVbar $2]
                    >>= ecpFromExp' }

lexps :: { forall b. DisambECP b => PV [Located b] }
        : lexps ',' texp           { $1 >>= \ $1 ->
                                     unECP $3 >>= \ $3 ->
                                     addAnnotation (gl $ head $ $1)
                                                            AnnComma (gl $2) >>
                                      return (((:) $! $3) $! $1) }
        | texp ',' texp             { unECP $1 >>= \ $1 ->
                                      unECP $3 >>= \ $3 ->
                                      addAnnotation (gl $1) AnnComma (gl $2) >>
                                      return [$3,$1] }

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : pquals   { case (unLoc $1) of
                    [qs] -> sL1 $1 qs
                    -- We just had one thing in our "parallel" list so
                    -- we simply return that thing directly

                    qss -> sL1 $1 [sL1 $1 $ ParStmt noExtField [ParStmtBlock noExtField qs [] noSyntaxExpr |
                                            qs <- qss]
                                            noExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt GhcPs (LHsExpr GhcPs)]] }
    : squals '|' pquals
                     {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $2) >>
                        return (sLL $1 $> (reverse (unLoc $1) : unLoc $3)) }
    | squals         { L (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }   -- In reverse order, because the last
                                        -- one can "grab" the earlier ones
    : squals ',' transformqual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                amsL (comb2 $1 $>) (fst $ unLoc $3) >>
                return (sLL $1 $> [sLL $1 $> ((snd $ unLoc $3) (reverse (unLoc $1)))]) }
    | squals ',' qual
             {% runPV $3 >>= \ $3 ->
                addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                return (sLL $1 $> ($3 : unLoc $1)) }
    | transformqual        {% ams $1 (fst $ unLoc $1) >>
                              return (sLL $1 $> [L (getLoc $1) ((snd $ unLoc $1) [])]) }
    | qual                               {% runPV $1 >>= \ $1 ->
                                            return $ sL1 $1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { sLL $1 $> ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { sL1 $1 [$2] }

-- It is possible to enable bracketing (associating) qualifier lists
-- by uncommenting the lines with {| |} above. Due to a lack of
-- consensus on the syntax, this feature is not being used until we
-- get user demand.

transformqual :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)] -> Stmt GhcPs (LHsExpr GhcPs)) }
                        -- Function is applied to a list of stmts *in order*
    : 'then' exp              {% runPV (unECP $2) >>= \ $2 -> return $
                                 sLL $1 $> ([mj AnnThen $1], \ss -> (mkTransformStmt ss $2)) }
    | 'then' exp 'by' exp     {% runPV (unECP $2) >>= \ $2 ->
                                 runPV (unECP $4) >>= \ $4 ->
                                 return $ sLL $1 $> ([mj AnnThen $1,mj AnnBy  $3],
                                                     \ss -> (mkTransformByStmt ss $2 $4)) }
    | 'then' 'group' 'using' exp
            {% runPV (unECP $4) >>= \ $4 ->
               return $ sLL $1 $> ([mj AnnThen $1,mj AnnGroup $2,mj AnnUsing $3],
                                   \ss -> (mkGroupUsingStmt ss $4)) }

    | 'then' 'group' 'by' exp 'using' exp
            {% runPV (unECP $4) >>= \ $4 ->
               runPV (unECP $6) >>= \ $6 ->
               return $ sLL $1 $> ([mj AnnThen $1,mj AnnGroup $2,mj AnnBy $3,mj AnnUsing $5],
                                   \ss -> (mkGroupByUsingStmt ss $4 $6)) }

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
                               addAnnotation (gl $ head $ unLoc $1) AnnComma
                                             (gl $2) >>
                               return (sLL $1 $> ($3 : unLoc $1)) }
    | qual                  {% runPV $1 >>= \ $1 ->
                               return $ sL1 $1 [$1] }

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { forall b. DisambECP b => PV (Located ([AddAnn],[LMatch GhcPs (Located b)])) }
        : '{'            alts '}'  { $2 >>= \ $2 -> return $
                                     sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                               ,(reverse (snd $ unLoc $2))) }
        |     vocurly    alts  close { $2 >>= \ $2 -> return $
                                       L (getLoc $2) (fst $ unLoc $2
                                        ,(reverse (snd $ unLoc $2))) }
        | '{'                 '}'    { return $ sLL $1 $> ([moc $1,mcc $2],[]) }
        |     vocurly          close { return $ noLoc ([],[]) }

alts    :: { forall b. DisambECP b => PV (Located ([AddAnn],[LMatch GhcPs (Located b)])) }
        : alts1                    { $1 >>= \ $1 -> return $
                                     sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
        | ';' alts                 { $2 >>= \ $2 -> return $
                                     sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2))
                                               ,snd $ unLoc $2) }

alts1   :: { forall b. DisambECP b => PV (Located ([AddAnn],[LMatch GhcPs (Located b)])) }
        : alts1 ';' alt         { $1 >>= \ $1 ->
                                  $3 >>= \ $3 ->
                                     if null (snd $ unLoc $1)
                                     then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                  ,[$3]))
                                     else (ams (head $ snd $ unLoc $1)
                                               (mj AnnSemi $2:(fst $ unLoc $1))
                                           >> return (sLL $1 $> ([],$3 : (snd $ unLoc $1))) ) }
        | alts1 ';'             {  $1 >>= \ $1 ->
                                   if null (snd $ unLoc $1)
                                     then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                  ,snd $ unLoc $1))
                                     else (ams (head $ snd $ unLoc $1)
                                               (mj AnnSemi $2:(fst $ unLoc $1))
                                           >> return (sLL $1 $> ([],snd $ unLoc $1))) }
        | alt                   { $1 >>= \ $1 -> return $ sL1 $1 ([],[$1]) }

alt     :: { forall b. DisambECP b => PV (LMatch GhcPs (Located b)) }
           : pat alt_rhs  { $2 >>= \ $2 ->
                            ams (sLL $1 $> (Match { m_ext = noExtField
                                                  , m_ctxt = CaseAlt
                                                  , m_pats = [$1]
                                                  , m_grhss = snd $ unLoc $2 }))
                                      (fst $ unLoc $2)}

alt_rhs :: { forall b. DisambECP b => PV (Located ([AddAnn],GRHSs GhcPs (Located b))) }
        : ralt wherebinds           { $1 >>= \alt ->
                                      return $ sLL alt $> (fst $ unLoc $2, GRHSs noExtField (unLoc alt) (snd $ unLoc $2)) }

ralt :: { forall b. DisambECP b => PV (Located [LGRHS GhcPs (Located b)]) }
        : '->' exp            { unECP $2 >>= \ $2 ->
                                ams (sLL $1 $> (unguardedRHS (comb2 $1 $2) $2))
                                    [mu AnnRarrow $1] }
        | gdpats              { $1 >>= \gdpats ->
                                return $ sL1 gdpats (reverse (unLoc gdpats)) }

gdpats :: { forall b. DisambECP b => PV (Located [LGRHS GhcPs (Located b)]) }
        : gdpats gdpat { $1 >>= \gdpats ->
                         $2 >>= \gdpat ->
                         return $ sLL gdpats gdpat (gdpat : unLoc gdpats) }
        | gdpat        { $1 >>= \gdpat -> return $ sL1 gdpat [gdpat] }

-- layout for MultiWayIf doesn't begin with an open brace, because it's hard to
-- generate the open brace in addition to the vertical bar in the lexer, and
-- we don't need it.
ifgdpats :: { Located ([AddAnn],[LGRHS GhcPs (LHsExpr GhcPs)]) }
         : '{' gdpats '}'                 {% runPV $2 >>= \ $2 ->
                                             return $ sLL $1 $> ([moc $1,mcc $3],unLoc $2)  }
         |     gdpats close               {% runPV $1 >>= \ $1 ->
                                             return $ sL1 $1 ([],unLoc $1) }

gdpat   :: { forall b. DisambECP b => PV (LGRHS GhcPs (Located b)) }
        : '|' guardquals '->' exp
                                   { unECP $4 >>= \ $4 ->
                                     ams (sL (comb2 $1 $>) $ GRHS noExtField (unLoc $2) $4)
                                         [mj AnnVbar $1,mu AnnRarrow $3] }

-- 'pat' recognises a pattern, including one with a bang at the top
--      e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat GhcPs }
pat     :  exp          {% (checkPattern <=< runPV) (unECP $1) }

bindpat :: { LPat GhcPs }
bindpat :  exp            {% -- See Note [Parser-Validator Hint] in GHC.Parser.PostProcess
                             checkPattern_hints [SuggestMissingDo]
                                              (unECP $1) }

apat   :: { LPat GhcPs }
apat    : aexp                  {% (checkPattern <=< runPV) (unECP $1) }

apats  :: { [LPat GhcPs] }
        : apat apats            { $1 : $2 }
        | {- empty -}           { [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { forall b. DisambECP b => PV (Located ([AddAnn],[LStmt GhcPs (Located b)])) }
        : '{'           stmts '}'       { $2 >>= \ $2 -> return $
                                          sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                             ,(reverse $ snd $ unLoc $2)) } -- AZ:performance of reverse?
        |     vocurly   stmts close     { $2 >>= \ $2 -> return $
                                          L (gl $2) (fst $ unLoc $2
                                                    ,reverse $ snd $ unLoc $2) }

--      do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead

stmts :: { forall b. DisambECP b => PV (Located ([AddAnn],[LStmt GhcPs (Located b)])) }
        : stmts ';' stmt  { $1 >>= \ $1 ->
                            $3 >>= \ $3 ->
                            if null (snd $ unLoc $1)
                              then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                     ,$3 : (snd $ unLoc $1)))
                              else do
                               { ams (head $ snd $ unLoc $1) [mj AnnSemi $2]
                               ; return $ sLL $1 $> (fst $ unLoc $1,$3 :(snd $ unLoc $1)) }}

        | stmts ';'     {  $1 >>= \ $1 ->
                           if null (snd $ unLoc $1)
                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1),snd $ unLoc $1))
                             else do
                               { ams (head $ snd $ unLoc $1)
                                               [mj AnnSemi $2]
                               ; return $1 }
          }
        | stmt                   { $1 >>= \ $1 ->
                                   return $ sL1 $1 ([],[$1]) }
        | {- empty -}            { return $ noLoc ([],[]) }


-- For typing stmts at the GHCi prompt, where
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt GhcPs (LHsExpr GhcPs)) }
        : stmt                          {% fmap Just (runPV $1) }
        | {- nothing -}                 { Nothing }

-- For GHC API.
e_stmt :: { LStmt GhcPs (LHsExpr GhcPs) }
        : stmt                          {% runPV $1 }

stmt  :: { forall b. DisambECP b => PV (LStmt GhcPs (Located b)) }
        : qual                          { $1 }
        | 'rec' stmtlist                {  $2 >>= \ $2 ->
                                           ams (sLL $1 $> $ mkRecStmt (snd $ unLoc $2))
                                               (mj AnnRec $1:(fst $ unLoc $2)) }

qual  :: { forall b. DisambECP b => PV (LStmt GhcPs (Located b)) }
    : bindpat '<-' exp                   { unECP $3 >>= \ $3 ->
                                           ams (sLL $1 $> $ mkPsBindStmt $1 $3)
                                               [mu AnnLarrow $2] }
    | exp                                { unECP $1 >>= \ $1 ->
                                           return $ sL1 $1 $ mkBodyStmt $1 }
    | 'let' binds                        { ams (sLL $1 $> $ LetStmt noExtField (snd $ unLoc $2))
                                               (mj AnnLet $1:(fst $ unLoc $2)) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds  :: { forall b. DisambECP b => PV ([AddAnn],([LHsRecField GhcPs (Located b)], Maybe SrcSpan)) }
        : fbinds1                       { $1 }
        | {- empty -}                   { return ([],([], Nothing)) }

fbinds1 :: { forall b. DisambECP b => PV ([AddAnn],([LHsRecField GhcPs (Located b)], Maybe SrcSpan)) }
        : fbind ',' fbinds1
                 { $1 >>= \ $1 ->
                   $3 >>= \ $3 ->
                   addAnnotation (gl $1) AnnComma (gl $2) >>
                   return (case $3 of (ma,(flds, dd)) -> (ma,($1 : flds, dd))) }
        | fbind                         { $1 >>= \ $1 ->
                                          return ([],([$1], Nothing)) }
        | '..'                          { return ([mj AnnDotdot $1],([],   Just (getLoc $1))) }

fbind   :: { forall b. DisambECP b => PV (LHsRecField GhcPs (Located b)) }
        : qvar '=' texp  { unECP $3 >>= \ $3 ->
                           ams  (sLL $1 $> $ HsRecField (sL1 $1 $ mkFieldOcc $1) $3 False)
                                [mj AnnEqual $2] }
                        -- RHS is a 'texp', allowing view patterns (#6038)
                        -- and, incidentally, sections.  Eg
                        -- f (R { x = show -> s }) = ...

        | qvar          { placeHolderPunRhs >>= \rhs ->
                          return $ sLL $1 $> $ HsRecField (sL1 $1 $ mkFieldOcc $1) rhs True }
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
dbind   : ipvar '=' exp                {% runPV (unECP $3) >>= \ $3 ->
                                          ams (sLL $1 $> (IPBind noExtField (Left $1) $3))
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

-- See Note [ExplicitTuple] in GHC.Hs.Expr
sysdcon_nolist :: { Located DataCon }  -- Wired in data constructors
        : '(' ')'               {% ams (sLL $1 $> unitDataCon) [mop $1,mcp $2] }
        | '(' commas ')'        {% ams (sLL $1 $> $ tupleDataCon Boxed (snd $2 + 1))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' '#)'             {% ams (sLL $1 $> $ unboxedUnitDataCon) [mo $1,mc $2] }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ tupleDataCon Unboxed (snd $2 + 1))
                                       (mo $1:mc $3:(mcommas (fst $2))) }

-- See Note [Empty lists] in GHC.Hs.Expr
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


-- See Note [Unit tuples] in GHC.Hs.Type for the distinction
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
        | '(' '->' ')'          {% ams (sLL $1 $> $ getRdrName unrestrictedFunTyCon)
                                       [mop $1,mu AnnRarrow $2,mcp $3] }
        | '[' ']'               {% ams (sLL $1 $> $ listTyCon_RDR) [mos $1,mcs $2] }

oqtycon :: { Located RdrName }  -- An "ordinary" qualified tycon;
                                -- These can appear in export lists
        : qtycon                        { $1 }
        | '(' qtyconsym ')'             {% ams (sLL $1 $> (unLoc $2))
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
        -- See Note [%shift: qtyconop -> qtyconsym]
        : qtyconsym %shift              { $1 }
        | '`' qtycon '`'                {% ams (sLL $1 $> (unLoc $2))
                                               [mj AnnBackquote $1,mj AnnVal $2
                                               ,mj AnnBackquote $3] }

qtycon :: { Located RdrName }   -- Qualified or unqualified
        : QCONID            { sL1 $1 $! mkQual tcClsName (getQCONID $1) }
        | tycon             { $1 }

tycon   :: { Located RdrName }  -- Unqualified
        : CONID                   { sL1 $1 $! mkUnqual tcClsName (getCONID $1) }

qtyconsym :: { Located RdrName }
        : QCONSYM            { sL1 $1 $! mkQual tcClsName (getQCONSYM $1) }
        | QVARSYM            { sL1 $1 $! mkQual tcClsName (getQVARSYM $1) }
        | tyconsym           { $1 }

tyconsym :: { Located RdrName }
        : CONSYM                { sL1 $1 $! mkUnqual tcClsName (getCONSYM $1) }
        | VARSYM                { sL1 $1 $!
                                    -- See Note [eqTyCon (~) is built-in syntax] in GHC.Builtin.Types
                                    if getVARSYM $1 == fsLit "~"
                                      then eqTyCon_RDR
                                      else mkUnqual tcClsName (getVARSYM $1) }
        | ':'                   { sL1 $1 $! consDataCon_RDR }
        | '-'                   { sL1 $1 $! mkUnqual tcClsName (fsLit "-") }
        | '.'                   { sL1 $1 $! mkUnqual tcClsName (fsLit ".") }


-----------------------------------------------------------------------------
-- Operators

op      :: { Located RdrName }   -- used in infix decls
        : varop                 { $1 }
        | conop                 { $1 }
        | '->'                  { sL1 $1 $ getRdrName unrestrictedFunTyCon }

varop   :: { Located RdrName }
        : varsym                { $1 }
        | '`' varid '`'         {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qop     :: { forall b. DisambInfixOp b => PV (Located b) }   -- used in sections
        : qvarop                { mkHsVarOpPV $1 }
        | qconop                { mkHsConOpPV $1 }
        | hole_op               { $1 }

qopm    :: { forall b. DisambInfixOp b => PV (Located b) }   -- used in sections
        : qvaropm               { mkHsVarOpPV $1 }
        | qconop                { mkHsConOpPV $1 }
        | hole_op               { $1 }

hole_op :: { forall b. DisambInfixOp b => PV (Located b) }   -- used in sections
hole_op : '`' '_' '`'           { amms (mkHsInfixHolePV (comb2 $1 $>))
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

tyvarid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual tvName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual tvName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual tvName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual tvName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual tvName (fsLit "interruptible") }
        -- If this changes relative to varid, update 'checkRuleTyVarBndrNames'
        -- in GHC.Parser.PostProcess
        -- See Note [Parsing explicit foralls in Rules]

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

-- Note that 'role' and 'family' get lexed separately regardless of
-- the use of extensions. However, because they are listed here,
-- this is OK and they can be used as normal varids.
-- See Note [Lexing type pseudo-keywords] in GHC.Parser.Lexer
varid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual varName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual varName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual varName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual varName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual varName (fsLit "interruptible")}
        | 'forall'         { sL1 $1 $! mkUnqual varName (fsLit "forall") }
        | 'family'         { sL1 $1 $! mkUnqual varName (fsLit "family") }
        | 'role'           { sL1 $1 $! mkUnqual varName (fsLit "role") }
        -- If this changes relative to tyvarid, update 'checkRuleTyVarBndrNames'
        -- in GHC.Parser.PostProcess
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
            | '*'       { sL1 $1 (fsLit (starSym (isUnicode $1))) }

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
        | PRIMFLOAT         { sL1 $1 $ HsFloatPrim  noExtField $ getPRIMFLOAT $1 }
        | PRIMDOUBLE        { sL1 $1 $ HsDoublePrim noExtField $ getPRIMDOUBLE $1 }

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

{
happyError :: P a
happyError = srcParseFail

getVARID        (L _ (ITvarid    x)) = x
getCONID        (L _ (ITconid    x)) = x
getVARSYM       (L _ (ITvarsym   x)) = x
getCONSYM       (L _ (ITconsym   x)) = x
getDO           (L _ (ITdo      x)) = x
getMDO          (L _ (ITmdo     x)) = x
getQVARID       (L _ (ITqvarid   x)) = x
getQCONID       (L _ (ITqconid   x)) = x
getQVARSYM      (L _ (ITqvarsym  x)) = x
getQCONSYM      (L _ (ITqconsym  x)) = x
getIPDUPVARID   (L _ (ITdupipvarid   x)) = x
getLABELVARID   (L _ (ITlabelvarid   x)) = x
getCHAR         (L _ (ITchar   _ x)) = x
getSTRING       (L _ (ITstring _ x)) = x
getINTEGER      (L _ (ITinteger x))  = x
getRATIONAL     (L _ (ITrational x)) = x
getPRIMCHAR     (L _ (ITprimchar _ x)) = x
getPRIMSTRING   (L _ (ITprimstring _ x)) = x
getPRIMINTEGER  (L _ (ITprimint  _ x)) = x
getPRIMWORD     (L _ (ITprimword _ x)) = x
getPRIMFLOAT    (L _ (ITprimfloat x)) = x
getPRIMDOUBLE   (L _ (ITprimdouble x)) = x
getINLINE       (L _ (ITinline_prag _ inl conl)) = (inl,conl)
getSPEC_INLINE  (L _ (ITspec_inline_prag _ True))  = (Inline,  FunLike)
getSPEC_INLINE  (L _ (ITspec_inline_prag _ False)) = (NoInline,FunLike)
getCOMPLETE_PRAGs (L _ (ITcomplete_prag x)) = x
getVOCURLY      (L (RealSrcSpan l _) ITvocurly) = srcSpanStartCol l

getINTEGERs     (L _ (ITinteger (IL src _ _))) = src
getCHARs        (L _ (ITchar       src _)) = src
getSTRINGs      (L _ (ITstring     src _)) = src
getPRIMCHARs    (L _ (ITprimchar   src _)) = src
getPRIMSTRINGs  (L _ (ITprimstring src _)) = src
getPRIMINTEGERs (L _ (ITprimint    src _)) = src
getPRIMWORDs    (L _ (ITprimword   src _)) = src

-- See Note [Pragma source text] in "GHC.Types.Basic" for the following
getINLINE_PRAGs       (L _ (ITinline_prag       src _ _)) = src
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

getStringLiteral l = StringLiteral (getSTRINGs l) (getSTRING l)

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
                   then addFatalError $ PsError PsErrSpaceInSCC [] (getLoc lt)
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

comb5 :: Located a -> Located b -> Located c -> Located d -> Located e -> SrcSpan
comb5 a b c d e = a `seq` b `seq` c `seq` d `seq` e `seq`
    (combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
       combineSrcSpans (getLoc c) $ combineSrcSpans (getLoc d) (getLoc e))

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` a `seq` L span a

-- See Note [Adding location info] for how these utility functions are used

-- replaced last 3 CPP macros in this file
{-# INLINE sL0 #-}
sL0 :: a -> Located a
sL0 = L noSrcSpan       -- #define L0   L noSrcSpan

{-# INLINE sL1 #-}
sL1 :: Located a -> b -> Located b
sL1 x = sL (getLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sLL #-}
sLL :: Located a -> Located b -> c -> Located c
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

-- Hint about linear types
hintLinear :: MonadP m => SrcSpan -> m ()
hintLinear span = do
  linearEnabled <- getBit LinearTypesBit
  unless linearEnabled $ addError $ PsError PsErrLinearFunction [] span

-- Does this look like (a %m)?
looksLikeMult :: LHsType GhcPs -> Located RdrName -> LHsType GhcPs -> Bool
looksLikeMult ty1 l_op ty2
  | Unqual op_name <- unLoc l_op
  , occNameFS op_name == fsLit "%"
  , Just ty1_pos <- getBufSpan (getLoc ty1)
  , Just pct_pos <- getBufSpan (getLoc l_op)
  , Just ty2_pos <- getBufSpan (getLoc ty2)
  , bufSpanEnd ty1_pos /= bufSpanStart pct_pos
  , bufSpanEnd pct_pos == bufSpanStart ty2_pos
  = True
  | otherwise = False

-- Hint about the MultiWayIf extension
hintMultiWayIf :: SrcSpan -> P ()
hintMultiWayIf span = do
  mwiEnabled <- getBit MultiWayIfBit
  unless mwiEnabled $ addError $ PsError PsErrMultiWayIf [] span

-- Hint about explicit-forall
hintExplicitForall :: Located Token -> P ()
hintExplicitForall tok = do
    forall   <- getBit ExplicitForallBit
    rulePrag <- getBit InRulePragBit
    unless (forall || rulePrag) $ addError $ PsError (PsErrExplicitForall (isUnicode tok)) [] (getLoc tok)

-- Hint about qualified-do
hintQualifiedDo :: Located Token -> P ()
hintQualifiedDo tok = do
    qualifiedDo   <- getBit QualifiedDoBit
    case maybeQDoDoc of
      Just qdoDoc | not qualifiedDo ->
        addError $ PsError (PsErrIllegalQualifiedDo qdoDoc) [] (getLoc tok)
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
    addFatalError $ PsError (PsErrEmptyDoubleQuotes thQuotes) [] span

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************

For the general principles of the following routines, see Note [Api annotations]
in GHC.Parser.Annotation

-}

-- |Construct an AddAnn from the annotation keyword and the location
-- of the keyword itself
mj :: AnnKeywordId -> Located e -> AddAnn
mj a l = AddAnn a (gl l)


-- |Construct an AddAnn from the annotation keyword and the Located Token. If
-- the token has a unicode equivalent and this has been used, provide the
-- unicode variant of the annotation.
mu :: AnnKeywordId -> Located Token -> AddAnn
mu a lt@(L l t) = AddAnn (toUnicodeAnn a lt) l

-- | If the 'Token' is using its unicode variant return the unicode variant of
--   the annotation
toUnicodeAnn :: AnnKeywordId -> Located Token -> AnnKeywordId
toUnicodeAnn a t = if isUnicode t then unicodeAnn a else a

toUnicode :: Located Token -> IsUnicodeSyntax
toUnicode t = if isUnicode t then UnicodeSyntax else NormalSyntax

gl :: Located a -> SrcSpan
gl = getLoc

-- |Add an annotation to the located element, and return the located
-- element as a pass through
aa :: Located a -> (AnnKeywordId, Located c) -> P (Located a)
aa a@(L l _) (b,s) = addAnnotation l b (gl s) >> return a

-- |Add an annotation to a located element resulting from a monadic action
am :: P (Located a) -> (AnnKeywordId, Located b) -> P (Located a)
am a (b,s) = do
  av@(L l _) <- a
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
ams :: MonadP m => Located a -> [AddAnn] -> m (Located a)
ams a@(L l _) bs = addAnnsAt l bs >> return a

amsL :: SrcSpan -> [AddAnn] -> P ()
amsL sp bs = addAnnsAt sp bs >> return ()

-- |Add all [AddAnn] to an AST element, and wrap it in a 'Just'
ajs :: MonadP m => Located a -> [AddAnn] -> m (Maybe (Located a))
ajs a bs = Just <$> ams a bs

-- |Add a list of AddAnns to the given AST element, where the AST element is the
--  result of a monadic action
amms :: MonadP m => m (Located a) -> [AddAnn] -> m (Located a)
amms a bs = do { av@(L l _) <- a
               ; addAnnsAt l bs
               ; return av }

-- |Add a list of AddAnns to the AST element, and return the element as a
--  OrdList
amsu :: Located a -> [AddAnn] -> P (OrdList (Located a))
amsu a@(L l _) bs = addAnnsAt l bs >> return (unitOL a)

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
mcommas = map (AddAnn AnnCommaTuple)

-- |Given a list of the locations of '|'s, provide a [AddAnn] with an AnnVbar
--  entry for each SrcSpan
mvbars :: [SrcSpan] -> [AddAnn]
mvbars = map (AddAnn AnnVbar)

-- |Get the location of the last element of a OrdList, or noSrcSpan
oll :: OrdList (Located a) -> SrcSpan
oll l =
  if isNilOL l then noSrcSpan
               else getLoc (lastOL l)

-- |Add a semicolon annotation in the right place in a list. If the
-- leading list is empty, add it to the tail
asl :: [Located a] -> Located b -> Located a -> P ()
asl [] (L ls _) (L l _) = addAnnotation l          AnnSemi ls
asl (x:_xs) (L ls _) _x = addAnnotation (getLoc x) AnnSemi ls

-- | Parse a Haskell module with Haddock comments.
-- This is done in two steps:
--
-- * 'parseModuleNoHaddock' to build the AST
-- * 'addHaddockToModule' to insert Haddock comments into it
--
-- This is the only parser entry point that deals with Haddock comments.
-- The other entry points ('parseDeclaration', 'parseExpression', etc) do
-- not insert them into the AST.
parseModule :: P (Located HsModule)
parseModule = parseModuleNoHaddock >>= addHaddockToModule
}
