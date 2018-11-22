-----------------------------------------------------------------------------
-- (c) The University of Glasgow, 2006
--
-- GHC's lexer for Haskell 2010 [1].
--
-- This is a combination of an Alex-generated lexer [2] from a regex
-- definition, with some hand-coded bits. [3]
--
-- Completely accurate information about token-spans within the source
-- file is maintained.  Every token has a start and end RealSrcLoc
-- attached to it.
--
-- References:
-- [1] https://www.haskell.org/onlinereport/haskell2010/haskellch2.html
-- [2] http://www.haskell.org/alex/
-- [3] https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Parser
--
-----------------------------------------------------------------------------

--   ToDo / known bugs:
--    - parsing integers is a bit slow
--    - readRational is a bit slow
--
--   Known bugs, that were also in the previous version:
--    - M... should be 3 tokens, not 1.
--    - pragma-end should be only valid in a pragma

--   qualified operator NOTES.
--
--   - If M.(+) is a single lexeme, then..
--     - Probably (+) should be a single lexeme too, for consistency.
--       Otherwise ( + ) would be a prefix operator, but M.( + ) would not be.
--     - But we have to rule out reserved operators, otherwise (..) becomes
--       a different lexeme.
--     - Should we therefore also rule out reserved operators in the qualified
--       form?  This is quite difficult to achieve.  We don't do it for
--       qualified varids.


-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment top"

{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Lexer (
   Token(..), lexer, pragState, mkPState, mkPStatePure, PState(..),
   P(..), ParseResult(..), mkParserFlags, ParserFlags(..), getRealSrcLoc,
   getPState, extopt, withThisPackage,
   failLocMsgP, failSpanMsgP, srcParseFail,
   getMessages,
   popContext, pushModuleContext, setLastToken, setSrcLoc,
   activeContext, nextIsEOF,
   getLexState, popLexState, pushLexState,
   extension, bangPatEnabled, datatypeContextsEnabled,
   traditionalRecordSyntaxEnabled,
   explicitForallEnabled,
   inRulePrag,
   explicitNamespacesEnabled,
   patternSynonymsEnabled,
   sccProfilingOn, hpcEnabled,
   starIsTypeEnabled,
   addWarning,
   lexTokenStream,
   addAnnotation,AddAnn,addAnnsAt,mkParensApiAnn,
   commentToAnnotation
  ) where

import GhcPrelude

-- base
import Control.Monad
import Control.Monad.Fail as MonadFail
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Word

import EnumSet (EnumSet)
import qualified EnumSet

-- ghc-boot
import qualified GHC.LanguageExtensions as LangExt

-- bytestring
import Data.ByteString (ByteString)

-- containers
import Data.Map (Map)
import qualified Data.Map as Map

-- compiler/utils
import Bag
import Outputable
import StringBuffer
import FastString
import UniqFM
import Util             ( readRational, readHexRational )

-- compiler/main
import ErrUtils
import DynFlags

-- compiler/basicTypes
import SrcLoc
import Module
import BasicTypes     ( InlineSpec(..), RuleMatchInfo(..),
                        IntegralLit(..), FractionalLit(..),
                        SourceText(..) )

-- compiler/parser
import Ctype

import ApiAnnotation
}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

-- NB: The logic behind these definitions is also reflected in basicTypes/Lexeme.hs
-- Any changes here should likely be reflected there.
$unispace    = \x05 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n -- TODO #8424
$tab         = \t

$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']

$unilarge  = \x01 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]

$unismall  = \x02 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]

$unigraphic = \x06 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$graphic   = [$small $large $symbol $digit $special $unigraphic \"\']

$binit     = 0-1
$octit     = 0-7
$hexit     = [$decdigit A-F a-f]

$uniidchar = \x07 -- Trick Alex into handling Unicode. See [Unicode in Alex].
$idchar    = [$small $large $digit $uniidchar \']

$pragmachar = [$small $large $digit]

$docsym    = [\| \^ \* \$]


-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@varid     = $small $idchar*          -- variable identifiers
@conid     = $large $idchar*          -- constructor identifiers

@varsym    = ($symbol # \:) $symbol*  -- variable (operator) symbol
@consym    = \: $symbol*              -- constructor (operator) symbol

-- See Note [Lexing NumericUnderscores extension] and #14473
@numspc       = _*                   -- numeric spacer (#14473)
@decimal      = $decdigit(@numspc $decdigit)*
@binary       = $binit(@numspc $binit)*
@octal        = $octit(@numspc $octit)*
@hexadecimal  = $hexit(@numspc $hexit)*
@exponent     = @numspc [eE] [\-\+]? @decimal
@bin_exponent = @numspc [pP] [\-\+]? @decimal

@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid
@qvarsym = @qual @varsym
@qconsym = @qual @consym

@floating_point = @numspc @decimal \. @decimal @exponent? | @numspc @decimal @exponent
@hex_floating_point = @numspc @hexadecimal \. @hexadecimal @bin_exponent? | @numspc @hexadecimal @bin_exponent

-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-
@signed = @negative ?


-- -----------------------------------------------------------------------------
-- Alex "Identifier"

haskell :-


-- -----------------------------------------------------------------------------
-- Alex "Rules"

-- everywhere: skip whitespace
$white_no_nl+ ;
$tab          { warnTab }

-- Everywhere: deal with nested comments.  We explicitly rule out
-- pragmas, "{-#", so that we don't accidentally treat them as comments.
-- (this can happen even though pragmas will normally take precedence due to
-- longest-match, because pragmas aren't valid in every state, but comments
-- are). We also rule out nested Haddock comments, if the -haddock flag is
-- set.

"{-" / { isNormalComment } { nested_comment lexToken }

-- Single-line comments are a bit tricky.  Haskell 98 says that two or
-- more dashes followed by a symbol should be parsed as a varsym, so we
-- have to exclude those.

-- Since Haddock comments aren't valid in every state, we need to rule them
-- out here.

-- The following two rules match comments that begin with two dashes, but
-- continue with a different character. The rules test that this character
-- is not a symbol (in which case we'd have a varsym), and that it's not a
-- space followed by a Haddock comment symbol (docsym) (in which case we'd
-- have a Haddock comment). The rules then munch the rest of the line.

"-- " ~$docsym .* { lineCommentToken }
"--" [^$symbol \ ] .* { lineCommentToken }

-- Next, match Haddock comments if no -haddock flag

"-- " $docsym .* / { ifExtension (not . haddockEnabled) } { lineCommentToken }

-- Now, when we've matched comments that begin with 2 dashes and continue
-- with a different character, we need to match comments that begin with three
-- or more dashes (which clearly can't be Haddock comments). We only need to
-- make sure that the first non-dash character isn't a symbol, and munch the
-- rest of the line.

"---"\-* ~$symbol .* { lineCommentToken }

-- Since the previous rules all match dashes followed by at least one
-- character, we also need to match a whole line filled with just dashes.

"--"\-* / { atEOL } { lineCommentToken }

-- We need this rule since none of the other single line comment rules
-- actually match this case.

"-- " / { atEOL } { lineCommentToken }

-- 'bol' state: beginning of a line.  Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
--
-- One slight wibble here: what if the line begins with {-#? In
-- theory, we have to lex the pragma to see if it's one we recognise,
-- and if it is, then we backtrack and do_bol, otherwise we treat it
-- as a nested comment.  We don't bother with this: if the line begins
-- with {-#, then we'll assume it's a pragma we know about and go for do_bol.
<bol> {
  \n                                    ;
  ^\# line                              { begin line_prag1 }
  ^\# / { followedByDigit }             { begin line_prag1 }
  ^\# pragma .* \n                      ; -- GCC 3.3 CPP generated, apparently
  ^\# \! .* \n                          ; -- #!, for scripts
  ()                                    { do_bol }
}

-- after a layout keyword (let, where, do, of), we begin a new layout
-- context if the curly brace is missing.
-- Careful! This stuff is quite delicate.
<layout, layout_do, layout_if> {
  \{ / { notFollowedBy '-' }            { hopefully_open_brace }
        -- we might encounter {-# here, but {- has been handled already
  \n                                    ;
  ^\# (line)?                           { begin line_prag1 }
}

-- after an 'if', a vertical bar starts a layout context for MultiWayIf
<layout_if> {
  \| / { notFollowedBySymbol }          { new_layout_context True dontGenerateSemic ITvbar }
  ()                                    { pop }
}

-- do is treated in a subtly different way, see new_layout_context
<layout>    ()                          { new_layout_context True  generateSemic ITvocurly }
<layout_do> ()                          { new_layout_context False generateSemic ITvocurly }

-- after a new layout context which was found to be to the left of the
-- previous context, we have generated a '{' token, and we now need to
-- generate a matching '}' token.
<layout_left>  ()                       { do_layout_left }

<0,option_prags> \n                     { begin bol }

"{-#" $whitechar* $pragmachar+ / { known_pragma linePrags }
                                { dispatch_pragmas linePrags }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag1> {
  @decimal $white_no_nl+ \" [$graphic \ ]* \"  { setLineAndFile line_prag1a }
  ()                                           { failLinePrag1 }
}
<line_prag1a> .*                               { popLinePrag1 }

-- Haskell-style line pragmas, of the form
--    {-# LINE <line> "<file>" #-}
<line_prag2> {
  @decimal $white_no_nl+ \" [$graphic \ ]* \"  { setLineAndFile line_prag2a }
}
<line_prag2a> "#-}"|"-}"                       { pop }
   -- NOTE: accept -} at the end of a LINE pragma, for compatibility
   -- with older versions of GHC which generated these.

-- Haskell-style column pragmas, of the form
--    {-# COLUMN <column> #-}
<column_prag> @decimal $whitechar* "#-}" { setColumn }

<0,option_prags> {
  "{-#" $whitechar* $pragmachar+
        $whitechar+ $pragmachar+ / { known_pragma twoWordPrags }
                                 { dispatch_pragmas twoWordPrags }

  "{-#" $whitechar* $pragmachar+ / { known_pragma oneWordPrags }
                                 { dispatch_pragmas oneWordPrags }

  -- We ignore all these pragmas, but don't generate a warning for them
  "{-#" $whitechar* $pragmachar+ / { known_pragma ignoredPrags }
                                 { dispatch_pragmas ignoredPrags }

  -- ToDo: should only be valid inside a pragma:
  "#-}"                          { endPrag }
}

<option_prags> {
  "{-#"  $whitechar* $pragmachar+ / { known_pragma fileHeaderPrags }
                                   { dispatch_pragmas fileHeaderPrags }
}

<0> {
  -- In the "0" mode we ignore these pragmas
  "{-#"  $whitechar* $pragmachar+ / { known_pragma fileHeaderPrags }
                     { nested_comment lexToken }
}

<0,option_prags> {
  "{-#"  { warnThen Opt_WarnUnrecognisedPragmas (text "Unrecognised pragma")
                    (nested_comment lexToken) }
}

-- '0' state: ordinary lexemes

-- Haddock comments

<0,option_prags> {
  "-- " $docsym      / { ifExtension haddockEnabled } { multiline_doc_comment }
  "{-" \ ? $docsym   / { ifExtension haddockEnabled } { nested_doc_comment }
}

-- "special" symbols

<0> {
  "[|"        / { ifExtension thQuotesEnabled } { token (ITopenExpQuote NoE
                                                                NormalSyntax) }
  "[||"       / { ifExtension thQuotesEnabled } { token (ITopenTExpQuote NoE) }
  "[e|"       / { ifExtension thQuotesEnabled } { token (ITopenExpQuote HasE
                                                                NormalSyntax) }
  "[e||"      / { ifExtension thQuotesEnabled } { token (ITopenTExpQuote HasE) }
  "[p|"       / { ifExtension thQuotesEnabled } { token ITopenPatQuote }
  "[d|"       / { ifExtension thQuotesEnabled } { layout_token ITopenDecQuote }
  "[t|"       / { ifExtension thQuotesEnabled } { token ITopenTypQuote }
  "|]"        / { ifExtension thQuotesEnabled } { token (ITcloseQuote
                                                                NormalSyntax) }
  "||]"       / { ifExtension thQuotesEnabled } { token ITcloseTExpQuote }
  \$ @varid   / { ifExtension thEnabled } { skip_one_varid ITidEscape }
  "$$" @varid / { ifExtension thEnabled } { skip_two_varid ITidTyEscape }
  "$("        / { ifExtension thEnabled } { token ITparenEscape }
  "$$("       / { ifExtension thEnabled } { token ITparenTyEscape }

  "[" @varid "|"  / { ifExtension qqEnabled }
                     { lex_quasiquote_tok }

  -- qualified quasi-quote (#5555)
  "[" @qvarid "|"  / { ifExtension qqEnabled }
                     { lex_qquasiquote_tok }

  $unigraphic -- ⟦
    / { ifCurrentChar '⟦' `alexAndPred`
        ifExtension (\i -> unicodeSyntaxEnabled i && thQuotesEnabled i) }
    { token (ITopenExpQuote NoE UnicodeSyntax) }
  $unigraphic -- ⟧
    / { ifCurrentChar '⟧' `alexAndPred`
        ifExtension (\i -> unicodeSyntaxEnabled i && thQuotesEnabled i) }
    { token (ITcloseQuote UnicodeSyntax) }
}

  -- See Note [Lexing type applications]
<0> {
    [^ $idchar \) ] ^
  "@"
    / { ifExtension typeApplicationEnabled `alexAndPred` notFollowedBySymbol }
    { token ITtypeApp }
}

<0> {
  "(|" / { ifExtension arrowsEnabled `alexAndPred` notFollowedBySymbol }
                                        { special (IToparenbar NormalSyntax) }
  "|)" / { ifExtension arrowsEnabled }  { special (ITcparenbar NormalSyntax) }

  $unigraphic -- ⦇
    / { ifCurrentChar '⦇' `alexAndPred`
        ifExtension (\i -> unicodeSyntaxEnabled i && arrowsEnabled i) }
    { special (IToparenbar UnicodeSyntax) }
  $unigraphic -- ⦈
    / { ifCurrentChar '⦈' `alexAndPred`
        ifExtension (\i -> unicodeSyntaxEnabled i && arrowsEnabled i) }
    { special (ITcparenbar UnicodeSyntax) }
}

<0> {
  \? @varid / { ifExtension ipEnabled } { skip_one_varid ITdupipvarid }
}

<0> {
  "#" @varid / { ifExtension overloadedLabelsEnabled }
               { skip_one_varid ITlabelvarid }
}

<0> {
  "(#" / { orExtensions unboxedTuplesEnabled unboxedSumsEnabled }
         { token IToubxparen }
  "#)" / { orExtensions unboxedTuplesEnabled unboxedSumsEnabled }
         { token ITcubxparen }
}

<0,option_prags> {
  \(                                    { special IToparen }
  \)                                    { special ITcparen }
  \[                                    { special ITobrack }
  \]                                    { special ITcbrack }
  \,                                    { special ITcomma }
  \;                                    { special ITsemi }
  \`                                    { special ITbackquote }

  \{                                    { open_brace }
  \}                                    { close_brace }
}

<0,option_prags> {
  @qvarid                       { idtoken qvarid }
  @qconid                       { idtoken qconid }
  @varid                        { varid }
  @conid                        { idtoken conid }
}

<0> {
  @qvarid "#"+      / { ifExtension magicHashEnabled } { idtoken qvarid }
  @qconid "#"+      / { ifExtension magicHashEnabled } { idtoken qconid }
  @varid "#"+       / { ifExtension magicHashEnabled } { varid }
  @conid "#"+       / { ifExtension magicHashEnabled } { idtoken conid }
}

-- ToDo: - move `var` and (sym) into lexical syntax?
--       - remove backquote from $special?
<0> {
  @qvarsym                                         { idtoken qvarsym }
  @qconsym                                         { idtoken qconsym }
  @varsym                                          { varsym }
  @consym                                          { consym }
}

-- For the normal boxed literals we need to be careful
-- when trying to be close to Haskell98

-- Note [Lexing NumericUnderscores extension] (#14473)
--
-- NumericUnderscores extension allows underscores in numeric literals.
-- Multiple underscores are represented with @numspc macro.
-- To be simpler, we have only the definitions with underscores.
-- And then we have a separate function (tok_integral and tok_frac)
-- that validates the literals.
-- If extensions are not enabled, check that there are no underscores.
--
<0> {
  -- Normal integral literals (:: Num a => a, from Integer)
  @decimal                                                               { tok_num positive 0 0 decimal }
  0[bB] @numspc @binary        / { ifExtension binaryLiteralsEnabled }   { tok_num positive 2 2 binary }
  0[oO] @numspc @octal                                                   { tok_num positive 2 2 octal }
  0[xX] @numspc @hexadecimal                                             { tok_num positive 2 2 hexadecimal }
  @negative @decimal           / { ifExtension negativeLiteralsEnabled } { tok_num negative 1 1 decimal }
  @negative 0[bB] @numspc @binary  / { ifExtension negativeLiteralsEnabled `alexAndPred`
                                       ifExtension binaryLiteralsEnabled }   { tok_num negative 3 3 binary }
  @negative 0[oO] @numspc @octal   / { ifExtension negativeLiteralsEnabled } { tok_num negative 3 3 octal }
  @negative 0[xX] @numspc @hexadecimal / { ifExtension negativeLiteralsEnabled } { tok_num negative 3 3 hexadecimal }

  -- Normal rational literals (:: Fractional a => a, from Rational)
  @floating_point                                                        { tok_frac 0 tok_float }
  @negative @floating_point    / { ifExtension negativeLiteralsEnabled } { tok_frac 0 tok_float }
  0[xX] @numspc @hex_floating_point     / { ifExtension hexFloatLiteralsEnabled } { tok_frac 0 tok_hex_float }
  @negative 0[xX] @numspc @hex_floating_point / { ifExtension hexFloatLiteralsEnabled `alexAndPred`
                                                  ifExtension negativeLiteralsEnabled } { tok_frac 0 tok_hex_float }
}

<0> {
  -- Unboxed ints (:: Int#) and words (:: Word#)
  -- It's simpler (and faster?) to give separate cases to the negatives,
  -- especially considering octal/hexadecimal prefixes.
  @decimal                     \# / { ifExtension magicHashEnabled } { tok_primint positive 0 1 decimal }
  0[bB] @numspc @binary        \# / { ifExtension magicHashEnabled `alexAndPred`
                                      ifExtension binaryLiteralsEnabled } { tok_primint positive 2 3 binary }
  0[oO] @numspc @octal         \# / { ifExtension magicHashEnabled } { tok_primint positive 2 3 octal }
  0[xX] @numspc @hexadecimal   \# / { ifExtension magicHashEnabled } { tok_primint positive 2 3 hexadecimal }
  @negative @decimal           \# / { ifExtension magicHashEnabled } { tok_primint negative 1 2 decimal }
  @negative 0[bB] @numspc @binary  \# / { ifExtension magicHashEnabled `alexAndPred`
                                          ifExtension binaryLiteralsEnabled } { tok_primint negative 3 4 binary }
  @negative 0[oO] @numspc @octal   \# / { ifExtension magicHashEnabled } { tok_primint negative 3 4 octal }
  @negative 0[xX] @numspc @hexadecimal \# / { ifExtension magicHashEnabled } { tok_primint negative 3 4 hexadecimal }

  @decimal                     \# \# / { ifExtension magicHashEnabled } { tok_primword 0 2 decimal }
  0[bB] @numspc @binary        \# \# / { ifExtension magicHashEnabled `alexAndPred`
                                         ifExtension binaryLiteralsEnabled } { tok_primword 2 4 binary }
  0[oO] @numspc @octal         \# \# / { ifExtension magicHashEnabled } { tok_primword 2 4 octal }
  0[xX] @numspc @hexadecimal   \# \# / { ifExtension magicHashEnabled } { tok_primword 2 4 hexadecimal }

  -- Unboxed floats and doubles (:: Float#, :: Double#)
  -- prim_{float,double} work with signed literals
  @signed @floating_point \# / { ifExtension magicHashEnabled } { tok_frac 1 tok_primfloat }
  @signed @floating_point \# \# / { ifExtension magicHashEnabled } { tok_frac 2 tok_primdouble }
}

-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0> {
  \'                            { lex_char_tok }
  \"                            { lex_string_tok }
}

-- Note [Lexing type applications]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The desired syntax for type applications is to prefix the type application
-- with '@', like this:
--
--   foo @Int @Bool baz bum
--
-- This, of course, conflicts with as-patterns. The conflict arises because
-- expressions and patterns use the same parser, and also because we want
-- to allow type patterns within expression patterns.
--
-- Disambiguation is accomplished by requiring *something* to appear between
-- type application and the preceding token. This something must end with
-- a character that cannot be the end of the variable bound in an as-pattern.
-- Currently (June 2015), this means that the something cannot end with a
-- $idchar or a close-paren. (The close-paren is necessary if the as-bound
-- identifier is symbolic.)
--
-- Note that looking for whitespace before the '@' is insufficient, because
-- of this pathological case:
--
--   foo {- hi -}@Int
--
-- This design is predicated on the fact that as-patterns are generally
-- whitespace-free, and also that this whole thing is opt-in, with the
-- TypeApplications extension.

-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{

-- -----------------------------------------------------------------------------
-- The token type

data Token
  = ITas                        -- Haskell keywords
  | ITcase
  | ITclass
  | ITdata
  | ITdefault
  | ITderiving
  | ITdo
  | ITelse
  | IThiding
  | ITforeign
  | ITif
  | ITimport
  | ITin
  | ITinfix
  | ITinfixl
  | ITinfixr
  | ITinstance
  | ITlet
  | ITmodule
  | ITnewtype
  | ITof
  | ITqualified
  | ITthen
  | ITtype
  | ITwhere

  | ITforall            IsUnicodeSyntax -- GHC extension keywords
  | ITexport
  | ITlabel
  | ITdynamic
  | ITsafe
  | ITinterruptible
  | ITunsafe
  | ITstdcallconv
  | ITccallconv
  | ITcapiconv
  | ITprimcallconv
  | ITjavascriptcallconv
  | ITmdo
  | ITfamily
  | ITrole
  | ITgroup
  | ITby
  | ITusing
  | ITpattern
  | ITstatic
  | ITstock
  | ITanyclass
  | ITvia

  -- Backpack tokens
  | ITunit
  | ITsignature
  | ITdependency
  | ITrequires

  -- Pragmas, see  note [Pragma source text] in BasicTypes
  | ITinline_prag       SourceText InlineSpec RuleMatchInfo
  | ITspec_prag         SourceText                -- SPECIALISE
  | ITspec_inline_prag  SourceText Bool    -- SPECIALISE INLINE (or NOINLINE)
  | ITsource_prag       SourceText
  | ITrules_prag        SourceText
  | ITwarning_prag      SourceText
  | ITdeprecated_prag   SourceText
  | ITline_prag         SourceText  -- not usually produced, see 'use_pos_prags'
  | ITcolumn_prag       SourceText  -- not usually produced, see 'use_pos_prags'
  | ITscc_prag          SourceText
  | ITgenerated_prag    SourceText
  | ITcore_prag         SourceText         -- hdaume: core annotations
  | ITunpack_prag       SourceText
  | ITnounpack_prag     SourceText
  | ITann_prag          SourceText
  | ITcomplete_prag     SourceText
  | ITclose_prag
  | IToptions_prag String
  | ITinclude_prag String
  | ITlanguage_prag
  | ITminimal_prag      SourceText
  | IToverlappable_prag SourceText  -- instance overlap mode
  | IToverlapping_prag  SourceText  -- instance overlap mode
  | IToverlaps_prag     SourceText  -- instance overlap mode
  | ITincoherent_prag   SourceText  -- instance overlap mode
  | ITctype             SourceText
  | ITcomment_line_prag         -- See Note [Nested comment line pragmas]

  | ITdotdot                    -- reserved symbols
  | ITcolon
  | ITdcolon            IsUnicodeSyntax
  | ITequal
  | ITlam
  | ITlcase
  | ITvbar
  | ITlarrow            IsUnicodeSyntax
  | ITrarrow            IsUnicodeSyntax
  | ITat
  | ITtilde
  | ITdarrow            IsUnicodeSyntax
  | ITminus
  | ITbang
  | ITstar              IsUnicodeSyntax
  | ITdot

  | ITbiglam                    -- GHC-extension symbols

  | ITocurly                    -- special symbols
  | ITccurly
  | ITvocurly
  | ITvccurly
  | ITobrack
  | ITopabrack                  -- [:, for parallel arrays with -XParallelArrays
  | ITcpabrack                  -- :], for parallel arrays with -XParallelArrays
  | ITcbrack
  | IToparen
  | ITcparen
  | IToubxparen
  | ITcubxparen
  | ITsemi
  | ITcomma
  | ITunderscore
  | ITbackquote
  | ITsimpleQuote               --  '

  | ITvarid   FastString        -- identifiers
  | ITconid   FastString
  | ITvarsym  FastString
  | ITconsym  FastString
  | ITqvarid  (FastString,FastString)
  | ITqconid  (FastString,FastString)
  | ITqvarsym (FastString,FastString)
  | ITqconsym (FastString,FastString)

  | ITdupipvarid   FastString   -- GHC extension: implicit param: ?x
  | ITlabelvarid   FastString   -- Overloaded label: #x

  | ITchar     SourceText Char       -- Note [Literal source text] in BasicTypes
  | ITstring   SourceText FastString -- Note [Literal source text] in BasicTypes
  | ITinteger  IntegralLit           -- Note [Literal source text] in BasicTypes
  | ITrational FractionalLit

  | ITprimchar   SourceText Char     -- Note [Literal source text] in BasicTypes
  | ITprimstring SourceText ByteString -- Note [Literal source text] @BasicTypes
  | ITprimint    SourceText Integer  -- Note [Literal source text] in BasicTypes
  | ITprimword   SourceText Integer  -- Note [Literal source text] in BasicTypes
  | ITprimfloat  FractionalLit
  | ITprimdouble FractionalLit

  -- Template Haskell extension tokens
  | ITopenExpQuote HasE IsUnicodeSyntax --  [| or [e|
  | ITopenPatQuote                      --  [p|
  | ITopenDecQuote                      --  [d|
  | ITopenTypQuote                      --  [t|
  | ITcloseQuote IsUnicodeSyntax        --  |]
  | ITopenTExpQuote HasE                --  [|| or [e||
  | ITcloseTExpQuote                    --  ||]
  | ITidEscape   FastString             --  $x
  | ITparenEscape                       --  $(
  | ITidTyEscape   FastString           --  $$x
  | ITparenTyEscape                     --  $$(
  | ITtyQuote                           --  ''
  | ITquasiQuote (FastString,FastString,RealSrcSpan)
    -- ITquasiQuote(quoter, quote, loc)
    -- represents a quasi-quote of the form
    -- [quoter| quote |]
  | ITqQuasiQuote (FastString,FastString,FastString,RealSrcSpan)
    -- ITqQuasiQuote(Qual, quoter, quote, loc)
    -- represents a qualified quasi-quote of the form
    -- [Qual.quoter| quote |]

  -- Arrow notation extension
  | ITproc
  | ITrec
  | IToparenbar  IsUnicodeSyntax --  (|
  | ITcparenbar  IsUnicodeSyntax --  |)
  | ITlarrowtail IsUnicodeSyntax --  -<
  | ITrarrowtail IsUnicodeSyntax --  >-
  | ITLarrowtail IsUnicodeSyntax --  -<<
  | ITRarrowtail IsUnicodeSyntax --  >>-

  -- type application '@' (lexed differently than as-pattern '@',
  -- due to checking for preceding whitespace)
  | ITtypeApp


  | ITunknown String            -- Used when the lexer can't make sense of it
  | ITeof                       -- end of file token

  -- Documentation annotations
  | ITdocCommentNext  String     -- something beginning '-- |'
  | ITdocCommentPrev  String     -- something beginning '-- ^'
  | ITdocCommentNamed String     -- something beginning '-- $'
  | ITdocSection      Int String -- a section heading
  | ITdocOptions      String     -- doc options (prune, ignore-exports, etc)
  | ITlineComment     String     -- comment starting by "--"
  | ITblockComment    String     -- comment in {- -}

  deriving Show

instance Outputable Token where
  ppr x = text (show x)


-- the bitmap provided as the third component indicates whether the
-- corresponding extension keyword is valid under the extension options
-- provided to the compiler; if the extension corresponding to *any* of the
-- bits set in the bitmap is enabled, the keyword is valid (this setup
-- facilitates using a keyword in two different extensions that can be
-- activated independently)
--
reservedWordsFM :: UniqFM (Token, ExtsBitmap)
reservedWordsFM = listToUFM $
    map (\(x, y, z) -> (mkFastString x, (y, z)))
        [( "_",              ITunderscore,    0 ),
         ( "as",             ITas,            0 ),
         ( "case",           ITcase,          0 ),
         ( "class",          ITclass,         0 ),
         ( "data",           ITdata,          0 ),
         ( "default",        ITdefault,       0 ),
         ( "deriving",       ITderiving,      0 ),
         ( "do",             ITdo,            0 ),
         ( "else",           ITelse,          0 ),
         ( "hiding",         IThiding,        0 ),
         ( "if",             ITif,            0 ),
         ( "import",         ITimport,        0 ),
         ( "in",             ITin,            0 ),
         ( "infix",          ITinfix,         0 ),
         ( "infixl",         ITinfixl,        0 ),
         ( "infixr",         ITinfixr,        0 ),
         ( "instance",       ITinstance,      0 ),
         ( "let",            ITlet,           0 ),
         ( "module",         ITmodule,        0 ),
         ( "newtype",        ITnewtype,       0 ),
         ( "of",             ITof,            0 ),
         ( "qualified",      ITqualified,     0 ),
         ( "then",           ITthen,          0 ),
         ( "type",           ITtype,          0 ),
         ( "where",          ITwhere,         0 ),

         ( "forall",         ITforall NormalSyntax,
                                              xbit ExplicitForallBit .|.
                                              xbit InRulePragBit),
         ( "mdo",            ITmdo,           xbit RecursiveDoBit),
             -- See Note [Lexing type pseudo-keywords]
         ( "family",         ITfamily,        0 ),
         ( "role",           ITrole,          0 ),
         ( "pattern",        ITpattern,       xbit PatternSynonymsBit),
         ( "static",         ITstatic,        0 ),
         ( "stock",          ITstock,         0 ),
         ( "anyclass",       ITanyclass,      0 ),
         ( "via",            ITvia,           0 ),
         ( "group",          ITgroup,         xbit TransformComprehensionsBit),
         ( "by",             ITby,            xbit TransformComprehensionsBit),
         ( "using",          ITusing,         xbit TransformComprehensionsBit),

         ( "foreign",        ITforeign,       xbit FfiBit),
         ( "export",         ITexport,        xbit FfiBit),
         ( "label",          ITlabel,         xbit FfiBit),
         ( "dynamic",        ITdynamic,       xbit FfiBit),
         ( "safe",           ITsafe,          xbit FfiBit .|.
                                              xbit SafeHaskellBit),
         ( "interruptible",  ITinterruptible, xbit InterruptibleFfiBit),
         ( "unsafe",         ITunsafe,        xbit FfiBit),
         ( "stdcall",        ITstdcallconv,   xbit FfiBit),
         ( "ccall",          ITccallconv,     xbit FfiBit),
         ( "capi",           ITcapiconv,      xbit CApiFfiBit),
         ( "prim",           ITprimcallconv,  xbit FfiBit),
         ( "javascript",     ITjavascriptcallconv, xbit FfiBit),

         ( "unit",           ITunit,          0 ),
         ( "dependency",     ITdependency,       0 ),
         ( "signature",      ITsignature,     0 ),

         ( "rec",            ITrec,           xbit ArrowsBit .|.
                                              xbit RecursiveDoBit),
         ( "proc",           ITproc,          xbit ArrowsBit)
     ]

{-----------------------------------
Note [Lexing type pseudo-keywords]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One might think that we wish to treat 'family' and 'role' as regular old
varids whenever -XTypeFamilies and -XRoleAnnotations are off, respectively.
But, there is no need to do so. These pseudo-keywords are not stolen syntax:
they are only used after the keyword 'type' at the top-level, where varids are
not allowed. Furthermore, checks further downstream (TcTyClsDecls) ensure that
type families and role annotations are never declared without their extensions
on. In fact, by unconditionally lexing these pseudo-keywords as special, we
can get better error messages.

Also, note that these are included in the `varid` production in the parser --
a key detail to make all this work.
-------------------------------------}

reservedSymsFM :: UniqFM (Token, ExtsBitmap -> Bool)
reservedSymsFM = listToUFM $
    map (\ (x,y,z) -> (mkFastString x,(y,z)))
      [ ("..",  ITdotdot,              always)
        -- (:) is a reserved op, meaning only list cons
       ,(":",   ITcolon,               always)
       ,("::",  ITdcolon NormalSyntax, always)
       ,("=",   ITequal,               always)
       ,("\\",  ITlam,                 always)
       ,("|",   ITvbar,                always)
       ,("<-",  ITlarrow NormalSyntax, always)
       ,("->",  ITrarrow NormalSyntax, always)
       ,("@",   ITat,                  always)
       ,("~",   ITtilde,               always)
       ,("=>",  ITdarrow NormalSyntax, always)
       ,("-",   ITminus,               always)
       ,("!",   ITbang,                always)

       ,("*", ITstar NormalSyntax, starIsTypeEnabled)

        -- For 'forall a . t'
       ,(".", ITdot,  always) -- \i -> explicitForallEnabled i || inRulePrag i)

       ,("-<",  ITlarrowtail NormalSyntax, arrowsEnabled)
       ,(">-",  ITrarrowtail NormalSyntax, arrowsEnabled)
       ,("-<<", ITLarrowtail NormalSyntax, arrowsEnabled)
       ,(">>-", ITRarrowtail NormalSyntax, arrowsEnabled)

       ,("∷",   ITdcolon UnicodeSyntax, unicodeSyntaxEnabled)
       ,("⇒",   ITdarrow UnicodeSyntax, unicodeSyntaxEnabled)
       ,("∀",   ITforall UnicodeSyntax, unicodeSyntaxEnabled)
       ,("→",   ITrarrow UnicodeSyntax, unicodeSyntaxEnabled)
       ,("←",   ITlarrow UnicodeSyntax, unicodeSyntaxEnabled)

       ,("⤙",   ITlarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤚",   ITrarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤛",   ITLarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤜",   ITRarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("★",   ITstar UnicodeSyntax,
                  \i -> unicodeSyntaxEnabled i && starIsTypeEnabled i)

        -- ToDo: ideally, → and ∷ should be "specials", so that they cannot
        -- form part of a large operator.  This would let us have a better
        -- syntax for kinds: ɑ∷*→* would be a legal kind signature. (maybe).
       ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

token, layout_token :: Token -> Action
token t span _buf _len = return (L span t)
layout_token t span _buf _len = pushLexState layout >> return (L span t)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

skip_one_varid :: (FastString -> Token) -> Action
skip_one_varid f span buf len
  = return (L span $! f (lexemeToFastString (stepOn buf) (len-1)))

skip_two_varid :: (FastString -> Token) -> Action
skip_two_varid f span buf len
  = return (L span $! f (lexemeToFastString (stepOn (stepOn buf)) (len-2)))

strtoken :: (String -> Token) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = do _ <- popLexState
                         lexToken
-- See Note [Nested comment line pragmas]
failLinePrag1 :: Action
failLinePrag1 span _buf _len = do
  b <- extension inNestedComment
  if b then return (L span ITcomment_line_prag)
       else lexError "lexical error in pragma"

-- See Note [Nested comment line pragmas]
popLinePrag1 :: Action
popLinePrag1 span _buf _len = do
  b <- extension inNestedComment
  if b then return (L span ITcomment_line_prag) else do
    _ <- popLexState
    lexToken

hopefully_open_brace :: Action
hopefully_open_brace span buf len
 = do relaxed <- extension relaxedLayout
      ctx <- getContext
      (AI l _) <- getInput
      let offset = srcLocCol l
          isOK = relaxed ||
                 case ctx of
                 Layout prev_off _ : _ -> prev_off < offset
                 _                     -> True
      if isOK then pop_and open_brace span buf len
              else failSpanMsgP (RealSrcSpan span) (text "Missing block")

pop_and :: Action -> Action
pop_and act span buf len = do _ <- popLexState
                              act span buf len

{-# INLINE nextCharIs #-}
nextCharIs :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIs buf p = not (atEnd buf) && p (currentChar buf)

{-# INLINE nextCharIsNot #-}
nextCharIsNot :: StringBuffer -> (Char -> Bool) -> Bool
nextCharIsNot buf p = not (nextCharIs buf p)

notFollowedBy :: Char -> AlexAccPred ExtsBitmap
notFollowedBy char _ _ _ (AI _ buf)
  = nextCharIsNot buf (== char)

notFollowedBySymbol :: AlexAccPred ExtsBitmap
notFollowedBySymbol _ _ _ (AI _ buf)
  = nextCharIsNot buf (`elem` "!#$%&*+./<=>?@\\^|-~")

followedByDigit :: AlexAccPred ExtsBitmap
followedByDigit _ _ _ (AI _ buf)
  = afterOptionalSpace buf (\b -> nextCharIs b (`elem` ['0'..'9']))

ifCurrentChar :: Char -> AlexAccPred ExtsBitmap
ifCurrentChar char _ (AI _ buf) _ _
  = nextCharIs buf (== char)

-- We must reject doc comments as being ordinary comments everywhere.
-- In some cases the doc comment will be selected as the lexeme due to
-- maximal munch, but not always, because the nested comment rule is
-- valid in all states, but the doc-comment rules are only valid in
-- the non-layout states.
isNormalComment :: AlexAccPred ExtsBitmap
isNormalComment bits _ _ (AI _ buf)
  | haddockEnabled bits = notFollowedByDocOrPragma
  | otherwise           = nextCharIsNot buf (== '#')
  where
    notFollowedByDocOrPragma
       = afterOptionalSpace buf (\b -> nextCharIsNot b (`elem` "|^*$#"))

afterOptionalSpace :: StringBuffer -> (StringBuffer -> Bool) -> Bool
afterOptionalSpace buf p
    = if nextCharIs buf (== ' ')
      then p (snd (nextChar buf))
      else p buf

atEOL :: AlexAccPred ExtsBitmap
atEOL _ _ _ (AI _ buf) = atEnd buf || currentChar buf == '\n'

ifExtension :: (ExtsBitmap -> Bool) -> AlexAccPred ExtsBitmap
ifExtension pred bits _ _ _ = pred bits

orExtensions :: (ExtsBitmap -> Bool) -> (ExtsBitmap -> Bool) -> AlexAccPred ExtsBitmap
orExtensions pred1 pred2 bits _ _ _ = pred1 bits || pred2 bits

multiline_doc_comment :: Action
multiline_doc_comment span buf _len = withLexedDocType (worker "")
  where
    worker commentAcc input docType checkNextLine = case alexGetChar' input of
      Just ('\n', input')
        | checkNextLine -> case checkIfCommentLine input' of
          Just input -> worker ('\n':commentAcc) input docType checkNextLine
          Nothing -> docCommentEnd input commentAcc docType buf span
        | otherwise -> docCommentEnd input commentAcc docType buf span
      Just (c, input) -> worker (c:commentAcc) input docType checkNextLine
      Nothing -> docCommentEnd input commentAcc docType buf span

    -- Check if the next line of input belongs to this doc comment as well.
    -- A doc comment continues onto the next line when the following
    -- conditions are met:
    --   * The line starts with "--"
    --   * The line doesn't start with "---".
    --   * The line doesn't start with "-- $", because that would be the
    --     start of a /new/ named haddock chunk (#10398).
    checkIfCommentLine :: AlexInput -> Maybe AlexInput
    checkIfCommentLine input = check (dropNonNewlineSpace input)
      where
        check input = do
          ('-', input) <- alexGetChar' input
          ('-', input) <- alexGetChar' input
          (c, after_c) <- alexGetChar' input
          case c of
            '-' -> Nothing
            ' ' -> case alexGetChar' after_c of
                     Just ('$', _) -> Nothing
                     _ -> Just input
            _   -> Just input

        dropNonNewlineSpace input = case alexGetChar' input of
          Just (c, input')
            | isSpace c && c /= '\n' -> dropNonNewlineSpace input'
            | otherwise -> input
          Nothing -> input

lineCommentToken :: Action
lineCommentToken span buf len = do
  b <- extension rawTokenStreamEnabled
  if b then strtoken ITlineComment span buf len else lexToken

{-
  nested comments require traversing by hand, they can't be parsed
  using regular expressions.
-}
nested_comment :: P (RealLocated Token) -> Action
nested_comment cont span buf len = do
  input <- getInput
  go (reverse $ lexemeToString buf len) (1::Int) input
  where
    go commentAcc 0 input = do
      setInput input
      b <- extension rawTokenStreamEnabled
      if b
        then docCommentEnd input commentAcc ITblockComment buf span
        else cont
    go commentAcc n input = case alexGetChar' input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('\125',input) -> go ('\125':'-':commentAcc) (n-1) input -- '}'
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar' input of  -- '{' char
        Nothing  -> errBrace input span
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
      -- See Note [Nested comment line pragmas]
      Just ('\n',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('#',_) -> do (parsedAcc,input) <- parseNestedPragma input
                           go (parsedAcc ++ '\n':commentAcc) n input
        Just (_,_)   -> go ('\n':commentAcc) n input
      Just (c,input) -> go (c:commentAcc) n input

nested_doc_comment :: Action
nested_doc_comment span buf _len = withLexedDocType (go "")
  where
    go commentAcc input docType _ = case alexGetChar' input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar' input of
        Nothing -> errBrace input span
        Just ('\125',input) ->
          docCommentEnd input commentAcc docType buf span
        Just (_,_) -> go ('-':commentAcc) input docType False
      Just ('\123', input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('-',input) -> do
          setInput input
          let cont = do input <- getInput; go commentAcc input docType False
          nested_comment cont span buf _len
        Just (_,_) -> go ('\123':commentAcc) input docType False
      -- See Note [Nested comment line pragmas]
      Just ('\n',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('#',_) -> do (parsedAcc,input) <- parseNestedPragma input
                           go (parsedAcc ++ '\n':commentAcc) input docType False
        Just (_,_)   -> go ('\n':commentAcc) input docType False
      Just (c,input) -> go (c:commentAcc) input docType False

-- See Note [Nested comment line pragmas]
parseNestedPragma :: AlexInput -> P (String,AlexInput)
parseNestedPragma input@(AI _ buf) = do
  origInput <- getInput
  setInput input
  setExts (.|. xbit InNestedCommentBit)
  pushLexState bol
  lt <- lexToken
  _ <- popLexState
  setExts (.&. complement (xbit InNestedCommentBit))
  postInput@(AI _ postBuf) <- getInput
  setInput origInput
  case unRealSrcSpan lt of
    ITcomment_line_prag -> do
      let bytes = byteDiff buf postBuf
          diff  = lexemeToString buf bytes
      return (reverse diff, postInput)
    lt' -> panic ("parseNestedPragma: unexpected token" ++ (show lt'))

{-
Note [Nested comment line pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to ignore cpp-preprocessor-generated #line pragmas if they were inside
nested comments.

Now, when parsing a nested comment, if we encounter a line starting with '#' we
call parseNestedPragma, which executes the following:
1. Save the current lexer input (loc, buf) for later
2. Set the current lexer input to the beginning of the line starting with '#'
3. Turn the 'InNestedComment' extension on
4. Push the 'bol' lexer state
5. Lex a token. Due to (2), (3), and (4), this should always lex a single line
   or less and return the ITcomment_line_prag token. This may set source line
   and file location if a #line pragma is successfully parsed
6. Restore lexer input and state to what they were before we did all this
7. Return control to the function parsing a nested comment, informing it of
   what the lexer parsed

Regarding (5) above:
Every exit from the 'bol' lexer state (do_bol, popLinePrag1, failLinePrag1)
checks if the 'InNestedComment' extension is set. If it is, that function will
return control to parseNestedPragma by returning the ITcomment_line_prag token.

See #314 for more background on the bug this fixes.
-}

withLexedDocType :: (AlexInput -> (String -> Token) -> Bool -> P (RealLocated Token))
                 -> P (RealLocated Token)
withLexedDocType lexDocComment = do
  input@(AI _ buf) <- getInput
  case prevChar buf ' ' of
    -- The `Bool` argument to lexDocComment signals whether or not the next
    -- line of input might also belong to this doc comment.
    '|' -> lexDocComment input ITdocCommentNext True
    '^' -> lexDocComment input ITdocCommentPrev True
    '$' -> lexDocComment input ITdocCommentNamed True
    '*' -> lexDocSection 1 input
    _ -> panic "withLexedDocType: Bad doc type"
 where
    lexDocSection n input = case alexGetChar' input of
      Just ('*', input) -> lexDocSection (n+1) input
      Just (_,   _)     -> lexDocComment input (ITdocSection n) False
      Nothing -> do setInput input; lexToken -- eof reached, lex it normally

-- RULES pragmas turn on the forall and '.' keywords, and we turn them
-- off again at the end of the pragma.
rulePrag :: Action
rulePrag span buf len = do
  setExts (.|. xbit InRulePragBit)
  let !src = lexemeToString buf len
  return (L span (ITrules_prag (SourceText src)))

-- When 'use_pos_prags' is not set, it is expected that we emit a token instead
-- of updating the position in 'PState'
linePrag :: Action
linePrag span buf len = do
  ps <- getPState
  if use_pos_prags ps
    then begin line_prag2 span buf len
    else let !src = lexemeToString buf len
         in return (L span (ITline_prag (SourceText src)))

-- When 'use_pos_prags' is not set, it is expected that we emit a token instead
-- of updating the position in 'PState'
columnPrag :: Action
columnPrag span buf len = do
  ps <- getPState
  let !src = lexemeToString buf len
  if use_pos_prags ps
    then begin column_prag span buf len
    else let !src = lexemeToString buf len
         in return (L span (ITcolumn_prag (SourceText src)))

endPrag :: Action
endPrag span _buf _len = do
  setExts (.&. complement (xbit InRulePragBit))
  return (L span ITclose_prag)

-- docCommentEnd
-------------------------------------------------------------------------------
-- This function is quite tricky. We can't just return a new token, we also
-- need to update the state of the parser. Why? Because the token is longer
-- than what was lexed by Alex, and the lexToken function doesn't know this, so
-- it writes the wrong token length to the parser state. This function is
-- called afterwards, so it can just update the state.

docCommentEnd :: AlexInput -> String -> (String -> Token) -> StringBuffer ->
                 RealSrcSpan -> P (RealLocated Token)
docCommentEnd input commentAcc docType buf span = do
  setInput input
  let (AI loc nextBuf) = input
      comment = reverse commentAcc
      span' = mkRealSrcSpan (realSrcSpanStart span) loc
      last_len = byteDiff buf nextBuf

  span `seq` setLastToken span' last_len
  return (L span' (docType comment))

errBrace :: AlexInput -> RealSrcSpan -> P a
errBrace (AI end _) span = failLocMsgP (realSrcSpanStart span) end "unterminated `{-'"

open_brace, close_brace :: Action
open_brace span _str _len = do
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L span ITocurly)
close_brace span _str _len = do
  popContext
  return (L span ITccurly)

qvarid, qconid :: StringBuffer -> Int -> Token
qvarid buf len = ITqvarid $! splitQualName buf len False
qconid buf len = ITqconid $! splitQualName buf len False

splitQualName :: StringBuffer -> Int -> Bool -> (FastString,FastString)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
splitQualName orig_buf len parens = split orig_buf orig_buf
  where
    split buf dot_buf
        | orig_buf `byteDiff` buf >= len  = done dot_buf
        | c == '.'                        = found_dot buf'
        | otherwise                       = split buf' dot_buf
      where
       (c,buf') = nextChar buf

    -- careful, we might get names like M....
    -- so, if the character after the dot is not upper-case, this is
    -- the end of the qualifier part.
    found_dot buf -- buf points after the '.'
        | isUpper c    = split buf' buf
        | otherwise    = done buf
      where
       (c,buf') = nextChar buf

    done dot_buf =
        (lexemeToFastString orig_buf (qual_size - 1),
         if parens -- Prelude.(+)
            then lexemeToFastString (stepOn dot_buf) (len - qual_size - 2)
            else lexemeToFastString dot_buf (len - qual_size))
      where
        qual_size = orig_buf `byteDiff` dot_buf

varid :: Action
varid span buf len =
  case lookupUFM reservedWordsFM fs of
    Just (ITcase, _) -> do
      lastTk <- getLastTk
      keyword <- case lastTk of
        Just ITlam -> do
          lambdaCase <- extension lambdaCaseEnabled
          if lambdaCase
            then return ITlcase
            else failMsgP "Illegal lambda-case (use -XLambdaCase)"
        _ -> return ITcase
      maybe_layout keyword
      return $ L span keyword
    Just (ITstatic, _) -> do
      staticPointers <- extension staticPointersEnabled
      if staticPointers
        then return $ L span ITstatic
        else return $ L span $ ITvarid fs
    Just (keyword, 0) -> do
      maybe_layout keyword
      return $ L span keyword
    Just (keyword, exts) -> do
      extsEnabled <- extension $ \i -> exts .&. i /= 0
      if extsEnabled
        then do
          maybe_layout keyword
          return $ L span keyword
        else
          return $ L span $ ITvarid fs
    Nothing ->
      return $ L span $ ITvarid fs
  where
    !fs = lexemeToFastString buf len

conid :: StringBuffer -> Int -> Token
conid buf len = ITconid $! lexemeToFastString buf len

qvarsym, qconsym :: StringBuffer -> Int -> Token
qvarsym buf len = ITqvarsym $! splitQualName buf len False
qconsym buf len = ITqconsym $! splitQualName buf len False

varsym, consym :: Action
varsym = sym ITvarsym
consym = sym ITconsym

sym :: (FastString -> Token) -> Action
sym con span buf len =
  case lookupUFM reservedSymsFM fs of
    Just (keyword, exts) -> do
      extsEnabled <- extension exts
      let !tk | extsEnabled = keyword
              | otherwise   = con fs
      return $ L span tk
    Nothing ->
      return $ L span $! con fs
  where
    !fs = lexemeToFastString buf len

-- Variations on the integral numeric literal.
tok_integral :: (SourceText -> Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len = do
  numericUnderscores <- extension numericUnderscoresEnabled  -- #14473
  let src = lexemeToString buf len
  if (not numericUnderscores) && ('_' `elem` src)
    then failMsgP "Use NumericUnderscores to allow underscores in integer literals"
    else return $ L span $ itint (SourceText src)
       $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral $ \case
    st@(SourceText ('-':_)) -> itint st (const True)
    st@(SourceText _)       -> itint st (const False)
    st@NoSourceText         -> itint st (< 0)
  where
    itint :: SourceText -> (Integer -> Bool) -> Integer -> Token
    itint !st is_negative !val = ITinteger ((IL st $! is_negative val) val)

tok_primint :: (Integer -> Integer)
            -> Int -> Int
            -> (Integer, (Char->Int)) -> Action
tok_primint = tok_integral ITprimint


tok_primword :: Int -> Int
             -> (Integer, (Char->Int)) -> Action
tok_primword = tok_integral ITprimword positive
positive, negative :: (Integer -> Integer)
positive = id
negative = negate
decimal, octal, hexadecimal :: (Integer, Char -> Int)
decimal = (10,octDecDigit)
binary = (2,octDecDigit)
octal = (8,octDecDigit)
hexadecimal = (16,hexDigit)

-- readRational can understand negative rationals, exponents, everything.
tok_frac :: Int -> (String -> Token) -> Action
tok_frac drop f span buf len = do
  numericUnderscores <- extension numericUnderscoresEnabled  -- #14473
  let src = lexemeToString buf (len-drop)
  if (not numericUnderscores) && ('_' `elem` src)
    then failMsgP "Use NumericUnderscores to allow underscores in floating literals"
    else return (L span $! (f $! src))

tok_float, tok_primfloat, tok_primdouble :: String -> Token
tok_float        str = ITrational   $! readFractionalLit str
tok_hex_float    str = ITrational   $! readHexFractionalLit str
tok_primfloat    str = ITprimfloat  $! readFractionalLit str
tok_primdouble   str = ITprimdouble $! readFractionalLit str

readFractionalLit :: String -> FractionalLit
readFractionalLit str = ((FL $! (SourceText str)) $! is_neg) $! readRational str
                        where is_neg = case str of ('-':_) -> True
                                                   _       -> False
readHexFractionalLit :: String -> FractionalLit
readHexFractionalLit str =
  FL { fl_text  = SourceText str
     , fl_neg   = case str of
                    '-' : _ -> True
                    _       -> False
     , fl_value = readHexRational str
     }

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len = do
        -- See Note [Nested comment line pragmas]
        b <- extension inNestedComment
        if b then return (L span ITcomment_line_prag) else do
          (pos, gen_semic) <- getOffside
          case pos of
              LT -> do
                  --trace "layout: inserting '}'" $ do
                  popContext
                  -- do NOT pop the lex state, we might have a ';' to insert
                  return (L span ITvccurly)
              EQ | gen_semic -> do
                  --trace "layout: inserting ';'" $ do
                  _ <- popLexState
                  return (L span ITsemi)
              _ -> do
                  _ <- popLexState
                  lexToken

-- certain keywords put us in the "layout" state, where we might
-- add an opening curly brace.
maybe_layout :: Token -> P ()
maybe_layout t = do -- If the alternative layout rule is enabled then
                    -- we never create an implicit layout context here.
                    -- Layout is handled XXX instead.
                    -- The code for closing implicit contexts, or
                    -- inserting implicit semi-colons, is therefore
                    -- irrelevant as it only applies in an implicit
                    -- context.
                    alr <- extension alternativeLayoutRule
                    unless alr $ f t
    where f ITdo    = pushLexState layout_do
          f ITmdo   = pushLexState layout_do
          f ITof    = pushLexState layout
          f ITlcase = pushLexState layout
          f ITlet   = pushLexState layout
          f ITwhere = pushLexState layout
          f ITrec   = pushLexState layout
          f ITif    = pushLexState layout_if
          f _       = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
new_layout_context :: Bool -> Bool -> Token -> Action
new_layout_context strict gen_semic tok span _buf len = do
    _ <- popLexState
    (AI l _) <- getInput
    let offset = srcLocCol l - len
    ctx <- getContext
    nondecreasing <- extension nondecreasingIndentation
    let strict' = strict || not nondecreasing
    case ctx of
        Layout prev_off _ : _  |
           (strict'     && prev_off >= offset  ||
            not strict' && prev_off > offset) -> do
                -- token is indented to the left of the previous context.
                -- we must generate a {} sequence now.
                pushLexState layout_left
                return (L span tok)
        _ -> do setContext (Layout offset gen_semic : ctx)
                return (L span tok)

do_layout_left :: Action
do_layout_left span _buf _len = do
    _ <- popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- LINE pragmas

setLineAndFile :: Int -> Action
setLineAndFile code span buf len = do
  let src = lexemeToString buf (len - 1)  -- drop trailing quotation mark
      linenumLen = length $ head $ words src
      linenum = parseUnsignedInteger buf linenumLen 10 octDecDigit
      file = mkFastString $ go $ drop 1 $ dropWhile (/= '"') src
          -- skip everything through first quotation mark to get to the filename
        where go ('\\':c:cs) = c : go cs
              go (c:cs)      = c : go cs
              go []          = []
              -- decode escapes in the filename.  e.g. on Windows
              -- when our filenames have backslashes in, gcc seems to
              -- escape the backslashes.  One symptom of not doing this
              -- is that filenames in error messages look a bit strange:
              --   C:\\foo\bar.hs
              -- only the first backslash is doubled, because we apply
              -- System.FilePath.normalise before printing out
              -- filenames and it does not remove duplicate
              -- backslashes after the drive letter (should it?).
  setAlrLastLoc $ alrInitialLoc file
  setSrcLoc (mkRealSrcLoc file (fromIntegral linenum - 1) (srcSpanEndCol span))
      -- subtract one: the line number refers to the *following* line
  addSrcFile file
  _ <- popLexState
  pushLexState code
  lexToken

setColumn :: Action
setColumn span buf len = do
  let column =
        case reads (lexemeToString buf len) of
          [(column, _)] -> column
          _ -> error "setColumn: expected integer" -- shouldn't happen
  setSrcLoc (mkRealSrcLoc (srcSpanFile span) (srcSpanEndLine span)
                          (fromIntegral (column :: Integer)))
  _ <- popLexState
  lexToken

alrInitialLoc :: FastString -> RealSrcSpan
alrInitialLoc file = mkRealSrcSpan loc loc
    where -- This is a hack to ensure that the first line in a file
          -- looks like it is after the initial location:
          loc = mkRealSrcLoc file (-1) (-1)

-- -----------------------------------------------------------------------------
-- Options, includes and language pragmas.

lex_string_prag :: (String -> Token) -> Action
lex_string_prag mkTok span _buf _len
    = do input <- getInput
         start <- getRealSrcLoc
         tok <- go [] input
         end <- getRealSrcLoc
         return (L (mkRealSrcSpan start end) tok)
    where go acc input
              = if isString input "#-}"
                   then do setInput input
                           return (mkTok (reverse acc))
                   else case alexGetChar input of
                          Just (c,i) -> go (c:acc) i
                          Nothing -> err input
          isString _ [] = True
          isString i (x:xs)
              = case alexGetChar i of
                  Just (c,i') | c == x    -> isString i' xs
                  _other -> False
          err (AI end _) = failLocMsgP (realSrcSpanStart span) end "unterminated options pragma"


-- -----------------------------------------------------------------------------
-- Strings & Chars

-- This stuff is horrible.  I hates it.

lex_string_tok :: Action
lex_string_tok span buf _len = do
  tok <- lex_string ""
  (AI end bufEnd) <- getInput
  let
    tok' = case tok of
            ITprimstring _ bs -> ITprimstring (SourceText src) bs
            ITstring _ s -> ITstring (SourceText src) s
            _ -> panic "lex_string_tok"
    src = lexemeToString buf (cur bufEnd - cur buf)
  return (L (mkRealSrcSpan (realSrcSpanStart span) end) tok')

lex_string :: String -> P Token
lex_string s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error i

    Just ('"',i)  -> do
        setInput i
        magicHash <- extension magicHashEnabled
        if magicHash
          then do
            i <- getInput
            case alexGetChar' i of
              Just ('#',i) -> do
                   setInput i
                   if any (> '\xFF') s
                    then failMsgP "primitive string literal must contain only characters <= \'\\xFF\'"
                    else let bs = unsafeMkByteString (reverse s)
                         in return (ITprimstring (SourceText (reverse s)) bs)
              _other ->
                return (ITstring (SourceText (reverse s))
                                 (mkFastString (reverse s)))
          else
                return (ITstring (SourceText (reverse s))
                                 (mkFastString (reverse s)))

    Just ('\\',i)
        | Just ('&',i) <- next -> do
                setInput i; lex_string s
        | Just (c,i) <- next, c <= '\x7f' && is_space c -> do
                           -- is_space only works for <= '\x7f' (#3751, #5425)
                setInput i; lex_stringgap s
        where next = alexGetChar' i

    Just (c, i1) -> do
        case c of
          '\\' -> do setInput i1; c' <- lex_escape; lex_string (c':s)
          c | isAny c -> do setInput i1; lex_string (c:s)
          _other -> lit_error i

lex_stringgap :: String -> P Token
lex_stringgap s = do
  i <- getInput
  c <- getCharOrFail i
  case c of
    '\\' -> lex_string s
    c | c <= '\x7f' && is_space c -> lex_stringgap s
                           -- is_space only works for <= '\x7f' (#3751, #5425)
    _other -> lit_error i


lex_char_tok :: Action
-- Here we are basically parsing character literals, such as 'x' or '\n'
-- but we additionally spot 'x and ''T, returning ITsimpleQuote and
-- ITtyQuote respectively, but WITHOUT CONSUMING the x or T part
-- (the parser does that).
-- So we have to do two characters of lookahead: when we see 'x we need to
-- see if there's a trailing quote
lex_char_tok span buf _len = do        -- We've seen '
   i1 <- getInput       -- Look ahead to first character
   let loc = realSrcSpanStart span
   case alexGetChar' i1 of
        Nothing -> lit_error  i1

        Just ('\'', i2@(AI end2 _)) -> do       -- We've seen ''
                   setInput i2
                   return (L (mkRealSrcSpan loc end2)  ITtyQuote)

        Just ('\\', i2@(AI _end2 _)) -> do      -- We've seen 'backslash
                  setInput i2
                  lit_ch <- lex_escape
                  i3 <- getInput
                  mc <- getCharOrFail i3 -- Trailing quote
                  if mc == '\'' then finish_char_tok buf loc lit_ch
                                else lit_error i3

        Just (c, i2@(AI _end2 _))
                | not (isAny c) -> lit_error i1
                | otherwise ->

                -- We've seen 'x, where x is a valid character
                --  (i.e. not newline etc) but not a quote or backslash
           case alexGetChar' i2 of      -- Look ahead one more character
                Just ('\'', i3) -> do   -- We've seen 'x'
                        setInput i3
                        finish_char_tok buf loc c
                _other -> do            -- We've seen 'x not followed by quote
                                        -- (including the possibility of EOF)
                                        -- Just parse the quote only
                        let (AI end _) = i1
                        return (L (mkRealSrcSpan loc end) ITsimpleQuote)

finish_char_tok :: StringBuffer -> RealSrcLoc -> Char -> P (RealLocated Token)
finish_char_tok buf loc ch  -- We've already seen the closing quote
                        -- Just need to check for trailing #
  = do  magicHash <- extension magicHashEnabled
        i@(AI end bufEnd) <- getInput
        let src = lexemeToString buf (cur bufEnd - cur buf)
        if magicHash then do
            case alexGetChar' i of
              Just ('#',i@(AI end _)) -> do
                setInput i
                return (L (mkRealSrcSpan loc end)
                          (ITprimchar (SourceText src) ch))
              _other ->
                return (L (mkRealSrcSpan loc end)
                          (ITchar (SourceText src) ch))
            else do
              return (L (mkRealSrcSpan loc end) (ITchar (SourceText src) ch))

isAny :: Char -> Bool
isAny c | c > '\x7f' = isPrint c
        | otherwise  = is_any c

lex_escape :: P Char
lex_escape = do
  i0 <- getInput
  c <- getCharOrFail i0
  case c of
        'a'   -> return '\a'
        'b'   -> return '\b'
        'f'   -> return '\f'
        'n'   -> return '\n'
        'r'   -> return '\r'
        't'   -> return '\t'
        'v'   -> return '\v'
        '\\'  -> return '\\'
        '"'   -> return '\"'
        '\''  -> return '\''
        '^'   -> do i1 <- getInput
                    c <- getCharOrFail i1
                    if c >= '@' && c <= '_'
                        then return (chr (ord c - ord '@'))
                        else lit_error i1

        'x'   -> readNum is_hexdigit 16 hexDigit
        'o'   -> readNum is_octdigit  8 octDecDigit
        x | is_decdigit x -> readNum2 is_decdigit 10 octDecDigit (octDecDigit x)

        c1 ->  do
           i <- getInput
           case alexGetChar' i of
            Nothing -> lit_error i0
            Just (c2,i2) ->
              case alexGetChar' i2 of
                Nothing -> do lit_error i0
                Just (c3,i3) ->
                   let str = [c1,c2,c3] in
                   case [ (c,rest) | (p,c) <- silly_escape_chars,
                                     Just rest <- [stripPrefix p str] ] of
                          (escape_char,[]):_ -> do
                                setInput i3
                                return escape_char
                          (escape_char,_:_):_ -> do
                                setInput i2
                                return escape_char
                          [] -> lit_error i0

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Char
readNum is_digit base conv = do
  i <- getInput
  c <- getCharOrFail i
  if is_digit c
        then readNum2 is_digit base conv (conv c)
        else lit_error i

readNum2 :: (Char -> Bool) -> Int -> (Char -> Int) -> Int -> P Char
readNum2 is_digit base conv i = do
  input <- getInput
  read i input
  where read i input = do
          case alexGetChar' input of
            Just (c,input') | is_digit c -> do
               let i' = i*base + conv c
               if i' > 0x10ffff
                  then setInput input >> lexError "numeric escape sequence out of range"
                  else read i' input'
            _other -> do
              setInput input; return (chr i)


silly_escape_chars :: [(String, Char)]
silly_escape_chars = [
        ("NUL", '\NUL'),
        ("SOH", '\SOH'),
        ("STX", '\STX'),
        ("ETX", '\ETX'),
        ("EOT", '\EOT'),
        ("ENQ", '\ENQ'),
        ("ACK", '\ACK'),
        ("BEL", '\BEL'),
        ("BS", '\BS'),
        ("HT", '\HT'),
        ("LF", '\LF'),
        ("VT", '\VT'),
        ("FF", '\FF'),
        ("CR", '\CR'),
        ("SO", '\SO'),
        ("SI", '\SI'),
        ("DLE", '\DLE'),
        ("DC1", '\DC1'),
        ("DC2", '\DC2'),
        ("DC3", '\DC3'),
        ("DC4", '\DC4'),
        ("NAK", '\NAK'),
        ("SYN", '\SYN'),
        ("ETB", '\ETB'),
        ("CAN", '\CAN'),
        ("EM", '\EM'),
        ("SUB", '\SUB'),
        ("ESC", '\ESC'),
        ("FS", '\FS'),
        ("GS", '\GS'),
        ("RS", '\RS'),
        ("US", '\US'),
        ("SP", '\SP'),
        ("DEL", '\DEL')
        ]

-- before calling lit_error, ensure that the current input is pointing to
-- the position of the error in the buffer.  This is so that we can report
-- a correct location to the user, but also so we can detect UTF-8 decoding
-- errors if they occur.
lit_error :: AlexInput -> P a
lit_error i = do setInput i; lexError "lexical error in string/character literal"

getCharOrFail :: AlexInput -> P Char
getCharOrFail i =  do
  case alexGetChar' i of
        Nothing -> lexError "unexpected end-of-file in string/character literal"
        Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------
-- QuasiQuote

lex_qquasiquote_tok :: Action
lex_qquasiquote_tok span buf len = do
  let (qual, quoter) = splitQualName (stepOn buf) (len - 2) False
  quoteStart <- getRealSrcLoc
  quote <- lex_quasiquote quoteStart ""
  end <- getRealSrcLoc
  return (L (mkRealSrcSpan (realSrcSpanStart span) end)
           (ITqQuasiQuote (qual,
                           quoter,
                           mkFastString (reverse quote),
                           mkRealSrcSpan quoteStart end)))

lex_quasiquote_tok :: Action
lex_quasiquote_tok span buf len = do
  let quoter = tail (lexemeToString buf (len - 1))
                -- 'tail' drops the initial '[',
                -- while the -1 drops the trailing '|'
  quoteStart <- getRealSrcLoc
  quote <- lex_quasiquote quoteStart ""
  end <- getRealSrcLoc
  return (L (mkRealSrcSpan (realSrcSpanStart span) end)
           (ITquasiQuote (mkFastString quoter,
                          mkFastString (reverse quote),
                          mkRealSrcSpan quoteStart end)))

lex_quasiquote :: RealSrcLoc -> String -> P String
lex_quasiquote start s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> quasiquote_error start

    -- NB: The string "|]" terminates the quasiquote,
    -- with absolutely no escaping. See the extensive
    -- discussion on Trac #5348 for why there is no
    -- escape handling.
    Just ('|',i)
        | Just (']',i) <- alexGetChar' i
        -> do { setInput i; return s }

    Just (c, i) -> do
         setInput i; lex_quasiquote start (c : s)

quasiquote_error :: RealSrcLoc -> P a
quasiquote_error start = do
  (AI end buf) <- getInput
  reportLexError start end buf "unterminated quasiquotation"

-- -----------------------------------------------------------------------------
-- Warnings

warnTab :: Action
warnTab srcspan _buf _len = do
    addTabWarning srcspan
    lexToken

warnThen :: WarningFlag -> SDoc -> Action -> Action
warnThen option warning action srcspan buf len = do
    addWarning option (RealSrcSpan srcspan) warning
    action srcspan buf len

-- -----------------------------------------------------------------------------
-- The Parse Monad

-- | Do we want to generate ';' layout tokens? In some cases we just want to
-- generate '}', e.g. in MultiWayIf we don't need ';'s because '|' separates
-- alternatives (unlike a `case` expression where we need ';' to as a separator
-- between alternatives).
type GenSemic = Bool

generateSemic, dontGenerateSemic :: GenSemic
generateSemic     = True
dontGenerateSemic = False

data LayoutContext
  = NoLayout
  | Layout !Int !GenSemic
  deriving Show

data ParseResult a
  = POk PState a
  | PFailed
        (DynFlags -> Messages) -- A function that returns warnings that
                               -- accumulated during parsing, including
                               -- the warnings related to tabs.
        SrcSpan                -- The start and end of the text span related
                               -- to the error.  Might be used in environments
                               -- which can show this span, e.g. by
                               -- highlighting it.
        MsgDoc                 -- The error message

-- | Test whether a 'WarningFlag' is set
warnopt :: WarningFlag -> ParserFlags -> Bool
warnopt f options = f `EnumSet.member` pWarningFlags options

-- | Test whether a 'LangExt.Extension' is set
extopt :: LangExt.Extension -> ParserFlags -> Bool
extopt f options = f `EnumSet.member` pExtensionFlags options

-- | The subset of the 'DynFlags' used by the parser
data ParserFlags = ParserFlags {
    pWarningFlags   :: EnumSet WarningFlag
  , pExtensionFlags :: EnumSet LangExt.Extension
  , pThisPackage    :: UnitId      -- ^ key of package currently being compiled
  , pExtsBitmap     :: !ExtsBitmap -- ^ bitmap of permitted extensions
  }

data PState = PState {
        buffer     :: StringBuffer,
        options    :: ParserFlags,
        -- This needs to take DynFlags as an argument until
        -- we have a fix for #10143
        messages   :: DynFlags -> Messages,
        tab_first  :: Maybe RealSrcSpan, -- pos of first tab warning in the file
        tab_count  :: !Int,              -- number of tab warnings in the file
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        context    :: [LayoutContext],
        lex_state  :: [Int],
        srcfiles   :: [FastString],
        -- Used in the alternative layout rule:
        -- These tokens are the next ones to be sent out. They are
        -- just blindly emitted, without the rule looking at them again:
        alr_pending_implicit_tokens :: [RealLocated Token],
        -- This is the next token to be considered or, if it is Nothing,
        -- we need to get the next token from the input stream:
        alr_next_token :: Maybe (RealLocated Token),
        -- This is what we consider to be the location of the last token
        -- emitted:
        alr_last_loc :: RealSrcSpan,
        -- The stack of layout contexts:
        alr_context :: [ALRContext],
        -- Are we expecting a '{'? If it's Just, then the ALRLayout tells
        -- us what sort of layout the '{' will open:
        alr_expecting_ocurly :: Maybe ALRLayout,
        -- Have we just had the '}' for a let block? If so, than an 'in'
        -- token doesn't need to close anything:
        alr_justClosedExplicitLetBlock :: Bool,

        -- If this is enabled, '{-# LINE ... -#}' and '{-# COLUMN ... #-}'
        -- update the 'loc' field. Otherwise, those pragmas are lexed as tokens.
        use_pos_prags :: Bool,

        -- The next three are used to implement Annotations giving the
        -- locations of 'noise' tokens in the source, so that users of
        -- the GHC API can do source to source conversions.
        -- See note [Api annotations] in ApiAnnotation.hs
        annotations :: [(ApiAnnKey,[SrcSpan])],
        comment_q :: [Located AnnotationComment],
        annotations_comments :: [(SrcSpan,[Located AnnotationComment])]
     }
        -- last_loc and last_len are used when generating error messages,
        -- and in pushCurrentContext only.  Sigh, if only Happy passed the
        -- current token to happyError, we could at least get rid of last_len.
        -- Getting rid of last_loc would require finding another way to
        -- implement pushCurrentContext (which is only called from one place).

data ALRContext = ALRNoLayout Bool{- does it contain commas? -}
                              Bool{- is it a 'let' block? -}
                | ALRLayout ALRLayout Int
data ALRLayout = ALRLayoutLet
               | ALRLayoutWhere
               | ALRLayoutOf
               | ALRLayoutDo

newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = returnP
  (<*>) = ap

instance Monad P where
  (>>=) = thenP
  fail = MonadFail.fail

instance MonadFail P where
  fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed warnFn span err -> PFailed warnFn span err

failP :: String -> P a
failP msg =
  P $ \s ->
    PFailed (getMessages s) (RealSrcSpan (last_loc s)) (text msg)

failMsgP :: String -> P a
failMsgP msg =
  P $ \s ->
    PFailed (getMessages s) (RealSrcSpan (last_loc s)) (text msg)

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str =
  P $ \s ->
    PFailed (getMessages s) (RealSrcSpan (mkRealSrcSpan loc1 loc2)) (text str)

failSpanMsgP :: SrcSpan -> SDoc -> P a
failSpanMsgP span msg =
  P $ \s ->
    PFailed (getMessages s) span msg

getPState :: P PState
getPState = P $ \s -> POk s s

withThisPackage :: (UnitId -> a) -> P a
withThisPackage f = P $ \s@(PState{options = o}) -> POk s (f (pThisPackage o))

extension :: (ExtsBitmap -> Bool) -> P Bool
extension p = P $ \s -> POk s (p $! (pExtsBitmap . options) s)

getExts :: P ExtsBitmap
getExts = P $ \s -> POk s (pExtsBitmap . options $ s)

setExts :: (ExtsBitmap -> ExtsBitmap) -> P ()
setExts f = P $ \s -> POk s {
  options =
    let p = options s
    in  p { pExtsBitmap = f (pExtsBitmap p) }
  } ()

setSrcLoc :: RealSrcLoc -> P ()
setSrcLoc new_loc = P $ \s -> POk s{loc=new_loc} ()

getRealSrcLoc :: P RealSrcLoc
getRealSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

addSrcFile :: FastString -> P ()
addSrcFile f = P $ \s -> POk s{ srcfiles = f : srcfiles s } ()

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: Token -> P ()
setLastTk tk = P $ \s -> POk s { last_tk = Just tk } ()

getLastTk :: P (Maybe Token)
getLastTk = P $ \s@(PState { last_tk = last_tk }) -> POk s last_tk

data AlexInput = AI RealSrcLoc StringBuffer

{-
Note [Unicode in Alex]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although newer versions of Alex support unicode, this grammar is processed with
the old style '--latin1' behaviour. This means that when implementing the
functions

    alexGetByte       :: AlexInput -> Maybe (Word8,AlexInput)
    alexInputPrevChar :: AlexInput -> Char

which Alex uses to take apart our 'AlexInput', we must

  * return a latin1 character in the 'Word8' that 'alexGetByte' expects
  * return a latin1 character in 'alexInputPrevChar'.

We handle this in 'adjustChar' by squishing entire classes of unicode
characters into single bytes.
-}

{-# INLINE adjustChar #-}
adjustChar :: Char -> Word8
adjustChar c = fromIntegral $ ord adj_c
  where non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'

        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected
                -- in basicTypes/Lexeme.hs
                -- Any changes here should likely be reflected there.

                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic

-- Getting the previous 'Char' isn't enough here - we need to convert it into
-- the same format that 'alexGetByte' would have produced.
--
-- See Note [Unicode in Alex] and #13986.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = chr (fromIntegral (adjustChar pc))
  where pc = prevChar buf '\n'

-- backwards compatibility for Alex 2.x
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar inp = case alexGetByte inp of
                    Nothing    -> Nothing
                    Just (b,i) -> c `seq` Just (c,i)
                       where c = chr $ fromIntegral b

-- See Note [Unicode in Alex]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = adjustChar c

-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()

nextIsEOF :: P Bool
nextIsEOF = do
  AI _ s <- getInput
  return $ atEnd s

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

popNextToken :: P (Maybe (RealLocated Token))
popNextToken
    = P $ \s@PState{ alr_next_token = m } ->
              POk (s {alr_next_token = Nothing}) m

activeContext :: P Bool
activeContext = do
  ctxt <- getALRContext
  expc <- getAlrExpectingOCurly
  impt <- implicitTokenPending
  case (ctxt,expc) of
    ([],Nothing) -> return impt
    _other       -> return True

setAlrLastLoc :: RealSrcSpan -> P ()
setAlrLastLoc l = P $ \s -> POk (s {alr_last_loc = l}) ()

getAlrLastLoc :: P RealSrcSpan
getAlrLastLoc = P $ \s@(PState {alr_last_loc = l}) -> POk s l

getALRContext :: P [ALRContext]
getALRContext = P $ \s@(PState {alr_context = cs}) -> POk s cs

setALRContext :: [ALRContext] -> P ()
setALRContext cs = P $ \s -> POk (s {alr_context = cs}) ()

getALRTransitional :: P Bool
getALRTransitional = P $ \s@PState {options = o} ->
  POk s (extopt LangExt.AlternativeLayoutRuleTransitional o)

getJustClosedExplicitLetBlock :: P Bool
getJustClosedExplicitLetBlock
 = P $ \s@(PState {alr_justClosedExplicitLetBlock = b}) -> POk s b

setJustClosedExplicitLetBlock :: Bool -> P ()
setJustClosedExplicitLetBlock b
 = P $ \s -> POk (s {alr_justClosedExplicitLetBlock = b}) ()

setNextToken :: RealLocated Token -> P ()
setNextToken t = P $ \s -> POk (s {alr_next_token = Just t}) ()

implicitTokenPending :: P Bool
implicitTokenPending
    = P $ \s@PState{ alr_pending_implicit_tokens = ts } ->
              case ts of
              [] -> POk s False
              _  -> POk s True

popPendingImplicitToken :: P (Maybe (RealLocated Token))
popPendingImplicitToken
    = P $ \s@PState{ alr_pending_implicit_tokens = ts } ->
              case ts of
              [] -> POk s Nothing
              (t : ts') -> POk (s {alr_pending_implicit_tokens = ts'}) (Just t)

setPendingImplicitTokens :: [RealLocated Token] -> P ()
setPendingImplicitTokens ts = P $ \s -> POk (s {alr_pending_implicit_tokens = ts}) ()

getAlrExpectingOCurly :: P (Maybe ALRLayout)
getAlrExpectingOCurly = P $ \s@(PState {alr_expecting_ocurly = b}) -> POk s b

setAlrExpectingOCurly :: Maybe ALRLayout -> P ()
setAlrExpectingOCurly b = P $ \s -> POk (s {alr_expecting_ocurly = b}) ()

-- for reasons of efficiency, flags indicating language extensions (eg,
-- -fglasgow-exts or -XParallelArrays) are represented by a bitmap
-- stored in an unboxed Word64
type ExtsBitmap = Word64

xbit :: ExtBits -> ExtsBitmap
xbit = bit . fromEnum

xtest :: ExtBits -> ExtsBitmap -> Bool
xtest ext xmap = testBit xmap (fromEnum ext)

data ExtBits
  = FfiBit
  | InterruptibleFfiBit
  | CApiFfiBit
  | ArrowsBit
  | ThBit
  | ThQuotesBit
  | IpBit
  | OverloadedLabelsBit -- #x overloaded labels
  | ExplicitForallBit -- the 'forall' keyword and '.' symbol
  | BangPatBit -- Tells the parser to understand bang-patterns
               -- (doesn't affect the lexer)
  | PatternSynonymsBit -- pattern synonyms
  | HaddockBit-- Lex and parse Haddock comments
  | MagicHashBit -- "#" in both functions and operators
  | RecursiveDoBit -- mdo
  | UnicodeSyntaxBit -- the forall symbol, arrow symbols, etc
  | UnboxedTuplesBit -- (# and #)
  | UnboxedSumsBit -- (# and #)
  | DatatypeContextsBit
  | TransformComprehensionsBit
  | QqBit -- enable quasiquoting
  | InRulePragBit
  | InNestedCommentBit -- See Note [Nested comment line pragmas]
  | RawTokenStreamBit -- producing a token stream with all comments included
  | SccProfilingOnBit
  | HpcBit
  | AlternativeLayoutRuleBit
  | RelaxedLayoutBit
  | NondecreasingIndentationBit
  | SafeHaskellBit
  | TraditionalRecordSyntaxBit
  | ExplicitNamespacesBit
  | LambdaCaseBit
  | BinaryLiteralsBit
  | NegativeLiteralsBit
  | HexFloatLiteralsBit
  | TypeApplicationsBit
  | StaticPointersBit
  | NumericUnderscoresBit
  | StarIsTypeBit
  deriving Enum


always :: ExtsBitmap -> Bool
always           _     = True
arrowsEnabled :: ExtsBitmap -> Bool
arrowsEnabled = xtest ArrowsBit
thEnabled :: ExtsBitmap -> Bool
thEnabled = xtest ThBit
thQuotesEnabled :: ExtsBitmap -> Bool
thQuotesEnabled = xtest ThQuotesBit
ipEnabled :: ExtsBitmap -> Bool
ipEnabled = xtest IpBit
overloadedLabelsEnabled :: ExtsBitmap -> Bool
overloadedLabelsEnabled = xtest OverloadedLabelsBit
explicitForallEnabled :: ExtsBitmap -> Bool
explicitForallEnabled = xtest ExplicitForallBit
bangPatEnabled :: ExtsBitmap -> Bool
bangPatEnabled = xtest BangPatBit
haddockEnabled :: ExtsBitmap -> Bool
haddockEnabled = xtest HaddockBit
magicHashEnabled :: ExtsBitmap -> Bool
magicHashEnabled = xtest MagicHashBit
unicodeSyntaxEnabled :: ExtsBitmap -> Bool
unicodeSyntaxEnabled = xtest UnicodeSyntaxBit
unboxedTuplesEnabled :: ExtsBitmap -> Bool
unboxedTuplesEnabled = xtest UnboxedTuplesBit
unboxedSumsEnabled :: ExtsBitmap -> Bool
unboxedSumsEnabled = xtest UnboxedSumsBit
datatypeContextsEnabled :: ExtsBitmap -> Bool
datatypeContextsEnabled = xtest DatatypeContextsBit
qqEnabled :: ExtsBitmap -> Bool
qqEnabled = xtest QqBit
inRulePrag :: ExtsBitmap -> Bool
inRulePrag = xtest InRulePragBit
inNestedComment :: ExtsBitmap -> Bool
inNestedComment = xtest InNestedCommentBit
rawTokenStreamEnabled :: ExtsBitmap -> Bool
rawTokenStreamEnabled = xtest RawTokenStreamBit
alternativeLayoutRule :: ExtsBitmap -> Bool
alternativeLayoutRule = xtest AlternativeLayoutRuleBit
hpcEnabled :: ExtsBitmap -> Bool
hpcEnabled = xtest HpcBit
relaxedLayout :: ExtsBitmap -> Bool
relaxedLayout = xtest RelaxedLayoutBit
nondecreasingIndentation :: ExtsBitmap -> Bool
nondecreasingIndentation = xtest NondecreasingIndentationBit
sccProfilingOn :: ExtsBitmap -> Bool
sccProfilingOn = xtest SccProfilingOnBit
traditionalRecordSyntaxEnabled :: ExtsBitmap -> Bool
traditionalRecordSyntaxEnabled = xtest TraditionalRecordSyntaxBit

explicitNamespacesEnabled :: ExtsBitmap -> Bool
explicitNamespacesEnabled = xtest ExplicitNamespacesBit
lambdaCaseEnabled :: ExtsBitmap -> Bool
lambdaCaseEnabled = xtest LambdaCaseBit
binaryLiteralsEnabled :: ExtsBitmap -> Bool
binaryLiteralsEnabled = xtest BinaryLiteralsBit
negativeLiteralsEnabled :: ExtsBitmap -> Bool
negativeLiteralsEnabled = xtest NegativeLiteralsBit
hexFloatLiteralsEnabled :: ExtsBitmap -> Bool
hexFloatLiteralsEnabled = xtest HexFloatLiteralsBit
patternSynonymsEnabled :: ExtsBitmap -> Bool
patternSynonymsEnabled = xtest PatternSynonymsBit
typeApplicationEnabled :: ExtsBitmap -> Bool
typeApplicationEnabled = xtest TypeApplicationsBit
staticPointersEnabled :: ExtsBitmap -> Bool
staticPointersEnabled = xtest StaticPointersBit
numericUnderscoresEnabled :: ExtsBitmap -> Bool
numericUnderscoresEnabled = xtest NumericUnderscoresBit
starIsTypeEnabled :: ExtsBitmap -> Bool
starIsTypeEnabled = xtest StarIsTypeBit

-- PState for parsing options pragmas
--
pragState :: DynFlags -> StringBuffer -> RealSrcLoc -> PState
pragState dynflags buf loc = (mkPState dynflags buf loc) {
                                 lex_state = [bol, option_prags, 0]
                             }

-- | Extracts the flag information needed for parsing
mkParserFlags :: DynFlags -> ParserFlags
mkParserFlags flags =
    ParserFlags {
      pWarningFlags = DynFlags.warningFlags flags
    , pExtensionFlags = DynFlags.extensionFlags flags
    , pThisPackage = DynFlags.thisPackage flags
    , pExtsBitmap = bitmap
    }
  where
    bitmap = safeHaskellBit .|. langExtBits .|. optBits
    safeHaskellBit =
          SafeHaskellBit `setBitIf` safeImportsOn flags
    langExtBits =
          FfiBit                      `xoptBit` LangExt.ForeignFunctionInterface
      .|. InterruptibleFfiBit         `xoptBit` LangExt.InterruptibleFFI
      .|. CApiFfiBit                  `xoptBit` LangExt.CApiFFI
      .|. ArrowsBit                   `xoptBit` LangExt.Arrows
      .|. ThBit                       `xoptBit` LangExt.TemplateHaskell
      .|. ThQuotesBit                 `xoptBit` LangExt.TemplateHaskellQuotes
      .|. QqBit                       `xoptBit` LangExt.QuasiQuotes
      .|. IpBit                       `xoptBit` LangExt.ImplicitParams
      .|. OverloadedLabelsBit         `xoptBit` LangExt.OverloadedLabels
      .|. ExplicitForallBit           `xoptBit` LangExt.ExplicitForAll
      .|. BangPatBit                  `xoptBit` LangExt.BangPatterns
      .|. MagicHashBit                `xoptBit` LangExt.MagicHash
      .|. RecursiveDoBit              `xoptBit` LangExt.RecursiveDo
      .|. UnicodeSyntaxBit            `xoptBit` LangExt.UnicodeSyntax
      .|. UnboxedTuplesBit            `xoptBit` LangExt.UnboxedTuples
      .|. UnboxedSumsBit              `xoptBit` LangExt.UnboxedSums
      .|. DatatypeContextsBit         `xoptBit` LangExt.DatatypeContexts
      .|. TransformComprehensionsBit  `xoptBit` LangExt.TransformListComp
      .|. TransformComprehensionsBit  `xoptBit` LangExt.MonadComprehensions
      .|. AlternativeLayoutRuleBit    `xoptBit` LangExt.AlternativeLayoutRule
      .|. RelaxedLayoutBit            `xoptBit` LangExt.RelaxedLayout
      .|. NondecreasingIndentationBit `xoptBit` LangExt.NondecreasingIndentation
      .|. TraditionalRecordSyntaxBit  `xoptBit` LangExt.TraditionalRecordSyntax
      .|. ExplicitNamespacesBit       `xoptBit` LangExt.ExplicitNamespaces
      .|. LambdaCaseBit               `xoptBit` LangExt.LambdaCase
      .|. BinaryLiteralsBit           `xoptBit` LangExt.BinaryLiterals
      .|. NegativeLiteralsBit         `xoptBit` LangExt.NegativeLiterals
      .|. HexFloatLiteralsBit         `xoptBit` LangExt.HexFloatLiterals
      .|. PatternSynonymsBit          `xoptBit` LangExt.PatternSynonyms
      .|. TypeApplicationsBit         `xoptBit` LangExt.TypeApplications
      .|. StaticPointersBit           `xoptBit` LangExt.StaticPointers
      .|. NumericUnderscoresBit       `xoptBit` LangExt.NumericUnderscores
      .|. StarIsTypeBit               `xoptBit` LangExt.StarIsType
    optBits =
          HaddockBit        `goptBit` Opt_Haddock
      .|. RawTokenStreamBit `goptBit` Opt_KeepRawTokenStream
      .|. HpcBit            `goptBit` Opt_Hpc
      .|. SccProfilingOnBit `goptBit` Opt_SccProfilingOn

    xoptBit bit ext = bit `setBitIf` xopt ext flags
    goptBit bit opt = bit `setBitIf` gopt opt flags

    setBitIf :: ExtBits -> Bool -> ExtsBitmap
    b `setBitIf` cond | cond      = xbit b
                      | otherwise = 0

-- | Creates a parse state from a 'DynFlags' value
mkPState :: DynFlags -> StringBuffer -> RealSrcLoc -> PState
mkPState flags = mkPStatePure (mkParserFlags flags)

-- | Creates a parse state from a 'ParserFlags' value
mkPStatePure :: ParserFlags -> StringBuffer -> RealSrcLoc -> PState
mkPStatePure options buf loc =
  PState {
      buffer        = buf,
      options       = options,
      messages      = const emptyMessages,
      tab_first     = Nothing,
      tab_count     = 0,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc loc,
      last_len      = 0,
      loc           = loc,
      context       = [],
      lex_state     = [bol, 0],
      srcfiles      = [],
      alr_pending_implicit_tokens = [],
      alr_next_token = Nothing,
      alr_last_loc = alrInitialLoc (fsLit "<no file>"),
      alr_context = [],
      alr_expecting_ocurly = Nothing,
      alr_justClosedExplicitLetBlock = False,
      use_pos_prags = True,
      annotations = [],
      comment_q = [],
      annotations_comments = []
    }

addWarning :: WarningFlag -> SrcSpan -> SDoc -> P ()
addWarning option srcspan warning
 = P $ \s@PState{messages=m, options=o} ->
       let
           m' d =
               let (ws, es) = m d
                   warning' = makeIntoWarning (Reason option) $
                      mkWarnMsg d srcspan alwaysQualify warning
                   ws' = if warnopt option o then ws `snocBag` warning' else ws
               in (ws', es)
       in POk s{messages=m'} ()

addTabWarning :: RealSrcSpan -> P ()
addTabWarning srcspan
 = P $ \s@PState{tab_first=tf, tab_count=tc, options=o} ->
       let tf' = if isJust tf then tf else Just srcspan
           tc' = tc + 1
           s' = if warnopt Opt_WarnTabs o
                then s{tab_first = tf', tab_count = tc'}
                else s
       in POk s' ()

mkTabWarning :: PState -> DynFlags -> Maybe ErrMsg
mkTabWarning PState{tab_first=tf, tab_count=tc} d =
  let middle = if tc == 1
        then text ""
        else text ", and in" <+> speakNOf (tc - 1) (text "further location")
      message = text "Tab character found here"
                <> middle
                <> text "."
                $+$ text "Please use spaces instead."
  in fmap (\s -> makeIntoWarning (Reason Opt_WarnTabs) $
                 mkWarnMsg d (RealSrcSpan s) alwaysQualify message) tf

getMessages :: PState -> DynFlags -> Messages
getMessages p@PState{messages=m} d =
  let (ws, es) = m d
      tabwarning = mkTabWarning p d
      ws' = maybe ws (`consBag` ws) tabwarning
  in (ws', es)

getContext :: P [LayoutContext]
getContext = P $ \s@PState{context=ctx} -> POk s ctx

setContext :: [LayoutContext] -> P ()
setContext ctx = P $ \s -> POk s{context=ctx} ()

popContext :: P ()
popContext = P $ \ s@(PState{ buffer = buf, options = o, context = ctx,
                              last_len = len, last_loc = last_loc }) ->
  case ctx of
        (_:tl) ->
          POk s{ context = tl } ()
        []     ->
          PFailed (getMessages s) (RealSrcSpan last_loc) (srcParseErr o buf len)

-- Push a new layout context at the indentation of the last token read.
pushCurrentContext :: GenSemic -> P ()
pushCurrentContext gen_semic = P $ \ s@PState{ last_loc=loc, context=ctx } ->
    POk s{context = Layout (srcSpanStartCol loc) gen_semic : ctx} ()

-- This is only used at the outer level of a module when the 'module' keyword is
-- missing.
pushModuleContext :: P ()
pushModuleContext = pushCurrentContext generateSemic

getOffside :: P (Ordering, Bool)
getOffside = P $ \s@PState{last_loc=loc, context=stk} ->
                let offs = srcSpanStartCol loc in
                let ord = case stk of
                            Layout n gen_semic : _ ->
                              --trace ("layout: " ++ show n ++ ", offs: " ++ show offs) $
                              (compare offs n, gen_semic)
                            _ ->
                              (GT, dontGenerateSemic)
                in POk s ord

-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: ParserFlags
  -> StringBuffer       -- current buffer (placed just after the last token)
  -> Int                -- length of the previous token
  -> MsgDoc
srcParseErr options buf len
  = if null token
         then text "parse error (possibly incorrect indentation or mismatched brackets)"
         else text "parse error on input" <+> quotes (text token)
              $$ ppWhen (not th_enabled && token == "$") -- #7396
                        (text "Perhaps you intended to use TemplateHaskell")
              $$ ppWhen (token == "<-")
                        (if mdoInLast100
                           then text "Perhaps you intended to use RecursiveDo"
                           else text "Perhaps this statement should be within a 'do' block?")
              $$ ppWhen (token == "=")
                        (text "Perhaps you need a 'let' in a 'do' block?"
                         $$ text "e.g. 'let x = 5' instead of 'x = 5'")
              $$ ppWhen (not ps_enabled && pattern == "pattern ") -- #12429
                        (text "Perhaps you intended to use PatternSynonyms")
  where token = lexemeToString (offsetBytes (-len) buf) len
        pattern = decodePrevNChars 8 buf
        last100 = decodePrevNChars 100 buf
        mdoInLast100 = "mdo" `isInfixOf` last100
        th_enabled = extopt LangExt.TemplateHaskell options
        ps_enabled = extopt LangExt.PatternSynonyms options

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \s@PState{ buffer = buf, options = o, last_len = len,
                            last_loc = last_loc } ->
    PFailed (getMessages s) (RealSrcSpan last_loc) (srcParseErr o buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: String -> P a
lexError str = do
  loc <- getRealSrcLoc
  (AI end buf) <- getInput
  reportLexError loc end buf str

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: Bool -> (Located Token -> P a) -> P a
lexer queueComments cont = do
  alr <- extension alternativeLayoutRule
  let lexTokenFun = if alr then lexTokenAlr else lexToken
  (L span tok) <- lexTokenFun
  --trace ("token: " ++ show tok) $ do

  case tok of
    ITeof -> addAnnotationOnly noSrcSpan AnnEofPos (RealSrcSpan span)
    _ -> return ()

  if (queueComments && isDocComment tok)
    then queueComment (L (RealSrcSpan span) tok)
    else return ()

  if (queueComments && isComment tok)
    then queueComment (L (RealSrcSpan span) tok) >> lexer queueComments cont
    else cont (L (RealSrcSpan span) tok)

lexTokenAlr :: P (RealLocated Token)
lexTokenAlr = do mPending <- popPendingImplicitToken
                 t <- case mPending of
                      Nothing ->
                          do mNext <- popNextToken
                             t <- case mNext of
                                  Nothing -> lexToken
                                  Just next -> return next
                             alternativeLayoutRuleToken t
                      Just t ->
                          return t
                 setAlrLastLoc (getRealSrcSpan t)
                 case unRealSrcSpan t of
                     ITwhere -> setAlrExpectingOCurly (Just ALRLayoutWhere)
                     ITlet   -> setAlrExpectingOCurly (Just ALRLayoutLet)
                     ITof    -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITlcase -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITdo    -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     ITmdo   -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     ITrec   -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     _       -> return ()
                 return t

alternativeLayoutRuleToken :: RealLocated Token -> P (RealLocated Token)
alternativeLayoutRuleToken t
    = do context <- getALRContext
         lastLoc <- getAlrLastLoc
         mExpectingOCurly <- getAlrExpectingOCurly
         transitional <- getALRTransitional
         justClosedExplicitLetBlock <- getJustClosedExplicitLetBlock
         setJustClosedExplicitLetBlock False
         let thisLoc = getRealSrcSpan t
             thisCol = srcSpanStartCol thisLoc
             newLine = srcSpanStartLine thisLoc > srcSpanEndLine lastLoc
         case (unRealSrcSpan t, context, mExpectingOCurly) of
             -- This case handles a GHC extension to the original H98
             -- layout rule...
             (ITocurly, _, Just alrLayout) ->
                 do setAlrExpectingOCurly Nothing
                    let isLet = case alrLayout of
                                ALRLayoutLet -> True
                                _ -> False
                    setALRContext (ALRNoLayout (containsCommas ITocurly) isLet : context)
                    return t
             -- ...and makes this case unnecessary
             {-
             -- I think our implicit open-curly handling is slightly
             -- different to John's, in how it interacts with newlines
             -- and "in"
             (ITocurly, _, Just _) ->
                 do setAlrExpectingOCurly Nothing
                    setNextToken t
                    lexTokenAlr
             -}
             (_, ALRLayout _ col : _ls, Just expectingOCurly)
              | (thisCol > col) ||
                (thisCol == col &&
                 isNonDecreasingIndentation expectingOCurly) ->
                 do setAlrExpectingOCurly Nothing
                    setALRContext (ALRLayout expectingOCurly thisCol : context)
                    setNextToken t
                    return (L thisLoc ITocurly)
              | otherwise ->
                 do setAlrExpectingOCurly Nothing
                    setPendingImplicitTokens [L lastLoc ITccurly]
                    setNextToken t
                    return (L lastLoc ITocurly)
             (_, _, Just expectingOCurly) ->
                 do setAlrExpectingOCurly Nothing
                    setALRContext (ALRLayout expectingOCurly thisCol : context)
                    setNextToken t
                    return (L thisLoc ITocurly)
             -- We do the [] cases earlier than in the spec, as we
             -- have an actual EOF token
             (ITeof, ALRLayout _ _ : ls, _) ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITccurly)
             (ITeof, _, _) ->
                 return t
             -- the other ITeof case omitted; general case below covers it
             (ITin, _, _)
              | justClosedExplicitLetBlock ->
                 return t
             (ITin, ALRLayout ALRLayoutLet _ : ls, _)
              | newLine ->
                 do setPendingImplicitTokens [t]
                    setALRContext ls
                    return (L thisLoc ITccurly)
             -- This next case is to handle a transitional issue:
             (ITwhere, ALRLayout _ col : ls, _)
              | newLine && thisCol == col && transitional ->
                 do addWarning Opt_WarnAlternativeLayoutRuleTransitional
                               (RealSrcSpan thisLoc)
                               (transitionalAlternativeLayoutWarning
                                    "`where' clause at the same depth as implicit layout block")
                    setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITccurly)
             -- This next case is to handle a transitional issue:
             (ITvbar, ALRLayout _ col : ls, _)
              | newLine && thisCol == col && transitional ->
                 do addWarning Opt_WarnAlternativeLayoutRuleTransitional
                               (RealSrcSpan thisLoc)
                               (transitionalAlternativeLayoutWarning
                                    "`|' at the same depth as implicit layout block")
                    setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITccurly)
             (_, ALRLayout _ col : ls, _)
              | newLine && thisCol == col ->
                 do setNextToken t
                    return (L thisLoc ITsemi)
              | newLine && thisCol < col ->
                 do setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITccurly)
             -- We need to handle close before open, as 'then' is both
             -- an open and a close
             (u, _, _)
              | isALRclose u ->
                 case context of
                 ALRLayout _ _ : ls ->
                     do setALRContext ls
                        setNextToken t
                        return (L thisLoc ITccurly)
                 ALRNoLayout _ isLet : ls ->
                     do let ls' = if isALRopen u
                                     then ALRNoLayout (containsCommas u) False : ls
                                     else ls
                        setALRContext ls'
                        when isLet $ setJustClosedExplicitLetBlock True
                        return t
                 [] ->
                     do let ls = if isALRopen u
                                    then [ALRNoLayout (containsCommas u) False]
                                    else []
                        setALRContext ls
                        -- XXX This is an error in John's code, but
                        -- it looks reachable to me at first glance
                        return t
             (u, _, _)
              | isALRopen u ->
                 do setALRContext (ALRNoLayout (containsCommas u) False : context)
                    return t
             (ITin, ALRLayout ALRLayoutLet _ : ls, _) ->
                 do setALRContext ls
                    setPendingImplicitTokens [t]
                    return (L thisLoc ITccurly)
             (ITin, ALRLayout _ _ : ls, _) ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITccurly)
             -- the other ITin case omitted; general case below covers it
             (ITcomma, ALRLayout _ _ : ls, _)
              | topNoLayoutContainsCommas ls ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITccurly)
             (ITwhere, ALRLayout ALRLayoutDo _ : ls, _) ->
                 do setALRContext ls
                    setPendingImplicitTokens [t]
                    return (L thisLoc ITccurly)
             -- the other ITwhere case omitted; general case below covers it
             (_, _, _) -> return t

transitionalAlternativeLayoutWarning :: String -> SDoc
transitionalAlternativeLayoutWarning msg
    = text "transitional layout will not be accepted in the future:"
   $$ text msg

isALRopen :: Token -> Bool
isALRopen ITcase          = True
isALRopen ITif            = True
isALRopen ITthen          = True
isALRopen IToparen        = True
isALRopen ITobrack        = True
isALRopen ITocurly        = True
-- GHC Extensions:
isALRopen IToubxparen     = True
isALRopen ITparenEscape   = True
isALRopen ITparenTyEscape = True
isALRopen _               = False

isALRclose :: Token -> Bool
isALRclose ITof     = True
isALRclose ITthen   = True
isALRclose ITelse   = True
isALRclose ITcparen = True
isALRclose ITcbrack = True
isALRclose ITccurly = True
-- GHC Extensions:
isALRclose ITcubxparen = True
isALRclose _        = False

isNonDecreasingIndentation :: ALRLayout -> Bool
isNonDecreasingIndentation ALRLayoutDo = True
isNonDecreasingIndentation _           = False

containsCommas :: Token -> Bool
containsCommas IToparen = True
containsCommas ITobrack = True
-- John doesn't have {} as containing commas, but records contain them,
-- which caused a problem parsing Cabal's Distribution.Simple.InstallDirs
-- (defaultInstallDirs).
containsCommas ITocurly = True
-- GHC Extensions:
containsCommas IToubxparen = True
containsCommas _        = False

topNoLayoutContainsCommas :: [ALRContext] -> Bool
topNoLayoutContainsCommas [] = False
topNoLayoutContainsCommas (ALRLayout _ _ : ls) = topNoLayoutContainsCommas ls
topNoLayoutContainsCommas (ALRNoLayout b _ : _) = b

lexToken :: P (RealLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  exts <- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do
        let span = mkRealSrcSpan loc1 loc1
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkRealSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes
        case unRealSrcSpan lt of
          ITlineComment _  -> return lt
          ITblockComment _ -> return lt
          lt' -> do
            setLastTk lt'
            return lt

reportLexError :: RealSrcLoc -> RealSrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP loc1 loc2 (str ++ " at end of input")
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
     else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)

lexTokenStream :: StringBuffer -> RealSrcLoc -> DynFlags -> ParseResult [Located Token]
lexTokenStream buf loc dflags = unP go initState
    where dflags' = gopt_set (gopt_unset dflags Opt_Haddock) Opt_KeepRawTokenStream
          initState = (mkPState dflags' buf loc) { use_pos_prags = False }
          go = do
            ltok <- lexer False return
            case ltok of
              L _ ITeof -> return []
              _ -> liftM (ltok:) go

linePrags = Map.singleton "line" linePrag

fileHeaderPrags = Map.fromList([("options", lex_string_prag IToptions_prag),
                                 ("options_ghc", lex_string_prag IToptions_prag),
                                 ("options_haddock", lex_string_prag ITdocOptions),
                                 ("language", token ITlanguage_prag),
                                 ("include", lex_string_prag ITinclude_prag)])

ignoredPrags = Map.fromList (map ignored pragmas)
               where ignored opt = (opt, nested_comment lexToken)
                     impls = ["hugs", "nhc98", "jhc", "yhc", "catch", "derive"]
                     options_pragmas = map ("options_" ++) impls
                     -- CFILES is a hugs-only thing.
                     pragmas = options_pragmas ++ ["cfiles", "contract"]

oneWordPrags = Map.fromList [
     ("rules", rulePrag),
     ("inline",
         strtoken (\s -> (ITinline_prag (SourceText s) Inline FunLike))),
     ("inlinable",
         strtoken (\s -> (ITinline_prag (SourceText s) Inlinable FunLike))),
     ("inlineable",
         strtoken (\s -> (ITinline_prag (SourceText s) Inlinable FunLike))),
                                    -- Spelling variant
     ("notinline",
         strtoken (\s -> (ITinline_prag (SourceText s) NoInline FunLike))),
     ("specialize", strtoken (\s -> ITspec_prag (SourceText s))),
     ("source", strtoken (\s -> ITsource_prag (SourceText s))),
     ("warning", strtoken (\s -> ITwarning_prag (SourceText s))),
     ("deprecated", strtoken (\s -> ITdeprecated_prag (SourceText s))),
     ("scc", strtoken (\s -> ITscc_prag (SourceText s))),
     ("generated", strtoken (\s -> ITgenerated_prag (SourceText s))),
     ("core", strtoken (\s -> ITcore_prag (SourceText s))),
     ("unpack", strtoken (\s -> ITunpack_prag (SourceText s))),
     ("nounpack", strtoken (\s -> ITnounpack_prag (SourceText s))),
     ("ann", strtoken (\s -> ITann_prag (SourceText s))),
     ("minimal", strtoken (\s -> ITminimal_prag (SourceText s))),
     ("overlaps", strtoken (\s -> IToverlaps_prag (SourceText s))),
     ("overlappable", strtoken (\s -> IToverlappable_prag (SourceText s))),
     ("overlapping", strtoken (\s -> IToverlapping_prag (SourceText s))),
     ("incoherent", strtoken (\s -> ITincoherent_prag (SourceText s))),
     ("ctype", strtoken (\s -> ITctype (SourceText s))),
     ("complete", strtoken (\s -> ITcomplete_prag (SourceText s))),
     ("column", columnPrag)
     ]

twoWordPrags = Map.fromList [
     ("inline conlike",
         strtoken (\s -> (ITinline_prag (SourceText s) Inline ConLike))),
     ("notinline conlike",
         strtoken (\s -> (ITinline_prag (SourceText s) NoInline ConLike))),
     ("specialize inline",
         strtoken (\s -> (ITspec_inline_prag (SourceText s) True))),
     ("specialize notinline",
         strtoken (\s -> (ITspec_inline_prag (SourceText s) False)))
     ]

dispatch_pragmas :: Map String Action -> Action
dispatch_pragmas prags span buf len = case Map.lookup (clean_pragma (lexemeToString buf len)) prags of
                                       Just found -> found span buf len
                                       Nothing -> lexError "unknown pragma"

known_pragma :: Map String Action -> AlexAccPred ExtsBitmap
known_pragma prags _ (AI _ startbuf) _ (AI _ curbuf)
 = isKnown && nextCharIsNot curbuf pragmaNameChar
    where l = lexemeToString startbuf (byteDiff startbuf curbuf)
          isKnown = isJust $ Map.lookup (clean_pragma l) prags
          pragmaNameChar c = isAlphaNum c || c == '_'

clean_pragma :: String -> String
clean_pragma prag = canon_ws (map toLower (unprefix prag))
                    where unprefix prag' = case stripPrefix "{-#" prag' of
                                             Just rest -> rest
                                             Nothing -> prag'
                          canonical prag' = case prag' of
                                              "noinline" -> "notinline"
                                              "specialise" -> "specialize"
                                              "constructorlike" -> "conlike"
                                              _ -> prag'
                          canon_ws s = unwords (map canonical (words s))



{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************
-}

-- | Encapsulated call to addAnnotation, requiring only the SrcSpan of
--   the AST construct the annotation belongs to; together with the
--   AnnKeywordId, this is the key of the annotation map.
--
--   This type is useful for places in the parser where it is not yet
--   known what SrcSpan an annotation should be added to.  The most
--   common situation is when we are parsing a list: the annotations
--   need to be associated with the AST element that *contains* the
--   list, not the list itself.  'AddAnn' lets us defer adding the
--   annotations until we finish parsing the list and are now parsing
--   the enclosing element; we then apply the 'AddAnn' to associate
--   the annotations.  Another common situation is where a common fragment of
--   the AST has been factored out but there is no separate AST node for
--   this fragment (this occurs in class and data declarations). In this
--   case, the annotation belongs to the parent data declaration.
--
--   The usual way an 'AddAnn' is created is using the 'mj' ("make jump")
--   function, and then it can be discharged using the 'ams' function.
type AddAnn = SrcSpan -> P ()

addAnnotation :: SrcSpan          -- SrcSpan of enclosing AST construct
              -> AnnKeywordId     -- The first two parameters are the key
              -> SrcSpan          -- The location of the keyword itself
              -> P ()
addAnnotation l a v = do
  addAnnotationOnly l a v
  allocateComments l

addAnnotationOnly :: SrcSpan -> AnnKeywordId -> SrcSpan -> P ()
addAnnotationOnly l a v = P $ \s -> POk s {
  annotations = ((l,a), [v]) : annotations s
  } ()

-- |Given a location and a list of AddAnn, apply them all to the location.
addAnnsAt :: SrcSpan -> [AddAnn] -> P ()
addAnnsAt loc anns = mapM_ (\a -> a loc) anns

-- |Given a 'SrcSpan' that surrounds a 'HsPar' or 'HsParTy', generate
-- 'AddAnn' values for the opening and closing bordering on the start
-- and end of the span
mkParensApiAnn :: SrcSpan -> [AddAnn]
mkParensApiAnn (UnhelpfulSpan _)  = []
mkParensApiAnn s@(RealSrcSpan ss) = [mj AnnOpenP lo,mj AnnCloseP lc]
  where
    mj a l = (\s -> addAnnotation s a l)
    f = srcSpanFile ss
    sl = srcSpanStartLine ss
    sc = srcSpanStartCol ss
    el = srcSpanEndLine ss
    ec = srcSpanEndCol ss
    lo = mkSrcSpan (srcSpanStart s)         (mkSrcLoc f sl (sc+1))
    lc = mkSrcSpan (mkSrcLoc f el (ec - 1)) (srcSpanEnd s)

queueComment :: Located Token -> P()
queueComment c = P $ \s -> POk s {
  comment_q = commentToAnnotation c : comment_q s
  } ()

-- | Go through the @comment_q@ in @PState@ and remove all comments
-- that belong within the given span
allocateComments :: SrcSpan -> P ()
allocateComments ss = P $ \s ->
  let
    (before,rest)  = break (\(L l _) -> isSubspanOf l ss) (comment_q s)
    (middle,after) = break (\(L l _) -> not (isSubspanOf l ss)) rest
    comment_q' = before ++ after
    newAnns = if null middle then []
                             else [(ss,middle)]
  in
    POk s {
       comment_q = comment_q'
     , annotations_comments = newAnns ++ (annotations_comments s)
     } ()

commentToAnnotation :: Located Token -> Located AnnotationComment
commentToAnnotation (L l (ITdocCommentNext s))  = L l (AnnDocCommentNext s)
commentToAnnotation (L l (ITdocCommentPrev s))  = L l (AnnDocCommentPrev s)
commentToAnnotation (L l (ITdocCommentNamed s)) = L l (AnnDocCommentNamed s)
commentToAnnotation (L l (ITdocSection n s))    = L l (AnnDocSection n s)
commentToAnnotation (L l (ITdocOptions s))      = L l (AnnDocOptions s)
commentToAnnotation (L l (ITlineComment s))     = L l (AnnLineComment s)
commentToAnnotation (L l (ITblockComment s))    = L l (AnnBlockComment s)
commentToAnnotation _                           = panic "commentToAnnotation"

-- ---------------------------------------------------------------------

isComment :: Token -> Bool
isComment (ITlineComment     _)   = True
isComment (ITblockComment    _)   = True
isComment _ = False

isDocComment :: Token -> Bool
isDocComment (ITdocCommentNext  _)   = True
isDocComment (ITdocCommentPrev  _)   = True
isDocComment (ITdocCommentNamed _)   = True
isDocComment (ITdocSection      _ _) = True
isDocComment (ITdocOptions      _)   = True
isDocComment _ = False
}
