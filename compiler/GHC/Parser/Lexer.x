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
-- [3] https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE PatternSynonyms #-}


{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Parser.Lexer (
   Token(..), lexer, lexerDbg,
   ParserOpts(..), mkParserOpts,
   PState (..), initParserState, initPragState,
   P(..), ParseResult(POk, PFailed),
   allocateComments, allocatePriorComments, allocateFinalComments,
   MonadP(..),
   getRealSrcLoc, getPState,
   failMsgP, failLocMsgP, srcParseFail,
   getPsErrorMessages, getPsMessages,
   popContext, pushModuleContext, setLastToken, setSrcLoc,
   activeContext, nextIsEOF,
   getLexState, popLexState, pushLexState,
   ExtBits(..),
   xtest, xunset, xset,
   disableHaddock,
   lexTokenStream,
   mkParensEpAnn,
   getCommentsFor, getPriorCommentsFor, getFinalCommentsFor,
   getEofPos,
   commentToAnnotation,
   HdkComment(..),
   warnopt,
   adjustChar,
   addPsMessage
  ) where

import GHC.Prelude
import qualified GHC.Data.Strict as Strict

-- base
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List (stripPrefix, isInfixOf, partition)
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Word
import Debug.Trace (trace)

import GHC.Data.EnumSet as EnumSet

-- ghc-boot
import qualified GHC.LanguageExtensions as LangExt

-- bytestring
import Data.ByteString (ByteString)

-- containers
import Data.Map (Map)
import qualified Data.Map as Map

-- compiler
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Types.Error
import GHC.Types.Unique.FM
import GHC.Data.Maybe
import GHC.Data.OrdList
import GHC.Utils.Misc ( readSignificandExponentPair, readHexSignificandExponentPair )

import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Types.Basic ( InlineSpec(..), RuleMatchInfo(..))
import GHC.Hs.Doc

import GHC.Parser.CharClass

import GHC.Parser.Annotation
import GHC.Driver.Flags
import GHC.Parser.Errors.Basic
import GHC.Parser.Errors.Types
import GHC.Parser.Errors.Ppr ()
}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

-- NB: The logic behind these definitions is also reflected in "GHC.Utils.Lexeme"
-- Any changes here should likely be reflected there.
$unispace    = \x05 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n -- TODO #8424
$tab         = \t

$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$decdigit  = $ascdigit -- exactly $ascdigit, no more no less.
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']

$unilarge  = \x01 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]

$unismall  = \x02 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]

$uniidchar = \x07 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$idchar    = [$small $large $digit $uniidchar \']

$unigraphic = \x06 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$graphic   = [$small $large $symbol $digit $idchar $special $unigraphic \"\']

$binit     = 0-1
$octit     = 0-7
$hexit     = [$decdigit A-F a-f]

$pragmachar = [$small $large $digit $uniidchar ]

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

@binarylit      = 0[bB] @numspc @binary
@octallit       = 0[oO] @numspc @octal
@hexadecimallit = 0[xX] @numspc @hexadecimal

@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid
@qvarsym = @qual @varsym
@qconsym = @qual @consym

-- QualifiedDo needs to parse "M.do" not as a variable, so as to keep the
-- layout rules.
@qdo    = @qual "do"
@qmdo   = @qual "mdo"

@floating_point = @numspc @decimal \. @decimal @exponent? | @numspc @decimal @exponent
@hex_floating_point = @numspc @hexadecimal \. @hexadecimal @bin_exponent? | @numspc @hexadecimal @bin_exponent

-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-


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

"{-" / { isNormalComment } { nested_comment }

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

"-- " $docsym .* / { alexNotPred (ifExtension HaddockBit) } { lineCommentToken }

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

-- Everywhere: check for smart quotes--they are not allowed outside of strings
$unigraphic / { isSmartQuote } { smart_quote_error }

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
  ^\# \! .* \n                          ; -- #!, for scripts  -- gcc
  ^\  \# \! .* \n                       ; --  #!, for scripts -- clang; See #6132
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
                     { nested_comment }
}

<0,option_prags> {

-- This code would eagerly accept and hence discard, e.g., "LANGUAGE MagicHash".
--  "{-#" $whitechar* $pragmachar+
--        $whitechar+ $pragmachar+
--        { warn_unknown_prag twoWordPrags }

  "{-#" $whitechar* $pragmachar+
        { warn_unknown_prag (Map.unions [ oneWordPrags, fileHeaderPrags, ignoredPrags, linePrags ]) }

  "{-#" { warn_unknown_prag Map.empty }
}

-- '0' state: ordinary lexemes

-- Haddock comments

"-- " $docsym      / { ifExtension HaddockBit } { multiline_doc_comment }
"{-" \ ? $docsym   / { ifExtension HaddockBit } { nested_doc_comment }

-- "special" symbols

<0> {

  -- Don't check ThQuotesBit here as the renamer can produce a better
  -- error message than the lexer (see the thQuotesEnabled check in rnBracket).
  "[|"  { token (ITopenExpQuote NoE NormalSyntax) }
  "[||" { token (ITopenTExpQuote NoE) }
  "|]"  { token (ITcloseQuote NormalSyntax) }
  "||]" { token ITcloseTExpQuote }

  -- Check ThQuotesBit here as to not steal syntax.
  "[e|"       / { ifExtension ThQuotesBit } { token (ITopenExpQuote HasE NormalSyntax) }
  "[e||"      / { ifExtension ThQuotesBit } { token (ITopenTExpQuote HasE) }
  "[p|"       / { ifExtension ThQuotesBit } { token ITopenPatQuote }
  "[d|"       / { ifExtension ThQuotesBit } { layout_token ITopenDecQuote }
  "[t|"       / { ifExtension ThQuotesBit } { token ITopenTypQuote }

  "[" @varid "|"  / { ifExtension QqBit }   { lex_quasiquote_tok }

  -- qualified quasi-quote (#5555)
  "[" @qvarid "|"  / { ifExtension QqBit }  { lex_qquasiquote_tok }

  $unigraphic -- ⟦
    / { ifCurrentChar '⟦' `alexAndPred`
        ifExtension UnicodeSyntaxBit `alexAndPred`
        ifExtension ThQuotesBit }
    { token (ITopenExpQuote NoE UnicodeSyntax) }
  $unigraphic -- ⟧
    / { ifCurrentChar '⟧' `alexAndPred`
        ifExtension UnicodeSyntaxBit `alexAndPred`
        ifExtension ThQuotesBit }
    { token (ITcloseQuote UnicodeSyntax) }
}

<0> {
  "(|"
    / { ifExtension ArrowsBit `alexAndPred`
        notFollowedBySymbol }
    { special (IToparenbar NormalSyntax) }
  "|)"
    / { ifExtension ArrowsBit }
    { special (ITcparenbar NormalSyntax) }

  $unigraphic -- ⦇
    / { ifCurrentChar '⦇' `alexAndPred`
        ifExtension UnicodeSyntaxBit `alexAndPred`
        ifExtension ArrowsBit }
    { special (IToparenbar UnicodeSyntax) }
  $unigraphic -- ⦈
    / { ifCurrentChar '⦈' `alexAndPred`
        ifExtension UnicodeSyntaxBit `alexAndPred`
        ifExtension ArrowsBit }
    { special (ITcparenbar UnicodeSyntax) }
}

<0> {
  \? @varid / { ifExtension IpBit } { skip_one_varid ITdupipvarid }
}

<0> {
  "#" $idchar+ / { ifExtension OverloadedLabelsBit } { skip_one_varid_src ITlabelvarid }
  "#" \" / { ifExtension OverloadedLabelsBit } { lex_quoted_label }
}

<0> {
  "(#" / { ifExtension UnboxedParensBit }
         { token IToubxparen }
  "#)" / { ifExtension UnboxedParensBit }
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
  @qdo                                      { qdo_token ITdo }
  @qmdo    / { ifExtension RecursiveDoBit } { qdo_token ITmdo }
  @qvarid                       { idtoken qvarid }
  @qconid                       { idtoken qconid }
  @varid                        { varid }
  @conid                        { idtoken conid }
}

<0> {
  @qvarid "#"+      / { ifExtension MagicHashBit } { idtoken qvarid }
  @qconid "#"+      / { ifExtension MagicHashBit } { idtoken qconid }
  @varid "#"+       / { ifExtension MagicHashBit } { varid }
  @conid "#"+       / { ifExtension MagicHashBit } { idtoken conid }
}

-- ToDo: - move `var` and (sym) into lexical syntax?
--       - remove backquote from $special?
<0> {
  @qvarsym                                         { idtoken qvarsym }
  @qconsym                                         { idtoken qconsym }
  @varsym                                          { with_op_ws varsym }
  @consym                                          { with_op_ws consym }
}

-- For the normal boxed literals we need to be careful
-- when trying to be close to Haskell98

-- Note [Lexing NumericUnderscores extension] (#14473)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- NumericUnderscores extension allows underscores in numeric literals.
-- Multiple underscores are represented with @numspc macro.
-- To be simpler, we have only the definitions with underscores.
-- And then we have a separate function (tok_integral and tok_frac)
-- that validates the literals.
-- If extensions are not enabled, check that there are no underscores.
--
<0> {
  -- Normal integral literals (:: Num a => a, from Integer)
  @decimal                                                      { tok_num positive 0 0 decimal }
  @binarylit                / { ifExtension BinaryLiteralsBit } { tok_num positive 2 2 binary }
  @octallit                                                     { tok_num positive 2 2 octal }
  @hexadecimallit                                               { tok_num positive 2 2 hexadecimal }
  @negative @decimal        / { negLitPred }                    { tok_num negative 1 1 decimal }
  @negative @binarylit      / { negLitPred `alexAndPred`
                                ifExtension BinaryLiteralsBit } { tok_num negative 3 3 binary }
  @negative @octallit       / { negLitPred }                    { tok_num negative 3 3 octal }
  @negative @hexadecimallit / { negLitPred }                    { tok_num negative 3 3 hexadecimal }

  -- Normal rational literals (:: Fractional a => a, from Rational)
  @floating_point                                                            { tok_frac 0 tok_float }
  @negative @floating_point            / { negLitPred }                      { tok_frac 0 tok_float }
  0[xX] @numspc @hex_floating_point    / { ifExtension HexFloatLiteralsBit } { tok_frac 0 tok_hex_float }
  @negative 0[xX] @numspc @hex_floating_point
                                       / { ifExtension HexFloatLiteralsBit `alexAndPred`
                                           negLitPred }                      { tok_frac 0 tok_hex_float }
}

<0> {
  -- Unboxed ints (:: Int#) and words (:: Word#)
  -- It's simpler (and faster?) to give separate cases to the negatives,
  -- especially considering octal/hexadecimal prefixes.
  @decimal                     \# / { ifExtension MagicHashBit }        { tok_primint positive 0 1 decimal }
  @binarylit                   \# / { ifExtension MagicHashBit `alexAndPred`
                                      ifExtension BinaryLiteralsBit }   { tok_primint positive 2 3 binary }
  @octallit                    \# / { ifExtension MagicHashBit }        { tok_primint positive 2 3 octal }
  @hexadecimallit              \# / { ifExtension MagicHashBit }        { tok_primint positive 2 3 hexadecimal }
  @negative @decimal           \# / { negHashLitPred MagicHashBit }     { tok_primint negative 1 2 decimal }
  @negative @binarylit         \# / { negHashLitPred MagicHashBit `alexAndPred`
                                      ifExtension BinaryLiteralsBit }   { tok_primint negative 3 4 binary }
  @negative @octallit          \# / { negHashLitPred MagicHashBit }     { tok_primint negative 3 4 octal }
  @negative @hexadecimallit    \# / { negHashLitPred MagicHashBit }     { tok_primint negative 3 4 hexadecimal }

  @decimal                  \# \# / { ifExtension MagicHashBit }        { tok_primword 0 2 decimal }
  @binarylit                \# \# / { ifExtension MagicHashBit `alexAndPred`
                                      ifExtension BinaryLiteralsBit }   { tok_primword 2 4 binary }
  @octallit                 \# \# / { ifExtension MagicHashBit }        { tok_primword 2 4 octal }
  @hexadecimallit           \# \# / { ifExtension MagicHashBit }        { tok_primword 2 4 hexadecimal }

  -- Unboxed floats and doubles (:: Float#, :: Double#)
  -- prim_{float,double} work with signed literals
  @floating_point                  \# / { ifExtension MagicHashBit }        { tok_frac 1 tok_primfloat }
  @floating_point               \# \# / { ifExtension MagicHashBit }        { tok_frac 2 tok_primdouble }

  @negative @floating_point        \# / { negHashLitPred MagicHashBit }     { tok_frac 1 tok_primfloat }
  @negative @floating_point     \# \# / { negHashLitPred MagicHashBit }     { tok_frac 2 tok_primdouble }

  @decimal                  \#"Int8"   / { ifExtension ExtendedLiteralsBit } { tok_primint8 positive 0 decimal }
  @binarylit                \#"Int8"   / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint8 positive 2 binary }
  @octallit                 \#"Int8"   / { ifExtension ExtendedLiteralsBit } { tok_primint8 positive 2 octal }
  @hexadecimallit           \#"Int8"   / { ifExtension ExtendedLiteralsBit } { tok_primint8 positive 2 hexadecimal }
  @negative @decimal        \#"Int8"   / { negHashLitPred ExtendedLiteralsBit } { tok_primint8 negative 1 decimal }
  @negative @binarylit      \#"Int8"   / { negHashLitPred ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint8 negative 3 binary }
  @negative @octallit       \#"Int8"   / { negHashLitPred ExtendedLiteralsBit } { tok_primint8 negative 3 octal }
  @negative @hexadecimallit \#"Int8"   / { negHashLitPred ExtendedLiteralsBit } { tok_primint8 negative 3 hexadecimal }

  @decimal                  \#"Int16"  / { ifExtension ExtendedLiteralsBit } { tok_primint16 positive 0 decimal }
  @binarylit                \#"Int16"  / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint16 positive 2 binary }
  @octallit                 \#"Int16"  / { ifExtension ExtendedLiteralsBit } { tok_primint16 positive 2 octal }
  @hexadecimallit           \#"Int16"  / { ifExtension ExtendedLiteralsBit } { tok_primint16 positive 2 hexadecimal }
  @negative @decimal        \#"Int16"  / { negHashLitPred ExtendedLiteralsBit} { tok_primint16 negative 1 decimal }
  @negative @binarylit      \#"Int16"  / { negHashLitPred ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint16 negative 3 binary }
  @negative @octallit       \#"Int16"  / { negHashLitPred ExtendedLiteralsBit} { tok_primint16 negative 3 octal }
  @negative @hexadecimallit \#"Int16"  / { negHashLitPred ExtendedLiteralsBit} { tok_primint16 negative 3 hexadecimal }

  @decimal                  \#"Int32"  / { ifExtension ExtendedLiteralsBit } { tok_primint32 positive 0 decimal }
  @binarylit                \#"Int32"  / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint32 positive 2 binary }
  @octallit                 \#"Int32"  / { ifExtension ExtendedLiteralsBit } { tok_primint32 positive 2 octal }
  @hexadecimallit           \#"Int32"  / { ifExtension ExtendedLiteralsBit } { tok_primint32 positive 2 hexadecimal }
  @negative @decimal        \#"Int32"  / { negHashLitPred ExtendedLiteralsBit } { tok_primint32 negative 1 decimal }
  @negative @binarylit      \#"Int32"  / { negHashLitPred ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint32 negative 3 binary }
  @negative @octallit       \#"Int32"  / { negHashLitPred ExtendedLiteralsBit} { tok_primint32 negative 3 octal }
  @negative @hexadecimallit \#"Int32"  / { negHashLitPred ExtendedLiteralsBit} { tok_primint32 negative 3 hexadecimal }

  @decimal                  \#"Int64"  / { ifExtension ExtendedLiteralsBit } { tok_primint64 positive 0 decimal }
  @binarylit                \#"Int64"  / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint64 positive 2 binary }
  @octallit                 \#"Int64"  / { ifExtension ExtendedLiteralsBit } { tok_primint64 positive 2 octal }
  @hexadecimallit           \#"Int64"  / { ifExtension ExtendedLiteralsBit } { tok_primint64 positive 2 hexadecimal }
  @negative @decimal        \#"Int64"  / { negHashLitPred ExtendedLiteralsBit } { tok_primint64 negative 1 decimal }
  @negative @binarylit      \#"Int64"  / { negHashLitPred ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint64 negative 3 binary }
  @negative @octallit       \#"Int64"  / { negHashLitPred ExtendedLiteralsBit } { tok_primint64 negative 3 octal }
  @negative @hexadecimallit \#"Int64"  / { negHashLitPred ExtendedLiteralsBit } { tok_primint64 negative 3 hexadecimal }

  @decimal                  \#"Int"    / { ifExtension ExtendedLiteralsBit } { tok_primint positive 0 4 decimal }
  @binarylit                \#"Int"    / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint positive 2 6 binary }
  @octallit                 \#"Int"    / { ifExtension ExtendedLiteralsBit } { tok_primint positive 2 6 octal }
  @hexadecimallit           \#"Int"    / { ifExtension ExtendedLiteralsBit } { tok_primint positive 2 6 hexadecimal }
  @negative @decimal        \#"Int"    / { negHashLitPred ExtendedLiteralsBit } { tok_primint negative 1 5 decimal }
  @negative @binarylit      \#"Int"    / { negHashLitPred ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint negative 3 7 binary }
  @negative @octallit       \#"Int"    / { negHashLitPred ExtendedLiteralsBit } { tok_primint negative 3 7 octal }
  @negative @hexadecimallit \#"Int"    / { negHashLitPred ExtendedLiteralsBit } { tok_primint negative 3 7 hexadecimal }

  @decimal                  \#"Word8"  / { ifExtension ExtendedLiteralsBit } { tok_primword8 0 decimal }
  @binarylit                \#"Word8"  / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primword8 2 binary }
  @octallit                 \#"Word8"  / { ifExtension ExtendedLiteralsBit } { tok_primword8 2 octal }
  @hexadecimallit           \#"Word8"  / { ifExtension ExtendedLiteralsBit } { tok_primword8 2 hexadecimal }

  @decimal                  \#"Word16" / { ifExtension ExtendedLiteralsBit } { tok_primword16 0 decimal }
  @binarylit                \#"Word16" / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primword16 2 binary }
  @octallit                 \#"Word16" / { ifExtension ExtendedLiteralsBit } { tok_primword16 2 octal }
  @hexadecimallit           \#"Word16" / { ifExtension ExtendedLiteralsBit } { tok_primword16 2 hexadecimal }

  @decimal                  \#"Word32" / { ifExtension ExtendedLiteralsBit } { tok_primword32 0 decimal }
  @binarylit                \#"Word32" / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primword32 2 binary }
  @octallit                 \#"Word32" / { ifExtension ExtendedLiteralsBit } { tok_primword32 2 octal }
  @hexadecimallit           \#"Word32" / { ifExtension ExtendedLiteralsBit } { tok_primword32 2 hexadecimal }

  @decimal                  \#"Word64" / { ifExtension ExtendedLiteralsBit } { tok_primword64 0 decimal }
  @binarylit                \#"Word64" / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primword64 2 binary }
  @octallit                 \#"Word64" / { ifExtension ExtendedLiteralsBit } { tok_primword64 2 octal }
  @hexadecimallit           \#"Word64" / { ifExtension ExtendedLiteralsBit } { tok_primword64 2 hexadecimal }

  @decimal                  \#"Word"   / { ifExtension ExtendedLiteralsBit } { tok_primword 0 5 decimal }
  @binarylit                \#"Word"   / { ifExtension ExtendedLiteralsBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primword 2 7 binary }
  @octallit                 \#"Word"   / { ifExtension ExtendedLiteralsBit } { tok_primword 2 7 octal }
  @hexadecimallit           \#"Word"   / { ifExtension ExtendedLiteralsBit } { tok_primword 2 7 hexadecimal }

}

-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0> {
  \'                            { lex_char_tok }
  \"                            { lex_string_tok }
}

-- Note [Whitespace-sensitive operator parsing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In accord with GHC Proposal #229 https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst
-- we classify operator occurrences into four categories:
--
--     a ! b   -- a loose infix occurrence
--     a!b     -- a tight infix occurrence
--     a !b    -- a prefix occurrence
--     a! b    -- a suffix occurrence
--
-- The rules are a bit more elaborate than simply checking for whitespace, in
-- order to accommodate the following use cases:
--
--     f (!a) = ...    -- prefix occurrence
--     g (a !)         -- loose infix occurrence
--     g (! a)         -- loose infix occurrence
--
-- The precise rules are as follows:
--
--  * Identifiers, literals, and opening brackets (, (#, (|, [, [|, [||, [p|,
--    [e|, [t|, {, ⟦, ⦇, are considered "opening tokens". The function
--    followedByOpeningToken tests whether the next token is an opening token.
--
--  * Identifiers, literals, and closing brackets ), #), |), ], |], }, ⟧, ⦈,
--    are considered "closing tokens". The function precededByClosingToken tests
--    whether the previous token is a closing token.
--
--  * Whitespace, comments, separators, and other tokens, are considered
--    neither opening nor closing.
--
--  * Any unqualified operator occurrence is classified as prefix, suffix, or
--    tight/loose infix, based on preceding and following tokens:
--
--       precededByClosingToken | followedByOpeningToken | Occurrence
--      ------------------------+------------------------+------------
--       False                  | True                   | prefix
--       True                   | False                  | suffix
--       True                   | True                   | tight infix
--       False                  | False                  | loose infix
--      ------------------------+------------------------+------------
--
-- A loose infix occurrence is always considered an operator. Other types of
-- occurrences may be assigned a special per-operator meaning override:
--
--   Operator |  Occurrence   | Token returned
--  ----------+---------------+------------------------------------------
--    !       |  prefix       | ITbang
--            |               |   strictness annotation or bang pattern,
--            |               |   e.g.  f !x = rhs, data T = MkT !a
--            |  not prefix   | ITvarsym "!"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  xs ! 3, (! x), Int ! Bool
--  ----------+---------------+------------------------------------------
--    ~       |  prefix       | ITtilde
--            |               |   laziness annotation or lazy pattern,
--            |               |   e.g.  f ~x = rhs, data T = MkT ~a
--            |  not prefix   | ITvarsym "~"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  xs ~ 3, (~ x), Int ~ Bool
--  ----------+---------------+------------------------------------------
--    .       |  prefix       | ITproj True
--            |               |   field projection,
--            |               |   e.g.  .x
--            |  tight infix  | ITproj False
--            |               |   field projection,
--            |               |   e.g. r.x
--            |  suffix       | ITdot
--            |               |   function composition,
--            |               |   e.g. f. g
--            |  loose infix  | ITdot
--            |               |   function composition,
--            |               |   e.g.  f . g
--  ----------+---------------+------------------------------------------
--    $  $$   |  prefix       | ITdollar, ITdollardollar
--            |               |   untyped or typed Template Haskell splice,
--            |               |   e.g.  $(f x), $$(f x), $$"str"
--            |  not prefix   | ITvarsym "$", ITvarsym "$$"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  f $ g x, a $$ b
--  ----------+---------------+------------------------------------------
--    @       |  prefix       | ITtypeApp
--            |               |   type application, e.g.  fmap @Maybe
--            |  tight infix  | ITat
--            |               |   as-pattern, e.g.  f p@(a,b) = rhs
--            |  suffix       | parse error
--            |               |   e.g. f p@ x = rhs
--            |  loose infix  | ITvarsym "@"
--            |               |   ordinary operator or type operator,
--            |               |   e.g.  f @ g, (f @)
--  ----------+---------------+------------------------------------------
--
-- Also, some of these overrides are guarded behind language extensions.
-- According to the specification, we must determine the occurrence based on
-- surrounding *tokens* (see the proposal for the exact rules). However, in
-- the implementation we cheat a little and do the classification based on
-- characters, for reasons of both simplicity and efficiency (see
-- 'followedByOpeningToken' and 'precededByClosingToken')
--
-- When an operator is subject to a meaning override, it is mapped to special
-- token: ITbang, ITtilde, ITat, ITdollar, ITdollardollar. Otherwise, it is
-- returned as ITvarsym.
--
-- For example, this is how we process the (!):
--
--    precededByClosingToken | followedByOpeningToken | Token
--   ------------------------+------------------------+-------------
--    False                  | True                   | ITbang
--    True                   | False                  | ITvarsym "!"
--    True                   | True                   | ITvarsym "!"
--    False                  | False                  | ITvarsym "!"
--   ------------------------+------------------------+-------------
--
-- And this is how we process the (@):
--
--    precededByClosingToken | followedByOpeningToken | Token
--   ------------------------+------------------------+-------------
--    False                  | True                   | ITtypeApp
--    True                   | False                  | parse error
--    True                   | True                   | ITat
--    False                  | False                  | ITvarsym "@"
--   ------------------------+------------------------+-------------

-- -----------------------------------------------------------------------------
-- Alex "Haskell code fragment bottom"

{

-- Operator whitespace occurrence. See Note [Whitespace-sensitive operator parsing].
data OpWs
  = OpWsPrefix         -- a !b
  | OpWsSuffix         -- a! b
  | OpWsTightInfix     -- a!b
  | OpWsLooseInfix     -- a ! b
  deriving Show

-- -----------------------------------------------------------------------------
-- The token type

data Token
  = ITas                        -- Haskell keywords
  | ITcase
  | ITclass
  | ITdata
  | ITdefault
  | ITderiving
  | ITdo (Maybe FastString)
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
  | ITmdo (Maybe FastString)
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

  -- Pragmas, see  Note [Pragma source text] in "GHC.Types.SourceText"
  | ITinline_prag       SourceText InlineSpec RuleMatchInfo
  | ITopaque_prag       SourceText
  | ITspec_prag         SourceText                -- SPECIALISE
  | ITspec_inline_prag  SourceText Bool    -- SPECIALISE INLINE (or NOINLINE)
  | ITsource_prag       SourceText
  | ITrules_prag        SourceText
  | ITwarning_prag      SourceText
  | ITdeprecated_prag   SourceText
  | ITline_prag         SourceText  -- not usually produced, see 'UsePosPragsBit'
  | ITcolumn_prag       SourceText  -- not usually produced, see 'UsePosPragsBit'
  | ITscc_prag          SourceText
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
  | ITlcases
  | ITvbar
  | ITlarrow            IsUnicodeSyntax
  | ITrarrow            IsUnicodeSyntax
  | ITdarrow            IsUnicodeSyntax
  | ITlolly       -- The (⊸) arrow (for LinearTypes)
  | ITminus       -- See Note [Minus tokens]
  | ITprefixminus -- See Note [Minus tokens]
  | ITbang     -- Prefix (!) only, e.g. f !x = rhs
  | ITtilde    -- Prefix (~) only, e.g. f ~x = rhs
  | ITat       -- Tight infix (@) only, e.g. f x@pat = rhs
  | ITtypeApp  -- Prefix (@) only, e.g. f @t
  | ITpercent  -- Prefix (%) only, e.g. a %1 -> b
  | ITstar              IsUnicodeSyntax
  | ITdot
  | ITproj Bool -- Extension: OverloadedRecordDotBit

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
  | ITlabelvarid SourceText FastString   -- Overloaded label: #x
                                         -- The SourceText is required because we can
                                         -- have a string literal as a label
                                         -- Note [Literal source text] in "GHC.Types.SourceText"

  | ITchar     SourceText Char       -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITstring   SourceText FastString -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITinteger  IntegralLit           -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITrational FractionalLit

  | ITprimchar   SourceText Char     -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimstring SourceText ByteString -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimint    SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimword   SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimint8   SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimint16  SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimint32  SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimint64  SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimword8  SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimword16 SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimword32 SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
  | ITprimword64 SourceText Integer  -- Note [Literal source text] in "GHC.Types.SourceText"
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
  | ITdollar                            --  prefix $
  | ITdollardollar                      --  prefix $$
  | ITtyQuote                           --  ''
  | ITquasiQuote (FastString,FastString,PsSpan)
    -- ITquasiQuote(quoter, quote, loc)
    -- represents a quasi-quote of the form
    -- [quoter| quote |]
  | ITqQuasiQuote (FastString,FastString,FastString,PsSpan)
    -- ITqQuasiQuote(Qual, quoter, quote, loc)
    -- represents a qualified quasi-quote of the form
    -- [Qual.quoter| quote |]

  -- Arrow notation extension
  | ITproc
  | ITrec
  | IToparenbar  IsUnicodeSyntax -- ^ @(|@
  | ITcparenbar  IsUnicodeSyntax -- ^ @|)@
  | ITlarrowtail IsUnicodeSyntax -- ^ @-<@
  | ITrarrowtail IsUnicodeSyntax -- ^ @>-@
  | ITLarrowtail IsUnicodeSyntax -- ^ @-<<@
  | ITRarrowtail IsUnicodeSyntax -- ^ @>>-@

  | ITunknown String             -- ^ Used when the lexer can't make sense of it
  | ITeof                        -- ^ end of file token

  -- Documentation annotations. See Note [PsSpan in Comments]
  | ITdocComment   HsDocString PsSpan -- ^ The HsDocString contains more details about what
                                      -- this is and how to pretty print it
  | ITdocOptions   String      PsSpan -- ^ doc options (prune, ignore-exports, etc)
  | ITlineComment  String      PsSpan -- ^ comment starting by "--"
  | ITblockComment String      PsSpan -- ^ comment in {- -}

  deriving Show

instance Outputable Token where
  ppr x = text (show x)

{- Note [PsSpan in Comments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When using the Api Annotations to exact print a modified AST, managing
the space before a comment is important.  The PsSpan in the comment
token allows this to happen, and this location is tracked in prev_loc
in PState.  This only tracks physical tokens, so is not updated for
zero-width ones.

We also use this to track the space before the end-of-file marker.
-}

{- Note [Minus tokens]
~~~~~~~~~~~~~~~~~~~~~~
A minus sign can be used in prefix form (-x) and infix form (a - b).

When LexicalNegation is on:
  * ITprefixminus  represents the prefix form
  * ITvarsym "-"   represents the infix form
  * ITminus        is not used

When LexicalNegation is off:
  * ITminus        represents all forms
  * ITprefixminus  is not used
  * ITvarsym "-"   is not used
-}

{- Note [Why not LexicalNegationBit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One might wonder why we define NoLexicalNegationBit instead of
LexicalNegationBit. The problem lies in the following line in reservedSymsFM:

    ,("-", ITminus, NormalSyntax, xbit NoLexicalNegationBit)

We want to generate ITminus only when LexicalNegation is off. How would one
do it if we had LexicalNegationBit? I (int-index) tried to use bitwise
complement:

    ,("-", ITminus, NormalSyntax, complement (xbit LexicalNegationBit))

This did not work, so I opted for NoLexicalNegationBit instead.
-}


-- the bitmap provided as the third component indicates whether the
-- corresponding extension keyword is valid under the extension options
-- provided to the compiler; if the extension corresponding to *any* of the
-- bits set in the bitmap is enabled, the keyword is valid (this setup
-- facilitates using a keyword in two different extensions that can be
-- activated independently)
--
reservedWordsFM :: UniqFM FastString (Token, ExtsBitmap)
reservedWordsFM = listToUFM $
    map (\(x, y, z) -> (mkFastString x, (y, z)))
        [( "_",              ITunderscore,    0 ),
         ( "as",             ITas,            0 ),
         ( "case",           ITcase,          0 ),
         ( "cases",          ITlcases,        xbit LambdaCaseBit ),
         ( "class",          ITclass,         0 ),
         ( "data",           ITdata,          0 ),
         ( "default",        ITdefault,       0 ),
         ( "deriving",       ITderiving,      0 ),
         ( "do",             ITdo Nothing,    0 ),
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

         ( "forall",         ITforall NormalSyntax, 0),
         ( "mdo",            ITmdo Nothing,   xbit RecursiveDoBit),
             -- See Note [Lexing type pseudo-keywords]
         ( "family",         ITfamily,        0 ),
         ( "role",           ITrole,          0 ),
         ( "pattern",        ITpattern,       xbit PatternSynonymsBit),
         ( "static",         ITstatic,        xbit StaticPointersBit ),
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
not allowed. Furthermore, checks further downstream (GHC.Tc.TyCl) ensure that
type families and role annotations are never declared without their extensions
on. In fact, by unconditionally lexing these pseudo-keywords as special, we
can get better error messages.

Also, note that these are included in the `varid` production in the parser --
a key detail to make all this work.
-------------------------------------}

reservedSymsFM :: UniqFM FastString (Token, IsUnicodeSyntax, ExtsBitmap)
reservedSymsFM = listToUFM $
    map (\ (x,w,y,z) -> (mkFastString x,(w,y,z)))
      [ ("..",  ITdotdot,                   NormalSyntax,  0 )
        -- (:) is a reserved op, meaning only list cons
       ,(":",   ITcolon,                    NormalSyntax,  0 )
       ,("::",  ITdcolon NormalSyntax,      NormalSyntax,  0 )
       ,("=",   ITequal,                    NormalSyntax,  0 )
       ,("\\",  ITlam,                      NormalSyntax,  0 )
       ,("|",   ITvbar,                     NormalSyntax,  0 )
       ,("<-",  ITlarrow NormalSyntax,      NormalSyntax,  0 )
       ,("->",  ITrarrow NormalSyntax,      NormalSyntax,  0 )
       ,("=>",  ITdarrow NormalSyntax,      NormalSyntax,  0 )
       ,("-",   ITminus,                    NormalSyntax,  xbit NoLexicalNegationBit)

       ,("*",   ITstar NormalSyntax,        NormalSyntax,  xbit StarIsTypeBit)

       ,("-<",  ITlarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
       ,(">-",  ITrarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
       ,("-<<", ITLarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
       ,(">>-", ITRarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)

       ,("∷",   ITdcolon UnicodeSyntax,     UnicodeSyntax, 0 )
       ,("⇒",   ITdarrow UnicodeSyntax,     UnicodeSyntax, 0 )
       ,("∀",   ITforall UnicodeSyntax,     UnicodeSyntax, 0 )
       ,("→",   ITrarrow UnicodeSyntax,     UnicodeSyntax, 0 )
       ,("←",   ITlarrow UnicodeSyntax,     UnicodeSyntax, 0 )

       ,("⊸",   ITlolly, UnicodeSyntax, 0)

       ,("⤙",   ITlarrowtail UnicodeSyntax, UnicodeSyntax, xbit ArrowsBit)
       ,("⤚",   ITrarrowtail UnicodeSyntax, UnicodeSyntax, xbit ArrowsBit)
       ,("⤛",   ITLarrowtail UnicodeSyntax, UnicodeSyntax, xbit ArrowsBit)
       ,("⤜",   ITRarrowtail UnicodeSyntax, UnicodeSyntax, xbit ArrowsBit)

       ,("★",   ITstar UnicodeSyntax,       UnicodeSyntax, xbit StarIsTypeBit)

        -- ToDo: ideally, → and ∷ should be "specials", so that they cannot
        -- form part of a large operator.  This would let us have a better
        -- syntax for kinds: ɑ∷*→* would be a legal kind signature. (maybe).
       ]

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = PsSpan -> StringBuffer -> Int -> StringBuffer -> P (PsLocated Token)

special :: Token -> Action
special tok span _buf _len _buf2 = return (L span tok)

token, layout_token :: Token -> Action
token t span _buf _len _buf2 = return (L span t)
layout_token t span _buf _len _buf2 = pushLexState layout >> return (L span t)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len _buf2 = return (L span $! (f buf len))

qdo_token :: (Maybe FastString -> Token) -> Action
qdo_token con span buf len _buf2 = do
    maybe_layout token
    return (L span $! token)
  where
    !token = con $! Just $! fst $! splitQualName buf len False

skip_one_varid :: (FastString -> Token) -> Action
skip_one_varid f span buf len _buf2
  = return (L span $! f (lexemeToFastString (stepOn buf) (len-1)))

skip_one_varid_src :: (SourceText -> FastString -> Token) -> Action
skip_one_varid_src f span buf len _buf2
  = return (L span $! f (SourceText $ lexemeToFastString (stepOn buf) (len-1))
                        (lexemeToFastString (stepOn buf) (len-1)))

skip_two_varid :: (FastString -> Token) -> Action
skip_two_varid f span buf len _buf2
  = return (L span $! f (lexemeToFastString (stepOn (stepOn buf)) (len-2)))

strtoken :: (String -> Token) -> Action
strtoken f span buf len _buf2 =
  return (L span $! (f $! lexemeToString buf len))

fstrtoken :: (FastString -> Token) -> Action
fstrtoken f span buf len _buf2 =
  return (L span $! (f $! lexemeToFastString buf len))

begin :: Int -> Action
begin code _span _str _len _buf2 = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len _buf2 =
  do _ <- popLexState
     lexToken
-- See Note [Nested comment line pragmas]
failLinePrag1 :: Action
failLinePrag1 span _buf _len _buf2 = do
  b <- getBit InNestedCommentBit
  if b then return (L span ITcomment_line_prag)
       else lexError LexErrorInPragma

-- See Note [Nested comment line pragmas]
popLinePrag1 :: Action
popLinePrag1 span _buf _len _buf2 = do
  b <- getBit InNestedCommentBit
  if b then return (L span ITcomment_line_prag) else do
    _ <- popLexState
    lexToken

hopefully_open_brace :: Action
hopefully_open_brace span buf len buf2
 = do relaxed <- getBit RelaxedLayoutBit
      ctx <- getContext
      (AI l _) <- getInput
      let offset = srcLocCol (psRealLoc l)
          isOK = relaxed ||
                 case ctx of
                 Layout prev_off _ : _ -> prev_off < offset
                 _                     -> True
      if isOK then pop_and open_brace span buf len buf2
              else addFatalError $
                     mkPlainErrorMsgEnvelope (mkSrcSpanPs span) PsErrMissingBlock

pop_and :: Action -> Action
pop_and act span buf len buf2 =
  do _ <- popLexState
     act span buf len buf2

-- See Note [Whitespace-sensitive operator parsing]
followedByOpeningToken, precededByClosingToken :: AlexAccPred ExtsBitmap
followedByOpeningToken _ _ _ (AI _ buf) = followedByOpeningToken' buf
precededByClosingToken _ (AI _ buf) _ _ = precededByClosingToken' buf

-- The input is the buffer *after* the token.
followedByOpeningToken' :: StringBuffer -> Bool
followedByOpeningToken' buf
  | atEnd buf = False
  | otherwise =
      case nextChar buf of
        ('{', buf') -> nextCharIsNot buf' (== '-')
        ('(', _) -> True
        ('[', _) -> True
        ('\"', _) -> True
        ('\'', _) -> True
        ('_', _) -> True
        ('⟦', _) -> True
        ('⦇', _) -> True
        (c, _) -> isAlphaNum c

-- The input is the buffer *before* the token.
precededByClosingToken' :: StringBuffer -> Bool
precededByClosingToken' buf =
  case prevChar buf '\n' of
    '}' -> decodePrevNChars 1 buf /= "-"
    ')' -> True
    ']' -> True
    '\"' -> True
    '\'' -> True
    '_' -> True
    '⟧' -> True
    '⦈' -> True
    c -> isAlphaNum c

get_op_ws :: StringBuffer -> StringBuffer -> OpWs
get_op_ws buf1 buf2 =
    mk_op_ws (precededByClosingToken' buf1) (followedByOpeningToken' buf2)
  where
    mk_op_ws False True  = OpWsPrefix
    mk_op_ws True  False = OpWsSuffix
    mk_op_ws True  True  = OpWsTightInfix
    mk_op_ws False False = OpWsLooseInfix

{-# INLINE with_op_ws #-}
with_op_ws :: (OpWs -> Action) -> Action
with_op_ws act span buf len buf2 = act (get_op_ws buf buf2) span buf len buf2

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
  | HaddockBit `xtest` bits = notFollowedByDocOrPragma
  | otherwise               = nextCharIsNot buf (== '#')
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

-- Check if we should parse a negative literal (e.g. -123) as a single token.
negLitPred :: AlexAccPred ExtsBitmap
negLitPred =
    prefix_minus `alexAndPred`
    (negative_literals `alexOrPred` lexical_negation)
  where
    negative_literals = ifExtension NegativeLiteralsBit

    lexical_negation  =
      -- See Note [Why not LexicalNegationBit]
      alexNotPred (ifExtension NoLexicalNegationBit)

    prefix_minus =
      -- Note [prefix_minus in negLitPred and negHashLitPred]
      alexNotPred precededByClosingToken

-- Check if we should parse an unboxed negative literal (e.g. -123#) as a single token.
negHashLitPred :: ExtBits -> AlexAccPred ExtsBitmap
negHashLitPred ext = prefix_minus `alexAndPred` magic_hash
  where
    magic_hash = ifExtension ext -- Either MagicHashBit or ExtendedLiteralsBit
    prefix_minus =
      -- Note [prefix_minus in negLitPred and negHashLitPred]
      alexNotPred precededByClosingToken

{- Note [prefix_minus in negLitPred and negHashLitPred]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to parse -1 as a single token, but x-1 as three tokens.
So in negLitPred (and negHashLitPred) we require that we have a prefix
occurrence of the minus sign. See Note [Whitespace-sensitive operator parsing]
for a detailed definition of a prefix occurrence.

The condition for a prefix occurrence of an operator is:

  not precededByClosingToken && followedByOpeningToken

but we don't check followedByOpeningToken when parsing a negative literal.
It holds simply because we immediately lex a literal after the minus.
-}

ifExtension :: ExtBits -> AlexAccPred ExtsBitmap
ifExtension extBits bits _ _ _ = extBits `xtest` bits

alexNotPred p userState in1 len in2
  = not (p userState in1 len in2)

alexOrPred p1 p2 userState in1 len in2
  = p1 userState in1 len in2 || p2 userState in1 len in2

multiline_doc_comment :: Action
multiline_doc_comment span buf _len _buf2 = {-# SCC "multiline_doc_comment" #-} withLexedDocType worker
  where
    worker input@(AI start_loc _) docType checkNextLine = go start_loc "" [] input
      where
        go start_loc curLine prevLines input@(AI end_loc _) = case alexGetChar' input of
            Just ('\n', input')
              | checkNextLine -> case checkIfCommentLine input' of
                Just input@(AI next_start _) ->  go next_start "" (locatedLine : prevLines) input -- Start a new line
                Nothing -> endComment
              | otherwise -> endComment
            Just (c, input) -> go start_loc (c:curLine) prevLines input
            Nothing -> endComment
          where
            lineSpan = mkSrcSpanPs $ mkPsSpan start_loc end_loc
            locatedLine = L lineSpan (mkHsDocStringChunk $ reverse curLine)
            commentLines = NE.reverse $ locatedLine :| prevLines
            endComment = docCommentEnd input (docType (\dec -> MultiLineDocString dec commentLines)) buf span

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
lineCommentToken span buf len buf2 = do
  b <- getBit RawTokenStreamBit
  if b then do
         lt <- getLastLocIncludingComments
         strtoken (\s -> ITlineComment s lt) span buf len buf2
       else lexToken


{-
  nested comments require traversing by hand, they can't be parsed
  using regular expressions.
-}
nested_comment :: Action
nested_comment span buf len _buf2 = {-# SCC "nested_comment" #-} do
  l <- getLastLocIncludingComments
  let endComment input (L _ comment) = commentEnd lexToken input (Nothing, ITblockComment comment l) buf span
  input <- getInput
  -- Include decorator in comment
  let start_decorator = reverse $ lexemeToString buf len
  nested_comment_logic endComment start_decorator input span

nested_doc_comment :: Action
nested_doc_comment span buf _len _buf2 = {-# SCC "nested_doc_comment" #-} withLexedDocType worker
  where
    worker input@(AI start_loc _) docType _checkNextLine = nested_comment_logic endComment "" input (mkPsSpan start_loc (psSpanEnd span))
      where
        endComment input lcomment
          = docCommentEnd input (docType (\d -> NestedDocString d (mkHsDocStringChunk . dropTrailingDec <$> lcomment))) buf span

        dropTrailingDec [] = []
        dropTrailingDec "-}" = ""
        dropTrailingDec (x:xs) = x:dropTrailingDec xs

{-# INLINE nested_comment_logic #-}
-- | Includes the trailing '-}' decorators
-- drop the last two elements with the callback if you don't want them to be included
nested_comment_logic
  :: (AlexInput -> Located String -> P (PsLocated Token))  -- ^ Continuation that gets the rest of the input and the lexed comment
  -> String -- ^ starting value for accumulator (reversed) - When we want to include a decorator '{-' in the comment
  -> AlexInput
  -> PsSpan
  -> P (PsLocated Token)
nested_comment_logic endComment commentAcc input span = go commentAcc (1::Int) input
  where
    go commentAcc 0 input@(AI end_loc _) = do
      let comment = reverse commentAcc
          cspan = mkSrcSpanPs $ mkPsSpan (psSpanStart span) end_loc
          lcomment = L cspan comment
      endComment input lcomment
    go commentAcc n input = case alexGetChar' input of
      Nothing -> errBrace input (psRealSpan span)
      Just ('-',input) -> case alexGetChar' input of
        Nothing  -> errBrace input (psRealSpan span)
        Just ('\125',input) -> go ('\125':'-':commentAcc) (n-1) input -- '}'
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar' input of  -- '{' char
        Nothing  -> errBrace input (psRealSpan span)
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
      -- See Note [Nested comment line pragmas]
      Just ('\n',input) -> case alexGetChar' input of
        Nothing  -> errBrace input (psRealSpan span)
        Just ('#',_) -> do (parsedAcc,input) <- parseNestedPragma input
                           go (parsedAcc ++ '\n':commentAcc) n input
        Just (_,_)   -> go ('\n':commentAcc) n input
      Just (c,input) -> go (c:commentAcc) n input

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
  case unLoc lt of
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

{-# INLINE withLexedDocType #-}
withLexedDocType :: (AlexInput -> ((HsDocStringDecorator -> HsDocString) -> (HdkComment, Token)) -> Bool -> P (PsLocated Token))
                 -> P (PsLocated Token)
withLexedDocType lexDocComment = do
  input@(AI _ buf) <- getInput
  l <- getLastLocIncludingComments
  case prevChar buf ' ' of
    -- The `Bool` argument to lexDocComment signals whether or not the next
    -- line of input might also belong to this doc comment.
    '|' -> lexDocComment input (mkHdkCommentNext l) True
    '^' -> lexDocComment input (mkHdkCommentPrev l) True
    '$' -> case lexDocName input of
       Nothing -> do setInput input; lexToken -- eof reached, lex it normally
       Just (name, input) -> lexDocComment input (mkHdkCommentNamed l name) True
    '*' -> lexDocSection l 1 input
    _ -> panic "withLexedDocType: Bad doc type"
 where
    lexDocSection l n input = case alexGetChar' input of
      Just ('*', input) -> lexDocSection l (n+1) input
      Just (_,   _)     -> lexDocComment input (mkHdkCommentSection l n) False
      Nothing -> do setInput input; lexToken -- eof reached, lex it normally

    lexDocName :: AlexInput -> Maybe (String, AlexInput)
    lexDocName = go ""
      where
        go acc input = case alexGetChar' input of
          Just (c, input')
            | isSpace c -> Just (reverse acc, input)
            | otherwise -> go (c:acc) input'
          Nothing -> Nothing

mkHdkCommentNext, mkHdkCommentPrev  :: PsSpan -> (HsDocStringDecorator -> HsDocString) -> (HdkComment, Token)
mkHdkCommentNext loc mkDS =  (HdkCommentNext ds,ITdocComment ds loc)
  where ds = mkDS HsDocStringNext
mkHdkCommentPrev loc mkDS =  (HdkCommentPrev ds,ITdocComment ds loc)
  where ds = mkDS HsDocStringPrevious

mkHdkCommentNamed :: PsSpan -> String -> (HsDocStringDecorator -> HsDocString) -> (HdkComment, Token)
mkHdkCommentNamed loc name mkDS = (HdkCommentNamed name ds, ITdocComment ds loc)
  where ds = mkDS (HsDocStringNamed name)

mkHdkCommentSection :: PsSpan -> Int -> (HsDocStringDecorator -> HsDocString) -> (HdkComment, Token)
mkHdkCommentSection loc n mkDS = (HdkCommentSection n ds, ITdocComment ds loc)
  where ds = mkDS (HsDocStringGroup n)

-- RULES pragmas turn on the forall and '.' keywords, and we turn them
-- off again at the end of the pragma.
rulePrag :: Action
rulePrag span buf len _buf2 = do
  setExts (.|. xbit InRulePragBit)
  let !src = lexemeToFastString buf len
  return (L span (ITrules_prag (SourceText src)))

-- When 'UsePosPragsBit' is not set, it is expected that we emit a token instead
-- of updating the position in 'PState'
linePrag :: Action
linePrag span buf len buf2 = do
  usePosPrags <- getBit UsePosPragsBit
  if usePosPrags
    then begin line_prag2 span buf len buf2
    else let !src = lexemeToFastString buf len
         in return (L span (ITline_prag (SourceText src)))

-- When 'UsePosPragsBit' is not set, it is expected that we emit a token instead
-- of updating the position in 'PState'
columnPrag :: Action
columnPrag span buf len buf2 = do
  usePosPrags <- getBit UsePosPragsBit
  if usePosPrags
    then begin column_prag span buf len buf2
    else let !src = lexemeToFastString buf len
         in return (L span (ITcolumn_prag (SourceText src)))

endPrag :: Action
endPrag span _buf _len _buf2 = do
  setExts (.&. complement (xbit InRulePragBit))
  return (L span ITclose_prag)

-- docCommentEnd
-------------------------------------------------------------------------------
-- This function is quite tricky. We can't just return a new token, we also
-- need to update the state of the parser. Why? Because the token is longer
-- than what was lexed by Alex, and the lexToken function doesn't know this, so
-- it writes the wrong token length to the parser state. This function is
-- called afterwards, so it can just update the state.

{-# INLINE commentEnd #-}
commentEnd :: P (PsLocated Token)
           -> AlexInput
           -> (Maybe HdkComment, Token)
           -> StringBuffer
           -> PsSpan
           -> P (PsLocated Token)
commentEnd cont input (m_hdk_comment, hdk_token) buf span = do
  setInput input
  let (AI loc nextBuf) = input
      span' = mkPsSpan (psSpanStart span) loc
      last_len = byteDiff buf nextBuf
  span `seq` setLastToken span' last_len
  whenIsJust m_hdk_comment $ \hdk_comment ->
    P $ \s -> POk (s {hdk_comments = hdk_comments s `snocOL` L span' hdk_comment}) ()
  b <- getBit RawTokenStreamBit
  if b then return (L span' hdk_token)
       else cont

{-# INLINE docCommentEnd #-}
docCommentEnd :: AlexInput -> (HdkComment, Token) -> StringBuffer ->
                 PsSpan -> P (PsLocated Token)
docCommentEnd input (hdk_comment, tok) buf span
  = commentEnd lexToken input (Just hdk_comment, tok) buf span

errBrace :: AlexInput -> RealSrcSpan -> P a
errBrace (AI end _) span =
  failLocMsgP (realSrcSpanStart span)
              (psRealLoc end)
              (\srcLoc -> mkPlainErrorMsgEnvelope srcLoc (PsErrLexer LexUnterminatedComment LexErrKind_EOF))

open_brace, close_brace :: Action
open_brace span _str _len _buf2 = do
  ctx <- getContext
  setContext (NoLayout:ctx)
  return (L span ITocurly)
close_brace span _str _len _buf2 = do
  popContext
  return (L span ITccurly)

qvarid, qconid :: StringBuffer -> Int -> Token
qvarid buf len = ITqvarid $! splitQualName buf len False
qconid buf len = ITqconid $! splitQualName buf len False

splitQualName :: StringBuffer -> Int -> Bool -> (FastString,FastString)
-- takes a StringBuffer and a length, and returns the module name
-- and identifier parts of a qualified name.  Splits at the *last* dot,
-- because of hierarchical module names.
--
-- Throws an error if the name is not qualified.
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

    done dot_buf
        | qual_size < 1 = error "splitQualName got an unqualified named"
        | otherwise =
        (lexemeToFastString orig_buf (qual_size - 1),
         if parens -- Prelude.(+)
            then lexemeToFastString (stepOn dot_buf) (len - qual_size - 2)
            else lexemeToFastString dot_buf (len - qual_size))
      where
        qual_size = orig_buf `byteDiff` dot_buf

varid :: Action
varid span buf len _buf2 =
  case lookupUFM reservedWordsFM fs of
    Just (ITcase, _) -> do
      lastTk <- getLastTk
      keyword <- case lastTk of
        Strict.Just (L _ ITlam) -> do
          lambdaCase <- getBit LambdaCaseBit
          unless lambdaCase $ do
            pState <- getPState
            addError $ mkPlainErrorMsgEnvelope (mkSrcSpanPs (last_loc pState)) PsErrLambdaCase
          return ITlcase
        _ -> return ITcase
      maybe_layout keyword
      return $ L span keyword
    Just (ITlcases, _) -> do
      lastTk <- getLastTk
      lambdaCase <- getBit LambdaCaseBit
      token <- case lastTk of
        Strict.Just (L _ ITlam) | lambdaCase -> return ITlcases
        _ -> return $ ITvarid fs
      maybe_layout token
      return $ L span token
    Just (keyword, 0) -> do
      maybe_layout keyword
      return $ L span keyword
    Just (keyword, i) -> do
      exts <- getExts
      if exts .&. i /= 0
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


errSuffixAt :: PsSpan -> P a
errSuffixAt span = do
    input <- getInput
    failLocMsgP start (go input start) (\srcSpan -> mkPlainErrorMsgEnvelope srcSpan $ PsErrSuffixAT)
  where
    start = psRealLoc (psSpanStart span)
    go inp loc
      | Just (c, i) <- alexGetChar inp
      , let next = advanceSrcLoc loc c =
          if c == ' '
          then go i next
          else next
      | otherwise = loc

-- See Note [Whitespace-sensitive operator parsing]
varsym :: OpWs -> Action
varsym opws@OpWsPrefix = sym $ \span exts s ->
  let warnExtConflict errtok =
        do { addPsMessage (mkSrcSpanPs span) (PsWarnOperatorWhitespaceExtConflict errtok)
           ; return (ITvarsym s) }
  in
  if | s == fsLit "@" ->
         return ITtypeApp  -- regardless of TypeApplications for better error messages
     | s == fsLit "%" ->
         if xtest LinearTypesBit exts
         then return ITpercent
         else warnExtConflict OperatorWhitespaceSymbol_PrefixPercent
     | s == fsLit "$" ->
         if xtest ThQuotesBit exts
         then return ITdollar
         else warnExtConflict OperatorWhitespaceSymbol_PrefixDollar
     | s == fsLit "$$" ->
         if xtest ThQuotesBit exts
         then return ITdollardollar
         else warnExtConflict OperatorWhitespaceSymbol_PrefixDollarDollar
     | s == fsLit "-" ->
         return ITprefixminus -- Only when LexicalNegation is on, otherwise we get ITminus
                              -- and don't hit this code path. See Note [Minus tokens]
     | s == fsLit ".", OverloadedRecordDotBit `xtest` exts ->
         return (ITproj True) -- e.g. '(.x)'
     | s == fsLit "." -> return ITdot
     | s == fsLit "!" -> return ITbang
     | s == fsLit "~" -> return ITtilde
     | otherwise ->
         do { warnOperatorWhitespace opws span s
            ; return (ITvarsym s) }
varsym opws@OpWsSuffix = sym $ \span _ s ->
  if | s == fsLit "@" -> errSuffixAt span
     | s == fsLit "." -> return ITdot
     | otherwise ->
         do { warnOperatorWhitespace opws span s
            ; return (ITvarsym s) }
varsym opws@OpWsTightInfix = sym $ \span exts s ->
  if | s == fsLit "@" -> return ITat
     | s == fsLit ".", OverloadedRecordDotBit `xtest` exts  -> return (ITproj False)
     | s == fsLit "." -> return ITdot
     | otherwise ->
         do { warnOperatorWhitespace opws span s
            ; return (ITvarsym s) }
varsym OpWsLooseInfix = sym $ \_ _ s ->
  if | s == fsLit "."
     -> return ITdot
     | otherwise
     -> return $ ITvarsym s

consym :: OpWs -> Action
consym opws = sym $ \span _exts s ->
  do { warnOperatorWhitespace opws span s
     ; return (ITconsym s) }

warnOperatorWhitespace :: OpWs -> PsSpan -> FastString -> P ()
warnOperatorWhitespace opws span s =
  whenIsJust (check_unusual_opws opws) $ \opws' ->
    addPsMessage
      (mkSrcSpanPs span)
      (PsWarnOperatorWhitespace s opws')

-- Check an operator occurrence for unusual whitespace (prefix, suffix, tight infix).
-- This determines if -Woperator-whitespace is triggered.
check_unusual_opws :: OpWs -> Maybe OperatorWhitespaceOccurrence
check_unusual_opws opws =
  case opws of
    OpWsPrefix     -> Just OperatorWhitespaceOccurrence_Prefix
    OpWsSuffix     -> Just OperatorWhitespaceOccurrence_Suffix
    OpWsTightInfix -> Just OperatorWhitespaceOccurrence_TightInfix
    OpWsLooseInfix -> Nothing

sym :: (PsSpan -> ExtsBitmap -> FastString -> P Token) -> Action
sym con span buf len _buf2 =
  case lookupUFM reservedSymsFM fs of
    Just (keyword, NormalSyntax, 0) ->
      return $ L span keyword
    Just (keyword, NormalSyntax, i) -> do
      exts <- getExts
      if exts .&. i /= 0
        then return $ L span keyword
        else L span <$!> con span exts fs
    Just (keyword, UnicodeSyntax, 0) -> do
      exts <- getExts
      if xtest UnicodeSyntaxBit exts
        then return $ L span keyword
        else L span <$!> con span exts fs
    Just (keyword, UnicodeSyntax, i) -> do
      exts <- getExts
      if exts .&. i /= 0 && xtest UnicodeSyntaxBit exts
        then return $ L span keyword
        else L span <$!> con span exts fs
    Nothing -> do
      exts <- getExts
      L span <$!> con span exts fs
  where
    !fs = lexemeToFastString buf len

-- Variations on the integral numeric literal.
tok_integral :: (SourceText -> Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len _buf2 = do
  numericUnderscores <- getBit NumericUnderscoresBit  -- #14473
  let src = lexemeToFastString buf len
  when ((not numericUnderscores) && ('_' `elem` unpackFS src)) $ do
    pState <- getPState
    let msg = PsErrNumUnderscores NumUnderscore_Integral
    addError $ mkPlainErrorMsgEnvelope (mkSrcSpanPs (last_loc pState)) msg
  return $ L span $ itint (SourceText src)
       $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral $ \case
    st@(SourceText (unconsFS -> Just ('-',_))) -> itint st (const True)
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

-- | Helper for defining @IntX@ primitive literal parsers (specifically for
--   the ExtendedLiterals extension, such as @123#Int8@).
tok_primintX :: (SourceText -> Integer -> Token)
             -> Int
             -> (Integer -> Integer)
             -> Int
             -> (Integer, (Char->Int)) -> Action
tok_primintX itint addlen transint transbuf =
    tok_integral itint transint transbuf (transbuf+addlen)

tok_primint8,     tok_primint16,  tok_primint32,  tok_primint64
    :: (Integer -> Integer)
    -> Int -> (Integer, (Char->Int)) -> Action
tok_primint8  = tok_primintX ITprimint8   5
tok_primint16 = tok_primintX ITprimint16  6
tok_primint32 = tok_primintX ITprimint32  6
tok_primint64 = tok_primintX ITprimint64  6

-- | Helper for defining @WordX@ primitive literal parsers (specifically for
--   the ExtendedLiterals extension, such as @234#Word8@).
tok_primwordX :: (SourceText -> Integer -> Token)
              -> Int
              -> Int
              -> (Integer, (Char->Int)) -> Action
tok_primwordX itint addlen transbuf =
    tok_integral itint positive transbuf (transbuf+addlen)

tok_primword8, tok_primword16, tok_primword32, tok_primword64
    :: Int -> (Integer, (Char->Int)) -> Action
tok_primword8  = tok_primwordX ITprimword8  6
tok_primword16 = tok_primwordX ITprimword16 7
tok_primword32 = tok_primwordX ITprimword32 7
tok_primword64 = tok_primwordX ITprimword64 7

-- readSignificandExponentPair can understand negative rationals, exponents, everything.
tok_frac :: Int -> (String -> Token) -> Action
tok_frac drop f span buf len _buf2 = do
  numericUnderscores <- getBit NumericUnderscoresBit  -- #14473
  let src = lexemeToString buf (len-drop)
  when ((not numericUnderscores) && ('_' `elem` src)) $ do
    pState <- getPState
    let msg = PsErrNumUnderscores NumUnderscore_Float
    addError $ mkPlainErrorMsgEnvelope (mkSrcSpanPs (last_loc pState)) msg
  return (L span $! (f $! src))

tok_float, tok_primfloat, tok_primdouble :: String -> Token
tok_float        str = ITrational   $! readFractionalLit str
tok_hex_float    str = ITrational   $! readHexFractionalLit str
tok_primfloat    str = ITprimfloat  $! readFractionalLit str
tok_primdouble   str = ITprimdouble $! readFractionalLit str

readFractionalLit, readHexFractionalLit :: String -> FractionalLit
readHexFractionalLit = readFractionalLitX readHexSignificandExponentPair Base2
readFractionalLit = readFractionalLitX readSignificandExponentPair Base10

readFractionalLitX :: (String -> (Integer, Integer))
                   -> FractionalExponentBase
                   -> String -> FractionalLit
readFractionalLitX readStr b str =
  mkSourceFractionalLit str is_neg i e b
  where
    is_neg = case str of
                    '-' : _ -> True
                    _      -> False
    (i, e) = readStr str

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len _buf2 = do
        -- See Note [Nested comment line pragmas]
        b <- getBit InNestedCommentBit
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
                    alr <- getBit AlternativeLayoutRuleBit
                    unless alr $ f t
    where f (ITdo _)    = pushLexState layout_do
          f (ITmdo _)   = pushLexState layout_do
          f ITof        = pushLexState layout
          f ITlcase     = pushLexState layout
          f ITlcases    = pushLexState layout
          f ITlet       = pushLexState layout
          f ITwhere     = pushLexState layout
          f ITrec       = pushLexState layout
          f ITif        = pushLexState layout_if
          f _           = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
new_layout_context :: Bool -> Bool -> Token -> Action
new_layout_context strict gen_semic tok span _buf len _buf2 = do
    _ <- popLexState
    (AI l _) <- getInput
    let offset = srcLocCol (psRealLoc l) - len
    ctx <- getContext
    nondecreasing <- getBit NondecreasingIndentationBit
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
do_layout_left span _buf _len _buf2 = do
    _ <- popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- LINE pragmas

setLineAndFile :: Int -> Action
setLineAndFile code (PsSpan span _) buf len _buf2 = do
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
  resetAlrLastLoc file
  setSrcLoc (mkRealSrcLoc file (fromIntegral linenum - 1) (srcSpanEndCol span))
      -- subtract one: the line number refers to the *following* line
  addSrcFile file
  _ <- popLexState
  pushLexState code
  lexToken

setColumn :: Action
setColumn (PsSpan span _) buf len _buf2 = do
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
lex_string_prag mkTok = lex_string_prag_comment mkTok'
  where
    mkTok' s _ = mkTok s

lex_string_prag_comment :: (String -> PsSpan -> Token) -> Action
lex_string_prag_comment mkTok span _buf _len _buf2
    = do input <- getInput
         start <- getParsedLoc
         l <- getLastLocIncludingComments
         tok <- go l [] input
         end <- getParsedLoc
         return (L (mkPsSpan start end) tok)
    where go l acc input
              = if isString input "#-}"
                   then do setInput input
                           return (mkTok (reverse acc) l)
                   else case alexGetChar input of
                          Just (c,i) -> go l (c:acc) i
                          Nothing -> err input
          isString _ [] = True
          isString i (x:xs)
              = case alexGetChar i of
                  Just (c,i') | c == x    -> isString i' xs
                  _other -> False
          err (AI end _) = failLocMsgP (realSrcSpanStart (psRealSpan span))
                                       (psRealLoc end)
                                       (\srcLoc -> mkPlainErrorMsgEnvelope srcLoc $ PsErrLexer LexUnterminatedOptions LexErrKind_EOF)

-- -----------------------------------------------------------------------------
-- Strings & Chars

-- This stuff is horrible.  I hates it.

lex_string_tok :: Action
lex_string_tok span buf _len _buf2 = do
  lexed <- lex_string
  (AI end bufEnd) <- getInput
  let
    tok = case lexed of
      LexedPrimString s -> ITprimstring (SourceText src) (unsafeMkByteString s)
      LexedRegularString s -> ITstring (SourceText src) (mkFastString s)
    src = lexemeToFastString buf (cur bufEnd - cur buf)
  return $ L (mkPsSpan (psSpanStart span) end) tok


lex_quoted_label :: Action
lex_quoted_label span buf _len _buf2 = do
  start <- getInput
  s <- lex_string_helper "" start
  (AI end bufEnd) <- getInput
  let
    token = ITlabelvarid (SourceText src) (mkFastString s)
    src = lexemeToFastString (stepOn buf) (cur bufEnd - cur buf - 1)
    start = psSpanStart span

  return $ L (mkPsSpan start end) token


data LexedString = LexedRegularString String | LexedPrimString String

lex_string :: P LexedString
lex_string = do
  start <- getInput
  s <- lex_string_helper "" start
  magicHash <- getBit MagicHashBit
  if magicHash
    then do
      i <- getInput
      case alexGetChar' i of
        Just ('#',i) -> do
          setInput i
          when (any (> '\xFF') s) $ do
            pState <- getPState
            let msg = PsErrPrimStringInvalidChar
            let err = mkPlainErrorMsgEnvelope (mkSrcSpanPs (last_loc pState)) msg
            addError err
          return $ LexedPrimString s
        _other ->
          return $ LexedRegularString s
    else
      return $ LexedRegularString s


lex_string_helper :: String -> AlexInput -> P String
lex_string_helper s start = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> lit_error i

    Just ('"',i)  -> do
      setInput i
      return (reverse s)

    Just ('\\',i)
        | Just ('&',i) <- next -> do
                setInput i; lex_string_helper s start
        | Just (c,i) <- next, c <= '\x7f' && is_space c -> do
                           -- is_space only works for <= '\x7f' (#3751, #5425)
                setInput i; lex_stringgap s start
        where next = alexGetChar' i

    Just (c, i1) -> do
        case c of
          '\\' -> do setInput i1; c' <- lex_escape; lex_string_helper (c':s) start
          c | isAny c -> do setInput i1; lex_string_helper (c:s) start
          _other | any isDoubleSmartQuote s -> do
            -- if the built-up string s contains a smart double quote character, it was
            -- likely the reason why the string literal was not lexed correctly
            setInput start -- rewind to the first character in the string literal
                           -- so we can find the smart quote character's location
            advance_to_smart_quote_character
            i2@(AI loc _) <- getInput
            case alexGetChar' i2 of
              Just (c, _) -> do add_nonfatal_smart_quote_error c loc; lit_error i
              Nothing -> lit_error i -- should never get here
          _other -> lit_error i


lex_stringgap :: String -> AlexInput -> P String
lex_stringgap s start = do
  i <- getInput
  c <- getCharOrFail i
  case c of
    '\\' -> lex_string_helper s start
    c | c <= '\x7f' && is_space c -> lex_stringgap s start
                           -- is_space only works for <= '\x7f' (#3751, #5425)
    _other -> lit_error i


lex_char_tok :: Action
-- Here we are basically parsing character literals, such as 'x' or '\n'
-- but we additionally spot 'x and ''T, returning ITsimpleQuote and
-- ITtyQuote respectively, but WITHOUT CONSUMING the x or T part
-- (the parser does that).
-- So we have to do two characters of lookahead: when we see 'x we need to
-- see if there's a trailing quote
lex_char_tok span buf _len _buf2 = do        -- We've seen '
   i1 <- getInput       -- Look ahead to first character
   let loc = psSpanStart span
   case alexGetChar' i1 of
        Nothing -> lit_error  i1

        Just ('\'', i2@(AI end2 _)) -> do       -- We've seen ''
                   setInput i2
                   return (L (mkPsSpan loc end2)  ITtyQuote)

        Just ('\\', i2@(AI end2 _)) -> do      -- We've seen 'backslash
                  setInput i2
                  lit_ch <- lex_escape
                  i3 <- getInput
                  mc <- getCharOrFail i3 -- Trailing quote
                  if mc == '\'' then finish_char_tok buf loc lit_ch
                  else if isSingleSmartQuote mc then add_smart_quote_error mc end2
                  else lit_error i3

        Just (c, i2@(AI end2 _))
                | not (isAny c) -> lit_error i1
                | otherwise ->

                -- We've seen 'x, where x is a valid character
                --  (i.e. not newline etc) but not a quote or backslash
           case alexGetChar' i2 of      -- Look ahead one more character
                Just ('\'', i3) -> do   -- We've seen 'x'
                        setInput i3
                        finish_char_tok buf loc c
                Just (c, _) | isSingleSmartQuote c -> add_smart_quote_error c end2
                _other -> do            -- We've seen 'x not followed by quote
                                        -- (including the possibility of EOF)
                                        -- Just parse the quote only
                        let (AI end _) = i1
                        return (L (mkPsSpan loc end) ITsimpleQuote)

finish_char_tok :: StringBuffer -> PsLoc -> Char -> P (PsLocated Token)
finish_char_tok buf loc ch  -- We've already seen the closing quote
                        -- Just need to check for trailing #
  = do  magicHash <- getBit MagicHashBit
        i@(AI end bufEnd) <- getInput
        let src = lexemeToFastString buf (cur bufEnd - cur buf)
        if magicHash then do
            case alexGetChar' i of
              Just ('#',i@(AI end bufEnd')) -> do
                setInput i
                -- Include the trailing # in SourceText
                let src' = lexemeToFastString buf (cur bufEnd' - cur buf)
                return (L (mkPsSpan loc end)
                          (ITprimchar (SourceText src') ch))
              _other ->
                return (L (mkPsSpan loc end)
                          (ITchar (SourceText src) ch))
            else do
              return (L (mkPsSpan loc end) (ITchar (SourceText src) ch))

isAny :: Char -> Bool
isAny c | c > '\x7f' = isPrint c
        | otherwise  = is_any c

lex_escape :: P Char
lex_escape = do
  i0@(AI loc _) <- getInput
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
        -- the next two patterns build up a Unicode smart quote error (#21843)
        smart_double_quote | isDoubleSmartQuote smart_double_quote ->
          add_smart_quote_error smart_double_quote loc
        smart_single_quote | isSingleSmartQuote smart_single_quote ->
          add_smart_quote_error smart_single_quote loc
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
                  then setInput input >> lexError LexNumEscapeRange
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
lit_error i = do setInput i; lexError LexStringCharLit

getCharOrFail :: AlexInput -> P Char
getCharOrFail i =  do
  case alexGetChar' i of
        Nothing -> lexError LexStringCharLitEOF
        Just (c,i)  -> do setInput i; return c

-- -----------------------------------------------------------------------------
-- QuasiQuote

lex_qquasiquote_tok :: Action
lex_qquasiquote_tok span buf len _buf2 = do
  let (qual, quoter) = splitQualName (stepOn buf) (len - 2) False
  quoteStart <- getParsedLoc
  quote <- lex_quasiquote (psRealLoc quoteStart) ""
  end <- getParsedLoc
  return (L (mkPsSpan (psSpanStart span) end)
           (ITqQuasiQuote (qual,
                           quoter,
                           mkFastString (reverse quote),
                           mkPsSpan quoteStart end)))

lex_quasiquote_tok :: Action
lex_quasiquote_tok span buf len _buf2 = do
  let quoter = tail (lexemeToString buf (len - 1))
                -- 'tail' drops the initial '[',
                -- while the -1 drops the trailing '|'
  quoteStart <- getParsedLoc
  quote <- lex_quasiquote (psRealLoc quoteStart) ""
  end <- getParsedLoc
  return (L (mkPsSpan (psSpanStart span) end)
           (ITquasiQuote (mkFastString quoter,
                          mkFastString (reverse quote),
                          mkPsSpan quoteStart end)))

lex_quasiquote :: RealSrcLoc -> String -> P String
lex_quasiquote start s = do
  i <- getInput
  case alexGetChar' i of
    Nothing -> quasiquote_error start

    -- NB: The string "|]" terminates the quasiquote,
    -- with absolutely no escaping. See the extensive
    -- discussion on #5348 for why there is no
    -- escape handling.
    Just ('|',i)
        | Just (']',i) <- alexGetChar' i
        -> do { setInput i; return s }

    Just (c, i) -> do
         setInput i; lex_quasiquote start (c : s)

quasiquote_error :: RealSrcLoc -> P a
quasiquote_error start = do
  (AI end buf) <- getInput
  reportLexError start (psRealLoc end) buf
    (\k srcLoc -> mkPlainErrorMsgEnvelope srcLoc (PsErrLexer LexUnterminatedQQ k))

-- -----------------------------------------------------------------------------
-- Unicode Smart Quote detection (#21843)

isDoubleSmartQuote :: Char -> Bool
isDoubleSmartQuote '“' = True
isDoubleSmartQuote '”' = True
isDoubleSmartQuote _ = False

isSingleSmartQuote :: Char -> Bool
isSingleSmartQuote '‘' = True
isSingleSmartQuote '’' = True
isSingleSmartQuote _ = False

isSmartQuote :: AlexAccPred ExtsBitmap
isSmartQuote _ _ _ (AI _ buf) = let c = prevChar buf ' ' in isSingleSmartQuote c || isDoubleSmartQuote c

smart_quote_error_message :: Char -> PsLoc -> MsgEnvelope PsMessage
smart_quote_error_message c loc =
  let (correct_char, correct_char_name) =
         if isSingleSmartQuote c then ('\'', "Single Quote") else ('"', "Quotation Mark")
      err = mkPlainErrorMsgEnvelope (mkSrcSpanPs (mkPsSpan loc loc)) $
              PsErrUnicodeCharLooksLike c correct_char correct_char_name in
    err

smart_quote_error :: Action
smart_quote_error span buf _len _buf2 = do
  let c = currentChar buf
  addFatalError (smart_quote_error_message c (psSpanStart span))

add_smart_quote_error :: Char -> PsLoc -> P a
add_smart_quote_error c loc = addFatalError (smart_quote_error_message c loc)

add_nonfatal_smart_quote_error :: Char -> PsLoc -> P ()
add_nonfatal_smart_quote_error c loc = addError (smart_quote_error_message c loc)

advance_to_smart_quote_character :: P ()
advance_to_smart_quote_character  = do
  i <- getInput
  case alexGetChar' i of
    Just (c, _) | isDoubleSmartQuote c -> return ()
    Just (_, i2) -> do setInput i2; advance_to_smart_quote_character
    Nothing -> return () -- should never get here

-- -----------------------------------------------------------------------------
-- Warnings

warnTab :: Action
warnTab srcspan _buf _len _buf2 = do
    addTabWarning (psRealSpan srcspan)
    lexToken

warnThen :: PsMessage -> Action -> Action
warnThen warning action srcspan buf len buf2 = do
    addPsMessage (RealSrcSpan (psRealSpan srcspan) Strict.Nothing) warning
    action srcspan buf len buf2

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

-- | The result of running a parser.
newtype ParseResult a = PR (# (# PState, a #) | PState #)

-- | The parser has consumed a (possibly empty) prefix of the input and produced
-- a result. Use 'getPsMessages' to check for accumulated warnings and non-fatal
-- errors.
--
-- The carried parsing state can be used to resume parsing.
pattern POk :: PState -> a -> ParseResult a
pattern POk s a = PR (# (# s , a #) | #)

-- | The parser has consumed a (possibly empty) prefix of the input and failed.
--
-- The carried parsing state can be used to resume parsing. It is the state
-- right before failure, including the fatal parse error. 'getPsMessages' and
-- 'getPsErrorMessages' must return a non-empty bag of errors.
pattern PFailed :: PState -> ParseResult a
pattern PFailed s = PR (# | s #)

{-# COMPLETE POk, PFailed #-}

-- | Test whether a 'WarningFlag' is set
warnopt :: WarningFlag -> ParserOpts -> Bool
warnopt f options = f `EnumSet.member` pWarningFlags options

-- | Parser options.
--
-- See 'mkParserOpts' to construct this.
data ParserOpts = ParserOpts
  { pExtsBitmap     :: !ExtsBitmap -- ^ bitmap of permitted extensions
  , pDiagOpts       :: !DiagOpts
    -- ^ Options to construct diagnostic messages.
  , pSupportedExts  :: [String]
    -- ^ supported extensions (only used for suggestions in error messages)
  }

pWarningFlags :: ParserOpts -> EnumSet WarningFlag
pWarningFlags opts = diag_warning_flags (pDiagOpts opts)

-- | Haddock comment as produced by the lexer. These are accumulated in 'PState'
-- and then processed in "GHC.Parser.PostProcess.Haddock". The location of the
-- 'HsDocString's spans over the contents of the docstring - i.e. it does not
-- include the decorator ("-- |", "{-|" etc.)
data HdkComment
  = HdkCommentNext HsDocString
  | HdkCommentPrev HsDocString
  | HdkCommentNamed String HsDocString
  | HdkCommentSection Int HsDocString
  deriving Show

data PState = PState {
        buffer     :: StringBuffer,
        options    :: ParserOpts,
        warnings   :: Messages PsMessage,
        errors     :: Messages PsMessage,
        tab_first  :: Strict.Maybe RealSrcSpan, -- pos of first tab warning in the file
        tab_count  :: !Word,             -- number of tab warnings in the file
        last_tk    :: Strict.Maybe (PsLocated Token), -- last non-comment token
        prev_loc   :: PsSpan,      -- pos of previous non-virtual token, including comments,
        last_loc   :: PsSpan,      -- pos of current token
        last_len   :: !Int,        -- len of current token
        loc        :: PsLoc,       -- current loc (end of prev token + 1)
        context    :: [LayoutContext],
        lex_state  :: [Int],
        srcfiles   :: [FastString],
        -- Used in the alternative layout rule:
        -- These tokens are the next ones to be sent out. They are
        -- just blindly emitted, without the rule looking at them again:
        alr_pending_implicit_tokens :: [PsLocated Token],
        -- This is the next token to be considered or, if it is Nothing,
        -- we need to get the next token from the input stream:
        alr_next_token :: Maybe (PsLocated Token),
        -- This is what we consider to be the location of the last token
        -- emitted:
        alr_last_loc :: PsSpan,
        -- The stack of layout contexts:
        alr_context :: [ALRContext],
        -- Are we expecting a '{'? If it's Just, then the ALRLayout tells
        -- us what sort of layout the '{' will open:
        alr_expecting_ocurly :: Maybe ALRLayout,
        -- Have we just had the '}' for a let block? If so, than an 'in'
        -- token doesn't need to close anything:
        alr_justClosedExplicitLetBlock :: Bool,

        -- The next three are used to implement Annotations giving the
        -- locations of 'noise' tokens in the source, so that users of
        -- the GHC API can do source to source conversions.
        -- See Note [exact print annotations] in GHC.Parser.Annotation
        eof_pos :: Strict.Maybe (Strict.Pair RealSrcSpan RealSrcSpan), -- pos, gap to prior token
        header_comments :: Strict.Maybe [LEpaComment],
        comment_q :: [LEpaComment],

        -- Haddock comments accumulated in ascending order of their location
        -- (BufPos). We use OrdList to get O(1) snoc.
        --
        -- See Note [Adding Haddock comments to the syntax tree] in GHC.Parser.PostProcess.Haddock
        hdk_comments :: OrdList (PsLocated HdkComment)
     }
        -- last_loc and last_len are used when generating error messages,
        -- and in pushCurrentContext only.  Sigh, if only Happy passed the
        -- current token to happyError, we could at least get rid of last_len.
        -- Getting rid of last_loc would require finding another way to
        -- implement pushCurrentContext (which is only called from one place).

        -- AZ question: setLastToken which sets last_loc and last_len
        -- is called when processing AlexToken, immediately prior to
        -- calling the action in the token.  So from the perspective
        -- of the action, it is the *current* token.  Do I understand
        -- correctly?

data ALRContext = ALRNoLayout Bool{- does it contain commas? -}
                              Bool{- is it a 'let' block? -}
                | ALRLayout ALRLayout Int
data ALRLayout = ALRLayoutLet
               | ALRLayoutWhere
               | ALRLayoutOf
               | ALRLayoutDo

-- | The parsing monad, isomorphic to @StateT PState Maybe@.
newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = returnP
  (<*>) = ap

instance Monad P where
  (>>=) = thenP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed s1 -> PFailed s1

failMsgP :: (SrcSpan -> MsgEnvelope PsMessage) -> P a
failMsgP f = do
  pState <- getPState
  addFatalError (f (mkSrcSpanPs (last_loc pState)))

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> (SrcSpan -> MsgEnvelope PsMessage) -> P a
failLocMsgP loc1 loc2 f =
  addFatalError (f (RealSrcSpan (mkRealSrcSpan loc1 loc2) Strict.Nothing))

getPState :: P PState
getPState = P $ \s -> POk s s

getExts :: P ExtsBitmap
getExts = P $ \s -> POk s (pExtsBitmap . options $ s)

setExts :: (ExtsBitmap -> ExtsBitmap) -> P ()
setExts f = P $ \s -> POk s {
  options =
    let p = options s
    in  p { pExtsBitmap = f (pExtsBitmap p) }
  } ()

setSrcLoc :: RealSrcLoc -> P ()
setSrcLoc new_loc =
  P $ \s@(PState{ loc = PsLoc _ buf_loc }) ->
  POk s{ loc = PsLoc new_loc buf_loc } ()

getRealSrcLoc :: P RealSrcLoc
getRealSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s (psRealLoc loc)

getParsedLoc :: P PsLoc
getParsedLoc  = P $ \s@(PState{ loc=loc }) -> POk s loc

addSrcFile :: FastString -> P ()
addSrcFile f = P $ \s -> POk s{ srcfiles = f : srcfiles s } ()

setEofPos :: RealSrcSpan -> RealSrcSpan -> P ()
setEofPos span gap = P $ \s -> POk s{ eof_pos = Strict.Just (span `Strict.And` gap) } ()

setLastToken :: PsSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: PsLocated Token -> P ()
setLastTk tk@(L l _) = P $ \s ->
  if isPointRealSpan (psRealSpan l)
    then POk s { last_tk = Strict.Just tk } ()
    else POk s { last_tk = Strict.Just tk
               , prev_loc = l } ()

setLastComment :: PsLocated Token -> P ()
setLastComment (L l _) = P $ \s -> POk s { prev_loc = l } ()

getLastTk :: P (Strict.Maybe (PsLocated Token))
getLastTk = P $ \s@(PState { last_tk = last_tk }) -> POk s last_tk

-- see Note [PsSpan in Comments]
getLastLocIncludingComments :: P PsSpan
getLastLocIncludingComments = P $ \s@(PState { prev_loc = prev_loc }) -> POk s prev_loc

getLastLoc :: P PsSpan
getLastLoc = P $ \s@(PState { last_loc = last_loc }) -> POk s last_loc

data AlexInput = AI !PsLoc !StringBuffer

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
                -- in "GHC.Utils.Lexeme"
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
                  LetterNumber          -> digit
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
        loc'   = advancePsLoc loc c
        byte   = adjustChar c

{-# INLINE alexGetChar' #-}
-- This version does not squash unicode characters, it is used when
-- lexing strings.
alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advancePsLoc loc c

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

popNextToken :: P (Maybe (PsLocated Token))
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

resetAlrLastLoc :: FastString -> P ()
resetAlrLastLoc file =
  P $ \s@(PState {alr_last_loc = PsSpan _ buf_span}) ->
  POk s{ alr_last_loc = PsSpan (alrInitialLoc file) buf_span } ()

setAlrLastLoc :: PsSpan -> P ()
setAlrLastLoc l = P $ \s -> POk (s {alr_last_loc = l}) ()

getAlrLastLoc :: P PsSpan
getAlrLastLoc = P $ \s@(PState {alr_last_loc = l}) -> POk s l

getALRContext :: P [ALRContext]
getALRContext = P $ \s@(PState {alr_context = cs}) -> POk s cs

setALRContext :: [ALRContext] -> P ()
setALRContext cs = P $ \s -> POk (s {alr_context = cs}) ()

getJustClosedExplicitLetBlock :: P Bool
getJustClosedExplicitLetBlock
 = P $ \s@(PState {alr_justClosedExplicitLetBlock = b}) -> POk s b

setJustClosedExplicitLetBlock :: Bool -> P ()
setJustClosedExplicitLetBlock b
 = P $ \s -> POk (s {alr_justClosedExplicitLetBlock = b}) ()

setNextToken :: PsLocated Token -> P ()
setNextToken t = P $ \s -> POk (s {alr_next_token = Just t}) ()

implicitTokenPending :: P Bool
implicitTokenPending
    = P $ \s@PState{ alr_pending_implicit_tokens = ts } ->
              case ts of
              [] -> POk s False
              _  -> POk s True

popPendingImplicitToken :: P (Maybe (PsLocated Token))
popPendingImplicitToken
    = P $ \s@PState{ alr_pending_implicit_tokens = ts } ->
              case ts of
              [] -> POk s Nothing
              (t : ts') -> POk (s {alr_pending_implicit_tokens = ts'}) (Just t)

setPendingImplicitTokens :: [PsLocated Token] -> P ()
setPendingImplicitTokens ts = P $ \s -> POk (s {alr_pending_implicit_tokens = ts}) ()

getAlrExpectingOCurly :: P (Maybe ALRLayout)
getAlrExpectingOCurly = P $ \s@(PState {alr_expecting_ocurly = b}) -> POk s b

setAlrExpectingOCurly :: Maybe ALRLayout -> P ()
setAlrExpectingOCurly b = P $ \s -> POk (s {alr_expecting_ocurly = b}) ()

-- | For reasons of efficiency, boolean parsing flags (eg, language extensions
-- or whether we are currently in a @RULE@ pragma) are represented by a bitmap
-- stored in a @Word64@.
type ExtsBitmap = Word64

xbit :: ExtBits -> ExtsBitmap
xbit = bit . fromEnum

xtest :: ExtBits -> ExtsBitmap -> Bool
xtest ext xmap = testBit xmap (fromEnum ext)

xset :: ExtBits -> ExtsBitmap -> ExtsBitmap
xset ext xmap = setBit xmap (fromEnum ext)

xunset :: ExtBits -> ExtsBitmap -> ExtsBitmap
xunset ext xmap = clearBit xmap (fromEnum ext)

-- | Various boolean flags, mostly language extensions, that impact lexing and
-- parsing. Note that a handful of these can change during lexing/parsing.
data ExtBits
  -- Flags that are constant once parsing starts
  = FfiBit
  | InterruptibleFfiBit
  | CApiFfiBit
  | ArrowsBit
  | ThBit
  | ThQuotesBit
  | IpBit
  | OverloadedLabelsBit -- #x overloaded labels
  | ExplicitForallBit -- the 'forall' keyword
  | BangPatBit -- Tells the parser to understand bang-patterns
               -- (doesn't affect the lexer)
  | PatternSynonymsBit -- pattern synonyms
  | HaddockBit-- Lex and parse Haddock comments
  | MagicHashBit -- "#" in both functions and operators
  | RecursiveDoBit -- mdo
  | QualifiedDoBit -- .do and .mdo
  | UnicodeSyntaxBit -- the forall symbol, arrow symbols, etc
  | UnboxedParensBit -- (# and #)
  | DatatypeContextsBit
  | MonadComprehensionsBit
  | TransformComprehensionsBit
  | QqBit -- enable quasiquoting
  | RawTokenStreamBit -- producing a token stream with all comments included
  | AlternativeLayoutRuleBit
  | ALRTransitionalBit
  | RelaxedLayoutBit
  | NondecreasingIndentationBit
  | SafeHaskellBit
  | TraditionalRecordSyntaxBit
  | ExplicitNamespacesBit
  | LambdaCaseBit
  | BinaryLiteralsBit
  | NegativeLiteralsBit
  | HexFloatLiteralsBit
  | StaticPointersBit
  | NumericUnderscoresBit
  | StarIsTypeBit
  | BlockArgumentsBit
  | NPlusKPatternsBit
  | DoAndIfThenElseBit
  | MultiWayIfBit
  | GadtSyntaxBit
  | ImportQualifiedPostBit
  | LinearTypesBit
  | NoLexicalNegationBit   -- See Note [Why not LexicalNegationBit]
  | OverloadedRecordDotBit
  | OverloadedRecordUpdateBit
  | ExtendedLiteralsBit
  | ListTuplePunsBit

  -- Flags that are updated once parsing starts
  | InRulePragBit
  | InNestedCommentBit -- See Note [Nested comment line pragmas]
  | UsePosPragsBit
    -- ^ If this is enabled, '{-# LINE ... -#}' and '{-# COLUMN ... #-}'
    -- update the internal position. Otherwise, those pragmas are lexed as
    -- tokens of their own.
  deriving Enum

{-# INLINE mkParserOpts #-}
mkParserOpts
  :: EnumSet LangExt.Extension  -- ^ permitted language extensions enabled
  -> DiagOpts                   -- ^ diagnostic options
  -> [String]                   -- ^ Supported Languages and Extensions
  -> Bool                       -- ^ are safe imports on?
  -> Bool                       -- ^ keeping Haddock comment tokens
  -> Bool                       -- ^ keep regular comment tokens

  -> Bool
  -- ^ If this is enabled, '{-# LINE ... -#}' and '{-# COLUMN ... #-}' update
  -- the internal position kept by the parser. Otherwise, those pragmas are
  -- lexed as 'ITline_prag' and 'ITcolumn_prag' tokens.

  -> ParserOpts
-- ^ Given exactly the information needed, set up the 'ParserOpts'
mkParserOpts extensionFlags diag_opts supported
  safeImports isHaddock rawTokStream usePosPrags =
    ParserOpts {
      pDiagOpts      = diag_opts
    , pExtsBitmap    = safeHaskellBit .|. langExtBits .|. optBits
    , pSupportedExts = supported
    }
  where
    safeHaskellBit = SafeHaskellBit `setBitIf` safeImports
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
      .|. QualifiedDoBit              `xoptBit` LangExt.QualifiedDo
      .|. UnicodeSyntaxBit            `xoptBit` LangExt.UnicodeSyntax
      .|. UnboxedParensBit            `orXoptsBit` [LangExt.UnboxedTuples, LangExt.UnboxedSums]
      .|. DatatypeContextsBit         `xoptBit` LangExt.DatatypeContexts
      .|. TransformComprehensionsBit  `xoptBit` LangExt.TransformListComp
      .|. MonadComprehensionsBit      `xoptBit` LangExt.MonadComprehensions
      .|. AlternativeLayoutRuleBit    `xoptBit` LangExt.AlternativeLayoutRule
      .|. ALRTransitionalBit          `xoptBit` LangExt.AlternativeLayoutRuleTransitional
      .|. RelaxedLayoutBit            `xoptBit` LangExt.RelaxedLayout
      .|. NondecreasingIndentationBit `xoptBit` LangExt.NondecreasingIndentation
      .|. TraditionalRecordSyntaxBit  `xoptBit` LangExt.TraditionalRecordSyntax
      .|. ExplicitNamespacesBit       `xoptBit` LangExt.ExplicitNamespaces
      .|. LambdaCaseBit               `xoptBit` LangExt.LambdaCase
      .|. BinaryLiteralsBit           `xoptBit` LangExt.BinaryLiterals
      .|. NegativeLiteralsBit         `xoptBit` LangExt.NegativeLiterals
      .|. HexFloatLiteralsBit         `xoptBit` LangExt.HexFloatLiterals
      .|. PatternSynonymsBit          `xoptBit` LangExt.PatternSynonyms
      .|. StaticPointersBit           `xoptBit` LangExt.StaticPointers
      .|. NumericUnderscoresBit       `xoptBit` LangExt.NumericUnderscores
      .|. StarIsTypeBit               `xoptBit` LangExt.StarIsType
      .|. BlockArgumentsBit           `xoptBit` LangExt.BlockArguments
      .|. NPlusKPatternsBit           `xoptBit` LangExt.NPlusKPatterns
      .|. DoAndIfThenElseBit          `xoptBit` LangExt.DoAndIfThenElse
      .|. MultiWayIfBit               `xoptBit` LangExt.MultiWayIf
      .|. GadtSyntaxBit               `xoptBit` LangExt.GADTSyntax
      .|. ImportQualifiedPostBit      `xoptBit` LangExt.ImportQualifiedPost
      .|. LinearTypesBit              `xoptBit` LangExt.LinearTypes
      .|. NoLexicalNegationBit        `xoptNotBit` LangExt.LexicalNegation -- See Note [Why not LexicalNegationBit]
      .|. OverloadedRecordDotBit      `xoptBit` LangExt.OverloadedRecordDot
      .|. OverloadedRecordUpdateBit   `xoptBit` LangExt.OverloadedRecordUpdate  -- Enable testing via 'getBit OverloadedRecordUpdateBit' in the parser (RecordDotSyntax parsing uses that information).
      .|. ExtendedLiteralsBit         `xoptBit` LangExt.ExtendedLiterals
      .|. ListTuplePunsBit            `xoptBit` LangExt.ListTuplePuns
    optBits =
          HaddockBit        `setBitIf` isHaddock
      .|. RawTokenStreamBit `setBitIf` rawTokStream
      .|. UsePosPragsBit    `setBitIf` usePosPrags

    xoptBit bit ext = bit `setBitIf` EnumSet.member ext extensionFlags
    xoptNotBit bit ext = bit `setBitIf` not (EnumSet.member ext extensionFlags)

    orXoptsBit bit exts = bit `setBitIf` any (`EnumSet.member` extensionFlags) exts

    setBitIf :: ExtBits -> Bool -> ExtsBitmap
    b `setBitIf` cond | cond      = xbit b
                      | otherwise = 0

disableHaddock :: ParserOpts -> ParserOpts
disableHaddock opts = upd_bitmap (xunset HaddockBit)
  where
    upd_bitmap f = opts { pExtsBitmap = f (pExtsBitmap opts) }


-- | Set parser options for parsing OPTIONS pragmas
initPragState :: ParserOpts -> StringBuffer -> RealSrcLoc -> PState
initPragState options buf loc = (initParserState options buf loc)
   { lex_state = [bol, option_prags, 0]
   }

-- | Creates a parse state from a 'ParserOpts' value
initParserState :: ParserOpts -> StringBuffer -> RealSrcLoc -> PState
initParserState options buf loc =
  PState {
      buffer        = buf,
      options       = options,
      errors        = emptyMessages,
      warnings      = emptyMessages,
      tab_first     = Strict.Nothing,
      tab_count     = 0,
      last_tk       = Strict.Nothing,
      prev_loc      = mkPsSpan init_loc init_loc,
      last_loc      = mkPsSpan init_loc init_loc,
      last_len      = 0,
      loc           = init_loc,
      context       = [],
      lex_state     = [bol, 0],
      srcfiles      = [],
      alr_pending_implicit_tokens = [],
      alr_next_token = Nothing,
      alr_last_loc = PsSpan (alrInitialLoc (fsLit "<no file>")) (BufSpan (BufPos 0) (BufPos 0)),
      alr_context = [],
      alr_expecting_ocurly = Nothing,
      alr_justClosedExplicitLetBlock = False,
      eof_pos = Strict.Nothing,
      header_comments = Strict.Nothing,
      comment_q = [],
      hdk_comments = nilOL
    }
  where init_loc = PsLoc loc (BufPos 0)

-- | An mtl-style class for monads that support parsing-related operations.
-- For example, sometimes we make a second pass over the parsing results to validate,
-- disambiguate, or rearrange them, and we do so in the PV monad which cannot consume
-- input but can report parsing errors, check for extension bits, and accumulate
-- parsing annotations. Both P and PV are instances of MonadP.
--
-- MonadP grants us convenient overloading. The other option is to have separate operations
-- for each monad: addErrorP vs addErrorPV, getBitP vs getBitPV, and so on.
--
class Monad m => MonadP m where
  -- | Add a non-fatal error. Use this when the parser can produce a result
  --   despite the error.
  --
  --   For example, when GHC encounters a @forall@ in a type,
  --   but @-XExplicitForAll@ is disabled, the parser constructs @ForAllTy@
  --   as if @-XExplicitForAll@ was enabled, adding a non-fatal error to
  --   the accumulator.
  --
  --   Control flow wise, non-fatal errors act like warnings: they are added
  --   to the accumulator and parsing continues. This allows GHC to report
  --   more than one parse error per file.
  --
  addError :: MsgEnvelope PsMessage -> m ()

  -- | Add a warning to the accumulator.
  --   Use 'getPsMessages' to get the accumulated warnings.
  addWarning :: MsgEnvelope PsMessage -> m ()

  -- | Add a fatal error. This will be the last error reported by the parser, and
  --   the parser will not produce any result, ending in a 'PFailed' state.
  addFatalError :: MsgEnvelope PsMessage -> m a

  -- | Check if a given flag is currently set in the bitmap.
  getBit :: ExtBits -> m Bool
  -- | Go through the @comment_q@ in @PState@ and remove all comments
  -- that belong within the given span
  allocateCommentsP :: RealSrcSpan -> m EpAnnComments
  -- | Go through the @comment_q@ in @PState@ and remove all comments
  -- that come before or within the given span
  allocatePriorCommentsP :: RealSrcSpan -> m EpAnnComments
  -- | Go through the @comment_q@ in @PState@ and remove all comments
  -- that come after the given span
  allocateFinalCommentsP :: RealSrcSpan -> m EpAnnComments

instance MonadP P where
  addError err
   = P $ \s -> POk s { errors = err `addMessage` errors s} ()

  -- If the warning is meant to be suppressed, GHC will assign
  -- a `SevIgnore` severity and the message will be discarded,
  -- so we can simply add it no matter what.
  addWarning w
   = P $ \s -> POk (s { warnings = w `addMessage` warnings s }) ()

  addFatalError err =
    addError err >> P PFailed

  getBit ext = P $ \s -> let b =  ext `xtest` pExtsBitmap (options s)
                         in b `seq` POk s b
  allocateCommentsP ss = P $ \s ->
    if null (comment_q s) then POk s emptyComments else  -- fast path
    let (comment_q', newAnns) = allocateComments ss (comment_q s) in
      POk s {
         comment_q = comment_q'
       } (EpaComments newAnns)
  allocatePriorCommentsP ss = P $ \s ->
    let (header_comments', comment_q', newAnns)
             = allocatePriorComments ss (comment_q s) (header_comments s) in
      POk s {
         header_comments = header_comments',
         comment_q = comment_q'
       } (EpaComments newAnns)
  allocateFinalCommentsP ss = P $ \s ->
    let (header_comments', comment_q', newAnns)
             = allocateFinalComments ss (comment_q s) (header_comments s) in
      POk s {
         header_comments = header_comments',
         comment_q = comment_q'
       } (EpaCommentsBalanced (Strict.fromMaybe [] header_comments') newAnns)

getCommentsFor :: (MonadP m) => SrcSpan -> m EpAnnComments
getCommentsFor (RealSrcSpan l _) = allocateCommentsP l
getCommentsFor _ = return emptyComments

getPriorCommentsFor :: (MonadP m) => SrcSpan -> m EpAnnComments
getPriorCommentsFor (RealSrcSpan l _) = allocatePriorCommentsP l
getPriorCommentsFor _ = return emptyComments

getFinalCommentsFor :: (MonadP m) => SrcSpan -> m EpAnnComments
getFinalCommentsFor (RealSrcSpan l _) = allocateFinalCommentsP l
getFinalCommentsFor _ = return emptyComments

getEofPos :: P (Strict.Maybe (Strict.Pair RealSrcSpan RealSrcSpan))
getEofPos = P $ \s@(PState { eof_pos = pos }) -> POk s pos

addPsMessage :: SrcSpan -> PsMessage -> P ()
addPsMessage srcspan msg = do
  diag_opts <- (pDiagOpts . options) <$> getPState
  addWarning (mkPlainMsgEnvelope diag_opts srcspan msg)

addTabWarning :: RealSrcSpan -> P ()
addTabWarning srcspan
 = P $ \s@PState{tab_first=tf, tab_count=tc, options=o} ->
       let tf' = tf <|> Strict.Just srcspan
           tc' = tc + 1
           s' = if warnopt Opt_WarnTabs o
                then s{tab_first = tf', tab_count = tc'}
                else s
       in POk s' ()

-- | Get a bag of the errors that have been accumulated so far.
--   Does not take -Werror into account.
getPsErrorMessages :: PState -> Messages PsMessage
getPsErrorMessages p = errors p

-- | Get the warnings and errors accumulated so far.
--   Does not take -Werror into account.
getPsMessages :: PState -> (Messages PsMessage, Messages PsMessage)
getPsMessages p =
  let ws = warnings p
      diag_opts = pDiagOpts (options p)
      -- we add the tabulation warning on the fly because
      -- we count the number of occurrences of tab characters
      ws' = case tab_first p of
        Strict.Nothing -> ws
        Strict.Just tf ->
          let msg = mkPlainMsgEnvelope diag_opts
                          (RealSrcSpan tf Strict.Nothing)
                          (PsWarnTab (tab_count p))
          in msg `addMessage` ws
  in (ws', errors p)

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
          unP (addFatalError $ srcParseErr o buf len (mkSrcSpanPs last_loc)) s

-- Push a new layout context at the indentation of the last token read.
pushCurrentContext :: GenSemic -> P ()
pushCurrentContext gen_semic = P $ \ s@PState{ last_loc=loc, context=ctx } ->
    POk s{context = Layout (srcSpanStartCol (psRealSpan loc)) gen_semic : ctx} ()

-- This is only used at the outer level of a module when the 'module' keyword is
-- missing.
pushModuleContext :: P ()
pushModuleContext = pushCurrentContext generateSemic

getOffside :: P (Ordering, Bool)
getOffside = P $ \s@PState{last_loc=loc, context=stk} ->
                let offs = srcSpanStartCol (psRealSpan loc) in
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
  :: ParserOpts
  -> StringBuffer       -- current buffer (placed just after the last token)
  -> Int                -- length of the previous token
  -> SrcSpan
  -> MsgEnvelope PsMessage
srcParseErr options buf len loc = mkPlainErrorMsgEnvelope loc (PsErrParse token details)
  where
   token = lexemeToString (offsetBytes (-len) buf) len
   pattern_ = decodePrevNChars 8 buf
   last100 = decodePrevNChars 100 buf
   doInLast100 = "do" `isInfixOf` last100
   mdoInLast100 = "mdo" `isInfixOf` last100
   th_enabled = ThQuotesBit `xtest` pExtsBitmap options
   ps_enabled = PatternSynonymsBit `xtest` pExtsBitmap options
   details = PsErrParseDetails {
       ped_th_enabled      = th_enabled
     , ped_do_in_last_100  = doInLast100
     , ped_mdo_in_last_100 = mdoInLast100
     , ped_pat_syn_enabled = ps_enabled
     , ped_pattern_parsed  = pattern_ == "pattern "
     }

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \s@PState{ buffer = buf, options = o, last_len = len,
                            last_loc = last_loc } ->
    unP (addFatalError $ srcParseErr o buf len (mkSrcSpanPs last_loc)) s

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: LexErr -> P a
lexError e = do
  loc <- getRealSrcLoc
  (AI end buf) <- getInput
  reportLexError loc (psRealLoc end) buf
    (\k srcLoc -> mkPlainErrorMsgEnvelope srcLoc $ PsErrLexer e k)

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer, lexerDbg :: Bool -> (Located Token -> P a) -> P a

lexer queueComments cont = do
  alr <- getBit AlternativeLayoutRuleBit
  let lexTokenFun = if alr then lexTokenAlr else lexToken
  (L span tok) <- lexTokenFun
  --trace ("token: " ++ show tok) $ do

  if (queueComments && isComment tok)
    then queueComment (L (psRealSpan span) tok) >> lexer queueComments cont
    else cont (L (mkSrcSpanPs span) tok)

-- Use this instead of 'lexer' in GHC.Parser to dump the tokens for debugging.
lexerDbg queueComments cont = lexer queueComments contDbg
  where
    contDbg tok = trace ("token: " ++ show (unLoc tok)) (cont tok)

lexTokenAlr :: P (PsLocated Token)
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
                 setAlrLastLoc (getLoc t)
                 case unLoc t of
                     ITwhere  -> setAlrExpectingOCurly (Just ALRLayoutWhere)
                     ITlet    -> setAlrExpectingOCurly (Just ALRLayoutLet)
                     ITof     -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITlcase  -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITlcases -> setAlrExpectingOCurly (Just ALRLayoutOf)
                     ITdo  _  -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     ITmdo _  -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     ITrec    -> setAlrExpectingOCurly (Just ALRLayoutDo)
                     _        -> return ()
                 return t

alternativeLayoutRuleToken :: PsLocated Token -> P (PsLocated Token)
alternativeLayoutRuleToken t
    = do context <- getALRContext
         lastLoc <- getAlrLastLoc
         mExpectingOCurly <- getAlrExpectingOCurly
         transitional <- getBit ALRTransitionalBit
         justClosedExplicitLetBlock <- getJustClosedExplicitLetBlock
         setJustClosedExplicitLetBlock False
         let thisLoc = getLoc t
             thisCol = srcSpanStartCol (psRealSpan thisLoc)
             newLine = srcSpanStartLine (psRealSpan thisLoc) > srcSpanEndLine (psRealSpan lastLoc)
         case (unLoc t, context, mExpectingOCurly) of
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
                    return (L thisLoc ITvocurly)
              | otherwise ->
                 do setAlrExpectingOCurly Nothing
                    setPendingImplicitTokens [L lastLoc ITvccurly]
                    setNextToken t
                    return (L lastLoc ITvocurly)
             (_, _, Just expectingOCurly) ->
                 do setAlrExpectingOCurly Nothing
                    setALRContext (ALRLayout expectingOCurly thisCol : context)
                    setNextToken t
                    return (L thisLoc ITvocurly)
             -- We do the [] cases earlier than in the spec, as we
             -- have an actual EOF token
             (ITeof, ALRLayout _ _ : ls, _) ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITvccurly)
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
                    return (L thisLoc ITvccurly)
             -- This next case is to handle a transitional issue:
             (ITwhere, ALRLayout _ col : ls, _)
              | newLine && thisCol == col && transitional ->
                 do addPsMessage
                      (mkSrcSpanPs thisLoc)
                      (PsWarnTransitionalLayout TransLayout_Where)
                    setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITvccurly)
             -- This next case is to handle a transitional issue:
             (ITvbar, ALRLayout _ col : ls, _)
              | newLine && thisCol == col && transitional ->
                 do addPsMessage
                      (mkSrcSpanPs thisLoc)
                      (PsWarnTransitionalLayout TransLayout_Pipe)
                    setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITvccurly)
             (_, ALRLayout _ col : ls, _)
              | newLine && thisCol == col ->
                 do setNextToken t
                    let loc = psSpanStart thisLoc
                        zeroWidthLoc = mkPsSpan loc loc
                    return (L zeroWidthLoc ITsemi)
              | newLine && thisCol < col ->
                 do setALRContext ls
                    setNextToken t
                    -- Note that we use lastLoc, as we may need to close
                    -- more layouts, or give a semicolon
                    return (L lastLoc ITvccurly)
             -- We need to handle close before open, as 'then' is both
             -- an open and a close
             (u, _, _)
              | isALRclose u ->
                 case context of
                 ALRLayout _ _ : ls ->
                     do setALRContext ls
                        setNextToken t
                        return (L thisLoc ITvccurly)
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
                    return (L thisLoc ITvccurly)
             (ITin, ALRLayout _ _ : ls, _) ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITvccurly)
             -- the other ITin case omitted; general case below covers it
             (ITcomma, ALRLayout _ _ : ls, _)
              | topNoLayoutContainsCommas ls ->
                 do setALRContext ls
                    setNextToken t
                    return (L thisLoc ITvccurly)
             (ITwhere, ALRLayout ALRLayoutDo _ : ls, _) ->
                 do setALRContext ls
                    setPendingImplicitTokens [t]
                    return (L thisLoc ITvccurly)
             -- the other ITwhere case omitted; general case below covers it
             (_, _, _) -> return t

isALRopen :: Token -> Bool
isALRopen ITcase          = True
isALRopen ITif            = True
isALRopen ITthen          = True
isALRopen IToparen        = True
isALRopen ITobrack        = True
isALRopen ITocurly        = True
-- GHC Extensions:
isALRopen IToubxparen     = True
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

lexToken :: P (PsLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  exts <- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do
        let span = mkPsSpan loc1 loc1
        lc <- getLastLocIncludingComments
        setEofPos (psRealSpan span) (psRealSpan lc)
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError (psRealLoc loc1) (psRealLoc loc2) buf
          (\k srcLoc -> mkPlainErrorMsgEnvelope srcLoc $ PsErrLexer LexError k)
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkPsSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes buf2
        let lt' = unLoc lt
        if (isComment lt') then setLastComment lt else setLastTk lt
        return lt

reportLexError :: RealSrcLoc
               -> RealSrcLoc
               -> StringBuffer
               -> (LexErrKind -> SrcSpan -> MsgEnvelope PsMessage)
               -> P a
reportLexError loc1 loc2 buf f
  | atEnd buf = failLocMsgP loc1 loc2 (f LexErrKind_EOF)
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (f LexErrKind_UTF8)
     else failLocMsgP loc1 loc2 (f (LexErrKind_Char c))

lexTokenStream :: ParserOpts -> StringBuffer -> RealSrcLoc -> ParseResult [Located Token]
lexTokenStream opts buf loc = unP go initState{ options = opts' }
    where
    new_exts  =   xunset UsePosPragsBit  -- parse LINE/COLUMN pragmas as tokens
                $ xset RawTokenStreamBit -- include comments
                $ pExtsBitmap opts
    opts'     = opts { pExtsBitmap = new_exts }
    initState = initParserState opts' buf loc
    go = do
      ltok <- lexer False return
      case ltok of
        L _ ITeof -> return []
        _ -> liftM (ltok:) go

linePrags = Map.singleton "line" linePrag

fileHeaderPrags = Map.fromList([("options", lex_string_prag IToptions_prag),
                                 ("options_ghc", lex_string_prag IToptions_prag),
                                 ("options_haddock", lex_string_prag_comment ITdocOptions),
                                 ("language", token ITlanguage_prag),
                                 ("include", lex_string_prag ITinclude_prag)])

ignoredPrags = Map.fromList (map ignored pragmas)
               where ignored opt = (opt, nested_comment)
                     impls = ["hugs", "nhc98", "jhc", "yhc", "catch", "derive"]
                     options_pragmas = map ("options_" ++) impls
                     -- CFILES is a hugs-only thing.
                     pragmas = options_pragmas ++ ["cfiles", "contract"]

oneWordPrags = Map.fromList [
     ("rules", rulePrag),
     ("inline",
         fstrtoken (\s -> (ITinline_prag (SourceText s) (Inline (SourceText s)) FunLike))),
     ("inlinable",
         fstrtoken (\s -> (ITinline_prag (SourceText s) (Inlinable (SourceText s)) FunLike))),
     ("inlineable",
         fstrtoken (\s -> (ITinline_prag (SourceText s) (Inlinable (SourceText s)) FunLike))),
                                    -- Spelling variant
     ("notinline",
         fstrtoken (\s -> (ITinline_prag (SourceText s) (NoInline (SourceText s)) FunLike))),
     ("opaque", fstrtoken (\s -> ITopaque_prag (SourceText s))),
     ("specialize", fstrtoken (\s -> ITspec_prag (SourceText s))),
     ("source", fstrtoken (\s -> ITsource_prag (SourceText s))),
     ("warning", fstrtoken (\s -> ITwarning_prag (SourceText s))),
     ("deprecated", fstrtoken (\s -> ITdeprecated_prag (SourceText s))),
     ("scc", fstrtoken (\s -> ITscc_prag (SourceText s))),
     ("unpack", fstrtoken (\s -> ITunpack_prag (SourceText s))),
     ("nounpack", fstrtoken (\s -> ITnounpack_prag (SourceText s))),
     ("ann", fstrtoken (\s -> ITann_prag (SourceText s))),
     ("minimal", fstrtoken (\s -> ITminimal_prag (SourceText s))),
     ("overlaps", fstrtoken (\s -> IToverlaps_prag (SourceText s))),
     ("overlappable", fstrtoken (\s -> IToverlappable_prag (SourceText s))),
     ("overlapping", fstrtoken (\s -> IToverlapping_prag (SourceText s))),
     ("incoherent", fstrtoken (\s -> ITincoherent_prag (SourceText s))),
     ("ctype", fstrtoken (\s -> ITctype (SourceText s))),
     ("complete", fstrtoken (\s -> ITcomplete_prag (SourceText s))),
     ("column", columnPrag)
     ]

twoWordPrags = Map.fromList [
     ("inline conlike",
         fstrtoken (\s -> (ITinline_prag (SourceText s) (Inline (SourceText s)) ConLike))),
     ("notinline conlike",
         fstrtoken (\s -> (ITinline_prag (SourceText s) (NoInline (SourceText s)) ConLike))),
     ("specialize inline",
         fstrtoken (\s -> (ITspec_inline_prag (SourceText s) True))),
     ("specialize notinline",
         fstrtoken (\s -> (ITspec_inline_prag (SourceText s) False)))
     ]

dispatch_pragmas :: Map String Action -> Action
dispatch_pragmas prags span buf len buf2 =
  case Map.lookup (clean_pragma (lexemeToString buf len)) prags of
    Just found -> found span buf len buf2
    Nothing -> lexError LexUnknownPragma

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

warn_unknown_prag :: Map String Action -> Action
warn_unknown_prag prags span buf len buf2 = do
  let uppercase    = map toUpper
      unknown_prag = uppercase (clean_pragma (lexemeToString buf len))
      suggestions  = map uppercase (Map.keys prags)
  addPsMessage (RealSrcSpan (psRealSpan span) Strict.Nothing) $
    PsWarnUnrecognisedPragma unknown_prag suggestions
  nested_comment span buf len buf2

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************
-}


-- |Given a 'RealSrcSpan' that surrounds a 'HsPar' or 'HsParTy', generate
-- 'AddEpAnn' values for the opening and closing bordering on the start
-- and end of the span
mkParensEpAnn :: RealSrcSpan -> (AddEpAnn, AddEpAnn)
mkParensEpAnn ss = (AddEpAnn AnnOpenP (EpaSpan (RealSrcSpan lo Strict.Nothing)),
                    AddEpAnn AnnCloseP (EpaSpan (RealSrcSpan lc Strict.Nothing)))
  where
    f = srcSpanFile ss
    sl = srcSpanStartLine ss
    sc = srcSpanStartCol ss
    el = srcSpanEndLine ss
    ec = srcSpanEndCol ss
    lo = mkRealSrcSpan (realSrcSpanStart ss)        (mkRealSrcLoc f sl (sc+1))
    lc = mkRealSrcSpan (mkRealSrcLoc f el (ec - 1)) (realSrcSpanEnd ss)

queueComment :: RealLocated Token -> P()
queueComment c = P $ \s -> POk s {
  comment_q = commentToAnnotation c : comment_q s
  } ()

allocateComments
  :: RealSrcSpan
  -> [LEpaComment]
  -> ([LEpaComment], [LEpaComment])
allocateComments ss comment_q =
  let
    (before,rest)  = break (\(L l _) -> isRealSubspanOf (anchor l) ss) comment_q
    (middle,after) = break (\(L l _) -> not (isRealSubspanOf (anchor l) ss)) rest
    comment_q' = before ++ after
    newAnns = middle
  in
    (comment_q', reverse newAnns)

-- Comments appearing without a line-break before the first
-- declaration are associated with the declaration
splitPriorComments
  :: RealSrcSpan
  -> [LEpaComment]
  -> ([LEpaComment], [LEpaComment])
splitPriorComments ss prior_comments =
  let
    -- True if there is only one line between the earlier and later span,
    -- And the token preceding the comment is on a different line
    cmp :: RealSrcSpan -> LEpaComment -> Bool
    cmp later (L l c)
         = srcSpanStartLine later - srcSpanEndLine (anchor l) == 1
          && srcSpanEndLine (ac_prior_tok c) /= srcSpanStartLine (anchor l)

    go :: [LEpaComment] -> RealSrcSpan -> [LEpaComment]
       -> ([LEpaComment], [LEpaComment])
    go decl_comments _ [] = ([],decl_comments)
    go decl_comments r (c@(L l _):cs) = if cmp r c
                              then go (c:decl_comments) (anchor l) cs
                              else (reverse (c:cs), decl_comments)
  in
    go [] ss prior_comments

allocatePriorComments
  :: RealSrcSpan
  -> [LEpaComment]
  -> Strict.Maybe [LEpaComment]
  -> (Strict.Maybe [LEpaComment], [LEpaComment], [LEpaComment])
allocatePriorComments ss comment_q mheader_comments =
  let
    cmp (L l _) = anchor l <= ss
    (newAnns,after) = partition cmp comment_q
    comment_q'= after
    (prior_comments, decl_comments) = splitPriorComments ss newAnns
  in
    case mheader_comments of
      Strict.Nothing -> (Strict.Just prior_comments, comment_q', decl_comments)
      Strict.Just _ -> (mheader_comments, comment_q', reverse newAnns)

allocateFinalComments
  :: RealSrcSpan
  -> [LEpaComment]
  -> Strict.Maybe [LEpaComment]
  -> (Strict.Maybe [LEpaComment], [LEpaComment], [LEpaComment])
allocateFinalComments _ss comment_q mheader_comments =
  -- We ignore the RealSrcSpan as the parser currently provides a
  -- point span at (1,1).
  case mheader_comments of
    Strict.Nothing -> (Strict.Just (reverse comment_q), [], [])
    Strict.Just _ -> (mheader_comments, [], reverse comment_q)

commentToAnnotation :: RealLocated Token -> LEpaComment
commentToAnnotation (L l (ITdocComment s ll))   = mkLEpaComment l ll (EpaDocComment s)
commentToAnnotation (L l (ITdocOptions s ll))   = mkLEpaComment l ll (EpaDocOptions s)
commentToAnnotation (L l (ITlineComment s ll))  = mkLEpaComment l ll (EpaLineComment s)
commentToAnnotation (L l (ITblockComment s ll)) = mkLEpaComment l ll (EpaBlockComment s)
commentToAnnotation _                           = panic "commentToAnnotation"

-- see Note [PsSpan in Comments]
mkLEpaComment :: RealSrcSpan -> PsSpan -> EpaCommentTok -> LEpaComment
mkLEpaComment l ll tok = L (realSpanAsAnchor l) (EpaComment tok (psRealSpan ll))

-- ---------------------------------------------------------------------

isComment :: Token -> Bool
isComment (ITlineComment  _ _) = True
isComment (ITblockComment _ _) = True
isComment (ITdocComment   _ _) = True
isComment (ITdocOptions   _ _) = True
isComment _                    = False
}
