-----------------------------------------------------------------------------
-- (c) The University of Glasgow, 2006
--
-- GHC's lexer.
--
-- This is a combination of an Alex-generated lexer from a regex
-- definition, with some hand-coded bits.
--
-- Completely accurate information about token-spans within the source
-- file is maintained.  Every token has a start and end RealSrcLoc
-- attached to it.
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

{
-- XXX The above flags turn off warnings in the generated code:
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- But alex still generates some code that causes the "lazy unlifted bindings"
-- warning, and old compilers don't know about it so we can't easily turn
-- it off, so for now we use the sledge hammer:
{-# OPTIONS_GHC -w #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Lexer (
   Token(..), lexer, pragState, mkPState, PState(..),
   P(..), ParseResult(..), getSrcLoc,
   getPState, getDynFlags, withThisPackage,
   failLocMsgP, failSpanMsgP, srcParseFail,
   getMessages,
   popContext, pushCurrentContext, setLastToken, setSrcLoc,
   activeContext, nextIsEOF,
   getLexState, popLexState, pushLexState,
   extension, bangPatEnabled, datatypeContextsEnabled,
   traditionalRecordSyntaxEnabled,
   typeLiteralsEnabled,
   explicitNamespacesEnabled, sccProfilingOn, hpcEnabled,
   addWarning,
   lexTokenStream
  ) where

import Bag
import ErrUtils
import Outputable
import StringBuffer
import FastString
import SrcLoc
import UniqFM
import DynFlags
import Module
import Ctype
import BasicTypes       ( InlineSpec(..), RuleMatchInfo(..), FractionalLit(..) )
import Util             ( readRational )

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import Data.Word
}

$unispace    = \x05 -- Trick Alex into handling Unicode. See alexGetChar.
$whitechar   = [\ \n\r\f\v $unispace]
$white_no_nl = $whitechar # \n
$tab         = \t

$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See alexGetChar.
$decdigit  = $ascdigit -- for now, should really be $digit (ToDo)
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See alexGetChar.
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$unilarge  = \x01 -- Trick Alex into handling Unicode. See alexGetChar.
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]

$unismall  = \x02 -- Trick Alex into handling Unicode. See alexGetChar.
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]

$unigraphic = \x06 -- Trick Alex into handling Unicode. See alexGetChar.
$graphic   = [$small $large $symbol $digit $special $unigraphic \:\"\']

$octit     = 0-7
$hexit     = [$decdigit A-F a-f]
$symchar   = [$symbol \:]
$nl        = [\n\r]
$idchar    = [$small $large $digit \']

$pragmachar = [$small $large $digit]

$docsym    = [\| \^ \* \$]

@varid     = $small $idchar*
@conid     = $large $idchar*

@varsym    = $symbol $symchar*
@consym    = \: $symchar*

@decimal     = $decdigit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

-- we support the hierarchical module name extension:
@qual = (@conid \.)+

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

-- normal signed numerical literals can only be explicitly negative,
-- not explicitly positive (contrast @exponent)
@negative = \-
@signed = @negative ?

haskell :-

-- everywhere: skip whitespace and comments
$white_no_nl+ ;
$tab+         { warn Opt_WarnTabs (text "Tab character") }

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

"-- " ~[$docsym \#] .* { lineCommentToken }
"--" [^$symbol : \ ] .* { lineCommentToken }

-- Next, match Haddock comments if no -haddock flag

"-- " [$docsym \#] .* / { ifExtension (not . haddockEnabled) } { lineCommentToken }

-- Now, when we've matched comments that begin with 2 dashes and continue
-- with a different character, we need to match comments that begin with three
-- or more dashes (which clearly can't be Haddock comments). We only need to
-- make sure that the first non-dash character isn't a symbol, and munch the
-- rest of the line.

"---"\-* [^$symbol :] .* { lineCommentToken }

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
  ^\# (line)?                           { begin line_prag1 }
  ^\# pragma .* \n                      ; -- GCC 3.3 CPP generated, apparently
  ^\# \! .* \n                          ; -- #!, for scripts
  ()                                    { do_bol }
}

-- after a layout keyword (let, where, do, of), we begin a new layout
-- context if the curly brace is missing.
-- Careful! This stuff is quite delicate.
<layout, layout_do> {
  \{ / { notFollowedBy '-' }            { hopefully_open_brace }
        -- we might encounter {-# here, but {- has been handled already
  \n                                    ;
  ^\# (line)?                           { begin line_prag1 }
}

-- do is treated in a subtly different way, see new_layout_context
<layout>    ()                          { new_layout_context True }
<layout_do> ()                          { new_layout_context False }

-- after a new layout context which was found to be to the left of the
-- previous context, we have generated a '{' token, and we now need to
-- generate a matching '}' token.
<layout_left>  ()                       { do_layout_left }

<0,option_prags> \n                     { begin bol }

"{-#" $whitechar* $pragmachar+ / { known_pragma linePrags }
                                { dispatch_pragmas linePrags }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag1> $decdigit+                 { setLine line_prag1a }
<line_prag1a> \" [$graphic \ ]* \"      { setFile line_prag1b }
<line_prag1b> .*                        { pop }

-- Haskell-style line pragmas, of the form
--    {-# LINE <line> "<file>" #-}
<line_prag2> $decdigit+                 { setLine line_prag2a }
<line_prag2a> \" [$graphic \ ]* \"      { setFile line_prag2b }
<line_prag2b> "#-}"|"-}"                { pop }
   -- NOTE: accept -} at the end of a LINE pragma, for compatibility
   -- with older versions of GHC which generated these.

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

  "-- #"                           { multiline_doc_comment }
}

<0> {
  -- In the "0" mode we ignore these pragmas
  "{-#"  $whitechar* $pragmachar+ / { known_pragma fileHeaderPrags }
                     { nested_comment lexToken }
}

<0> {
  "-- #" .* { lineCommentToken }
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
  "[:" / { ifExtension parrEnabled }    { token ITopabrack }
  ":]" / { ifExtension parrEnabled }    { token ITcpabrack }
}

<0> {
  "[|"      / { ifExtension thEnabled } { token ITopenExpQuote }
  "[e|"     / { ifExtension thEnabled } { token ITopenExpQuote }
  "[p|"     / { ifExtension thEnabled } { token ITopenPatQuote }
  "[d|"     / { ifExtension thEnabled } { layout_token ITopenDecQuote }
  "[t|"     / { ifExtension thEnabled } { token ITopenTypQuote }
  "|]"      / { ifExtension thEnabled } { token ITcloseQuote }
  \$ @varid / { ifExtension thEnabled } { skip_one_varid ITidEscape }
  "$("      / { ifExtension thEnabled } { token ITparenEscape }

-- For backward compatibility, accept the old dollar syntax
  "[$" @varid "|"  / { ifExtension qqEnabled }
                     { lex_quasiquote_tok }

  "[" @varid "|"  / { ifExtension qqEnabled }
                     { lex_quasiquote_tok }

  -- qualified quasi-quote (#5555)
  "[" @qual @varid "|"  / { ifExtension qqEnabled }
                          { lex_qquasiquote_tok }
}

<0> {
  "(|" / { ifExtension arrowsEnabled `alexAndPred` notFollowedBySymbol }
                                        { special IToparenbar }
  "|)" / { ifExtension arrowsEnabled }  { special ITcparenbar }
}

<0> {
  \? @varid / { ifExtension ipEnabled } { skip_one_varid ITdupipvarid }
}

<0> {
  "(#" / { ifExtension unboxedTuplesEnabled }
         { token IToubxparen }
  "#)" / { ifExtension unboxedTuplesEnabled }
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
  @qual @varid                  { idtoken qvarid }
  @qual @conid                  { idtoken qconid }
  @varid                        { varid }
  @conid                        { conid }
}

<0> {
  @qual @varid "#"+ / { ifExtension magicHashEnabled } { idtoken qvarid }
  @qual @conid "#"+ / { ifExtension magicHashEnabled } { idtoken qconid }
  @varid "#"+       / { ifExtension magicHashEnabled } { varid }
  @conid "#"+       / { ifExtension magicHashEnabled } { conid }
}

-- ToDo: - move `var` and (sym) into lexical syntax?
--       - remove backquote from $special?
<0> {
  @qual @varsym                                    { idtoken qvarsym }
  @qual @consym                                    { idtoken qconsym }
  @varsym                                          { varsym }
  @consym                                          { consym }
}

-- For the normal boxed literals we need to be careful
-- when trying to be close to Haskell98
<0> {
  -- Normal integral literals (:: Num a => a, from Integer)
  @decimal                                                               { tok_num positive 0 0 decimal }
  0[oO] @octal                                                           { tok_num positive 2 2 octal }
  0[xX] @hexadecimal                                                     { tok_num positive 2 2 hexadecimal }
  @negative @decimal           / { ifExtension negativeLiteralsEnabled } { tok_num negative 1 1 decimal }
  @negative 0[oO] @octal       / { ifExtension negativeLiteralsEnabled } { tok_num negative 3 3 octal }
  @negative 0[xX] @hexadecimal / { ifExtension negativeLiteralsEnabled } { tok_num negative 3 3 hexadecimal }

  -- Normal rational literals (:: Fractional a => a, from Rational)
  @floating_point                                                        { strtoken tok_float }
  @negative @floating_point    / { ifExtension negativeLiteralsEnabled } { strtoken tok_float }
}

<0> {
  -- Unboxed ints (:: Int#) and words (:: Word#)
  -- It's simpler (and faster?) to give separate cases to the negatives,
  -- especially considering octal/hexadecimal prefixes.
  @decimal                     \# / { ifExtension magicHashEnabled } { tok_primint positive 0 1 decimal }
  0[oO] @octal                 \# / { ifExtension magicHashEnabled } { tok_primint positive 2 3 octal }
  0[xX] @hexadecimal           \# / { ifExtension magicHashEnabled } { tok_primint positive 2 3 hexadecimal }
  @negative @decimal           \# / { ifExtension magicHashEnabled } { tok_primint negative 1 2 decimal }
  @negative 0[oO] @octal       \# / { ifExtension magicHashEnabled } { tok_primint negative 3 4 octal }
  @negative 0[xX] @hexadecimal \# / { ifExtension magicHashEnabled } { tok_primint negative 3 4 hexadecimal }

  @decimal                     \# \# / { ifExtension magicHashEnabled } { tok_primword 0 2 decimal }
  0[oO] @octal                 \# \# / { ifExtension magicHashEnabled } { tok_primword 2 4 octal }
  0[xX] @hexadecimal           \# \# / { ifExtension magicHashEnabled } { tok_primword 2 4 hexadecimal }

  -- Unboxed floats and doubles (:: Float#, :: Double#)
  -- prim_{float,double} work with signed literals
  @signed @floating_point \# / { ifExtension magicHashEnabled } { init_strtoken 1 tok_primfloat }
  @signed @floating_point \# \# / { ifExtension magicHashEnabled } { init_strtoken 2 tok_primdouble }
}

-- Strings and chars are lexed by hand-written code.  The reason is
-- that even if we recognise the string or char here in the regex
-- lexer, we would still have to parse the string afterward in order
-- to convert it to a String.
<0> {
  \'                            { lex_char_tok }
  \"                            { lex_string_tok }
}

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
  | ITscc                       -- ToDo: remove (we use {-# SCC "..." #-} now)

  | ITforall                    -- GHC extension keywords
  | ITforeign
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
  | ITmdo
  | ITfamily
  | ITgroup
  | ITby
  | ITusing
  | ITnominal
  | ITrepresentational
  | ITphantom

  -- Pragmas
  | ITinline_prag InlineSpec RuleMatchInfo
  | ITspec_prag                 -- SPECIALISE
  | ITspec_inline_prag Bool     -- SPECIALISE INLINE (or NOINLINE)
  | ITsource_prag
  | ITrules_prag
  | ITwarning_prag
  | ITdeprecated_prag
  | ITline_prag
  | ITscc_prag
  | ITgenerated_prag
  | ITcore_prag                 -- hdaume: core annotations
  | ITunpack_prag
  | ITnounpack_prag
  | ITann_prag
  | ITclose_prag
  | IToptions_prag String
  | ITinclude_prag String
  | ITlanguage_prag
  | ITvect_prag
  | ITvect_scalar_prag
  | ITnovect_prag
  | ITctype

  | ITdotdot                    -- reserved symbols
  | ITcolon
  | ITdcolon
  | ITequal
  | ITlam
  | ITlcase
  | ITvbar
  | ITlarrow
  | ITrarrow
  | ITat
  | ITtilde
  | ITtildehsh
  | ITdarrow
  | ITminus
  | ITbang
  | ITstar
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
  | ITprefixqvarsym (FastString,FastString)
  | ITprefixqconsym (FastString,FastString)

  | ITdupipvarid   FastString   -- GHC extension: implicit param: ?x

  | ITchar       Char
  | ITstring     FastString
  | ITinteger    Integer
  | ITrational   FractionalLit

  | ITprimchar   Char
  | ITprimstring ByteString
  | ITprimint    Integer
  | ITprimword   Integer
  | ITprimfloat  FractionalLit
  | ITprimdouble FractionalLit

  -- Template Haskell extension tokens
  | ITopenExpQuote              --  [| or [e|
  | ITopenPatQuote              --  [p|
  | ITopenDecQuote              --  [d|
  | ITopenTypQuote              --  [t|
  | ITcloseQuote                --  |]
  | ITidEscape   FastString     --  $x
  | ITparenEscape               --  $(
  | ITtyQuote                   --  ''
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
  | IToparenbar                 --  (|
  | ITcparenbar                 --  |)
  | ITlarrowtail                --  -<
  | ITrarrowtail                --  >-
  | ITLarrowtail                --  -<<
  | ITRarrowtail                --  >>-

  | ITunknown String            -- Used when the lexer can't make sense of it
  | ITeof                       -- end of file token

  -- Documentation annotations
  | ITdocCommentNext  String     -- something beginning '-- |'
  | ITdocCommentPrev  String     -- something beginning '-- ^'
  | ITdocCommentNamed String     -- something beginning '-- $'
  | ITdocSection      Int String -- a section heading
  | ITdocOptions      String     -- doc options (prune, ignore-exports, etc)
  | ITdocOptionsOld   String     -- doc options declared "-- # ..."-style
  | ITlineComment     String     -- comment starting by "--"
  | ITblockComment    String     -- comment in {- -}

  deriving Show

-- the bitmap provided as the third component indicates whether the
-- corresponding extension keyword is valid under the extension options
-- provided to the compiler; if the extension corresponding to *any* of the
-- bits set in the bitmap is enabled, the keyword is valid (this setup
-- facilitates using a keyword in two different extensions that can be
-- activated independently)
--
reservedWordsFM :: UniqFM (Token, Int)
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
         ( "_scc_",          ITscc,           0 ),            -- ToDo: remove

         ( "forall",         ITforall,        bit explicitForallBit .|.
                                              bit inRulePragBit),
         ( "mdo",            ITmdo,           bit recursiveDoBit),
         ( "family",         ITfamily,        bit tyFamBit),
         ( "group",          ITgroup,         bit transformComprehensionsBit),
         ( "by",             ITby,            bit transformComprehensionsBit),
         ( "using",          ITusing,         bit transformComprehensionsBit),

         ( "foreign",        ITforeign,       bit ffiBit),
         ( "export",         ITexport,        bit ffiBit),
         ( "label",          ITlabel,         bit ffiBit),
         ( "dynamic",        ITdynamic,       bit ffiBit),
         ( "safe",           ITsafe,          bit ffiBit .|.
                                              bit safeHaskellBit),
         ( "interruptible",  ITinterruptible, bit interruptibleFfiBit),
         ( "unsafe",         ITunsafe,        bit ffiBit),
         ( "stdcall",        ITstdcallconv,   bit ffiBit),
         ( "ccall",          ITccallconv,     bit ffiBit),
         ( "capi",           ITcapiconv,      bit cApiFfiBit),
         ( "prim",           ITprimcallconv,  bit ffiBit),

         ( "rec",            ITrec,           bit arrowsBit .|. 
                                              bit recursiveDoBit),
         ( "proc",           ITproc,          bit arrowsBit)
     ]

reservedUpcaseWordsFM :: UniqFM (Token, Int)
reservedUpcaseWordsFM = listToUFM $
    map (\(x, y, z) -> (mkFastString x, (y, z)))
       [ ( "N",     ITnominal,          0 ), -- no extension bit for better error msgs
         ( "R",     ITrepresentational, 0 ),
         ( "P",     ITphantom,          0 )
       ]

reservedSymsFM :: UniqFM (Token, Int -> Bool)
reservedSymsFM = listToUFM $
    map (\ (x,y,z) -> (mkFastString x,(y,z)))
      [ ("..",  ITdotdot,   always)
        -- (:) is a reserved op, meaning only list cons
       ,(":",   ITcolon,    always)
       ,("::",  ITdcolon,   always)
       ,("=",   ITequal,    always)
       ,("\\",  ITlam,      always)
       ,("|",   ITvbar,     always)
       ,("<-",  ITlarrow,   always)
       ,("->",  ITrarrow,   always)
       ,("@",   ITat,       always)
       ,("~",   ITtilde,    always)
       ,("~#",  ITtildehsh, magicHashEnabled)
       ,("=>",  ITdarrow,   always)
       ,("-",   ITminus,    always)
       ,("!",   ITbang,     always)

        -- For data T (a::*) = MkT
       ,("*", ITstar, always) -- \i -> kindSigsEnabled i || tyFamEnabled i)
        -- For 'forall a . t'
       ,(".", ITdot,  always) -- \i -> explicitForallEnabled i || inRulePrag i)

       ,("-<",  ITlarrowtail, arrowsEnabled)
       ,(">-",  ITrarrowtail, arrowsEnabled)
       ,("-<<", ITLarrowtail, arrowsEnabled)
       ,(">>-", ITRarrowtail, arrowsEnabled)

       ,("∷",   ITdcolon, unicodeSyntaxEnabled)
       ,("⇒",   ITdarrow, unicodeSyntaxEnabled)
       ,("∀",   ITforall, \i -> unicodeSyntaxEnabled i &&
                                explicitForallEnabled i)
       ,("→",   ITrarrow, unicodeSyntaxEnabled)
       ,("←",   ITlarrow, unicodeSyntaxEnabled)

       ,("⤙",   ITlarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤚",   ITrarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤛",   ITLarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤜",   ITRarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)

       ,("★", ITstar, unicodeSyntaxEnabled)

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

strtoken :: (String -> Token) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

init_strtoken :: Int -> (String -> Token) -> Action
-- like strtoken, but drops the last N character(s)
init_strtoken drop f span buf len =
  return (L span $! (f $! lexemeToString buf (len-drop)))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

pop :: Action
pop _span _buf _len = do _ <- popLexState
                         lexToken

hopefully_open_brace :: Action
hopefully_open_brace span buf len
 = do relaxed <- extension relaxedLayout
      ctx <- getContext
      (AI l _) <- getInput
      let offset = srcLocCol l
          isOK = relaxed ||
                 case ctx of
                 Layout prev_off : _ -> prev_off < offset
                 _                   -> True
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

notFollowedBy :: Char -> AlexAccPred Int
notFollowedBy char _ _ _ (AI _ buf)
  = nextCharIsNot buf (== char)

notFollowedBySymbol :: AlexAccPred Int
notFollowedBySymbol _ _ _ (AI _ buf)
  = nextCharIsNot buf (`elem` "!#$%&*+./<=>?@\\^|-~")

-- We must reject doc comments as being ordinary comments everywhere.
-- In some cases the doc comment will be selected as the lexeme due to
-- maximal munch, but not always, because the nested comment rule is
-- valid in all states, but the doc-comment rules are only valid in
-- the non-layout states.
isNormalComment :: AlexAccPred Int
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

atEOL :: AlexAccPred Int
atEOL _ _ _ (AI _ buf) = atEnd buf || currentChar buf == '\n'

ifExtension :: (Int -> Bool) -> AlexAccPred Int
ifExtension pred bits _ _ _ = pred bits

multiline_doc_comment :: Action
multiline_doc_comment span buf _len = withLexedDocType (worker "")
  where
    worker commentAcc input docType oneLine = case alexGetChar' input of
      Just ('\n', input')
        | oneLine -> docCommentEnd input commentAcc docType buf span
        | otherwise -> case checkIfCommentLine input' of
          Just input -> worker ('\n':commentAcc) input docType False
          Nothing -> docCommentEnd input commentAcc docType buf span
      Just (c, input) -> worker (c:commentAcc) input docType oneLine
      Nothing -> docCommentEnd input commentAcc docType buf span

    checkIfCommentLine input = check (dropNonNewlineSpace input)
      where
        check input = case alexGetChar' input of
          Just ('-', input) -> case alexGetChar' input of
            Just ('-', input) -> case alexGetChar' input of
              Just (c, _) | c /= '-' -> Just input
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing

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
nested_comment cont span _str _len = do
  input <- getInput
  go "" (1::Int) input
  where
    go commentAcc 0 input = do setInput input
                               b <- extension rawTokenStreamEnabled
                               if b
                                 then docCommentEnd input commentAcc ITblockComment _str span
                                 else cont
    go commentAcc n input = case alexGetChar' input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('\125',input) -> go commentAcc (n-1) input
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
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
      Just (c,input) -> go (c:commentAcc) input docType False

withLexedDocType :: (AlexInput -> (String -> Token) -> Bool -> P (RealLocated Token))
                 -> P (RealLocated Token)
withLexedDocType lexDocComment = do
  input@(AI _ buf) <- getInput
  case prevChar buf ' ' of
    '|' -> lexDocComment input ITdocCommentNext False
    '^' -> lexDocComment input ITdocCommentPrev False
    '$' -> lexDocComment input ITdocCommentNamed False
    '*' -> lexDocSection 1 input
    '#' -> lexDocComment input ITdocOptionsOld False
    _ -> panic "withLexedDocType: Bad doc type"
 where
    lexDocSection n input = case alexGetChar' input of
      Just ('*', input) -> lexDocSection (n+1) input
      Just (_,   _)     -> lexDocComment input (ITdocSection n) True
      Nothing -> do setInput input; lexToken -- eof reached, lex it normally

-- RULES pragmas turn on the forall and '.' keywords, and we turn them
-- off again at the end of the pragma.
rulePrag :: Action
rulePrag span _buf _len = do
  setExts (.|. bit inRulePragBit)
  return (L span ITrules_prag)

endPrag :: Action
endPrag span _buf _len = do
  setExts (.&. complement (bit inRulePragBit))
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
      lambdaCase <- extension lambdaCaseEnabled
      keyword <- if lambdaCase
                 then do
                   lastTk <- getLastTk
                   return $ case lastTk of
                     Just ITlam -> ITlcase
                     _          -> ITcase
                 else
                   return ITcase
      maybe_layout keyword
      return $ L span keyword
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

conid :: Action
conid span buf len =
  case lookupUFM reservedUpcaseWordsFM fs of
    Just (keyword, 0) -> return $ L span keyword

    Just (keyword, exts) -> do
      extsEnabled <- extension $ \i -> exts .&. i /= 0
      if extsEnabled
        then return $ L span keyword
        else return $ L span $ ITconid fs

    Nothing -> return $ L span $ ITconid fs
  where
    !fs = lexemeToFastString buf len

qvarsym, qconsym, prefixqvarsym, prefixqconsym :: StringBuffer -> Int -> Token
qvarsym buf len = ITqvarsym $! splitQualName buf len False
qconsym buf len = ITqconsym $! splitQualName buf len False
prefixqvarsym buf len = ITprefixqvarsym $! splitQualName buf len True
prefixqconsym buf len = ITprefixqconsym $! splitQualName buf len True

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
tok_integral :: (Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len
 = return $ L span $ itint $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

-- some conveniences for use with tok_integral
tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral ITinteger
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
octal = (8,octDecDigit)
hexadecimal = (16,hexDigit)

-- readRational can understand negative rationals, exponents, everything.
tok_float, tok_primfloat, tok_primdouble :: String -> Token
tok_float        str = ITrational   $! readFractionalLit str
tok_primfloat    str = ITprimfloat  $! readFractionalLit str
tok_primdouble   str = ITprimdouble $! readFractionalLit str

readFractionalLit :: String -> FractionalLit
readFractionalLit str = (FL $! str) $! readRational str

-- -----------------------------------------------------------------------------
-- Layout processing

-- we're at the first token on a line, insert layout tokens if necessary
do_bol :: Action
do_bol span _str _len = do
        pos <- getOffside
        case pos of
            LT -> do
                --trace "layout: inserting '}'" $ do
                popContext
                -- do NOT pop the lex state, we might have a ';' to insert
                return (L span ITvccurly)
            EQ -> do
                --trace "layout: inserting ';'" $ do
                _ <- popLexState
                return (L span ITsemi)
            GT -> do
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
          f _       = return ()

-- Pushing a new implicit layout context.  If the indentation of the
-- next token is not greater than the previous layout context, then
-- Haskell 98 says that the new layout context should be empty; that is
-- the lexer must generate {}.
--
-- We are slightly more lenient than this: when the new context is started
-- by a 'do', then we allow the new context to be at the same indentation as
-- the previous context.  This is what the 'strict' argument is for.
--
new_layout_context :: Bool -> Action
new_layout_context strict span _buf _len = do
    _ <- popLexState
    (AI l _) <- getInput
    let offset = srcLocCol l
    ctx <- getContext
    nondecreasing <- extension nondecreasingIndentation
    let strict' = strict || not nondecreasing
    case ctx of
        Layout prev_off : _  |
           (strict'     && prev_off >= offset  ||
            not strict' && prev_off > offset) -> do
                -- token is indented to the left of the previous context.
                -- we must generate a {} sequence now.
                pushLexState layout_left
                return (L span ITvocurly)
        _ -> do
                setContext (Layout offset : ctx)
                return (L span ITvocurly)

do_layout_left :: Action
do_layout_left span _buf _len = do
    _ <- popLexState
    pushLexState bol  -- we must be at the start of a line
    return (L span ITvccurly)

-- -----------------------------------------------------------------------------
-- LINE pragmas

setLine :: Int -> Action
setLine code span buf len = do
  let line = parseUnsignedInteger buf len 10 octDecDigit
  setSrcLoc (mkRealSrcLoc (srcSpanFile span) (fromIntegral line - 1) 1)
        -- subtract one: the line number refers to the *following* line
  _ <- popLexState
  pushLexState code
  lexToken

setFile :: Int -> Action
setFile code span buf len = do
  let file = mkFastString (go (lexemeToString (stepOn buf) (len-2)))
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
  setSrcLoc (mkRealSrcLoc file (srcSpanEndLine span) (srcSpanEndCol span))
  addSrcFile file
  _ <- popLexState
  pushLexState code
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
         start <- getSrcLoc
         tok <- go [] input
         end <- getSrcLoc
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
lex_string_tok span _buf _len = do
  tok <- lex_string ""
  end <- getSrcLoc
  return (L (mkRealSrcSpan (realSrcSpanStart span) end) tok)

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
                         in return (ITprimstring bs)
              _other ->
                return (ITstring (mkFastString (reverse s)))
          else
                return (ITstring (mkFastString (reverse s)))

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
-- but, when Template Haskell is on, we additionally spot
-- 'x and ''T, returning ITsimpleQuote and ITtyQuote respectively,
-- but WITHOUT CONSUMING the x or T part  (the parser does that).
-- So we have to do two characters of lookahead: when we see 'x we need to
-- see if there's a trailing quote
lex_char_tok span _buf _len = do        -- We've seen '
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
                  if mc == '\'' then finish_char_tok loc lit_ch
                                else lit_error i3

        Just (c, i2@(AI _end2 _))
                | not (isAny c) -> lit_error i1
                | otherwise ->

                -- We've seen 'x, where x is a valid character
                --  (i.e. not newline etc) but not a quote or backslash
           case alexGetChar' i2 of      -- Look ahead one more character
                Just ('\'', i3) -> do   -- We've seen 'x'
                        setInput i3
                        finish_char_tok loc c
                _other -> do            -- We've seen 'x not followed by quote
                                        -- (including the possibility of EOF)
                                        -- If TH is on, just parse the quote only
                        let (AI end _) = i1
                        return (L (mkRealSrcSpan loc end) ITsimpleQuote)

finish_char_tok :: RealSrcLoc -> Char -> P (RealLocated Token)
finish_char_tok loc ch  -- We've already seen the closing quote
                        -- Just need to check for trailing #
  = do  magicHash <- extension magicHashEnabled
        i@(AI end _) <- getInput
        if magicHash then do
                case alexGetChar' i of
                        Just ('#',i@(AI end _)) -> do
                                setInput i
                                return (L (mkRealSrcSpan loc end) (ITprimchar ch))
                        _other ->
                                return (L (mkRealSrcSpan loc end) (ITchar ch))
            else do
                   return (L (mkRealSrcSpan loc end) (ITchar ch))

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
  quoteStart <- getSrcLoc
  quote <- lex_quasiquote quoteStart ""
  end <- getSrcLoc
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
  quoteStart <- getSrcLoc
  quote <- lex_quasiquote quoteStart ""
  end <- getSrcLoc
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

warn :: WarningFlag -> SDoc -> Action
warn option warning srcspan _buf _len = do
    addWarning option (RealSrcSpan srcspan) warning
    lexToken

warnThen :: WarningFlag -> SDoc -> Action -> Action
warnThen option warning action srcspan buf len = do
    addWarning option (RealSrcSpan srcspan) warning
    action srcspan buf len

-- -----------------------------------------------------------------------------
-- The Parse Monad

data LayoutContext
  = NoLayout
  | Layout !Int
  deriving Show

data ParseResult a
  = POk PState a
  | PFailed
        SrcSpan         -- The start and end of the text span related to
                        -- the error.  Might be used in environments which can
                        -- show this span, e.g. by highlighting it.
        MsgDoc          -- The error message

data PState = PState {
        buffer     :: StringBuffer,
        dflags     :: DynFlags,
        messages   :: Messages,
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        extsBitmap :: !Int,        -- bitmap that determines permitted
                                   -- extensions
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
        -- This is what we consider to be the locatino of the last token
        -- emitted:
        alr_last_loc :: RealSrcSpan,
        -- The stack of layout contexts:
        alr_context :: [ALRContext],
        -- Are we expecting a '{'? If it's Just, then the ALRLayout tells
        -- us what sort of layout the '{' will open:
        alr_expecting_ocurly :: Maybe ALRLayout,
        -- Have we just had the '}' for a let block? If so, than an 'in'
        -- token doesn't need to close anything:
        alr_justClosedExplicitLetBlock :: Bool
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

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail = failP

returnP :: a -> P a
returnP a = a `seq` (P $ \s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed span err -> PFailed span err

failP :: String -> P a
failP msg = P $ \s -> PFailed (RealSrcSpan (last_loc s)) (text msg)

failMsgP :: String -> P a
failMsgP msg = P $ \s -> PFailed (RealSrcSpan (last_loc s)) (text msg)

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \_ -> PFailed (RealSrcSpan (mkRealSrcSpan loc1 loc2)) (text str)

failSpanMsgP :: SrcSpan -> SDoc -> P a
failSpanMsgP span msg = P $ \_ -> PFailed span msg

getPState :: P PState
getPState = P $ \s -> POk s s

instance HasDynFlags P where
    getDynFlags = P $ \s -> POk s (dflags s)

withThisPackage :: (PackageId -> a) -> P a
withThisPackage f
 = do pkg <- liftM thisPackage getDynFlags
      return $ f pkg

extension :: (Int -> Bool) -> P Bool
extension p = P $ \s -> POk s (p $! extsBitmap s)

getExts :: P Int
getExts = P $ \s -> POk s (extsBitmap s)

setExts :: (Int -> Int) -> P ()
setExts f = P $ \s -> POk s{ extsBitmap = f (extsBitmap s) } ()

setSrcLoc :: RealSrcLoc -> P ()
setSrcLoc new_loc = P $ \s -> POk s{loc=new_loc} ()

getSrcLoc :: P RealSrcLoc
getSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

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

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = prevChar buf '\n'

-- backwards compatibility for Alex 2.x
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar inp = case alexGetByte inp of
                    Nothing    -> Nothing
                    Just (b,i) -> c `seq` Just (c,i)
                       where c = chr $ fromIntegral b

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = fromIntegral $ ord adj_c

        non_graphic     = '\x0'
        upper           = '\x1'
        lower           = '\x2'
        digit           = '\x3'
        symbol          = '\x4'
        space           = '\x5'
        other_graphic   = '\x6'

        adj_c
          | c <= '\x06' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> other_graphic
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> other_graphic
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
-- stored in an unboxed Int

ffiBit :: Int
ffiBit= 0
interruptibleFfiBit :: Int
interruptibleFfiBit = 1
cApiFfiBit :: Int
cApiFfiBit = 2
parrBit :: Int
parrBit = 3
arrowsBit :: Int
arrowsBit  = 4
thBit :: Int
thBit = 5
ipBit :: Int
ipBit = 6
explicitForallBit :: Int
explicitForallBit = 7 -- the 'forall' keyword and '.' symbol
bangPatBit :: Int
bangPatBit = 8  -- Tells the parser to understand bang-patterns
                -- (doesn't affect the lexer)
tyFamBit :: Int
tyFamBit = 9    -- indexed type families: 'family' keyword and kind sigs
haddockBit :: Int
haddockBit = 10 -- Lex and parse Haddock comments
magicHashBit :: Int
magicHashBit = 11 -- "#" in both functions and operators
kindSigsBit :: Int
kindSigsBit = 12 -- Kind signatures on type variables
recursiveDoBit :: Int
recursiveDoBit = 13 -- mdo
unicodeSyntaxBit :: Int
unicodeSyntaxBit = 14 -- the forall symbol, arrow symbols, etc
unboxedTuplesBit :: Int
unboxedTuplesBit = 15 -- (# and #)
datatypeContextsBit :: Int
datatypeContextsBit = 16
transformComprehensionsBit :: Int
transformComprehensionsBit = 17
qqBit :: Int
qqBit = 18 -- enable quasiquoting
inRulePragBit :: Int
inRulePragBit = 19
rawTokenStreamBit :: Int
rawTokenStreamBit = 20 -- producing a token stream with all comments included
sccProfilingOnBit :: Int
sccProfilingOnBit = 21
hpcBit :: Int
hpcBit = 22
alternativeLayoutRuleBit :: Int
alternativeLayoutRuleBit = 23
relaxedLayoutBit :: Int
relaxedLayoutBit = 24
nondecreasingIndentationBit :: Int
nondecreasingIndentationBit = 25
safeHaskellBit :: Int
safeHaskellBit = 26
traditionalRecordSyntaxBit :: Int
traditionalRecordSyntaxBit = 27
typeLiteralsBit :: Int
typeLiteralsBit = 28
explicitNamespacesBit :: Int
explicitNamespacesBit = 29
lambdaCaseBit :: Int
lambdaCaseBit = 30
negativeLiteralsBit :: Int
negativeLiteralsBit = 31


always :: Int -> Bool
always           _     = True
parrEnabled :: Int -> Bool
parrEnabled      flags = testBit flags parrBit
arrowsEnabled :: Int -> Bool
arrowsEnabled    flags = testBit flags arrowsBit
thEnabled :: Int -> Bool
thEnabled        flags = testBit flags thBit
ipEnabled :: Int -> Bool
ipEnabled        flags = testBit flags ipBit
explicitForallEnabled :: Int -> Bool
explicitForallEnabled flags = testBit flags explicitForallBit
bangPatEnabled :: Int -> Bool
bangPatEnabled   flags = testBit flags bangPatBit
-- tyFamEnabled :: Int -> Bool
-- tyFamEnabled     flags = testBit flags tyFamBit
haddockEnabled :: Int -> Bool
haddockEnabled   flags = testBit flags haddockBit
magicHashEnabled :: Int -> Bool
magicHashEnabled flags = testBit flags magicHashBit
-- kindSigsEnabled :: Int -> Bool
-- kindSigsEnabled  flags = testBit flags kindSigsBit
unicodeSyntaxEnabled :: Int -> Bool
unicodeSyntaxEnabled flags = testBit flags unicodeSyntaxBit
unboxedTuplesEnabled :: Int -> Bool
unboxedTuplesEnabled flags = testBit flags unboxedTuplesBit
datatypeContextsEnabled :: Int -> Bool
datatypeContextsEnabled flags = testBit flags datatypeContextsBit
qqEnabled :: Int -> Bool
qqEnabled        flags = testBit flags qqBit
-- inRulePrag :: Int -> Bool
-- inRulePrag       flags = testBit flags inRulePragBit
rawTokenStreamEnabled :: Int -> Bool
rawTokenStreamEnabled flags = testBit flags rawTokenStreamBit
alternativeLayoutRule :: Int -> Bool
alternativeLayoutRule flags = testBit flags alternativeLayoutRuleBit
hpcEnabled :: Int -> Bool
hpcEnabled flags = testBit flags hpcBit
relaxedLayout :: Int -> Bool
relaxedLayout flags = testBit flags relaxedLayoutBit
nondecreasingIndentation :: Int -> Bool
nondecreasingIndentation flags = testBit flags nondecreasingIndentationBit
sccProfilingOn :: Int -> Bool
sccProfilingOn flags = testBit flags sccProfilingOnBit
traditionalRecordSyntaxEnabled :: Int -> Bool
traditionalRecordSyntaxEnabled flags = testBit flags traditionalRecordSyntaxBit
typeLiteralsEnabled :: Int -> Bool
typeLiteralsEnabled flags = testBit flags typeLiteralsBit

explicitNamespacesEnabled :: Int -> Bool
explicitNamespacesEnabled flags = testBit flags explicitNamespacesBit
lambdaCaseEnabled :: Int -> Bool
lambdaCaseEnabled flags = testBit flags lambdaCaseBit
negativeLiteralsEnabled :: Int -> Bool
negativeLiteralsEnabled flags = testBit flags negativeLiteralsBit

-- PState for parsing options pragmas
--
pragState :: DynFlags -> StringBuffer -> RealSrcLoc -> PState
pragState dynflags buf loc = (mkPState dynflags buf loc) {
                                 lex_state = [bol, option_prags, 0]
                             }

-- create a parse state
--
mkPState :: DynFlags -> StringBuffer -> RealSrcLoc -> PState
mkPState flags buf loc =
  PState {
      buffer        = buf,
      dflags        = flags,
      messages      = emptyMessages,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc loc,
      last_len      = 0,
      loc           = loc,
      extsBitmap    = fromIntegral bitmap,
      context       = [],
      lex_state     = [bol, 0],
      srcfiles      = [],
      alr_pending_implicit_tokens = [],
      alr_next_token = Nothing,
      alr_last_loc = alrInitialLoc (fsLit "<no file>"),
      alr_context = [],
      alr_expecting_ocurly = Nothing,
      alr_justClosedExplicitLetBlock = False
    }
    where
      bitmap =     ffiBit                      `setBitIf` xopt Opt_ForeignFunctionInterface flags
               .|. interruptibleFfiBit         `setBitIf` xopt Opt_InterruptibleFFI         flags
               .|. cApiFfiBit                  `setBitIf` xopt Opt_CApiFFI                  flags
               .|. parrBit                     `setBitIf` xopt Opt_ParallelArrays           flags
               .|. arrowsBit                   `setBitIf` xopt Opt_Arrows                   flags
               .|. thBit                       `setBitIf` xopt Opt_TemplateHaskell          flags
               .|. qqBit                       `setBitIf` xopt Opt_QuasiQuotes              flags
               .|. ipBit                       `setBitIf` xopt Opt_ImplicitParams           flags
               .|. explicitForallBit           `setBitIf` xopt Opt_ExplicitForAll           flags
               .|. bangPatBit                  `setBitIf` xopt Opt_BangPatterns             flags
               .|. tyFamBit                    `setBitIf` xopt Opt_TypeFamilies             flags
               .|. haddockBit                  `setBitIf` gopt Opt_Haddock                  flags
               .|. magicHashBit                `setBitIf` xopt Opt_MagicHash                flags
               .|. kindSigsBit                 `setBitIf` xopt Opt_KindSignatures           flags
               .|. recursiveDoBit              `setBitIf` xopt Opt_RecursiveDo              flags
               .|. unicodeSyntaxBit            `setBitIf` xopt Opt_UnicodeSyntax            flags
               .|. unboxedTuplesBit            `setBitIf` xopt Opt_UnboxedTuples            flags
               .|. datatypeContextsBit         `setBitIf` xopt Opt_DatatypeContexts         flags
               .|. transformComprehensionsBit  `setBitIf` xopt Opt_TransformListComp        flags
               .|. transformComprehensionsBit  `setBitIf` xopt Opt_MonadComprehensions      flags
               .|. rawTokenStreamBit           `setBitIf` gopt Opt_KeepRawTokenStream       flags
               .|. hpcBit                      `setBitIf` gopt Opt_Hpc                      flags
               .|. alternativeLayoutRuleBit    `setBitIf` xopt Opt_AlternativeLayoutRule    flags
               .|. relaxedLayoutBit            `setBitIf` xopt Opt_RelaxedLayout            flags
               .|. sccProfilingOnBit           `setBitIf` gopt Opt_SccProfilingOn           flags
               .|. nondecreasingIndentationBit `setBitIf` xopt Opt_NondecreasingIndentation flags
               .|. safeHaskellBit              `setBitIf` safeImportsOn                     flags
               .|. traditionalRecordSyntaxBit  `setBitIf` xopt Opt_TraditionalRecordSyntax  flags
               .|. typeLiteralsBit             `setBitIf` xopt Opt_DataKinds flags
               .|. explicitNamespacesBit       `setBitIf` xopt Opt_ExplicitNamespaces flags
               .|. lambdaCaseBit               `setBitIf` xopt Opt_LambdaCase               flags
               .|. negativeLiteralsBit         `setBitIf` xopt Opt_NegativeLiterals         flags
      --
      setBitIf :: Int -> Bool -> Int
      b `setBitIf` cond | cond      = bit b
                        | otherwise = 0

addWarning :: WarningFlag -> SrcSpan -> SDoc -> P ()
addWarning option srcspan warning
 = P $ \s@PState{messages=(ws,es), dflags=d} ->
       let warning' = mkWarnMsg d srcspan alwaysQualify warning
           ws' = if wopt option d then ws `snocBag` warning' else ws
       in POk s{messages=(ws', es)} ()

getMessages :: PState -> Messages
getMessages PState{messages=ms} = ms

getContext :: P [LayoutContext]
getContext = P $ \s@PState{context=ctx} -> POk s ctx

setContext :: [LayoutContext] -> P ()
setContext ctx = P $ \s -> POk s{context=ctx} ()

popContext :: P ()
popContext = P $ \ s@(PState{ buffer = buf, context = ctx,
                              last_len = len, last_loc = last_loc }) ->
  case ctx of
        (_:tl) -> POk s{ context = tl } ()
        []     -> PFailed (RealSrcSpan last_loc) (srcParseErr buf len)

-- Push a new layout context at the indentation of the last token read.
-- This is only used at the outer level of a module when the 'module'
-- keyword is missing.
pushCurrentContext :: P ()
pushCurrentContext = P $ \ s@PState{ last_loc=loc, context=ctx } ->
    POk s{context = Layout (srcSpanStartCol loc) : ctx} ()

getOffside :: P Ordering
getOffside = P $ \s@PState{last_loc=loc, context=stk} ->
                let offs = srcSpanStartCol loc in
                let ord = case stk of
                        (Layout n:_) -> --trace ("layout: " ++ show n ++ ", offs: " ++ show offs) $
                                        compare offs n
                        _            -> GT
                in POk s ord

-- ---------------------------------------------------------------------------
-- Construct a parse error

srcParseErr
  :: StringBuffer       -- current buffer (placed just after the last token)
  -> Int                -- length of the previous token
  -> MsgDoc
srcParseErr buf len
  = hcat [ if null token
             then ptext (sLit "parse error (possibly incorrect indentation or mismatched brackets)")
             else hcat [ptext (sLit "parse error on input "),
                        char '`', text token, char '\'']
    ]
  where token = lexemeToString (offsetBytes (-len) buf) len

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \PState{ buffer = buf, last_len = len,
                            last_loc = last_loc } ->
    PFailed (RealSrcSpan last_loc) (srcParseErr buf len)

-- A lexical error is reported at a particular position in the source file,
-- not over a token range.
lexError :: String -> P a
lexError str = do
  loc <- getSrcLoc
  (AI end buf) <- getInput
  reportLexError loc end buf str

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  alr <- extension alternativeLayoutRule
  let lexTokenFun = if alr then lexTokenAlr else lexToken
  (L span tok) <- lexTokenFun
  --trace ("token: " ++ show tok) $ do
  cont (L (RealSrcSpan span) tok)

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
                 setAlrLastLoc (getLoc t)
                 case unLoc t of
                     ITwhere -> setAlrExpectingOCurly (Just ALRLayoutWhere)
                     ITlet   -> setAlrExpectingOCurly (Just ALRLayoutLet)
                     ITof    -> setAlrExpectingOCurly (Just ALRLayoutOf)
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
         justClosedExplicitLetBlock <- getJustClosedExplicitLetBlock
         setJustClosedExplicitLetBlock False
         dflags <- getDynFlags
         let transitional = xopt Opt_AlternativeLayoutRuleTransitional dflags
             thisLoc = getLoc t
             thisCol = srcSpanStartCol thisLoc
             newLine = srcSpanStartLine thisLoc > srcSpanEndLine lastLoc
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
             (_, ALRLayout _ col : ls, Just expectingOCurly)
              | (thisCol > col) ||
                (thisCol == col &&
                 isNonDecreasingIntentation expectingOCurly) ->
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
                                    else ls
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
isALRopen ITcase        = True
isALRopen ITif          = True
isALRopen ITthen        = True
isALRopen IToparen      = True
isALRopen ITobrack      = True
isALRopen ITocurly      = True
-- GHC Extensions:
isALRopen IToubxparen   = True
isALRopen ITparenEscape = True
isALRopen _             = False

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

isNonDecreasingIntentation :: ALRLayout -> Bool
isNonDecreasingIntentation ALRLayoutDo = True
isNonDecreasingIntentation _           = False

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
        case unLoc lt of
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
          initState = mkPState dflags' buf loc
          go = do
            ltok <- lexer return
            case ltok of
              L _ ITeof -> return []
              _ -> liftM (ltok:) go

linePrags = Map.singleton "line" (begin line_prag2)

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

oneWordPrags = Map.fromList([("rules", rulePrag),
                           ("inline", token (ITinline_prag Inline FunLike)),
                           ("inlinable", token (ITinline_prag Inlinable FunLike)),
                           ("inlineable", token (ITinline_prag Inlinable FunLike)),
                                          -- Spelling variant
                           ("notinline", token (ITinline_prag NoInline FunLike)),
                           ("specialize", token ITspec_prag),
                           ("source", token ITsource_prag),
                           ("warning", token ITwarning_prag),
                           ("deprecated", token ITdeprecated_prag),
                           ("scc", token ITscc_prag),
                           ("generated", token ITgenerated_prag),
                           ("core", token ITcore_prag),
                           ("unpack", token ITunpack_prag),
                           ("nounpack", token ITnounpack_prag),
                           ("ann", token ITann_prag),
                           ("vectorize", token ITvect_prag),
                           ("novectorize", token ITnovect_prag),
                           ("ctype", token ITctype)])

twoWordPrags = Map.fromList([("inline conlike", token (ITinline_prag Inline ConLike)),
                             ("notinline conlike", token (ITinline_prag NoInline ConLike)),
                             ("specialize inline", token (ITspec_inline_prag True)),
                             ("specialize notinline", token (ITspec_inline_prag False)),
                             ("vectorize scalar", token ITvect_scalar_prag)])

dispatch_pragmas :: Map String Action -> Action
dispatch_pragmas prags span buf len = case Map.lookup (clean_pragma (lexemeToString buf len)) prags of
                                       Just found -> found span buf len
                                       Nothing -> lexError "unknown pragma"

known_pragma :: Map String Action -> AlexAccPred Int
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
                                              "vectorise" -> "vectorize"
                                              "novectorise" -> "novectorize"
                                              "constructorlike" -> "conlike"
                                              _ -> prag'
                          canon_ws s = unwords (map canonical (words s))
}
