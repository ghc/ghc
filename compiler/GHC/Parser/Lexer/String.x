{
{- |
This module defines lex states for strings.

This needs to be separate from the normal lexer because the normal lexer
automatically includes rules like skipping whitespace or lexing comments,
which we don't want in these contexts.
-}
module GHC.Parser.Lexer.String (
  AlexReturn (..),
  alexScan,
  Action (..),
  string_multi_content,
  string_inter_content,
) where

import GHC.Prelude

import GHC.Parser.Lexer.Interface
}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"
-- Copied from GHC/Parser/Lexer.x

$unispace    = \x05 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$nl          = [\n\r\f]
$space       = [\ $unispace]
$whitechar   = [$nl \v $space]
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
$charesc   = [a b f n r t v \\ \" \' \&]

$octit     = 0-7
$hexit     = [$decdigit A-F a-f]

-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"
-- Copied from GHC/Parser/Lexer.x

@numspc       = _*                   -- numeric spacer (#14473)
@decimal      = $decdigit(@numspc $decdigit)*
@octal        = $octit(@numspc $octit)*
@hexadecimal  = $hexit(@numspc $hexit)*
@gap = \\ $whitechar+ \\
@cntrl = $asclarge | \@ | \[ | \\ | \] | \^ | \_
@ascii = \^ @cntrl | "NUL" | "SOH" | "STX" | "ETX" | "EOT" | "ENQ" | "ACK"
       | "BEL" | "BS" | "HT" | "LF" | "VT" | "FF" | "CR" | "SO" | "SI" | "DLE"
       | "DC1" | "DC2" | "DC3" | "DC4" | "NAK" | "SYN" | "ETB" | "CAN"
       | "EM" | "SUB" | "ESC" | "FS" | "GS" | "RS" | "US" | "SP" | "DEL"
@escape     = \\ ( $charesc      | @ascii | @decimal | o @octal | x @hexadecimal )
@stringchar = ($graphic # [\\ \"])         | $space | @escape     | @gap
@stringinterchar = ($graphic # [\\ \" \$]) | $space | @escape     | @gap

:-

-- Define an empty rule so it compiles; callers should always explicitly specify a startcode
<0> () ;

-- See Note [Lexing multiline strings]
<string_multi_content> {
  -- Parse as much of the multiline string as possible, except for quotes
  @stringchar* ($nl ([\  $tab] | @gap)* @stringchar*)* { StringMultiContentAction }
  -- Allow bare quotes if it's not a triple quote
  (\" | \"\") / ([\n .] # \") { StringMultiContentAction }
}

-- Note [Lexing interpolated strings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Lexing interpolated strings will do the following steps:
--
-- 1. Lex `s"` => push `string_inter_content` lex state
-- 2. Repeatedly invoke the `string_inter_content` lex state in GHC/Parser/Lexer/String.x
-- 3. When encountering `${`, we do two things:
--     1. Push the `string_inter` lex state
--         * This lex state includes all expression-related lexer rules
--     2. Push the `InterStringLayout` layout context
--         * This is needed to identify the closing brace in the presence of
--           nested braces (e.g. `s"Person: ${show Person{name = "Alice"}}")
-- 4. When encountering `}`, check if `InterStringLayout` is the current layout context.
--    If so, pop `InterStringLayout` and `string_inter` and go back to invoking
--    `string_inter_content`
--
-- We define `string_inter_content` in GHC/Parser/Lexer/String.x to avoid including
-- the global rules when we're parsing the string content.

-- See Note [Parsing interpolated strings]
<string_inter_content> {
  @stringinterchar+ { StringInterContentRaw }
  \$ \{             { StringInterContentExpOpen }
  \"                { StringInterContentEnd }

  -- TODO(bchinn): check for smart quotes
}

-- -----------------------------------------------------------------------------
-- Haskell actions
{
data Action
  = StringMultiContentAction
  | StringInterContentRaw
  | StringInterContentExpOpen
  | StringInterContentEnd
  deriving (Show)
}
