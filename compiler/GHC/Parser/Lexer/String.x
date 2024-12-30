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
  string_multi_content,
) where

import GHC.Prelude

import GHC.Parser.Lexer.Interface
import GHC.Utils.Panic (panic)
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
@stringchar = ($graphic # [\\ \"]) | $space | @escape     | @gap

:-

-- Define an empty rule so it compiles; callers should always explicitly specify a startcode
<0> () ;

-- See Note [Lexing multiline strings]
<string_multi_content> {
  -- Parse as much of the multiline string as possible, except for quotes
  @stringchar* ($nl ([\  $tab] | @gap)* @stringchar*)* { string_multi_content_action }
  -- Allow bare quotes if it's not a triple quote
  (\" | \"\") / ([\n .] # \") { string_multi_content_action }
}

-- -----------------------------------------------------------------------------
-- Haskell actions
{
-- | Dummy action that should never be called. Should only be used in lex states
-- that are manually lexed in tok_string_multi.
string_multi_content_action :: a
string_multi_content_action = panic "string_multi_content_action unexpectedly invoked"
}
