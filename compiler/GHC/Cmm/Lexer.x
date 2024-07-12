-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004-2006
--
-- Lexer for concrete Cmm.  We try to stay close to the C-- spec, but there
-- are a few minor differences:
--
--   * extra keywords for our macros, and float32/float64 types
--   * global registers (Sp,Hp, etc.)
--
-----------------------------------------------------------------------------

{
module GHC.Cmm.Lexer (
   CmmToken(..), cmmlex,
  ) where

import GHC.Prelude

import GHC.Cmm.Expr

import GHC.Parser.Lexer
import GHC.Cmm.Parser.Monad
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Parser.CharClass
import GHC.Parser.Errors.Types
import GHC.Parser.Errors.Ppr ()
import GHC.Platform
import GHC.Utils.Error
import GHC.Utils.Misc
--import TRACE

import Data.Word
import Data.Char
}

$whitechar   = [\ \t\n\r\f\v\xa0] -- \xa0 is Unicode no-break space
$white_no_nl = $whitechar # \n

$ascdigit  = 0-9
$unidigit  = \x01 -- Trick Alex into handling Unicode. See alexGetChar.
$digit     = [$ascdigit $unidigit]
$octit     = 0-7
$hexit     = [$digit A-F a-f]

$unilarge  = \x03 -- Trick Alex into handling Unicode. See alexGetChar.
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge $unilarge]

$unismall  = \x04 -- Trick Alex into handling Unicode. See alexGetChar.
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall $unismall \_]

$namebegin = [$large $small \. \$ \@]
$namechar  = [$namebegin $digit]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@floating_point = [\-]? (@decimal \. @decimal @exponent? | @decimal @exponent)

@escape      = \\ ([abfnrt\\\'\"\?] | x $hexit{1,2} | $octit{1,3})
@strchar     = ($printable # [\"\\]) | @escape

cmm :-

$white_no_nl+           ;
^\# pragma .* \n        ; -- Apple GCC 3.3 CPP generates pragmas in its output

^\# (line)?             { begin line_prag }

-- single-line line pragmas, of the form
--    # <line> "<file>" <extra-stuff> \n
<line_prag> $digit+                     { setLine line_prag1 }
<line_prag1> \" [^\"]* \"       { setFile line_prag2 }
<line_prag2> .*                         { pop }

<0> {
  \n                    ;

  [\:\;\{\}\[\]\(\)\=\`\~\/\*\%\-\+\&\^\|\>\<\,\!]      { special_char }

  ".."                  { kw CmmT_DotDot }
  "::"                  { kw CmmT_DoubleColon }
  ">>"                  { kw CmmT_Shr }
  "<<"                  { kw CmmT_Shl }
  ">="                  { kw CmmT_Ge }
  "<="                  { kw CmmT_Le }
  "=="                  { kw CmmT_Eq }
  "!="                  { kw CmmT_Ne }
  "&&"                  { kw CmmT_BoolAnd }
  "||"                  { kw CmmT_BoolOr }
  "%relaxed"            { kw CmmT_Relaxed }
  "%acquire"            { kw CmmT_Acquire }
  "%release"            { kw CmmT_Release }
  "%seq_cst"            { kw CmmT_SeqCst  }

  "True"                { kw CmmT_True  }
  "False"               { kw CmmT_False }
  "likely"              { kw CmmT_likely}

  P@decimal             { global_regN 1 VanillaReg      gcWord }
  R@decimal             { global_regN 1 VanillaReg       bWord }
  F@decimal             { global_regN 1 FloatReg  (const $ cmmFloat W32) }
  D@decimal             { global_regN 1 DoubleReg (const $ cmmFloat W64) }
  L@decimal             { global_regN 1 LongReg   (const $ cmmBits  W64) }
  XMM@decimal           { global_regN 3 XmmReg    (const $ cmmVec 2 (cmmFloat W64)) }
  YMM@decimal           { global_regN 3 YmmReg    (const $ cmmVec 4 (cmmFloat W64)) }
  ZMM@decimal           { global_regN 3 ZmmReg    (const $ cmmVec 8 (cmmFloat W64)) }
  Sp                    { global_reg  Sp               bWord }
  SpLim                 { global_reg  SpLim            bWord }
  Hp                    { global_reg  Hp              gcWord }
  HpLim                 { global_reg  HpLim            bWord }
  CCCS                  { global_reg  CCCS             bWord }
  CurrentTSO            { global_reg  CurrentTSO       bWord }
  CurrentNursery        { global_reg  CurrentNursery   bWord }
  HpAlloc               { global_reg  HpAlloc          bWord }
  BaseReg               { global_reg  BaseReg          bWord }
  MachSp                { global_reg  MachSp           bWord }
  UnwindReturnReg       { global_reg  UnwindReturnReg  bWord }

  $namebegin $namechar* { name }

  0 @octal              { tok_octal }
  @decimal              { tok_decimal }
  0[xX] @hexadecimal    { tok_hexadecimal }
  @floating_point       { strtoken tok_float }

  \" @strchar* \"       { strtoken tok_string }
}

{
data CmmToken
  = CmmT_SpecChar  Char
  | CmmT_DotDot
  | CmmT_DoubleColon
  | CmmT_Shr
  | CmmT_Shl
  | CmmT_Ge
  | CmmT_Le
  | CmmT_Eq
  | CmmT_Ne
  | CmmT_BoolAnd
  | CmmT_BoolOr
  | CmmT_CLOSURE
  | CmmT_INFO_TABLE
  | CmmT_INFO_TABLE_RET
  | CmmT_INFO_TABLE_FUN
  | CmmT_INFO_TABLE_CONSTR
  | CmmT_INFO_TABLE_SELECTOR
  | CmmT_else
  | CmmT_export
  | CmmT_section
  | CmmT_goto
  | CmmT_if
  | CmmT_call
  | CmmT_jump
  | CmmT_foreign
  | CmmT_never
  | CmmT_prim
  | CmmT_reserve
  | CmmT_return
  | CmmT_returns
  | CmmT_import
  | CmmT_switch
  | CmmT_case
  | CmmT_default
  | CmmT_push
  | CmmT_unwind
  | CmmT_bits8
  | CmmT_bits16
  | CmmT_bits32
  | CmmT_bits64
  | CmmT_vec128
  | CmmT_vec256
  | CmmT_vec512
  | CmmT_float32
  | CmmT_float64
  | CmmT_gcptr
  | CmmT_GlobalReg GlobalRegUse
  | CmmT_Name      FastString
  | CmmT_String    String
  | CmmT_Int       Integer
  | CmmT_Float     Rational
  | CmmT_EOF
  | CmmT_False
  | CmmT_True
  | CmmT_likely
  | CmmT_Relaxed
  | CmmT_Acquire
  | CmmT_Release
  | CmmT_SeqCst
  deriving (Show)

-- -----------------------------------------------------------------------------
-- Lexer actions

type Action = PsSpan -> StringBuffer -> Int -> PD (PsLocated CmmToken)

begin :: Int -> Action
begin code _span _str _len = do liftP (pushLexState code); lexToken

pop :: Action
pop _span _buf _len = liftP popLexState >> lexToken

special_char :: Action
special_char span buf _len = return (L span (CmmT_SpecChar (currentChar buf)))

kw :: CmmToken -> Action
kw tok span _buf _len = return (L span tok)

global_regN :: Int -> (Int -> GlobalReg) -> (Platform -> CmmType) -> Action
global_regN ident_nb_chars con ty_fn span buf len
  = do { platform <- getPlatform
       ; let reg = con (fromIntegral n)
             ty = ty_fn platform
       ; return (L span (CmmT_GlobalReg (GlobalRegUse reg ty))) }
  where buf' = go ident_nb_chars buf
          where go 0 b = b
                go i b = go (i-1) (stepOn b)
        n = parseUnsignedInteger buf' (len-ident_nb_chars) 10 octDecDigit

global_reg :: GlobalReg -> (Platform -> CmmType) -> Action
global_reg reg ty_fn span _buf _len
  = do { platform <- getPlatform
       ; let ty = ty_fn platform
       ; return (L span (CmmT_GlobalReg (GlobalRegUse reg ty))) }

strtoken :: (String -> CmmToken) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

name :: Action
name span buf len =
  case lookupUFM reservedWordsFM fs of
        Just tok -> return (L span tok)
        Nothing  -> return (L span (CmmT_Name fs))
  where
        fs = lexemeToFastString buf len

reservedWordsFM = listToUFM $
        map (\(x, y) -> (mkFastString x, y)) [
        ( "CLOSURE",            CmmT_CLOSURE ),
        ( "INFO_TABLE",         CmmT_INFO_TABLE ),
        ( "INFO_TABLE_RET",     CmmT_INFO_TABLE_RET ),
        ( "INFO_TABLE_FUN",     CmmT_INFO_TABLE_FUN ),
        ( "INFO_TABLE_CONSTR",  CmmT_INFO_TABLE_CONSTR ),
        ( "INFO_TABLE_SELECTOR",CmmT_INFO_TABLE_SELECTOR ),
        ( "else",               CmmT_else ),
        ( "export",             CmmT_export ),
        ( "section",            CmmT_section ),
        ( "goto",               CmmT_goto ),
        ( "if",                 CmmT_if ),
        ( "call",               CmmT_call ),
        ( "jump",               CmmT_jump ),
        ( "foreign",            CmmT_foreign ),
        ( "never",              CmmT_never ),
        ( "prim",               CmmT_prim ),
        ( "reserve",            CmmT_reserve ),
        ( "return",             CmmT_return ),
        ( "returns",            CmmT_returns ),
        ( "import",             CmmT_import ),
        ( "switch",             CmmT_switch ),
        ( "case",               CmmT_case ),
        ( "default",            CmmT_default ),
        ( "push",               CmmT_push ),
        ( "unwind",             CmmT_unwind ),
        ( "bits8",              CmmT_bits8 ),
        ( "bits16",             CmmT_bits16 ),
        ( "bits32",             CmmT_bits32 ),
        ( "bits64",             CmmT_bits64 ),
        ( "vec128",             CmmT_vec128 ),
        ( "vec256",             CmmT_vec256 ),
        ( "vec512",             CmmT_vec512 ),
        ( "float32",            CmmT_float32 ),
        ( "float64",            CmmT_float64 ),
-- New forms
        ( "b8",                 CmmT_bits8 ),
        ( "b16",                CmmT_bits16 ),
        ( "b32",                CmmT_bits32 ),
        ( "b64",                CmmT_bits64 ),
        ( "f32",                CmmT_float32 ),
        ( "f64",                CmmT_float64 ),
        ( "gcptr",              CmmT_gcptr ),
        ( "likely",             CmmT_likely),
        ( "True",               CmmT_True  ),
        ( "False",              CmmT_False )
        ]

tok_decimal span buf len
  = return (L span (CmmT_Int  $! parseUnsignedInteger buf len 10 octDecDigit))

tok_octal span buf len
  = return (L span (CmmT_Int  $! parseUnsignedInteger (offsetBytes 1 buf) (len-1) 8 octDecDigit))

tok_hexadecimal span buf len
  = return (L span (CmmT_Int  $! parseUnsignedInteger (offsetBytes 2 buf) (len-2) 16 hexDigit))

tok_float str = CmmT_Float $! readRational str

tok_string str = CmmT_String (read str)
                 -- urk, not quite right, but it'll do for now

-- -----------------------------------------------------------------------------
-- Line pragmas

setLine :: Int -> Action
setLine code (PsSpan span _) buf len = do
  let line = parseUnsignedInteger buf len 10 octDecDigit
  liftP $ do
    setSrcLoc (mkRealSrcLoc (srcSpanFile span) (fromIntegral line - 1) 1)
          -- subtract one: the line number refers to the *following* line
    -- trace ("setLine "  ++ show line) $ do
    popLexState >> pushLexState code
  lexToken

setFile :: Int -> Action
setFile code (PsSpan span _) buf len = do
  let file = lexemeToFastString (stepOn buf) (len-2)
  liftP $ do
    setSrcLoc (mkRealSrcLoc file (srcSpanEndLine span) (srcSpanEndCol span))
    popLexState >> pushLexState code
  lexToken

-- -----------------------------------------------------------------------------
-- This is the top-level function: called from the parser each time a
-- new token is to be read from the input.

cmmlex :: (Located CmmToken -> PD a) -> PD a
cmmlex cont = do
  (L span tok) <- lexToken
  --trace ("token: " ++ show tok) $ do
  cont (L (mkSrcSpanPs span) tok)

lexToken :: PD (PsLocated CmmToken)
lexToken = do
  inp@(loc1,buf) <- getInput
  sc <- liftP getLexState
  case alexScan inp sc of
    AlexEOF -> do let span = mkPsSpan loc1 loc1
                  liftP (setLastToken span 0)
                  return (L span CmmT_EOF)
    AlexError (loc2,_) ->
      let msg srcLoc = mkPlainErrorMsgEnvelope srcLoc PsErrCmmLexer
      in liftP $ failLocMsgP (psRealLoc loc1) (psRealLoc loc2) msg
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(end,_buf2) len t -> do
        setInput inp2
        let span = mkPsSpan loc1 end
        span `seq` liftP (setLastToken span len)
        t span buf len

-- -----------------------------------------------------------------------------
-- Monad stuff

-- Stuff that Alex needs to know about our input type:
type AlexInput = (PsLoc,StringBuffer)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,s) = prevChar s '\n'

-- backwards compatibility for Alex 2.x
alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar inp = case alexGetByte inp of
                    Nothing    -> Nothing
                    Just (b,i) -> c `seq` Just (c,i)
                       where c = chr $ fromIntegral b

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (loc,s)
  | atEnd s   = Nothing
  | otherwise = b `seq` loc' `seq` s' `seq` Just (b, (loc', s'))
  where c    = currentChar s
        b    = fromIntegral $ ord $ c
        loc' = advancePsLoc loc c
        s'   = stepOn s

getInput :: PD AlexInput
getInput = PD $ \_ _ s@PState{ loc=l, buffer=b } -> POk s (l,b)

setInput :: AlexInput -> PD ()
setInput (l,b) = PD $ \_ _ s -> POk s{ loc=l, buffer=b } ()
}
