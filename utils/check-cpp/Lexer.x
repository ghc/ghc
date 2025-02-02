{
module Lexer (lex_tok, lexCppTokenStream ) where

import ParserM (
                St, init_pos,
                ParserM (..), Action, mkTv, Token(..), start_code,
                setStartCode,
                show_pos, position,
                AlexInput(..), alexGetByte)
-- import qualified ParserM as ParserM (input)
import Control.Monad

-- The lexer is based on
-- https://timsong-cpp.github.io/cppwp/n4140/lex.pptoken
}

words :-

    <0,def,expr> $white+             ;
----------------------------------------
    <0>         "#"                  { mkTv THash }
    <0>         "define"             { \i -> do {setStartCode def; mkTv TDefine i} }
    <0>         "include"            { mkTv TInclude }
    <0>         "ifdef"              { mkTv TIfdef }
    <0>         "ifndef"             { mkTv TIfndef }
    <0>         "if"                 { \i -> do {setStartCode other; mkTv TIf i} }
    <0>         "else"               { mkTv TElse }
    <0>         "endif"              { mkTv TEndif }
    <0>         ()                   { begin expr }

---------------------------------------
-- In a define. Params only, then other
    <def>         "("                  { mkTv TOpenParen }
    <def>         ","                  { mkTv TComma }
    <def>         ")"                  { mkTv TCloseParen }
    <def>         [a-z][a-zA-Z0-9\#_]* { mkTv TLowerName }
    <def>         [A-Z][a-zA-Z0-9\#_]* { mkTv TUpperName }
    <def>         ()                   { begin other }

---------------------------------------

    <expr>         "{"                  { mkTv TOpenBrace }
    <expr>         "}"                  { mkTv TCloseBrace }
    <expr>         "["                  { mkTv TOpenBracket }
    <expr>         "]"                  { mkTv TCloseBracket }
    <expr>         "#"                  { mkTv THash }
    <expr>         "##"                 { mkTv THashHash }
    <expr>         "("                  { mkTv TOpenParen }
    <expr>         ")"                  { mkTv TCloseParen }
    <expr>         "<:"                 { mkTv TLtColon }
    <expr>         ":>"                 { mkTv TColonGt}
    <expr>         "<%"                 { mkTv TLtPercent }
    <expr>         "%>"                 { mkTv TPercentGt }
    <expr>         "%:"                 { mkTv TPercentColon }
    <expr>         "%:%:"               { mkTv TPercentColonTwice }
    <expr>         ";"                  { mkTv TSemi }
    <expr>         ":"                  { mkTv TColon }
    <expr>         "..."                { mkTv TDotDotDot }
    <expr>         "new"                { mkTv TNew }
    <expr>         "delete"             { mkTv TDelete }
    <expr>         "?"                  { mkTv TQuestion }
    <expr>         "::"                 { mkTv TColonColon}
    <expr>         "."                  { mkTv TDot }
    <expr>         ".*"                 { mkTv TDotStar }
    <expr>         "+"                  { mkTv TPlus }
    <expr>         "-"                  { mkTv TMinus }
    <expr>         "*"                  { mkTv TStar }
    <expr>         "/"                  { mkTv TSlash }
    <expr>         "%"                  { mkTv TPercent }
    <expr>         "^"                  { mkTv TUpArrow }
    <expr>         "&"                  { mkTv TAmpersand }
    <expr>         "|"                  { mkTv TPipe }
    <expr>         "~"                  { mkTv TTilde }
    <expr>         "!"                  { mkTv TExclamation }
    <expr>         "="                  { mkTv TEqual }
    <expr>         "<"                  { mkTv TOpenAngle }
    <expr>         ">"                  { mkTv TCloseAngle }
    <expr>         "+="                 { mkTv TPlusEqual }
    <expr>         "-="                 { mkTv TMinusEqual }
    <expr>         "*="                 { mkTv TStarEqual }
    <expr>         "/="                 { mkTv TSlashEqual }
    <expr>         "%="                 { mkTv TPercentEqual }
    <expr>         "^="                 { mkTv TUpEqual }
    <expr>         "&="                 { mkTv TAmpersandEqual }
    <expr>         "|="                 { mkTv TPipeEqual }
    <expr>         "<<"                 { mkTv TLtLt }
    <expr>         ">>"                 { mkTv TGtGt }
    <expr>         ">>="                { mkTv TGtGtEqual }
    <expr>         "<<="                { mkTv TLtLtEqual }
    <expr>         "=="                 { mkTv TEqualEqual }
    <expr>         "!="                 { mkTv TExclaimEqual }
    <expr>         "<="                 { mkTv TLtEqual }
    <expr>         ">="                 { mkTv TGtEqual }
    <expr>         "&&"                 { mkTv TAmpersandTwice }
    <expr>         "||"                 { mkTv TPipePipe }
    <expr>         "++"                 { mkTv TPlusPlus }
    <expr>         "--"                 { mkTv TMinusMinus }
    <expr>         ","                  { mkTv TComma }
    <expr>         "->*"                { mkTv TMinusGtStar }
    <expr>         "->"                 { mkTv TMinusGt }
    <expr>         "and"                { mkTv TAnd }
    <expr>         "and_eq"             { mkTv TAndEq }
    <expr>         "bitand"             { mkTv TBitand }
    <expr>         "bitor"              { mkTv TBitor }
    <expr>         "compl"              { mkTv TCompl }
    <expr>         "not"                { mkTv TNot }
    <expr>         "not_eq"             { mkTv TNotEq }
    <expr>         "or"                 { mkTv TOr }
    <expr>         "or_eq"              { mkTv TOrEq }
    <expr>         "xor"                { mkTv TXor }
    <expr>         "xor_eq"             { mkTv TXorEq }
----------------------------------------
    <expr>         [a-z][a-zA-Z0-9\#_]* { mkTv TLowerName }
    <expr>         [A-Z][a-zA-Z0-9\#_]* { mkTv TUpperName }
    <expr>         \-? [0-9][0-9]*      { mkTv TInteger  }
    <expr>         \" [^\"]* \"         { mkTv (TString . tail . init) }
    <expr>         ()                   { begin other }

    <other>     .+                   { \i -> do {setStartCode 0;
                                                 mkTv TOther i} }

{

begin :: Int -> Action
begin sc _str =
  do setStartCode sc
     get_tok

get_tok :: ParserM Token
get_tok = ParserM $ \i st ->
   case alexScan i (start_code st) of
       AlexEOF -> Right (i, st, TEOF "")
       AlexError _ -> Left ("Lexical error at " ++ show_pos (position i))
       AlexSkip i' _ -> case get_tok of
                            ParserM f -> f i' st
       AlexToken i' l a -> case a $ take l $ ParserM.input i of
                               ParserM f -> f i' st

lex_tok :: (Token -> ParserM a) -> ParserM a
lex_tok cont = get_tok >>= cont

lexCppTokenStream :: String -> St -> Either String (AlexInput, St, [Token])
lexCppTokenStream s = unParserM go (AlexInput init_pos [] s)
    where
    go = do
      ltok <- lex_tok return
      case ltok of
        TEOF _ -> return []
        _ -> liftM (ltok:) go
}
