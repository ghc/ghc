{
module Lexer (lex_tok, lexCppTokenStream) where

import ParserM (
                St, init_pos,
                ParserM (..), Action, mkTv, Token(..), start_code,
                setStartCode,
                show_pos, position,
                AlexInput(..), alexGetByte)
import qualified ParserM as ParserM (input)
import Control.Monad

-- The lexer is based on
-- https://timsong-cpp.github.io/cppwp/n4140/lex.pptoken
}

words :-

    <0>         $white+              ;
---------------------------------------
    <0>         "{"                  { mkTv TOpenBrace }
    <0>         "}"                  { mkTv TCloseBrace }
    <0>         "["                  { mkTv TOpenBracket }
    <0>         "]"                  { mkTv TCloseBracket }
    <0>         "#"                  { mkTv THash }
    <0>         "##"                 { mkTv THashHash }
    <0>         "("                  { mkTv TOpenParen }
    <0>         ")"                  { mkTv TCloseParen }
    <0>         "<:"                 { mkTv TLtColon }
    <0>         ":>"                 { mkTv TColonGt}
    <0>         "<%"                 { mkTv TLtPercent }
    <0>         "%>"                 { mkTv TPercentGt }
    <0>         "%:"                 { mkTv TPercentColon }
    <0>         "%:%:"               { mkTv TPercentColonTwice }
    <0>         ";"                  { mkTv TSemi }
    <0>         ":"                  { mkTv TColon }
    <0>         "..."                { mkTv TDotDotDot }
    <0>         "new"                { mkTv TNew }
    <0>         "delete"             { mkTv TDelete }
    <0>         "?"                  { mkTv TQuestion }
    <0>         "::"                 { mkTv TColonColon}
    <0>         "."                  { mkTv TDot }
    <0>         ".*"                 { mkTv TDotStar }
    <0>         "+"                  { mkTv TPlus }
    <0>         "-"                  { mkTv TMinus }
    <0>         "*"                  { mkTv TStar }
    <0>         "/"                  { mkTv TSlash }
    <0>         "%"                  { mkTv TPercent }
    <0>         "^"                  { mkTv TUpArrow }
    <0>         "&"                  { mkTv TAmpersand }
    <0>         "|"                  { mkTv TPipe }
    <0>         "~"                  { mkTv TTilde }
    <0>         "!"                  { mkTv TExclamation }
    <0>         "="                  { mkTv TEqual }
    <0>         "<"                  { mkTv TOpenAngle }
    <0>         ">"                  { mkTv TCloseAngle }
    <0>         "+="                 { mkTv TPlusEqual }
    <0>         "-="                 { mkTv TMinusEqual }
    <0>         "*="                 { mkTv TStarEqual }
    <0>         "/="                 { mkTv TSlashEqual }
    <0>         "%="                 { mkTv TPercentEqual }
    <0>         "^="                 { mkTv TUpEqual }
    <0>         "&="                 { mkTv TAmpersandEqual }
    <0>         "|="                 { mkTv TPipeEqual }
    <0>         "<<"                 { mkTv TLtLt }
    <0>         ">>"                 { mkTv TGtGt }
    <0>         ">>="                { mkTv TGtGtEqual }
    <0>         "<<="                { mkTv TLtLtEqual }
    <0>         "=="                 { mkTv TEqualEqual }
    <0>         "!="                 { mkTv TExclaimEqual }
    <0>         "<="                 { mkTv TLtEqual }
    <0>         ">="                 { mkTv TGtEqual }
    <0>         "&&"                 { mkTv TAmpersandTwice }
    <0>         "||"                 { mkTv TPipePipe }
    <0>         "++"                 { mkTv TPlusPlus }
    <0>         "--"                 { mkTv TMinusMinus }
    <0>         ","                  { mkTv TComma }
    <0>         "->*"                { mkTv TMinusGtStar }
    <0>         "->"                 { mkTv TMinusGt }
    <0>         "and"                { mkTv TAnd }
    <0>         "and_eq"             { mkTv TAndEq }
    <0>         "bitand"             { mkTv TBitand }
    <0>         "bitor"              { mkTv TBitor }
    <0>         "compl"              { mkTv TCompl }
    <0>         "not"                { mkTv TNot }
    <0>         "not_eq"             { mkTv TNotEq }
    <0>         "or"                 { mkTv TOr }
    <0>         "or_eq"              { mkTv TOrEq }
    <0>         "xor"                { mkTv TXor }
    <0>         "xor_eq"             { mkTv TXorEq }
----------------------------------------
    <0>         [a-z][a-zA-Z0-9\#_]* { mkTv TLowerName }
    <0>         [A-Z][a-zA-Z0-9\#_]* { mkTv TUpperName }
    <0>         \-? [0-9][0-9]*      { mkTv TInteger  }
    <0>         \" [^\"]* \"         { mkTv (TString . tail . init) }
    <0>         ()                   { begin other }

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
