{
module Parser (parseDirective, parseExpr) where

import Lexer (lex_tok)
import ParserM (Token(..), ParserM, run_parser, get_pos, show_pos,
                happyError)
import Types
-- Needed when invoking happy -ad
import qualified GHC.Internal.Data.Tuple as Happy_Prelude
}

%name      directive
%name      expr
%expect    0
%tokentype { Token }
%monad     { ParserM }
%lexer     { lex_tok } { TEOF "" }

%token

    '{'                { TOpenBrace {} }
    '}'                { TCloseBrace {} }
    '['                { TOpenBracket {} }
    ']'                { TCloseBracket {} }
    '#'                { THash {} }
    '##'               { THashHash {} }
    '('                { TOpenParen {} }
    ')'                { TCloseParen {} }
    '<:'               { TLtColon {} }
    ':>'               { TColonGt{} }
    '<%'               { TLtPercent {} }
    '%>'               { TPercentGt {} }
    '%:'               { TPercentColon {} }
    '%:%:'             { TPercentColonTwice {} }
    ';'                { TSemi {} }
    ':'                { TColon {} }
    '...'              { TDotDotDot {} }
    'new'              { TNew {} }
    'delete'           { TDelete {} }
    '?'                { TQuestion {} }
    '::'               { TColonColon{} }
    '.'                { TDot {} }
    '.*'               { TDotStar {} }
    '+'                { TPlus {} }
    '-'                { TMinus {} }
    '*'                { TStar {} }
    '/'                { TSlash {} }
    '%'                { TPercent {} }
    '^'                { TUpArrow {} }
    '&'                { TAmpersand {} }
    '|'                { TPipe {} }
    '~'                { TTilde {} }
    '!'                { TExclamation {} }
    '='                { TEqual {} }
    '<'                { TOpenAngle {} }
    '>'                { TCloseAngle {} }
    '+='               { TPlusEqual {} }
    '-='               { TMinusEqual {} }
    '*='               { TStarEqual {} }
    '/='               { TSlashEqual {} }
    '%='               { TPercentEqual {} }
    '^='               { TUpEqual {} }
    '&='               { TAmpersandEqual {} }
    '|='               { TPipeEqual {} }
    '<<'               { TLtLt {} }
    '>>'               { TGtGt {} }
    '>>='              { TGtGtEqual {} }
    '<<='              { TLtLtEqual {} }
    '=='               { TEqualEqual {} }
    '!='               { TExclaimEqual {} }
    '<='               { TLtEqual {} }
    '>='               { TGtEqual {} }
    '&&'               { TAmpersandTwice {} }
    '||'               { TPipePipe {} }
    '++'               { TPlusPlus {} }
    '--'               { TMinusMinus {} }
    ','                { TComma {} }
    '->*'              { TMinusGtStar {} }
    '->'               { TMinusGt {} }
    'and'              { TAnd {} }
    'and_eq'           { TAndEq {} }
    'bitand'           { TBitand {} }
    'bitor'            { TBitor {} }
    'compl'            { TCompl {} }
    'not'              { TNot {} }
    'not_eq'           { TNotEq {} }
    'or'               { TOr {} }
    'or_eq'            { TOrEq {} }
    'xor'              { TXor {} }
    'xor_eq'           { TXorEq {} }

    lower_name         { TLowerName {} }
    upper_name         { TUpperName {} }
    integer            { TInteger {} }
    string             { TString {} }
    other              { TOther {} }

    'define'           { TDefine {} }
    'include'          { TInclude {} }
    'if'               { TIf {} }
    'ifdef'            { TIfdef {} }
    'ifndef'           { TIfndef {} }
    'else'             { TElse {} }
    'endif'            { TEndif {} }


-- Operator precedence. Earlier in the table is lower
%left '||'
%left '&&'
%left '=='
%left '>' '>=' '<' '<='
%left '+' '-'
%left '*' '/'

%%

directive :: { CppDirective }
directive : '#' 'define' define     { $3 }
          | '#' 'include' include   { $3 }
          | '#' 'ifdef' ifdef       { $3 }
          | '#' 'ifndef' ifndef     { $3 }
          | '#' 'if' if             { $3 }
          | '#' 'else'              { CppElse }
          | '#' 'endif'             { CppEndif }

name : lower_name { t_str $1 }
     | upper_name { t_str $1 }

define :: { CppDirective }
define : name other              { CppDefine $1 [t_str $2] }
       | name '(' args ')' rest  { CppDefine $1 (((t_str $2):$3) ++ (t_str $4:(reverse $5))) }
       | name                    { CppDefine $1 [] }

args :: { [String] }
args : name { [$1] }
     | name ',' args { ($1 : "," : $3) }

include :: { CppDirective }
include : string { CppInclude (t_str $1) }

ifdef :: { CppDirective }
ifdef : name { CppIfdef $1 }

ifndef :: { CppDirective }
ifndef : name { CppIfndef $1 }

if :: { CppDirective }
if : other { CppIf (t_str $1) }

expr :: { Expr }
expr : variable           { $1 }
     | integer            { IntVal (read $ t_str $1) }
     | '(' expr ')'       { $2 }
     | expr '||' expr     { Logic LogicalOr $1 $3 }
     | expr '&&' expr     { Logic LogicalAnd $1 $3 }
     | expr '==' expr     { Comp CmpEqual $1 $3 }
     | expr '>'  expr     { Comp CmpGt $1 $3 }
     | expr '>=' expr     { Comp CmpGtE $1 $3 }
     | expr '<'  expr     { Comp CmpLt $1 $3 }
     | expr '<=' expr     { Comp CmpLtE $1 $3 }

variable :: {Expr}
variable : name { Var $1 }


-- The lexer has a specific context for processing a #define
-- directive, to allow parameters to be parsed, before swallowing the
-- balance into an 'other' token with the rest of the string. It stays
-- in this context until it sees something that is not one of the
-- prefix tokens.
--
-- So when getting the rest, accumulate the possible prefix tokens
-- explicitly.
rest :: { [String] }
rest : '(' rest          { t_str $1 : $2 }
     | ')' rest          { t_str $1 : $2 }
     | lower_name rest   { t_str $1 : $2 }
     | upper_name rest   { t_str $1 : $2 }
     | other rest        { t_str $1 : $2 }
     | {- -}             { [] }

{
-- parseExpr :: String -> Either String Expr
parseExpr = run_parser expr

parseDirective :: String -> Either String CppDirective
parseDirective = run_parser directive
}
