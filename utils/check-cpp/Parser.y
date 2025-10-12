{
module Parser (parseExpr) where

import Lexer (lex_tok)
import ParserM (Token(..), ParserM, run_parser, get_pos, show_pos,
                happyError)
import State
-- Needed when invoking happy -ad
import qualified GHC.Internal.Data.Tuple as Happy_Prelude
}

%name      expr
%name      args
%name      parameters
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

    identifier         { TIdentifier {} }
    identifierLP       { TIdentifierLParen {} }
    integer            { TInteger {} }
    string             { TString {} }
    other              { TOther {} }

-- Operator precedence. Earlier in the table is lower
-- Note: this seems to require all the operators to appear in the same
-- rule.
-- Based on https://en.cppreference.com/w/cpp/language/operator_precedence
%left '||'
%left '&&'
%left '=='
%left '!='
%left '>' '>=' '<' '<='
%left '+' '-'
%left '*' '/'
%right '!'


%%

expr :: { Expr }
expr : variable           { $1 }
     | integer            { IntVal (read $ t_str $1) }
     | '(' expr ')'       { $2 }
     | expr '||' expr     { Logic LogicalOr $1 $3 }
     | expr '&&' expr     { Logic LogicalAnd $1 $3 }
     | expr '==' expr     { Comp CmpEqual $1 $3 }
     | expr '!=' expr     { Comp CmpNotEqual $1 $3 }
     | expr '>'  expr     { Comp CmpGt $1 $3 }
     | expr '>=' expr     { Comp CmpGtE $1 $3 }
     | expr '<'  expr     { Comp CmpLt $1 $3 }
     | expr '<=' expr     { Comp CmpLtE $1 $3 }
     | expr '+' expr      { Plus $1 $3 }
     | expr '-' expr      { Minus $1 $3 }
     | expr '*' expr      { Times $1 $3 }
     | '!' expr           { Not $2 }

variable :: {Expr}
variable : name { Var $1 }

name :: { String }
name : identifier { t_str $1 }

------------------------------------------------------------------------

-- function-like macro args, in definition
parameters :: { [String] }
parameters : '(' param_list ')' { reverse $2 }

param_list :: { [String] }
param_list : param_list ',' name  { $3 : $1 }
           | name                 { [$1] }

-- function-like macro args, in call
-- NOTE: according to https://timsong-cpp.github.io/cppwp/n4140/cpp#replace
--       we should only be paying attention to parens and commas.
args :: { [Expr] }
args : '(' arg_list ')' { reverse $2 }

arg_list :: { [Expr] }
arg_list : arg_list ',' expr  { $3 : $1 }
         | expr               { [$1] }

{
-- parseExpr :: String -> Either String Expr
parseExpr = run_parser expr
}
