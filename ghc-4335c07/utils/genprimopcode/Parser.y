{
module Parser (parse) where

import Lexer (lex_tok)
import ParserM (Token(..), ParserM, run_parser, get_pos, show_pos,
                happyError)
import Syntax
}

%name      parsex
%expect    0
%tokentype { Token }
%monad     { ParserM }
%lexer     { lex_tok } { TEOF }

%token
    '->'            { TArrow }
    '=>'            { TDArrow }
    '='             { TEquals }
    ','             { TComma }
    '('             { TOpenParen }
    ')'             { TCloseParen }
    '(#'            { TOpenParenHash }
    '#)'            { THashCloseParen }
    '{'             { TOpenBrace }
    '}'             { TCloseBrace }
    '['             { TOpenBracket }
    ']'             { TCloseBracket }
    '<'             { TOpenAngle }
    '>'             { TCloseAngle }
    section         { TSection }
    primop          { TPrimop }
    pseudoop        { TPseudoop }
    primtype        { TPrimtype }
    with            { TWith }
    defaults        { TDefaults }
    true            { TTrue }
    false           { TFalse }
    dyadic          { TDyadic }
    monadic         { TMonadic }
    compare         { TCompare }
    genprimop       { TGenPrimOp }
    fixity          { TFixity }
    infix           { TInfixN }
    infixl          { TInfixL }
    infixr          { TInfixR }
    nothing         { TNothing }
    vector          { TVector }
    SCALAR          { TSCALAR }
    VECTOR          { TVECTOR }
    VECTUPLE        { TVECTUPLE }
    thats_all_folks { TThatsAllFolks }
    lowerName       { TLowerName $$ }
    upperName       { TUpperName $$ }
    string          { TString $$ }
    integer         { TInteger $$ }
    noBraces        { TNoBraces $$ }

%%

info :: { Info }
info : pDefaults pEntries thats_all_folks { Info $1 $2 }

pDefaults :: { [Option] }
pDefaults : defaults pOptions { $2 }

pOptions :: { [Option] }
pOptions : pOption pOptions { $1 : $2 }
         | {- empty -}      { [] }

pOption :: { Option }
pOption : lowerName '=' false               { OptionFalse  $1 }
        | lowerName '=' true                { OptionTrue   $1 }
        | lowerName '=' pStuffBetweenBraces { OptionString $1 $3 }
        | lowerName '=' integer             { OptionInteger $1 $3 }
        | vector    '=' pVectorTemplate     { OptionVector $3 }
        | fixity    '=' pInfix              { OptionFixity $3 }

pInfix :: { Maybe Fixity }
pInfix : infix  integer { Just $ Fixity NoSourceText $2 InfixN }
       | infixl integer { Just $ Fixity NoSourceText $2 InfixL }
       | infixr integer { Just $ Fixity NoSourceText $2 InfixR }
       | nothing        { Nothing }


pEntries :: { [Entry] }
pEntries : pEntry pEntries { $1 : $2 }
         | {- empty -}   { [] }

pEntry :: { Entry }
pEntry : pPrimOpSpec   { $1 }
       | pPrimTypeSpec { $1 }
       | pPseudoOpSpec { $1 }
       | pSection      { $1 }

pPrimOpSpec :: { Entry }
pPrimOpSpec : primop upperName string pCategory pType
              pDesc pWithOptions
              { PrimOpSpec {
                    cons = $2,
                    name = $3,
                    cat = $4,
                    ty = $5,
                    desc = $6,
                    opts = $7
                }
              }

pPrimTypeSpec :: { Entry }
pPrimTypeSpec : primtype pType pDesc pWithOptions
                { PrimTypeSpec { ty = $2, desc = $3, opts = $4 } }

pPseudoOpSpec :: { Entry }
pPseudoOpSpec : pseudoop string pType pDesc pWithOptions
                { PseudoOpSpec { name = $2, ty = $3, desc = $4, opts = $5 } }

pSection :: { Entry }
pSection : section string pDesc { Section { title = $2, desc = $3 } }

pWithOptions :: { [Option] }
pWithOptions : with pOptions { $2 }
             | {- empty -}   { [] }

pCategory :: { Category }
pCategory : dyadic { Dyadic }
          | monadic { Monadic }
          | compare { Compare }
          | genprimop { GenPrimOp }

pDesc :: { String }
pDesc : pStuffBetweenBraces { $1 }
      | {- empty -}         { "" }

pStuffBetweenBraces :: { String }
pStuffBetweenBraces : '{' pInsides '}' { $2 }

pInsides :: { String }
pInsides : pInside pInsides { $1 ++ $2 }
         | {- empty -}      { "" }

pInside :: { String }
pInside : '{' pInsides '}' { "{" ++ $2 ++ "}" }
        | noBraces         { $1 }

pVectorTemplate :: { [(String, String, Int)] }
pVectorTemplate : '[' pVectors ']' { $2 }

pVectors :: { [(String, String, Int)] }
pVectors : pVector ',' pVectors { [$1] ++ $3 }
         | pVector              { [$1] }
         | {- empty -}          { [] }

pVector :: { (String, String, Int) }
pVector : '<' upperName ',' upperName ',' integer '>' { ($2, $4, $6) }
 
pType :: { Ty }
pType : paT '->' pType { TyF $1 $3 }
      | paT '=>' pType { TyC $1 $3 }
      | paT            { $1 }

-- Atomic types
paT :: { Ty }
paT : pTycon ppTs     { TyApp $1 $2 }
    | pUnboxedTupleTy { $1 }
    | '(' pType ')'   { $2 }
    | lowerName       { TyVar $1 }

pUnboxedTupleTy :: { Ty }
pUnboxedTupleTy : '(#' pCommaTypes '#)' { TyUTup $2 }

pCommaTypes :: { [Ty] }
pCommaTypes : pType ',' pCommaTypes { $1 : $3 }
            | pType                 { [$1] }

ppTs :: { [Ty] }
ppTs : ppT ppTs    { $1 : $2 }
     | {- empty -} { [] }

-- Primitive types
ppT :: { Ty }
ppT : lowerName { TyVar $1 }
    | pTycon    { TyApp $1 [] }

pTycon :: { TyCon }
pTycon : upperName { TyCon $1 }
       | '(' ')'   { TyCon "()" }
       | SCALAR    { SCALAR }
       | VECTOR    { VECTOR }
       | VECTUPLE  { VECTUPLE }

{
parse :: String -> Either String Info
parse = run_parser parsex
}

