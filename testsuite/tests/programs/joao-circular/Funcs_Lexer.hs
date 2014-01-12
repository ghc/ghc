--
-- Lexer for MicroC
--
-- João Saraiva
-- 2002
--


module Funcs_Lexer where
import Data.Char

--
-- Tokens passed to the parser
--

data Token
       = Tsemicolon
       | TopenB
       | TcloseB
       | TopenSB
       | TcloseSB
       | TopenCB
       | TcloseCB
       | Ttowdots
       | Tcomma
       | Tequal
       | Tand
       | Tor
       | Tassing
       | Tadd
       | Tmul
       | Tsub
       | Tdiv
       | Tgt
       | Tlt 
       | Tnot
       | Ttrue
       | Tfalse
       | Tint
       | Treal
       | Tbool
       | Tchar
       | Tif
       | Tthen
       | Telse
       | Twhile
       | Tinput
       | Tprint
       | TIdent String
       | TintVal Int
       | TrealVal Float
       deriving Show

lexer []       = []
lexer (';':cs) =  Tsemicolon : lexer cs
lexer ('(':cs) =  TopenB : lexer cs
lexer (')':cs) =  TcloseB : lexer cs
lexer ('[':cs) =  TopenSB : lexer cs
lexer (']':cs) =  TcloseSB : lexer cs
lexer ('{':cs) =  TopenCB : lexer cs
lexer ('}':cs) =  TcloseCB : lexer cs
lexer (':':cs) =  Ttowdots : lexer cs
lexer (',':cs) =  Tcomma : lexer cs

lexer ('=':'=':cs) = Tequal : lexer cs
lexer ('&':'&':cs) = Tor : lexer cs
lexer ('|':'|':cs) = Tand : lexer cs

lexer ('=':cs) =  Tassing : lexer cs
lexer ('+':cs) =  Tadd : lexer cs
lexer ('*':cs) =  Tmul : lexer cs
lexer ('-':cs) =  Tsub : lexer cs
lexer ('/':cs) =  Tdiv : lexer cs
lexer ('>':cs) =  Tgt : lexer cs
lexer ('<':cs) =  Tlt : lexer cs
lexer ('!':cs) =  Tnot : lexer cs

lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexSTR (c:cs)
      | isDigit c = lexINT (c:cs)

lexINT cs =
   case span isDigit cs of
      (var,rest) -> TintVal ((read var)::Int) : lexer rest


lexSTR cs = 
   case span isLegalChar cs of
      ("true",rest)  -> Ttrue : lexer rest
      ("false",rest) -> Tfalse : lexer rest
      ("int",rest)   -> Tint : lexer rest
      ("real",rest)  -> Treal : lexer rest
      ("bool",rest)  -> Tbool : lexer rest
      ("char",rest)  -> Tchar : lexer rest
      ("if",rest)    -> Tif : lexer rest
      ("then",rest)  -> Tthen : lexer rest
      ("else",rest)  -> Telse : lexer rest
      ("while",rest) -> Twhile : lexer rest
      ("input",rest) -> Tinput : lexer rest
      ("print",rest) -> Tprint : lexer rest
      (var,rest)     -> TIdent var : lexer rest

isLegalChar c = let x = ord c
                in (isAlpha c) || (isDigit c) || (c == '_') 

runScan = do s <- readFile "input"
             let t = lexer s
             putStr (show t)
             return () -- (scanner inp))

