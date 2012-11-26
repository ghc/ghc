module HpcLexer where

import Data.Char

data Token
        = ID String
        | SYM Char
        | INT Int
        | STR String
        | CAT String
        deriving (Eq,Show)

initLexer :: String -> [Token]
initLexer str = [ t | (_,_,t) <- lexer str 1 1 ]

lexer :: String -> Int -> Int ->  [(Int,Int,Token)]
lexer (c:cs) line column
  | c == '\n' = lexer cs (succ line) 1
  | c == '\"' = lexerSTR cs line (succ column)
  | c == '[' = lexerCAT cs "" line (succ column)
  | c `elem` "{};-:"
              = (line,column,SYM c) : lexer cs line (succ column)
  | isSpace c = lexer cs        line (succ column)
  | isAlpha c = lexerKW  cs [c] line (succ column)
  | isDigit c = lexerINT cs [c] line (succ column)
  | otherwise = error "lexer failure"
lexer [] _ _ = []

lexerKW :: String -> String -> Int -> Int -> [(Int,Int,Token)]
lexerKW  (c:cs) s line column
  | isAlpha c = lexerKW cs (s ++ [c]) line (succ column)
lexerKW  other s line column = (line,column,ID s) : lexer other line column

lexerINT :: String -> String -> Int -> Int -> [(Int,Int,Token)]
lexerINT  (c:cs) s line column
  | isDigit c = lexerINT cs (s ++ [c]) line (succ column)
lexerINT  other s line column = (line,column,INT (read s)) : lexer other line column

-- not technically correct for the new column count, but a good approximation.
lexerSTR :: String -> Int -> Int -> [(Int,Int,Token)]
lexerSTR cs line column
  = case lex ('"' : cs) of
      [(str,rest)] -> (line,succ column,STR (read str))
                   : lexer rest line (length (show str) + column + 1)
      _ -> error "bad string"

lexerCAT :: String -> String -> Int -> Int -> [(Int,Int,Token)]
lexerCAT (c:cs) s line column
  | c == ']'  =  (line,column,CAT s) : lexer cs line (succ column)
  | otherwise = lexerCAT cs (s ++ [c]) line (succ column)
lexerCAT  [] _ _ _ = error "lexer failure in CAT"

test :: IO ()
test = do
          t <- readFile "EXAMPLE.tc"
          print (initLexer t)

