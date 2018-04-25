-- !!! Testing the behaviour of Char.lexLitChar a little..

-- [March 2003]  We now allow \X and \O as escapes although the 
-- spec only permits \x and \o.  Seems more consistent. 

module Main where

import Data.Char

lex' str = do
  putStr ("lex " ++ str ++ " = ")
  print (lex str)

hexes = do
  lex' "'\\X00'"
  lex' "'\\x0f2'"
  lex' "'\\xf2'"
  lex' "'\\xf2t'"
  lex' "'\\X24'"
  lex' "'\\x24b'"
  lex' "'\\Xa4b'"
  lex' "'\\xa4bg'"

octs = do
  lex' "'\\o00'"
  lex' "'\\o05'"
  lex' "'\\o50'"
  lex' "'\\o72'"
  lex' "'\\o82'"
  lex' "'\\O24'"
  lex' "'\\O000024'"
  lex' "'\\024b'"
  lex' "'\\o14b'"
  lex' "'\\0a4bg'"

main = do
  hexes
  octs





