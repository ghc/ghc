-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Char
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Commonly used character parsers.
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Char
                  ( CharParser
                  , spaces, space
                  , newline, tab
                  , upper, lower, alphaNum, letter
                  , digit, hexDigit, octDigit
                  , char, string
                  , anyChar, oneOf, noneOf
                  , satisfy
                  ) where

import Prelude
import Data.Char
import Text.ParserCombinators.Parsec.Pos( updatePosChar, updatePosString )
import Text.ParserCombinators.Parsec.Prim

-----------------------------------------------------------
-- Type of character parsers
-----------------------------------------------------------
type CharParser st a    = GenParser Char st a

-----------------------------------------------------------
-- Character parsers
-----------------------------------------------------------
oneOf cs            = satisfy (\c -> elem c cs)
noneOf cs           = satisfy (\c -> not (elem c cs))

spaces              = skipMany space        <?> "white space"          
space               = satisfy (isSpace)     <?> "space"

newline             = char '\n'             <?> "new-line"
tab                 = char '\t'             <?> "tab"

upper               = satisfy (isUpper)     <?> "uppercase letter"
lower               = satisfy (isLower)     <?> "lowercase letter"
alphaNum            = satisfy (isAlphaNum)  <?> "letter or digit"
letter              = satisfy (isAlpha)     <?> "letter"
digit               = satisfy (isDigit)     <?> "digit"
hexDigit            = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit            = satisfy (isOctDigit)  <?> "octal digit"

char c              = satisfy (==c)  <?> show [c]
anyChar             = satisfy (const True)

-----------------------------------------------------------
-- Primitive character parsers
-----------------------------------------------------------
satisfy :: (Char -> Bool) -> CharParser st Char
satisfy f           = tokenPrim (\c -> show [c]) 
                                (\pos c cs -> updatePosChar pos c) 
                                (\c -> if f c then Just c else Nothing)

string :: String -> CharParser st String
string s            = tokens show updatePosString s
