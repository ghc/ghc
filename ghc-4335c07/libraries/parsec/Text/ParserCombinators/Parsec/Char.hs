-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Char
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Parsec compatibility module
--
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Char
    ( CharParser,
      spaces,
      space,
      newline,
      tab,
      upper,
      lower,
      alphaNum,
      letter,
      digit,
      hexDigit,
      octDigit,
      char,
      string,
      anyChar,
      oneOf,
      noneOf,
      satisfy
    ) where


import Text.Parsec.Char
import Text.Parsec.String

type CharParser st = GenParser Char st
