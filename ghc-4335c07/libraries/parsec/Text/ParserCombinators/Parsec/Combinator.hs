-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Combinator
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

module Text.ParserCombinators.Parsec.Combinator
    ( choice,
      count,
      between,
      option,
      optionMaybe,
      optional,
      skipMany1,
      many1,
      sepBy,
      sepBy1,
      endBy,
      endBy1,
      sepEndBy,
      sepEndBy1,
      chainl,
      chainl1,
      chainr,
      chainr1,
      eof,
      notFollowedBy,
      manyTill,
      lookAhead,
      anyToken
    ) where


import Text.Parsec.Combinator
