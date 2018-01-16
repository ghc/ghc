-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Prim
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

module Text.ParserCombinators.Parsec.Prim
    ( (<?>),
      (<|>),
      Parser,
      GenParser,
      runParser,
      parse,
      parseFromFile,
      parseTest,
      token,
      tokens,
      tokenPrim,
      tokenPrimEx,
      try,
      label,
      labels,
      unexpected,
      pzero,
      many,
      skipMany,
      getState,
      setState,
      updateState,
      getPosition,
      setPosition,
      getInput,
      setInput,
      State(..),
      getParserState,
      setParserState
    ) where

import Text.Parsec.Prim hiding (runParser, try)
import qualified Text.Parsec.Prim as N -- 'N' for 'New'
import Text.Parsec.String

import Text.Parsec.Error
import Text.Parsec.Pos

pzero :: GenParser tok st a
pzero = parserZero

runParser :: GenParser tok st a
          -> st
          -> SourceName
          -> [tok]
          -> Either ParseError a
runParser = N.runParser

try :: GenParser tok st a -> GenParser tok st a
try = N.try
