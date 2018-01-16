-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec
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

module Text.ParserCombinators.Parsec
    ( -- complete modules
      module Text.ParserCombinators.Parsec.Prim
    , module Text.ParserCombinators.Parsec.Combinator
    , module Text.ParserCombinators.Parsec.Char

    -- module Text.ParserCombinators.Parsec.Error
    , ParseError
    , errorPos

    -- module Text.ParserCombinators.Parsec.Pos
    , SourcePos
    , SourceName, Line, Column
    , sourceName, sourceLine, sourceColumn
    , incSourceLine, incSourceColumn
    , setSourceLine, setSourceColumn, setSourceName

    ) where

import Text.Parsec.String()

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
