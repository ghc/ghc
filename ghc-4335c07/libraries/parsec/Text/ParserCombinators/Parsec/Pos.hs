-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Pos
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

module Text.ParserCombinators.Parsec.Pos
    ( SourceName,
      Line,
      Column,
      SourcePos,
      sourceLine,
      sourceColumn,
      sourceName,
      incSourceLine,
      incSourceColumn,
      setSourceLine,
      setSourceColumn,
      setSourceName,
      newPos,
      initialPos,
      updatePosChar,
      updatePosString
    ) where


import Text.Parsec.Pos
