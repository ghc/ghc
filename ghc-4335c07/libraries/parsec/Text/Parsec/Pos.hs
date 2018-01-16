{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Pos
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Textual source positions.
--
-----------------------------------------------------------------------------

module Text.Parsec.Pos
    ( SourceName, Line, Column
    , SourcePos
    , sourceLine, sourceColumn, sourceName
    , incSourceLine, incSourceColumn
    , setSourceLine, setSourceColumn, setSourceName
    , newPos, initialPos
    , updatePosChar, updatePosString
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)

-- < Source positions: a file name, a line and a column
-- upper left is (1,1)

type SourceName = String
type Line       = Int
type Column     = Int

-- | The abstract data type @SourcePos@ represents source positions. It
-- contains the name of the source (i.e. file name), a line number and
-- a column number. @SourcePos@ is an instance of the 'Show', 'Eq' and
-- 'Ord' class.

data SourcePos  = SourcePos SourceName !Line !Column
    deriving ( Eq, Ord, Data, Typeable)

-- | Create a new 'SourcePos' with the given source name,
-- line number and column number.

newPos :: SourceName -> Line -> Column -> SourcePos
newPos name line column
    = SourcePos name line column

-- | Create a new 'SourcePos' with the given source name,
-- and line number and column number set to 1, the upper left.

initialPos :: SourceName -> SourcePos
initialPos name
    = newPos name 1 1

-- | Extracts the name of the source from a source position.

sourceName :: SourcePos -> SourceName
sourceName (SourcePos name _line _column) = name

-- | Extracts the line number from a source position.

sourceLine :: SourcePos -> Line
sourceLine (SourcePos _name line _column) = line

-- | Extracts the column number from a source position.

sourceColumn :: SourcePos -> Column
sourceColumn (SourcePos _name _line column) = column

-- | Increments the line number of a source position.

incSourceLine :: SourcePos -> Line -> SourcePos
incSourceLine (SourcePos name line column) n = SourcePos name (line+n) column

-- | Increments the column number of a source position.

incSourceColumn :: SourcePos -> Column -> SourcePos
incSourceColumn (SourcePos name line column) n = SourcePos name line (column+n)

-- | Set the name of the source.

setSourceName :: SourcePos -> SourceName -> SourcePos
setSourceName (SourcePos _name line column) n = SourcePos n line column

-- | Set the line number of a source position.

setSourceLine :: SourcePos -> Line -> SourcePos
setSourceLine (SourcePos name _line column) n = SourcePos name n column

-- | Set the column number of a source position.

setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceColumn (SourcePos name line _column) n = SourcePos name line n

-- | The expression @updatePosString pos s@ updates the source position
-- @pos@ by calling 'updatePosChar' on every character in @s@, ie.
-- @foldl updatePosChar pos string@.

updatePosString :: SourcePos -> String -> SourcePos
updatePosString pos string
    = foldl updatePosChar pos string

-- | Update a source position given a character. If the character is a
-- newline (\'\\n\') or carriage return (\'\\r\') the line number is
-- incremented by 1. If the character is a tab (\'\t\') the column
-- number is incremented to the nearest 8'th column, ie. @column + 8 -
-- ((column-1) \`mod\` 8)@. In all other cases, the column is
-- incremented by 1.

updatePosChar   :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos name line column) c
    = case c of
        '\n' -> SourcePos name (line+1) 1
        '\t' -> SourcePos name line (column + 8 - ((column-1) `mod` 8))
        _    -> SourcePos name line (column + 1)

instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"
