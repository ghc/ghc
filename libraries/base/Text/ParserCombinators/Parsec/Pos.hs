-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Pos
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Textual source positions.
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Pos
                  ( SourceName, Line, Column                 
                  , SourcePos
                  , sourceLine, sourceColumn, sourceName
                  , incSourceLine, incSourceColumn
                  , setSourceLine, setSourceColumn, setSourceName
                  , newPos, initialPos
                  , updatePosChar, updatePosString
                  ) where

-----------------------------------------------------------
-- Source Positions, a file name, a line and a column.
-- upper left is (1,1)
-----------------------------------------------------------                         
type SourceName     = String
type Line           = Int
type Column         = Int

data SourcePos      = SourcePos SourceName !Line !Column
		     deriving (Eq,Ord)
		

newPos :: SourceName -> Line -> Column -> SourcePos
newPos sourceName line column
    = SourcePos sourceName line column

initialPos sourceName
    = newPos sourceName 1 1

sourceName   (SourcePos name line column)   = name    
sourceLine   (SourcePos name line column)   = line    
sourceColumn (SourcePos name line column)   = column

incSourceLine   (SourcePos name line column) n    = SourcePos name (line+n) column
incSourceColumn (SourcePos name line column) n    = SourcePos name line (column+n)

setSourceName   (SourcePos name line column) n    = SourcePos n line column
setSourceLine   (SourcePos name line column) n    = SourcePos name n column
setSourceColumn (SourcePos name line column) n    = SourcePos name line n

-----------------------------------------------------------
-- Update source positions on characters
-----------------------------------------------------------                         
updatePosString :: SourcePos -> String -> SourcePos
updatePosString pos string
    = forcePos (foldl updatePosChar pos string)

updatePosChar   :: SourcePos -> Char -> SourcePos
updatePosChar pos@(SourcePos name line column) c   
    = forcePos $
      case c of
        '\n' -> SourcePos name (line+1) 1
        '\r' -> SourcePos name (line+1) 1
        '\t' -> SourcePos name line (column + 8 - ((column-1) `mod` 8))
        _    -> SourcePos name line (column + 1)
        

forcePos :: SourcePos -> SourcePos      
forcePos pos@(SourcePos name line column)
    = seq line (seq column (pos))

-----------------------------------------------------------
-- Show positions 
-----------------------------------------------------------                                                 
instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")" 
