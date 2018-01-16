-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.String
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Make Strings an instance of 'Stream' with 'Char' token type.
--
-----------------------------------------------------------------------------

module Text.Parsec.String
    ( Parser, GenParser, parseFromFile
    ) where

import Text.Parsec.Error
import Text.Parsec.Prim

type Parser = Parsec String ()
type GenParser tok st = Parsec [tok] st

-- | @parseFromFile p filePath@ runs a string parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- readFile fname
         return (runP p () fname input)
