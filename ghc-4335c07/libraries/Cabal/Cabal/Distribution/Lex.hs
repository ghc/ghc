-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Lex
-- Copyright   :  Ben Gamari 2015-2019
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains a simple lexer supporting quoted strings

module Distribution.Lex (
        tokenizeQuotedWords
 ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.DList

tokenizeQuotedWords :: String -> [String]
tokenizeQuotedWords = filter (not . null) . go False mempty
  where
    go :: Bool        -- ^ in quoted region
       -> DList Char  -- ^ accumulator
       -> String      -- ^ string to be parsed
       -> [String]    -- ^ parse result
    go _ accum []
      | [] <- accum' = []
      | otherwise    = [accum']
      where accum' = runDList accum

    go False  accum (c:cs)
      | isSpace c = runDList accum : go False mempty cs
      | c == '"'  = go True accum cs

    go True   accum (c:cs)
      | c == '"'  = go False accum cs

    go quoted accum (c:cs)
                  = go quoted (accum `mappend` singleton c) cs

