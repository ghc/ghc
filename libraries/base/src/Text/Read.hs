{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Text.Read
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Text.ParserCombinators.ReadP)
--
-- Converting strings to values.
--
-- The "Text.Read" module is the canonical place to import for
-- 'Read'-class facilities.  For GHC only, it offers an extended and much
-- improved 'Read' class, which constitutes a proposed alternative to the
-- Haskell 2010 'Read'.  In particular, writing parsers is easier, and
-- the parsers are much more efficient.
--

module Text.Read
    (-- *  The 'Read' class
     Read(..),
     ReadS,
     -- *  Haskell 2010 functions
     reads,
     read,
     readParen,
     lex,
     -- *  New parsing functions
     module Text.ParserCombinators.ReadPrec,
     Lexeme(..),
     lexP,
     parens,
     readListDefault,
     readListPrecDefault,
     readEither,
     readMaybe
     ) where

import GHC.Internal.Text.Read
import Text.ParserCombinators.ReadPrec
