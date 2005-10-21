{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
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
-- The "Text.Read" library is the canonical library to import for
-- 'Read'-class facilities.  For GHC only, it offers an extended and much
-- improved 'Read' class, which constitutes a proposed alternative to the 
-- Haskell 98 'Read'.  In particular, writing parsers is easier, and
-- the parsers are much more efficient.
--
-----------------------------------------------------------------------------

module Text.Read (
   -- * The 'Read' class
   Read(..),		-- The Read class
   ReadS, 		-- String -> Maybe (a,String)

   -- * Haskell 98 functions
   reads,		-- :: (Read a) => ReadS a
   read,		-- :: (Read a) => String -> a
   readParen, 		-- :: Bool -> ReadS a -> ReadS a
   lex,			-- :: ReadS String

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
   -- * New parsing functions
   module Text.ParserCombinators.ReadPrec,
   L.Lexeme(..),	
   lexP,		-- :: ReadPrec Lexeme
   parens,		-- :: ReadPrec a -> ReadPrec a
#endif
#ifdef __GLASGOW_HASKELL__
   readListDefault,	-- :: Read a => ReadS [a]
   readListPrecDefault,	-- :: Read a => ReadPrec [a]
#endif

 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Read
#endif   
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L
#endif   

#ifdef __HUGS__
-- copied from GHC.Read

lexP :: ReadPrec L.Lexeme
lexP = lift L.lex

parens :: ReadPrec a -> ReadPrec a
parens p = optional
 where
  optional  = p +++ mandatory
  mandatory = do
    L.Punc "(" <- lexP
    x          <- reset optional
    L.Punc ")" <- lexP
    return x
#endif
