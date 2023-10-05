{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
-- Haskell 2010 'Read'.  In particular, writing parsers is easier, and
-- the parsers are much more efficient.
--
-----------------------------------------------------------------------------

module Text.Read (
   -- * The 'Read' class
   Read(..),
   ReadS,

   -- * Haskell 2010 functions
   reads,
   read,
   readParen,
   lex,

   -- * New parsing functions
   module Text.ParserCombinators.ReadPrec,
   L.Lexeme(..),
   lexP,
   parens,
   readListDefault,
   readListPrecDefault,
   readEither,
   readMaybe

 ) where

import GHC.Base
import GHC.Read
import Data.Either
import Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L

-- $setup
-- >>> import Prelude

------------------------------------------------------------------------
-- utility functions

-- | equivalent to 'readsPrec' with a precedence of 0.
reads :: Read a => ReadS a
reads = readsPrec minPrec

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
--
-- >>> readEither "123" :: Either String Int
-- Right 123
--
-- >>> readEither "hello" :: Either String Int
-- Left "Prelude.read: no parse"
--
-- @since 4.6.0.0
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
--
-- >>> readMaybe "123" :: Maybe Int
-- Just 123
--
-- >>> readMaybe "hello" :: Maybe Int
-- Nothing
--
-- @since 4.6.0.0
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a

-- | The 'read' function reads input from a string, which must be
-- completely consumed by the input process. 'read' fails with an 'error' if the
-- parse is unsuccessful, and it is therefore discouraged from being used in
-- real applications. Use 'readMaybe' or 'readEither' for safe alternatives.
--
-- >>> read "123" :: Int
-- 123
--
-- >>> read "hello" :: Int
-- *** Exception: Prelude.read: no parse
read :: Read a => String -> a
read s = either errorWithoutStackTrace id (readEither s)
