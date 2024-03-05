{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Text.ParserCombinators.ReadP
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (local universal quantification)
--
-- This is a module of parser combinators, originally written by Koen Claessen.
-- It parses all alternatives in parallel, so it never keeps hold of
-- the beginning of the input string, a common source of space leaks with
-- other parsers.  The @('+++')@ choice combinator is genuinely commutative;
-- it makes no difference which branch is \"shorter\".

module Text.ParserCombinators.ReadP
    (-- *  The 'ReadP' type
     ReadP,
     -- *  Primitive operations
     get,
     look,
     (+++),
     (<++),
     gather,
     -- *  Other operations
     pfail,
     eof,
     satisfy,
     char,
     string,
     munch,
     munch1,
     skipSpaces,
     choice,
     count,
     between,
     option,
     optional,
     many,
     many1,
     skipMany,
     skipMany1,
     sepBy,
     sepBy1,
     endBy,
     endBy1,
     chainr,
     chainl,
     chainl1,
     chainr1,
     manyTill,
     -- *  Running a parser
     ReadS,
     readP_to_S,
     readS_to_P,
     -- *  Properties
     -- $properties
     ) where

import GHC.Internal.Text.ParserCombinators.ReadP
