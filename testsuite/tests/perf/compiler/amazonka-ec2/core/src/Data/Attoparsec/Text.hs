{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, TypeSynonymInstances #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Imports internal modules
#endif
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Data.Attoparsec.Text
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for 'Text' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.Text
    (
    -- * Parser types
      Parser

    -- * Running parsers
    , I.parseOnly

    -- * Parsing individual characters
    , I.anyChar

    -- ** Consume all remaining input
    , I.takeText

    -- * Numeric parsers
    , decimal
    , signed
    , double
    , rational

    -- * State observation and manipulation functions
    , I.endOfInput
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (*>), (<*), (<$>))
import Data.Word (Word)
#endif
import Control.Applicative ((<|>))
import Data.Attoparsec.Text.Internal (Parser, takeWhile1)
import Data.Char (isDigit, ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Attoparsec.Internal as I
import qualified Data.Attoparsec.Text.Internal as I (char, satisfy, takeWhile, parseOnly, anyChar, takeText)
import qualified Data.Text as T

-- $parsec
--
-- Compared to Parsec 3, attoparsec makes several tradeoffs.  It is
-- not intended for, or ideal for, all possible uses.
--
-- * While attoparsec can consume input incrementally, Parsec cannot.
--   Incremental input is a huge deal for efficient and secure network
--   and system programming, since it gives much more control to users
--   of the library over matters such as resource usage and the I/O
--   model to use.
--
-- * Much of the performance advantage of attoparsec is gained via
--   high-performance parsers such as 'I.takeWhile' and 'I.string'.
--   If you use complicated combinators that return lists of
--   characters, there is less performance difference between the two
--   libraries.
--
-- * Unlike Parsec 3, attoparsec does not support being used as a
--   monad transformer.
--
-- * attoparsec is specialised to deal only with strict 'Text'
--   input.  Efficiency concerns rule out both lists and lazy text.
--   The usual use for lazy text would be to allow consumption of very
--   large input without a large footprint.  For this need,
--   attoparsec's incremental input provides an excellent substitute,
--   with much more control over when input takes place.  If you must
--   use lazy text, see the 'Lazy' module, which feeds lazy chunks to
--   a regular parser.
--
-- * Parsec parsers can produce more helpful error messages than
--   attoparsec parsers.  This is a matter of focus: attoparsec avoids
--   the extra book-keeping in favour of higher performance.

-- $incremental
--
-- attoparsec supports incremental input, meaning that you can feed it
-- a 'Text' that represents only part of the expected total amount
-- of data to parse. If your parser reaches the end of a fragment of
-- input and could consume more input, it will suspend parsing and
-- return a 'T.Partial' continuation.
--
-- Supplying the 'T.Partial' continuation with another string will
-- resume parsing at the point where it was suspended, with the string
-- you supplied used as new input at the end of the existing
-- input. You must be prepared for the result of the resumed parse to
-- be another 'Partial' continuation.
--
-- To indicate that you have no more input, supply the 'Partial'
-- continuation with an 'T.empty' 'Text'.
--
-- Remember that some parsing combinators will not return a result
-- until they reach the end of input.  They may thus cause 'T.Partial'
-- results to be returned.
--
-- If you do not need support for incremental input, consider using
-- the 'I.parseOnly' function to run your parser.  It will never
-- prompt for more input.
--
-- /Note/: incremental input does /not/ imply that attoparsec will
-- release portions of its internal state for garbage collection as it
-- proceeds.  Its internal representation is equivalent to a single
-- 'Text': if you feed incremental input to an a parser, it will
-- require memory proportional to the amount of input you supply.
-- (This is necessary to support arbitrary backtracking.)

-- $performance
--
-- If you write an attoparsec-based parser carefully, it can be
-- realistic to expect it to perform similarly to a hand-rolled C
-- parser (measuring megabytes parsed per second).
--
-- To actually achieve high performance, there are a few guidelines
-- that it is useful to follow.
--
-- Use the 'Text'-oriented parsers whenever possible,
-- e.g. 'I.takeWhile1' instead of 'many1' 'I.anyChar'.  There is
-- about a factor of 100 difference in performance between the two
-- kinds of parser.
--
-- For very simple character-testing predicates, write them by hand
-- instead of using 'I.inClass' or 'I.notInClass'.  For instance, both
-- of these predicates test for an end-of-line character, but the
-- first is much faster than the second:
--
-- >endOfLine_fast c = c == '\r' || c == '\n'
-- >endOfLine_slow   = inClass "\r\n"
--
-- Make active use of benchmarking and profiling tools to measure,
-- find the problems with, and improve the performance of your parser.

-- | Parse and decode an unsigned decimal number.
decimal :: Integral a => Parser a
decimal = T.foldl' step 0 `fmap` takeWhile1 isDecimal
  where step a c = a * 10 + fromIntegral (ord c - 48)
{-# SPECIALISE decimal :: Parser Int #-}
{-# SPECIALISE decimal :: Parser Int8 #-}
{-# SPECIALISE decimal :: Parser Int16 #-}
{-# SPECIALISE decimal :: Parser Int32 #-}
{-# SPECIALISE decimal :: Parser Int64 #-}
{-# SPECIALISE decimal :: Parser Integer #-}
{-# SPECIALISE decimal :: Parser Word #-}
{-# SPECIALISE decimal :: Parser Word8 #-}
{-# SPECIALISE decimal :: Parser Word16 #-}
{-# SPECIALISE decimal :: Parser Word32 #-}
{-# SPECIALISE decimal :: Parser Word64 #-}

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'
{-# INLINE isDecimal #-}

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
signed :: Num a => Parser a -> Parser a
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
{-# SPECIALISE signed :: Parser Int8 -> Parser Int8 #-}
{-# SPECIALISE signed :: Parser Int16 -> Parser Int16 #-}
{-# SPECIALISE signed :: Parser Int32 -> Parser Int32 #-}
{-# SPECIALISE signed :: Parser Int64 -> Parser Int64 #-}
{-# SPECIALISE signed :: Parser Integer -> Parser Integer #-}
signed p = (negate <$> (I.char '-' *> p))
       <|> (I.char '+' *> p)
       <|> p

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double' or 'scientific'
-- instead.
rational :: Fractional a => Parser a
{-# SPECIALIZE rational :: Parser Double #-}
{-# SPECIALIZE rational :: Parser Float #-}
{-# SPECIALIZE rational :: Parser Rational #-}
{-# SPECIALIZE rational :: Parser Scientific #-}
rational = scientifically realToFrac

-- | Parse a 'Double'.
--
-- This parser accepts an optional leading sign character, followed by
-- at most one decimal digit.  The syntax is similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ is
-- consumed.
--
-- === Examples
--
-- These examples use this helper:
--
-- @
-- r :: 'Parser' a -> 'Data.Text.Text' -> 'Data.Attoparsec.Text.Result' a
-- r p s = 'feed' ('Data.Attoparsec.parse' p s) 'mempty'
-- @
--
-- Examples with behaviour identical to 'read', if you feed an empty
-- continuation to the first result:
--
-- > r double "3"     == Done "" 3.0
-- > r double "3.1"   == Done "" 3.1
-- > r double "3e4"   == Done "" 30000.0
-- > r double "3.1e4" == Done "" 31000.0
-- > r double "3e"    == Done "e" 3.0
--
-- Examples with behaviour identical to 'read':
--
-- > r double ".3"    == Fail ".3" _ _
-- > r double "e3"    == Fail "e3" _ _
--
-- Example of difference from 'read':
--
-- > r double "3.foo" == Done "foo" 3.0
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
double :: Parser Double
double = scientifically undefined

-- A strict pair
data SP = SP !Integer {-# UNPACK #-}!Int

{-# INLINE scientifically #-}
scientifically :: (Scientific -> a) -> Parser a
scientifically h = do
  !positive <- ((== '+') <$> I.satisfy (\c -> c == '-' || c == '+')) <|>
               pure True

  n <- decimal

  let f fracDigits = SP (T.foldl' step n fracDigits)
                        (negate $ T.length fracDigits)
      step a c = a * 10 + fromIntegral (ord c - 48)

  SP c e <- (I.satisfy (=='.') *> (f <$> I.takeWhile isDigit)) <|>
            pure (SP n 0)

  let !signedCoeff | positive  =  c
                   | otherwise = -c

  (I.satisfy (\w -> w == 'e' || w == 'E') *>
      fmap (h . undefined signedCoeff . (e +)) (signed decimal)) <|>
    return (h $ undefined signedCoeff    e)

data Scientific = Scientific

instance Num Scientific where
instance Eq Scientific where
instance Ord Scientific where
instance Fractional Scientific where
instance Real Scientific where
