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
    -- * Differences from Parsec
    -- $parsec

    -- * Incremental input
    -- $incremental

    -- * Performance considerations
    -- $performance

    -- * Parser types
      Parser
    , Result
    , T.IResult(..)
    , I.compareResults

    -- * Running parsers
    , parse
    , feed
    , I.parseOnly
    , parseWith
    , parseTest

    -- ** Result conversion
    , maybeResult
    , eitherResult

    -- * Parsing individual characters
    , I.char
    , I.anyChar
    , I.notChar
    , I.satisfy
    , I.satisfyWith
    , I.skip

    -- ** Lookahead
    , I.peekChar
    , I.peekChar'

    -- ** Special character parsers
    , digit
    , letter
    , space

    -- ** Character classes
    , I.inClass
    , I.notInClass

    -- * Efficient string handling
    , I.string
    , I.stringCI
    , I.asciiCI
    , skipSpace
    , I.skipWhile
    , I.scan
    , I.runScanner
    , I.take
    , I.takeWhile
    , I.takeWhile1
    , I.takeTill

    -- ** String combinators
    -- $specalt
    , (.*>)
    , (<*.)

    -- ** Consume all remaining input
    , I.takeText
    , I.takeLazyText

    -- * Text parsing
    , I.endOfLine
    , isEndOfLine
    , isHorizontalSpace

    -- * Numeric parsers
    , decimal
    , hexadecimal
    , signed
    , double
    , Number(..)
    , number
    , rational
    , scientific

    -- * Combinators
    , try
    , (<?>)
    , choice
    , count
    , option
    , many'
    , many1
    , many1'
    , manyTill
    , manyTill'
    , sepBy
    , sepBy'
    , sepBy1
    , sepBy1'
    , skipMany
    , skipMany1
    , eitherP
    , I.match
    -- * State observation and manipulation functions
    , I.endOfInput
    , I.atEnd
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (*>), (<*), (<$>))
import Data.Word (Word)
#endif
import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator
import Data.Attoparsec.Number (Number(..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Attoparsec.Text.Internal (Parser, Result, parse, takeWhile1)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Char (isAlpha, isDigit, isSpace, ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Attoparsec.Internal as I
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.Attoparsec.Text.Internal as I
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

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => I.Parser a -> Text -> IO ()
parseTest p s = print (parse p s)

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
parseWith :: Monad m =>
             (m Text)
          -- ^ An action that will be executed to provide the parser
          -- with more input, if necessary.  The action must return an
          -- 'T.empty' string when there is no more input available.
          -> I.Parser a
          -> Text
          -- ^ Initial input for the parser.
          -> m (Result a)
parseWith refill p s = step $ parse p s
  where step (T.Partial k) = (step . k) =<< refill
        step r           = return r
{-# INLINE parseWith #-}

-- | Convert a 'Result' value to a 'Maybe' value. A 'Partial' result
-- is treated as failure.
maybeResult :: Result r -> Maybe r
maybeResult (T.Done _ r) = Just r
maybeResult _            = Nothing

-- | Convert a 'Result' value to an 'Either' value. A 'Partial' result
-- is treated as failure.
eitherResult :: Result r -> Either String r
eitherResult (T.Done _ r)        = Right r
eitherResult (T.Fail _ [] msg)   = Left msg
eitherResult (T.Fail _ ctxs msg) = Left (intercalate " > " ctxs ++ ": " ++ msg)
eitherResult _                   = Left "Result: incomplete input"

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'
{-# INLINE isEndOfLine #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
{-# INLINE isHorizontalSpace #-}

-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = T.foldl' step 0 `fmap` takeWhile1 isHexDigit
  where
    isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')
    step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where w = ord c
{-# SPECIALISE hexadecimal :: Parser Int #-}
{-# SPECIALISE hexadecimal :: Parser Int8 #-}
{-# SPECIALISE hexadecimal :: Parser Int16 #-}
{-# SPECIALISE hexadecimal :: Parser Int32 #-}
{-# SPECIALISE hexadecimal :: Parser Int64 #-}
{-# SPECIALISE hexadecimal :: Parser Integer #-}
{-# SPECIALISE hexadecimal :: Parser Word #-}
{-# SPECIALISE hexadecimal :: Parser Word8 #-}
{-# SPECIALISE hexadecimal :: Parser Word16 #-}
{-# SPECIALISE hexadecimal :: Parser Word32 #-}
{-# SPECIALISE hexadecimal :: Parser Word64 #-}

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
double = scientifically Sci.toRealFloat

-- | Parse a number, attempting to preserve both speed and precision.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
number :: Parser Number
number = scientifically $ \s ->
            let e = Sci.base10Exponent s
                c = Sci.coefficient s
            in if e >= 0
               then I (c * 10 ^ e)
               else D (Sci.toRealFloat s)
{-# DEPRECATED number "Use 'scientific' instead." #-}

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
scientific :: Parser Scientific
scientific = scientifically id

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
      fmap (h . Sci.scientific signedCoeff . (e +)) (signed decimal)) <|>
    return (h $ Sci.scientific signedCoeff    e)

-- | Parse a single digit, as recognised by 'isDigit'.
digit :: Parser Char
digit = I.satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parse a letter, as recognised by 'isAlpha'.
letter :: Parser Char
letter = I.satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parse a space character, as recognised by 'isSpace'.
space :: Parser Char
space = I.satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = I.skipWhile isSpace
{-# INLINE skipSpace #-}

-- $specalt
--
-- If you enable the @OverloadedStrings@ language extension, you can
-- use the '*>' and '<*' combinators to simplify the common task of
-- matching a statically known string, then immediately parsing
-- something else.
--
-- Instead of writing something like this:
--
-- @
--'I.string' \"foo\" '*>' wibble
-- @
--
-- Using @OverloadedStrings@, you can omit the explicit use of
-- 'I.string', and write a more compact version:
--
-- @
-- \"foo\" '*>' wibble
-- @
--
-- (Note: the '.*>' and '<*.' combinators that were originally
-- provided for this purpose are obsolete and unnecessary, and will be
-- removed in the next major version.)

-- | /Obsolete/. A type-specialized version of '*>' for 'Text'. Use
-- '*>' instead.
(.*>) :: Text -> Parser a -> Parser a
s .*> f = I.string s *> f
{-# DEPRECATED (.*>) "This is no longer necessary, and will be removed. Use '*>' instead." #-}

-- | /Obsolete/. A type-specialized version of '<*' for 'Text'. Use
-- '*>' instead.
(<*.) :: Parser a -> Text -> Parser a
f <*. s = f <* I.string s
{-# DEPRECATED (<*.) "This is no longer necessary, and will be removed. Use '<*' instead." #-}
