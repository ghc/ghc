{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, TypeFamilies,
    TypeSynonymInstances, GADTs #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Imports internal modules
#endif
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Data.Attoparsec.ByteString.Char8
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, character-oriented combinator parsing for
-- 'B.ByteString' strings, loosely based on the Parsec library.

module Data.Attoparsec.ByteString.Char8
    (
    -- * Character encodings
    -- $encodings

    -- * Parser types
      Parser
    , A.Result
    , A.IResult(..)
    , I.compareResults

    -- * Running parsers
    , A.parse
    , A.feed
    , A.parseOnly
    , A.parseWith
    , A.parseTest

    -- ** Result conversion
    , A.maybeResult
    , A.eitherResult

    -- * Parsing individual characters
    , char
    , char8
    , anyChar
    , notChar
    , satisfy

    -- ** Lookahead
    , peekChar
    , peekChar'

    -- ** Special character parsers
    , digit
    , letter_iso8859_15
    , letter_ascii
    , space

    -- ** Fast predicates
    , isDigit
    , isDigit_w8
    , isAlpha_iso8859_15
    , isAlpha_ascii
    , isSpace
    , isSpace_w8

    -- *** Character classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , I.string
    , I.stringCI
    , skipSpace
    , skipWhile
    , I.take
    , scan
    , takeWhile
    , takeWhile1
    , takeTill

    -- ** String combinators
    -- $specalt
    , (.*>)
    , (<*.)

    -- ** Consume all remaining input
    , I.takeByteString
    , I.takeLazyByteString

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
import Control.Monad (void, when)
import Data.Attoparsec.ByteString.FastSet (charClass, memberChar)
import Data.Attoparsec.ByteString.Internal (Parser)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Number (Number(..))
import Data.Bits (Bits, (.|.), shiftL)
import Data.ByteString.Internal (c2w, w2c)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.String (IsString(..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Word (Word8, Word16, Word32, Word64)
import Prelude hiding (takeWhile)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Internal as I
import qualified Data.Attoparsec.Internal as I
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B

instance (a ~ B.ByteString) => IsString (Parser a) where
    fromString = I.string . B.pack

-- $encodings
--
-- This module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or
-- ISO-8859-15.  It /does not/ make any attempt to deal with character
-- encodings, multibyte characters, or wide characters.  In
-- particular, all attempts to use characters above code point U+00FF
-- will give wrong answers.
--
-- Code points below U+0100 are simply translated to and from their
-- numeric values, so e.g. the code point U+00A4 becomes the byte
-- @0xA4@ (which is the Euro symbol in ISO-8859-15, but the generic
-- currency sign in ISO-8859-1).  Haskell 'Char' values above U+00FF
-- are truncated, so e.g. U+1D6B7 is truncated to the byte @0xB7@.

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser B.ByteString
takeWhile1 p = I.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfy :: (Char -> Bool) -> Parser Char
satisfy = I.satisfyWith w2c
{-# INLINE satisfy #-}

-- | Match a letter, in the ISO-8859-15 encoding.
letter_iso8859_15 :: Parser Char
letter_iso8859_15 = satisfy isAlpha_iso8859_15 <?> "letter_iso8859_15"
{-# INLINE letter_iso8859_15 #-}

-- | Match a letter, in the ASCII encoding.
letter_ascii :: Parser Char
letter_ascii = satisfy isAlpha_ascii <?> "letter_ascii"
{-# INLINE letter_ascii #-}

-- | A fast alphabetic predicate for the ISO-8859-15 encoding
--
-- /Note/: For all character encodings other than ISO-8859-15, and
-- almost all Unicode code points above U+00A3, this predicate gives
-- /wrong answers/.
isAlpha_iso8859_15 :: Char -> Bool
isAlpha_iso8859_15 c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                       (c >= '\166' && moby c)
  where moby = notInClass "\167\169\171-\179\182\183\185\187\191\215\247"
        {-# NOINLINE moby #-}
{-# INLINE isAlpha_iso8859_15 #-}

-- | A fast alphabetic predicate for the ASCII encoding
--
-- /Note/: For all character encodings other than ASCII, and
-- almost all Unicode code points above U+007F, this predicate gives
-- /wrong answers/.
isAlpha_ascii :: Char -> Bool
isAlpha_ascii c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE isAlpha_ascii #-}

-- | Parse a single digit.
digit :: Parser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | A fast digit predicate.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

-- | A fast digit predicate.
isDigit_w8 :: Word8 -> Bool
isDigit_w8 w = w - 48 <= 9
{-# INLINE isDigit_w8 #-}

-- | Match any character.
anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

-- | Match any character, to perform lookahead. Returns 'Nothing' if
-- end of input has been reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
peekChar :: Parser (Maybe Char)
peekChar = (fmap w2c) `fmap` I.peekWord8
{-# INLINE peekChar #-}

-- | Match any character, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekChar' :: Parser Char
peekChar' = w2c `fmap` I.peekWord8'
{-# INLINE peekChar' #-}

-- | Fast predicate for matching ASCII space characters.
--
-- /Note/: This predicate only gives correct answers for the ASCII
-- encoding.  For instance, it does not recognise U+00A0 (non-breaking
-- space) as a space character, even though it is a valid ISO-8859-15
-- byte. For a Unicode-aware and only slightly slower predicate,
-- use 'Data.Char.isSpace'
isSpace :: Char -> Bool
isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

-- | Fast 'Word8' predicate for matching ASCII space characters.
isSpace_w8 :: Word8 -> Bool
isSpace_w8 w = w == 32 || w - 9 <= 4
{-# INLINE isSpace_w8 #-}


-- | Parse a space character.
--
-- /Note/: This parser only gives correct answers for the ASCII
-- encoding.  For instance, it does not recognise U+00A0 (non-breaking
-- space) as a space character, even though it is a valid ISO-8859-15
-- byte.
space :: Parser Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Match a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

-- | Match a specific character, but return its 'Word8' value.
char8 :: Char -> Parser Word8
char8 c = I.satisfy (== c2w c) <?> [c]
{-# INLINE char8 #-}

-- | Match any character except the given one.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

-- | Match any character in a set.
--
-- >vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- >halfAlphabet = inClass "a-nA-N"
--
-- To add a literal \'-\' to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Char -> Bool
inClass s = (`memberChar` mySet)
    where mySet = charClass s
{-# INLINE inClass #-}

-- | Match any character not in a set.
notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeWhile :: (Char -> Bool) -> Parser B.ByteString
takeWhile p = I.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
scan :: s -> (s -> Char -> Maybe s) -> Parser B.ByteString
scan s0 p = I.scan s0 (\s -> p s . w2c)
{-# INLINE scan #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeTill :: (Char -> Bool) -> Parser B.ByteString
takeTill p = I.takeTill (p . w2c)
{-# INLINE takeTill #-}

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = I.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = I.skipWhile isSpace_w8
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

-- | /Obsolete/. A type-specialized version of '*>' for
-- 'B.ByteString'. Use '*>' instead.
(.*>) :: B.ByteString -> Parser a -> Parser a
s .*> f = I.string s *> f
{-# DEPRECATED (.*>) "This is no longer necessary, and will be removed. Use '*>' instead." #-}

-- | /Obsolete/. A type-specialized version of '<*' for
-- 'B.ByteString'. Use '<*' instead.
(<*.) :: Parser a -> B.ByteString -> Parser a
f <*. s = f <* I.string s
{-# DEPRECATED (<*.) "This is no longer necessary, and will be removed. Use '<*' instead." #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10
{-# INLINE isEndOfLine #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = w == 32 || w == 9
{-# INLINE isHorizontalSpace #-}

-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = B8.foldl' step 0 `fmap` I.takeWhile1 isHexDigit
  where
    isHexDigit w = (w >= 48 && w <= 57) ||
                   (w >= 97 && w <= 102) ||
                   (w >= 65 && w <= 70)
    step a w | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
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
decimal = B8.foldl' step 0 `fmap` I.takeWhile1 isDigit_w8
  where step a w = a * 10 + fromIntegral (w - 48)
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

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
signed :: Num a => Parser a -> Parser a
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
{-# SPECIALISE signed :: Parser Int8 -> Parser Int8 #-}
{-# SPECIALISE signed :: Parser Int16 -> Parser Int16 #-}
{-# SPECIALISE signed :: Parser Int32 -> Parser Int32 #-}
{-# SPECIALISE signed :: Parser Int64 -> Parser Int64 #-}
{-# SPECIALISE signed :: Parser Integer -> Parser Integer #-}
signed p = (negate <$> (char8 '-' *> p))
       <|> (char8 '+' *> p)
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
-- r :: 'Parser' a -> 'Data.ByteString.ByteString' -> 'Data.Attoparsec.ByteString.Result' a
-- r p s = 'feed' ('Data.Attoparsec.parse' p s) 'mempty'
-- @
--
-- Examples with behaviour identical to 'read', if you feed an empty
-- continuation to the first result:
--
-- > double "3"     == Done "" 3.0
-- > double "3.1"   == Done "" 3.1
-- > double "3e4"   == Done "" 30000.0
-- > double "3.1e4" == Done "" 31000.0
-- > double "3e"    == Done "e" 3.0
--
-- Examples with behaviour identical to 'read':
--
-- > double ".3"    == Fail ".3" _ _
-- > double "e3"    == Fail "e3" _ _
--
-- Example of difference from 'read':
--
-- > double "3.foo" == Done "foo" 3.0
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
double :: Parser Double
double = scientifically Sci.toRealFloat

-- | Parse a number, attempting to preserve both speed and precision.
--
-- The syntax accepted by this parser is the same as for 'double'.
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
  let minus = 45
      plus  = 43
  sign <- I.peekWord8'
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $
    void $ I.anyWord8

  n <- decimal

  let f fracDigits = SP (B8.foldl' step n fracDigits)
                        (negate $ B8.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48)

  dotty <- I.peekWord8
  -- '.' -> ascii 46
  SP c e <- case dotty of
              Just 46 -> I.anyWord8 *> (f <$> I.takeWhile isDigit_w8)
              _       -> pure (SP n 0)

  let !signedCoeff | positive  =  c
                   | otherwise = -c

  let littleE = 101
      bigE    = 69
  (I.satisfy (\ex -> ex == littleE || ex == bigE) *>
      fmap (h . Sci.scientific signedCoeff . (e +)) (signed decimal)) <|>
    return (h $ Sci.scientific signedCoeff    e)
