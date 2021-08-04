{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ <= 710 && __GLASGOW_HASKELL__ >= 706
-- Work around a compiler bug
{-# OPTIONS_GHC -fsimpl-tick-factor=300 #-}
#endif
-- |
-- Module:      Data.Aeson.Parser.Internal
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently and correctly parse a JSON string.  The string must be
-- encoded as UTF-8.

module Data.Aeson.Parser.Internal
    (
    -- * Lazy parsers
      json, jsonEOF
    , jsonWith
    , jsonLast
    , jsonAccum
    , jsonNoDup
    , value
    , jstring
    , jstring_
    , scientific
    -- * Strict parsers
    , json', jsonEOF'
    , jsonWith'
    , jsonLast'
    , jsonAccum'
    , jsonNoDup'
    , value'
    -- * Helpers
    , decodeWith
    , decodeStrictWith
    , eitherDecodeWith
    , eitherDecodeStrictWith
    -- ** Handling objects with duplicate keys
    , fromListAccum
    , parseListNoDup
    ) where

import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Aeson.Types.Internal (IResult(..), JSONPath, Object, Result(..), Value(..))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfInput, isDigit_w8, signed, string)
import Data.Function (fix)
import Data.Functor.Compat (($>))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as Vector (empty, fromList, fromListN, reverse)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific as Sci
import Data.Aeson.Parser.Unescape (unescapeText)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson.Types

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

-- | Parse any JSON value.
--
-- The conversion of a parsed value to a Haskell value is deferred
-- until the Haskell value is needed.  This may improve performance if
-- only a subset of the results of conversions are needed, but at a
-- cost in thunk allocation.
--
-- This function is an alias for 'value'. In aeson 0.8 and earlier, it
-- parsed only object or array types, in conformance with the
-- now-obsolete RFC 4627.
--
-- ==== Warning
--
-- If an object contains duplicate keys, only the first one will be kept.
-- For a more flexible alternative, see 'jsonWith'.
json :: Parser Value
json = value

-- | Parse any JSON value.
--
-- This is a strict version of 'json' which avoids building up thunks
-- during parsing; it performs all conversions immediately.  Prefer
-- this version if most of the JSON data needs to be accessed.
--
-- This function is an alias for 'value''. In aeson 0.8 and earlier, it
-- parsed only object or array types, in conformance with the
-- now-obsolete RFC 4627.
--
-- ==== Warning
--
-- If an object contains duplicate keys, only the first one will be kept.
-- For a more flexible alternative, see 'jsonWith''.
json' :: Parser Value
json' = value'

-- Open recursion: object_, object_', array_, array_' are parameterized by the
-- toplevel Value parser to be called recursively, to keep the parameter
-- mkObject outside of the recursive loop for proper inlining.

object_ :: ([(Text, Value)] -> Either String Object) -> Parser Value -> Parser Value
object_ mkObject val = {-# SCC "object_" #-} Object <$> objectValues mkObject jstring val
{-# INLINE object_ #-}

object_' :: ([(Text, Value)] -> Either String Object) -> Parser Value -> Parser Value
object_' mkObject val' = {-# SCC "object_'" #-} do
  !vals <- objectValues mkObject jstring' val'
  return (Object vals)
 where
  jstring' = do
    !s <- jstring
    return s
{-# INLINE object_' #-}

objectValues :: ([(Text, Value)] -> Either String Object)
             -> Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues mkObject str val = do
  skipSpace
  w <- A.peekWord8'
  if w == CLOSE_CURLY
    then A.anyWord8 >> return H.empty
    else loop []
 where
  -- Why use acc pattern here, you may ask? because 'H.fromList' use 'unsafeInsert'
  -- and it's much faster because it's doing in place update to the 'HashMap'!
  loop acc = do
    k <- (str A.<?> "object key") <* skipSpace <* (char ':' A.<?> "':'")
    v <- (val A.<?> "object value") <* skipSpace
    ch <- A.satisfy (\w -> w == COMMA || w == CLOSE_CURLY) A.<?> "',' or '}'"
    let acc' = (k, v) : acc
    if ch == COMMA
      then skipSpace >> loop acc'
      else case mkObject acc' of
        Left err -> fail err
        Right obj -> pure obj
{-# INLINE objectValues #-}

array_ :: Parser Value -> Parser Value
array_ val = {-# SCC "array_" #-} Array <$> arrayValues val
{-# INLINE array_ #-}

array_' :: Parser Value -> Parser Value
array_' val = {-# SCC "array_'" #-} do
  !vals <- arrayValues val
  return (Array vals)
{-# INLINE array_' #-}

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  skipSpace
  w <- A.peekWord8'
  if w == CLOSE_SQUARE
    then A.anyWord8 >> return Vector.empty
    else loop [] 1
  where
    loop acc !len = do
      v <- (val A.<?> "json list value") <* skipSpace
      ch <- A.satisfy (\w -> w == COMMA || w == CLOSE_SQUARE) A.<?> "',' or ']'"
      if ch == COMMA
        then skipSpace >> loop (v:acc) (len+1)
        else return (Vector.reverse (Vector.fromListN len (v:acc)))
{-# INLINE arrayValues #-}

-- | Parse any JSON value. Synonym of 'json'.
value :: Parser Value
value = jsonWith (pure . H.fromList)

-- | Parse any JSON value.
--
-- This parser is parameterized by a function to construct an 'Object'
-- from a raw list of key-value pairs, where duplicates are preserved.
-- The pairs appear in __reverse order__ from the source.
--
-- ==== __Examples__
--
-- 'json' keeps only the first occurence of each key, using 'HashMap.Lazy.fromList'.
--
-- @
-- 'json' = 'jsonWith' ('Right' '.' 'H.fromList')
-- @
--
-- 'jsonLast' keeps the last occurence of each key, using
-- @'HashMap.Lazy.fromListWith' ('const' 'id')@.
--
-- @
-- 'jsonLast' = 'jsonWith' ('Right' '.' 'HashMap.Lazy.fromListWith' ('const' 'id'))
-- @
--
-- 'jsonAccum' keeps wraps all values in arrays to keep duplicates, using
-- 'fromListAccum'.
--
-- @
-- 'jsonAccum' = 'jsonWith' ('Right' . 'fromListAccum')
-- @
--
-- 'jsonNoDup' fails if any object contains duplicate keys, using 'parseListNoDup'.
--
-- @
-- 'jsonNoDup' = 'jsonWith' 'parseListNoDup'
-- @
jsonWith :: ([(Text, Value)] -> Either String Object) -> Parser Value
jsonWith mkObject = fix $ \value_ -> do
  skipSpace
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE  -> A.anyWord8 *> (String <$> jstring_)
    OPEN_CURLY    -> A.anyWord8 *> object_ mkObject value_
    OPEN_SQUARE   -> A.anyWord8 *> array_ value_
    C_f           -> string "false" $> Bool False
    C_t           -> string "true" $> Bool True
    C_n           -> string "null" $> Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> Number <$> scientific
      | otherwise -> fail "not a valid json value"
{-# INLINE jsonWith #-}

-- | Variant of 'json' which keeps only the last occurence of every key.
jsonLast :: Parser Value
jsonLast = jsonWith (Right . H.fromListWith (const id))

-- | Variant of 'json' wrapping all object mappings in 'Array' to preserve
-- key-value pairs with the same keys.
jsonAccum :: Parser Value
jsonAccum = jsonWith (Right . fromListAccum)

-- | Variant of 'json' which fails if any object contains duplicate keys.
jsonNoDup :: Parser Value
jsonNoDup = jsonWith parseListNoDup

-- | @'fromListAccum' kvs@ is an object mapping keys to arrays containing all
-- associated values from the original list @kvs@.
--
-- >>> fromListAccum [("apple", Bool True), ("apple", Bool False), ("orange", Bool False)]
-- fromList [("apple",Array [Bool False,Bool True]),("orange",Array [Bool False])]
fromListAccum :: [(Text, Value)] -> Object
fromListAccum =
  fmap (Array . Vector.fromList . ($ [])) . H.fromListWith (.) . (fmap . fmap) (:)

-- | @'fromListNoDup' kvs@ fails if @kvs@ contains duplicate keys.
parseListNoDup :: [(Text, Value)] -> Either String Object
parseListNoDup =
  H.traverseWithKey unwrap . H.fromListWith (\_ _ -> Nothing) . (fmap . fmap) Just
  where
    unwrap k Nothing = Left $ "found duplicate key: " ++ show k
    unwrap _ (Just v) = Right v

-- | Strict version of 'value'. Synonym of 'json''.
value' :: Parser Value
value' = jsonWith' (pure . H.fromList)

-- | Strict version of 'jsonWith'.
jsonWith' :: ([(Text, Value)] -> Either String Object) -> Parser Value
jsonWith' mkObject = fix $ \value_ -> do
  skipSpace
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE  -> do
                     !s <- A.anyWord8 *> jstring_
                     return (String s)
    OPEN_CURLY    -> A.anyWord8 *> object_' mkObject value_
    OPEN_SQUARE   -> A.anyWord8 *> array_' value_
    C_f           -> string "false" $> Bool False
    C_t           -> string "true" $> Bool True
    C_n           -> string "null" $> Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> do
                     !n <- scientific
                     return (Number n)
      | otherwise -> fail "not a valid json value"
{-# INLINE jsonWith' #-}

-- | Variant of 'json'' which keeps only the last occurence of every key.
jsonLast' :: Parser Value
jsonLast' = jsonWith' (pure . H.fromListWith (const id))

-- | Variant of 'json'' wrapping all object mappings in 'Array' to preserve
-- key-value pairs with the same keys.
jsonAccum' :: Parser Value
jsonAccum' = jsonWith' (pure . fromListAccum)

-- | Variant of 'json'' which fails if any object contains duplicate keys.
jsonNoDup' :: Parser Value
jsonNoDup' = jsonWith' parseListNoDup

-- | Parse a quoted JSON string.
jstring :: Parser Text
jstring = A.word8 DOUBLE_QUOTE *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Parser Text
{-# INLINE jstring_ #-}
jstring_ = do
  -- not sure whether >= or bit hackery is faster
  -- perfectly, we shouldn't care, it's compiler job.
  s <- A.takeWhile (\w -> w /= DOUBLE_QUOTE && w /= BACKSLASH && w >= 0x20 && w < 0x80)
  let txt = unsafeDecodeASCII s
  mw <- A.peekWord8
  case mw of
    Nothing           -> fail "string without end"
    Just DOUBLE_QUOTE -> A.anyWord8 $> txt
    Just w | w < 0x20 -> fail "unescaped control character"
    _                 -> jstringSlow s

-- | The input is assumed to contain only 7bit ASCII characters (i.e. @< 0x80@).
--   We use TE.decodeLatin1 here because TE.decodeASCII is currently (text-1.2.4.0)
--   deprecated and equal to TE.decodeUtf8, which is slower than TE.decodeLatin1.
unsafeDecodeASCII :: B.ByteString -> Text
unsafeDecodeASCII = TE.decodeLatin1

jstringSlow :: B.ByteString -> Parser Text
{-# INLINE jstringSlow #-}
jstringSlow s' = {-# SCC "jstringSlow" #-} do
  s <- A.scan startState go <* A.anyWord8
  case unescapeText (B.append s' s) of
    Right r  -> return r
    Left err -> fail $ show err
 where
    startState              = False
    go a c
      | a                  = Just False
      | c == DOUBLE_QUOTE  = Nothing
      | otherwise = let a' = c == backslash
                    in Just a'
      where backslash = BACKSLASH

decodeWith :: Parser Value -> (Value -> Result a) -> L.ByteString -> Maybe a
decodeWith p to s =
    case L.parse p s of
      L.Done _ v -> case to v of
                      Success a -> Just a
                      _         -> Nothing
      _          -> Nothing
{-# INLINE decodeWith #-}

decodeStrictWith :: Parser Value -> (Value -> Result a) -> B.ByteString
                 -> Maybe a
decodeStrictWith p to s =
    case either Error to (A.parseOnly p s) of
      Success a -> Just a
      _         -> Nothing
{-# INLINE decodeStrictWith #-}

eitherDecodeWith :: Parser Value -> (Value -> IResult a) -> L.ByteString
                 -> Either (JSONPath, String) a
eitherDecodeWith p to s =
    case L.parse p s of
      L.Done _ v     -> case to v of
                          ISuccess a      -> Right a
                          IError path msg -> Left (path, msg)
      L.Fail notparsed ctx msg -> Left ([], buildMsg notparsed ctx msg)
  where
    buildMsg :: L.ByteString -> [String] -> String -> String
    buildMsg notYetParsed [] msg = msg ++ formatErrorLine notYetParsed
    buildMsg notYetParsed (expectation:_) msg =
      msg ++ ". Expecting " ++ expectation ++ formatErrorLine notYetParsed
{-# INLINE eitherDecodeWith #-}

-- | Grab the first 100 bytes from the non parsed portion and
-- format to get nicer error messages
formatErrorLine :: L.ByteString -> String
formatErrorLine bs =
  C.unpack .
  -- if formatting results in empty ByteString just return that
  -- otherwise construct the error message with the bytestring builder
  (\bs' ->
     if BSL.null bs'
       then BSL.empty
       else
         B.toLazyByteString $
         B.stringUtf8 " at '" <> B.lazyByteString bs' <> B.stringUtf8 "'"
  ) .
  -- if newline is present cut at that position
  BSL.takeWhile (10 /=) .
  -- remove spaces, CR's, tabs, backslashes and quotes characters
  BSL.filter (`notElem` [9, 13, 32, 34, 47, 92]) .
  -- take 100 bytes
  BSL.take 100 $ bs

eitherDecodeStrictWith :: Parser Value -> (Value -> IResult a) -> B.ByteString
                       -> Either (JSONPath, String) a
eitherDecodeStrictWith p to s =
    case either (IError []) to (A.parseOnly p s) of
      ISuccess a      -> Right a
      IError path msg -> Left (path, msg)
{-# INLINE eitherDecodeStrictWith #-}

-- $lazy
--
-- The 'json' and 'value' parsers decouple identification from
-- conversion.  Identification occurs immediately (so that an invalid
-- JSON document can be rejected as early as possible), but conversion
-- to a Haskell value is deferred until that value is needed.
--
-- This decoupling can be time-efficient if only a smallish subset of
-- elements in a JSON value need to be inspected, since the cost of
-- conversion is zero for uninspected elements.  The trade off is an
-- increase in memory usage, due to allocation of thunks for values
-- that have not yet been converted.

-- $strict
--
-- The 'json'' and 'value'' parsers combine identification with
-- conversion.  They consume more CPU cycles up front, but have a
-- smaller memory footprint.

-- | Parse a top-level JSON value followed by optional whitespace and
-- end-of-input.  See also: 'json'.
jsonEOF :: Parser Value
jsonEOF = json <* skipSpace <* endOfInput

-- | Parse a top-level JSON value followed by optional whitespace and
-- end-of-input.  See also: 'json''.
jsonEOF' :: Parser Value
jsonEOF' = json' <* skipSpace <* endOfInput

-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpace :: Parser ()
skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

------------------ Copy-pasted and adapted from attoparsec ------------------

-- A strict pair
data SP = SP !Integer {-# UNPACK #-}!Int

decimal0 :: Parser Integer
decimal0 = do
  let zero = 48
  digits <- A.takeWhile1 isDigit_w8
  if B.length digits > 1 && B.unsafeHead digits == zero
    then fail "leading zero"
    else return (bsToInteger digits)

-- | Parse a JSON number.
scientific :: Parser Scientific
scientific = do
  let minus = 45
      plus  = 43
  sign <- A.peekWord8'
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $
    void A.anyWord8

  n <- decimal0

  let f fracDigits = SP (B.foldl' step n fracDigits)
                        (negate $ B.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48)

  dotty <- A.peekWord8
  -- '.' -> ascii 46
  SP c e <- case dotty of
              Just 46 -> A.anyWord8 *> (f <$> A.takeWhile1 isDigit_w8)
              _       -> pure (SP n 0)

  let !signedCoeff | positive  =  c
                   | otherwise = -c

  let littleE = 101
      bigE    = 69
  (A.satisfy (\ex -> ex == littleE || ex == bigE) *>
      fmap (Sci.scientific signedCoeff . (e +)) (signed decimal)) <|>
    return (Sci.scientific signedCoeff    e)
{-# INLINE scientific #-}

------------------ Copy-pasted and adapted from base ------------------------

bsToInteger :: B.ByteString -> Integer
bsToInteger bs
    | l > 40    = valInteger 10 l [ fromIntegral (w - 48) | w <- B.unpack bs ]
    | otherwise = bsToIntegerSimple bs
  where
    l = B.length bs

bsToIntegerSimple :: B.ByteString -> Integer
bsToIntegerSimple = B.foldl' step 0 where
  step a b = a * 10 + fromIntegral (b - 48) -- 48 = '0'

-- A sub-quadratic algorithm for Integer. Pairs of adjacent radix b
-- digits are combined into a single radix b^2 digit. This process is
-- repeated until we are left with a single digit. This algorithm
-- performs well only on large inputs, so we use the simple algorithm
-- for smaller inputs.
valInteger :: Integer -> Int -> [Integer] -> Integer
valInteger = go
  where
    go :: Integer -> Int -> [Integer] -> Integer
    go _ _ []  = 0
    go _ _ [d] = d
    go b l ds
        | l > 40 = b' `seq` go b' l' (combine b ds')
        | otherwise = valSimple b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else 0 : ds
        b' = b * b
        l' = (l + 1) `quot` 2

    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ []  = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"

-- The following algorithm is only linear for types whose Num operations
-- are in constant time.
valSimple :: Integer -> [Integer] -> Integer
valSimple base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d
