{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Aeson
    (
      aeson
    , value'
    ) where

import Data.ByteString.Builder
  (Builder, byteString, toLazyByteString, charUtf8, word8)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), (<$>), (<*), pure)
import Data.Monoid (mappend, mempty)
#endif

import Common (pathTo)
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
import Control.Monad (forM)
import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfInput, scientific,
                                         skipSpace, string)
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.List (sort)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Vector as Vector (Vector, foldl', fromList)
import Data.Word (Word8)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), dropExtension)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as L
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B
import qualified Data.HashMap.Strict as H
import Criterion.Main

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

data Result a = Error String
              | Success a
                deriving (Eq, Show)


-- | A JSON \"object\" (key\/value map).
type Object = H.HashMap Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
             deriving (Eq, Show)

instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = Vector.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = rnf n
    rnf (Bool b)   = rnf b
    rnf Null       = ()

-- | Parse a top-level JSON value.  This must be either an object or
-- an array, per RFC 4627.
--
-- The conversion of a parsed value to a Haskell value is deferred
-- until the Haskell value is needed.  This may improve performance if
-- only a subset of the results of conversions are needed, but at a
-- cost in thunk allocation.
json :: Parser Value
json = json_ object_ array_

-- | Parse a top-level JSON value.  This must be either an object or
-- an array, per RFC 4627.
--
-- This is a strict version of 'json' which avoids building up thunks
-- during parsing; it performs all conversions immediately.  Prefer
-- this version if most of the JSON data needs to be accessed.
json' :: Parser Value
json' = json_ object_' array_'

json_ :: Parser Value -> Parser Value -> Parser Value
json_ obj ary = do
  w <- skipSpace *> A.satisfy (\w -> w == OPEN_CURLY || w == OPEN_SQUARE)
  if w == OPEN_CURLY
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: Parser Value
object_ = {-# SCC "object_" #-} Object <$> objectValues jstring value

object_' :: Parser Value
object_' = {-# SCC "object_'" #-} do
  !vals <- objectValues jstring' value'
  return (Object vals)
 where
  jstring' = do
    !s <- jstring
    return s

objectValues :: Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues str val = do
  skipSpace
  let pair = liftA2 (,) (str <* skipSpace) (char ':' *> skipSpace *> val)
  H.fromList <$> commaSeparated pair CLOSE_CURLY
{-# INLINE objectValues #-}

array_ :: Parser Value
array_ = {-# SCC "array_" #-} Array <$> arrayValues value

array_' :: Parser Value
array_' = {-# SCC "array_'" #-} do
  !vals <- arrayValues value'
  return (Array vals)

commaSeparated :: Parser a -> Word8 -> Parser [a]
commaSeparated item endByte = do
  w <- A.peekWord8'
  if w == endByte
    then A.anyWord8 >> return []
    else loop
  where
    loop = do
      v <- item <* skipSpace
      ch <- A.satisfy $ \w -> w == COMMA || w == endByte
      if ch == COMMA
        then skipSpace >> (v:) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  skipSpace
  Vector.fromList <$> commaSeparated val CLOSE_SQUARE
{-# INLINE arrayValues #-}

-- | Parse any JSON value.  You should usually 'json' in preference to
-- this function, as this function relaxes the object-or-array
-- requirement of RFC 4627.
--
-- In particular, be careful in using this function if you think your
-- code might interoperate with Javascript.  A na&#xef;ve Javascript
-- library that parses JSON data using @eval@ is vulnerable to attack
-- unless the encoded data represents an object or an array.  JSON
-- implementations in other languages conform to that same restriction
-- to preserve interoperability and security.
value :: Parser Value
value = do
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE  -> A.anyWord8 *> (String <$> jstring_)
    OPEN_CURLY    -> A.anyWord8 *> object_
    OPEN_SQUARE   -> A.anyWord8 *> array_
    C_f           -> string "false" *> pure (Bool False)
    C_t           -> string "true" *> pure (Bool True)
    C_n           -> string "null" *> pure Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> Number <$> scientific
      | otherwise -> fail "not a valid json value"

-- | Strict version of 'value'. See also 'json''.
value' :: Parser Value
value' = do
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE  -> do
                     !s <- A.anyWord8 *> jstring_
                     return (String s)
    OPEN_CURLY    -> A.anyWord8 *> object_'
    OPEN_SQUARE   -> A.anyWord8 *> array_'
    C_f           -> string "false" *> pure (Bool False)
    C_t           -> string "true" *> pure (Bool True)
    C_n           -> string "null" *> pure Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> do
                     !n <- scientific
                     return (Number n)
      | otherwise -> fail "not a valid json value"

-- | Parse a quoted JSON string.
jstring :: Parser Text
jstring = A.word8 DOUBLE_QUOTE *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Parser Text
jstring_ = {-# SCC "jstring_" #-} do
  s <- A.scan False $ \s c -> if s then Just False
                                   else if c == DOUBLE_QUOTE
                                        then Nothing
                                        else Just (c == BACKSLASH)
  _ <- A.word8 DOUBLE_QUOTE
  s1 <- if BACKSLASH `B.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
         else return s

  case decodeUtf8' s1 of
      Right r  -> return r
      Left err -> fail $ show err

{-# INLINE jstring_ #-}

unescape :: Z.Parser ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/=BACKSLASH)
    let rest = do
          start <- Z.take 2
          let !slash = B.unsafeHead start
              !t = B.unsafeIndex start 1
              escape = case B.findIndex (==t) "\"\\/ntbrfu" of
                         Just i -> i
                         _      -> 255
          if slash /= BACKSLASH || escape == 255
            then fail "invalid JSON escape sequence"
            else do
            let cont m = go (acc `mappend` byteString h `mappend` m)
                {-# INLINE cont #-}
            if t /= 117 -- 'u'
              then cont (word8 (B.unsafeIndex mapping escape))
              else do
                   a <- hexQuad
                   if a < 0xd800 || a > 0xdfff
                     then cont (charUtf8 (chr a))
                     else do
                       b <- Z.string "\\u" *> hexQuad
                       if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
                         then let !c = ((a - 0xd800) `shiftL` 10) +
                                       (b - 0xdc00) + 0x10000
                              in cont (charUtf8 (chr c))
                         else fail "invalid UTF-16 surrogates"
    done <- Z.atEnd
    if done
      then return (acc `mappend` byteString h)
      else rest
  mapping = "\"\\/\n\t\b\r\f"

hexQuad :: Z.Parser Int
hexQuad = do
  s <- Z.take 4
  let hex n | w >= C_0 && w <= C_9 = w - C_0
            | w >= C_a && w <= C_f = w - 87
            | w >= C_A && w <= C_F = w - 55
            | otherwise          = 255
        where w = fromIntegral $ B.unsafeIndex s n
      a = hex 0; b = hex 1; c = hex 2; d = hex 3
  if (a .|. b .|. c .|. d) /= 255
    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
    else fail "invalid hex escape"

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
      Error _ -> Nothing
{-# INLINE decodeStrictWith #-}

eitherDecodeWith :: Parser Value -> (Value -> Result a) -> L.ByteString
                 -> Either String a
eitherDecodeWith p to s =
    case L.parse p s of
      L.Done _ v -> case to v of
                      Success a -> Right a
                      Error msg -> Left msg
      L.Fail _ _ msg -> Left msg
{-# INLINE eitherDecodeWith #-}

eitherDecodeStrictWith :: Parser Value -> (Value -> Result a) -> B.ByteString
                       -> Either String a
eitherDecodeStrictWith p to s =
    case either Error to (A.parseOnly p s) of
      Success a -> Right a
      Error msg -> Left msg
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

toByteString :: Builder -> ByteString
toByteString = L.toStrict . toLazyByteString
{-# INLINE toByteString #-}

aeson :: IO Benchmark
aeson = do
  path <- pathTo "json-data"
  names <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents path
  benches <- forM names $ \name -> do
    bs <- B.readFile (path </> name)
    return . bench (dropExtension name) $ nf (A.parseOnly jsonEOF') bs
  return $ bgroup "aeson" benches
