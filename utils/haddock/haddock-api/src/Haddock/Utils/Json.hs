{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Minimal JSON / RFC 7159 support
--
-- The API is heavily inspired by @aeson@'s API but puts emphasis on
-- simplicity rather than performance. The 'ToJSON' instances are
-- intended to have an encoding compatible with @aeson@'s encoding.
module Haddock.Utils.Json
  ( Value (..)
  , Object
  , object
  , Pair
  , (.=)
  , encodeToString
  , encodeToBuilder
  , ToJSON (toJSON)
  , Parser (..)
  , Result (..)
  , FromJSON (parseJSON)
  , withObject
  , withArray
  , withString
  , withDouble
  , withBool
  , fromJSON
  , parse
  , parseEither
  , (.:)
  , (.:?)
  , decode
  , decodeWith
  , eitherDecode
  , eitherDecodeWith
  , decodeFile
  , eitherDecodeFile
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), zipWithM, (>=>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Fail as Fail
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Int
import Data.List (intersperse)
import Data.Monoid
import Data.Word
import GHC.Natural
import qualified Text.Parsec.ByteString.Lazy as Parsec.Lazy
import qualified Text.ParserCombinators.Parsec as Parsec

import Haddock.Utils.Json.Parser
import Haddock.Utils.Json.Types

infixr 8 .=

-- | A key-value pair for encoding a JSON object.
(.=) :: ToJSON v => String -> v -> Pair
k .= v = (k, toJSON v)

-- | A type that can be converted to JSON.
class ToJSON a where
  -- | Convert a Haskell value to a JSON-friendly intermediate type.
  toJSON :: a -> Value

instance ToJSON () where
  toJSON () = Array []

instance ToJSON Value where
  toJSON = id

instance ToJSON Bool where
  toJSON = Bool

instance ToJSON a => ToJSON [a] where
  toJSON = Array . map toJSON

instance ToJSON a => ToJSON (Maybe a) where
  toJSON Nothing = Null
  toJSON (Just a) = toJSON a

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
  toJSON (a, b) = Array [toJSON a, toJSON b]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
  toJSON (a, b, c) = Array [toJSON a, toJSON b, toJSON c]

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where
  toJSON (a, b, c, d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]

instance ToJSON Float where
  toJSON = Number . realToFrac

instance ToJSON Double where
  toJSON = Number

instance ToJSON Int where toJSON = Number . realToFrac
instance ToJSON Int8 where toJSON = Number . realToFrac
instance ToJSON Int16 where toJSON = Number . realToFrac
instance ToJSON Int32 where toJSON = Number . realToFrac

instance ToJSON Word where toJSON = Number . realToFrac
instance ToJSON Word8 where toJSON = Number . realToFrac
instance ToJSON Word16 where toJSON = Number . realToFrac
instance ToJSON Word32 where toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Int64 where toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Word64 where toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Integer where toJSON = Number . fromInteger

------------------------------------------------------------------------------
-- 'BB.Builder'-based encoding

-- | Serialise value as JSON/UTF8-encoded 'Builder'
encodeToBuilder :: ToJSON a => a -> Builder
encodeToBuilder = encodeValueBB . toJSON

encodeValueBB :: Value -> Builder
encodeValueBB jv = case jv of
  Bool True -> "true"
  Bool False -> "false"
  Null -> "null"
  Number n
    | isNaN n || isInfinite n -> encodeValueBB Null
    | Just i <- doubleToInt64 n -> BB.int64Dec i
    | otherwise -> BB.doubleDec n
  Array a -> encodeArrayBB a
  String s -> encodeStringBB s
  Object o -> encodeObjectBB o

encodeArrayBB :: [Value] -> Builder
encodeArrayBB [] = "[]"
encodeArrayBB jvs = BB.char8 '[' <> go jvs <> BB.char8 ']'
  where
    go = Data.Monoid.mconcat . intersperse (BB.char8 ',') . map encodeValueBB

encodeObjectBB :: Object -> Builder
encodeObjectBB [] = "{}"
encodeObjectBB jvs = BB.char8 '{' <> go jvs <> BB.char8 '}'
  where
    go = Data.Monoid.mconcat . intersperse (BB.char8 ',') . map encPair
    encPair (l, x) = encodeStringBB l <> BB.char8 ':' <> encodeValueBB x

encodeStringBB :: String -> Builder
encodeStringBB str = BB.char8 '"' <> go str <> BB.char8 '"'
  where
    go = BB.stringUtf8 . escapeString

------------------------------------------------------------------------------
-- 'String'-based encoding

-- | Serialise value as JSON-encoded Unicode 'String'
encodeToString :: ToJSON a => a -> String
encodeToString jv = encodeValue (toJSON jv) []

encodeValue :: Value -> ShowS
encodeValue jv = case jv of
  Bool b -> showString (if b then "true" else "false")
  Null -> showString "null"
  Number n
    | isNaN n || isInfinite n -> encodeValue Null
    | Just i <- doubleToInt64 n -> shows i
    | otherwise -> shows n
  Array a -> encodeArray a
  String s -> encodeString s
  Object o -> encodeObject o

encodeArray :: [Value] -> ShowS
encodeArray [] = showString "[]"
encodeArray jvs = ('[' :) . go jvs . (']' :)
  where
    go [] = id
    go [x] = encodeValue x
    go (x : xs) = encodeValue x . (',' :) . go xs

encodeObject :: Object -> ShowS
encodeObject [] = showString "{}"
encodeObject jvs = ('{' :) . go jvs . ('}' :)
  where
    go [] = id
    go [(l, x)] = encodeString l . (':' :) . encodeValue x
    go ((l, x) : lxs) = encodeString l . (':' :) . encodeValue x . (',' :) . go lxs

encodeString :: String -> ShowS
encodeString str = ('"' :) . showString (escapeString str) . ('"' :)

------------------------------------------------------------------------------
-- helpers

-- | Try to convert 'Double' into 'Int64', return 'Nothing' if not
-- representable loss-free as integral 'Int64' value.
doubleToInt64 :: Double -> Maybe Int64
doubleToInt64 x
  | fromInteger x' == x
  , x' <= toInteger (maxBound :: Int64)
  , x' >= toInteger (minBound :: Int64) =
      Just (fromIntegral x')
  | otherwise = Nothing
  where
    x' = round x

-- | Minimally escape a 'String' in accordance with RFC 7159, "7. Strings"
escapeString :: String -> String
escapeString s
  | not (any needsEscape s) = s
  | otherwise = escape s
  where
    escape [] = []
    escape (x : xs) = case x of
      '\\' -> '\\' : '\\' : escape xs
      '"' -> '\\' : '"' : escape xs
      '\b' -> '\\' : 'b' : escape xs
      '\f' -> '\\' : 'f' : escape xs
      '\n' -> '\\' : 'n' : escape xs
      '\r' -> '\\' : 'r' : escape xs
      '\t' -> '\\' : 't' : escape xs
      c
        | ord c < 0x10 -> '\\' : 'u' : '0' : '0' : '0' : intToDigit (ord c) : escape xs
        | ord c < 0x20 -> '\\' : 'u' : '0' : '0' : '1' : intToDigit (ord c - 0x10) : escape xs
        | otherwise -> c : escape xs

    -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    needsEscape c = ord c < 0x20 || c `elem` ['\\', '"']

------------------------------------------------------------------------------
-- FromJSON

-- | Elements of a JSON path used to describe the location of an
-- error.
data JSONPathElement
  = -- | JSON path element of a key into an object,
    -- \"object.key\".
    Key String
  | -- | JSON path element of an index into an
    -- array, \"array[index]\".
    Index !Int
  deriving (Eq, Show, Ord)

type JSONPath = [JSONPathElement]

-- | Failure continuation.
type Failure f r = JSONPath -> String -> f r

-- | Success continuation.
type Success a f r = a -> f r

newtype Parser a = Parser
  { runParser
      :: forall f r
       . JSONPath
      -> Failure f r
      -> Success a f r
      -> f r
  }

modifyFailure :: (String -> String) -> Parser a -> Parser a
modifyFailure f (Parser p) = Parser $ \path kf ks ->
  p path (\p' m -> kf p' (f m)) ks

prependFailure :: String -> Parser a -> Parser a
prependFailure = modifyFailure . (++)

prependContext :: String -> Parser a -> Parser a
prependContext name = prependFailure ("parsing " ++ name ++ " failed, ")

typeMismatch :: String -> Value -> Parser a
typeMismatch expected actual =
  fail $ "expected " ++ expected ++ ", but encountered " ++ typeOf actual

instance Monad.Monad Parser where
  m >>= g = Parser $ \path kf ks ->
    runParser
      m
      path
      kf
      (\a -> runParser (g a) path kf ks)
  return = pure

instance Fail.MonadFail Parser where
  fail msg = Parser $ \path kf _ks -> kf (reverse path) msg

instance Functor Parser where
  fmap f m = Parser $ \path kf ks ->
    let ks' a = ks (f a)
     in runParser m path kf ks'

instance Applicative Parser where
  pure a = Parser $ \_path _kf ks -> ks a
  (<*>) = apP

instance Alternative Parser where
  empty = fail "empty"
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = fail "mzero"
  mplus a b = Parser $ \path kf ks ->
    runParser a path (\_ _ -> runParser b path kf ks) ks

instance Semigroup (Parser a) where
  (<>) = mplus

instance Monoid (Parser a) where
  mempty = fail "mempty"
  mappend = (<>)

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  b <$> e

(<?>) :: Parser a -> JSONPathElement -> Parser a
p <?> pathElem = Parser $ \path kf ks -> runParser p (pathElem : path) kf ks

parseIndexedJSON :: (Value -> Parser a) -> Int -> Value -> Parser a
parseIndexedJSON p idx value = p value <?> Index idx

unexpected :: Value -> Parser a
unexpected actual = fail $ "unexpected " ++ typeOf actual

withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _ f (Object obj) = f obj
withObject name _ v = prependContext name (typeMismatch "Object" v)

withArray :: String -> ([Value] -> Parser a) -> Value -> Parser a
withArray _ f (Array arr) = f arr
withArray name _ v = prependContext name (typeMismatch "Array" v)

withString :: String -> (String -> Parser a) -> Value -> Parser a
withString _ f (String txt) = f txt
withString name _ v = prependContext name (typeMismatch "String" v)

withDouble :: String -> (Double -> Parser a) -> Value -> Parser a
withDouble _ f (Number duble) = f duble
withDouble name _ v = prependContext name (typeMismatch "Number" v)

withBool :: String -> (Bool -> Parser a) -> Value -> Parser a
withBool _ f (Bool arr) = f arr
withBool name _ v = prependContext name (typeMismatch "Boolean" v)

class FromJSON a where
  parseJSON :: Value -> Parser a

  parseJSONList :: Value -> Parser [a]
  parseJSONList = withArray "[]" (zipWithM (parseIndexedJSON parseJSON) [0 ..])

instance FromJSON Bool where
  parseJSON (Bool b) = pure b
  parseJSON v = typeMismatch "Bool" v

instance FromJSON () where
  parseJSON =
    withArray "()" $ \v ->
      if null v
        then pure ()
        else prependContext "()" $ fail "expected an empty array"

instance FromJSON Char where
  parseJSON = withString "Char" parseChar

  parseJSONList (String s) = pure s
  parseJSONList v = typeMismatch "String" v

parseChar :: String -> Parser Char
parseChar t =
  if length t == 1
    then pure $ head t
    else prependContext "Char" $ fail "expected a string of length 1"

parseRealFloat :: RealFloat a => String -> Value -> Parser a
parseRealFloat _ (Number s) = pure $ realToFrac s
parseRealFloat _ Null = pure (0 / 0)
parseRealFloat name v = prependContext name (unexpected v)

instance FromJSON Double where
  parseJSON = parseRealFloat "Double"

instance FromJSON Float where
  parseJSON = parseRealFloat "Float"

parseNatural :: Integer -> Parser Natural
parseNatural integer =
  if integer < 0
    then fail $ "parsing Natural failed, unexpected negative number " <> show integer
    else pure $ fromIntegral integer

parseIntegralFromDouble :: Integral a => Double -> Parser a
parseIntegralFromDouble d =
  let r = toRational d
      x = truncate r
   in if toRational x == r
        then pure x
        else fail $ "unexpected floating number " <> show d

parseIntegral :: Integral a => String -> Value -> Parser a
parseIntegral name = withDouble name parseIntegralFromDouble

instance FromJSON Integer where
  parseJSON = parseIntegral "Integer"

instance FromJSON Natural where
  parseJSON =
    withDouble
      "Natural"
      (parseIntegralFromDouble >=> parseNatural)

instance FromJSON Int where
  parseJSON = parseIntegral "Int"

instance FromJSON Int8 where
  parseJSON = parseIntegral "Int8"

instance FromJSON Int16 where
  parseJSON = parseIntegral "Int16"

instance FromJSON Int32 where
  parseJSON = parseIntegral "Int32"

instance FromJSON Int64 where
  parseJSON = parseIntegral "Int64"

instance FromJSON Word where
  parseJSON = parseIntegral "Word"

instance FromJSON Word8 where
  parseJSON = parseIntegral "Word8"

instance FromJSON Word16 where
  parseJSON = parseIntegral "Word16"

instance FromJSON Word32 where
  parseJSON = parseIntegral "Word32"

instance FromJSON Word64 where
  parseJSON = parseIntegral "Word64"

instance FromJSON a => FromJSON [a] where
  parseJSON = parseJSONList

data Result a
  = Error String
  | Success a
  deriving (Eq, Show)

fromJSON :: FromJSON a => Value -> Result a
fromJSON = parse parseJSON

parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) [] (const Error) Success

parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) [] onError Right
  where
    onError path msg = Left (formatError path msg)

formatError :: JSONPath -> String -> String
formatError path msg = "Error in " ++ formatPath path ++ ": " ++ msg

formatPath :: JSONPath -> String
formatPath path = "$" ++ formatRelativePath path

formatRelativePath :: JSONPath -> String
formatRelativePath path = format "" path
  where
    format :: String -> JSONPath -> String
    format pfx [] = pfx
    format pfx (Index idx : parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key : parts) = format (pfx ++ formatKey key) parts

    formatKey :: String -> String
    formatKey key
      | isIdentifierKey key = "." ++ key
      | otherwise = "['" ++ escapeKey key ++ "']"

    isIdentifierKey :: String -> Bool
    isIdentifierKey [] = False
    isIdentifierKey (x : xs) = isAlpha x && all isAlphaNum xs

    escapeKey :: String -> String
    escapeKey = concatMap escapeChar

    escapeChar :: Char -> String
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

explicitParseField :: (Value -> Parser a) -> Object -> String -> Parser a
explicitParseField p obj key =
  case key `lookup` obj of
    Nothing -> fail $ "key " ++ key ++ " not found"
    Just v -> p v <?> Key key

(.:) :: FromJSON a => Object -> String -> Parser a
(.:) = explicitParseField parseJSON

explicitParseFieldMaybe :: (Value -> Parser a) -> Object -> String -> Parser (Maybe a)
explicitParseFieldMaybe p obj key =
  case key `lookup` obj of
    Nothing -> pure Nothing
    Just v -> Just <$> p v <?> Key key

(.:?) :: FromJSON a => Object -> String -> Parser (Maybe a)
(.:?) = explicitParseFieldMaybe parseJSON

decodeWith :: (Value -> Result a) -> BSL.ByteString -> Maybe a
decodeWith decoder bsl =
  case Parsec.parse parseJSONValue "<input>" bsl of
    Left _ -> Nothing
    Right json ->
      case decoder json of
        Success a -> Just a
        Error _ -> Nothing

decode :: FromJSON a => BSL.ByteString -> Maybe a
decode = decodeWith fromJSON

eitherDecodeWith :: (Value -> Result a) -> BSL.ByteString -> Either String a
eitherDecodeWith decoder bsl =
  case Parsec.parse parseJSONValue "<input>" bsl of
    Left parsecError -> Left (show parsecError)
    Right json ->
      case decoder json of
        Success a -> Right a
        Error err -> Left err

eitherDecode :: FromJSON a => BSL.ByteString -> Either String a
eitherDecode = eitherDecodeWith fromJSON

decodeFile :: FromJSON a => FilePath -> IO (Maybe a)
decodeFile filePath = do
  parsecResult <- Parsec.Lazy.parseFromFile parseJSONValue filePath
  case parsecResult of
    Right r ->
      case fromJSON r of
        Success a -> return (Just a)
        Error _ -> return Nothing
    Left _ -> return Nothing

eitherDecodeFile :: FromJSON a => FilePath -> IO (Either String a)
eitherDecodeFile filePath = do
  parsecResult <- Parsec.Lazy.parseFromFile parseJSONValue filePath
  case parsecResult of
    Right r ->
      case fromJSON r of
        Success a -> return (Right a)
        Error err -> return (Left err)
    Left err -> return $ Left (show err)
