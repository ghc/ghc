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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Word
import GHC.Natural
import qualified Text.ParserCombinators.Parsec as Parsec

import Haddock.Utils.Json.Parser
import Haddock.Utils.Json.Types

infixr 8 .=

-- | A key-value pair for encoding a JSON object.
(.=) :: ToJSON v => Text -> v -> Pair
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

instance ToJSON Text where
  toJSON = String

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

encodeStringBB :: Text -> Builder
encodeStringBB str = BB.char8 '"' <> encodeTextBB str <> BB.char8 '"'

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

-- | Minimally escape a 'Text' in accordance with RFC 7159, "7. Strings".
encodeTextBB :: Text -> Builder
encodeTextBB s
  | not (T.any needsEscape s) = BB.byteString (TE.encodeUtf8 s)
  | otherwise = T.foldr ((<>) . escapeChar) mempty s
  where
    escapeChar '\\' = BB.string8 "\\\\"
    escapeChar '"' = BB.string8 "\\\""
    escapeChar '\b' = BB.string8 "\\b"
    escapeChar '\f' = BB.string8 "\\f"
    escapeChar '\n' = BB.string8 "\\n"
    escapeChar '\r' = BB.string8 "\\r"
    escapeChar '\t' = BB.string8 "\\t"
    escapeChar c
      | ord c < 0x10 = BB.string8 "\\u000" <> BB.char8 (intToDigit (ord c))
      | ord c < 0x20 = BB.string8 "\\u001" <> BB.char8 (intToDigit (ord c - 0x10))
      | otherwise = BB.charUtf8 c

    -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    needsEscape c = ord c < 0x20 || c `elem` ['\\', '"']

------------------------------------------------------------------------------
-- FromJSON

-- | Elements of a JSON path used to describe the location of an
-- error.
data JSONPathElement
  = -- | JSON path element of a key into an object,
    -- \"object.key\".
    Key Text
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
  fail $ "expected " ++ expected ++ ", but encountered " ++ T.unpack (typeOf actual)

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
unexpected actual = fail $ "unexpected " ++ T.unpack (typeOf actual)

withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _ f (Object obj) = f obj
withObject name _ v = prependContext name (typeMismatch "Object" v)

withArray :: String -> ([Value] -> Parser a) -> Value -> Parser a
withArray _ f (Array arr) = f arr
withArray name _ v = prependContext name (typeMismatch "Array" v)

withString :: String -> (Text -> Parser a) -> Value -> Parser a
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

instance FromJSON Text where
  parseJSON (String s) = pure s
  parseJSON v = typeMismatch "Text" v

instance FromJSON Char where
  parseJSON = withString "Char" parseChar

  parseJSONList (String s) = pure (T.unpack s)
  parseJSONList v = typeMismatch "String" v

parseChar :: Text -> Parser Char
parseChar s = case T.uncons s of
  Nothing -> prependContext "Char" $ fail "expected a string of length 1, got an empty string"
  Just (c, rest)
    | T.null rest -> pure c
    | otherwise -> prependContext "Char" $ fail "expected a string of length 1, got a longer string"

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

    formatKey :: Text -> String
    formatKey key
      | isIdentifierKey key = "." ++ T.unpack key
      | otherwise = "['" ++ escapeKey key ++ "']"

    isIdentifierKey :: Text -> Bool
    isIdentifierKey t = case T.uncons t of
      Nothing -> False
      Just (x, xs) -> isAlpha x && T.all isAlphaNum xs

    escapeKey :: Text -> String
    escapeKey = concatMap escapeChar . T.unpack

    escapeChar :: Char -> String
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

explicitParseField :: (Value -> Parser a) -> Object -> Text -> Parser a
explicitParseField p obj key =
  case lookup key obj of
    Nothing -> fail $ "key " ++ T.unpack key ++ " not found"
    Just v -> p v <?> Key key

(.:) :: FromJSON a => Object -> Text -> Parser a
(.:) = explicitParseField parseJSON

explicitParseFieldMaybe :: (Value -> Parser a) -> Object -> Text -> Parser (Maybe a)
explicitParseFieldMaybe p obj key =
  case lookup key obj of
    Nothing -> pure Nothing
    Just v -> Just <$> p v <?> Key key

(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) = explicitParseFieldMaybe parseJSON

decodeWith :: (Value -> Result a) -> BSL.ByteString -> Maybe a
decodeWith decoder bsl =
  case TLE.decodeUtf8' bsl of
    Left _ -> Nothing
    Right txt ->
      case Parsec.parse parseJSONValue "<input>" (TL.toStrict txt) of
        Left _ -> Nothing
        Right json ->
          case decoder json of
            Success a -> Just a
            Error _ -> Nothing

decode :: FromJSON a => BSL.ByteString -> Maybe a
decode = decodeWith fromJSON

eitherDecodeWith :: (Value -> Result a) -> BSL.ByteString -> Either String a
eitherDecodeWith decoder bsl =
  case TLE.decodeUtf8' bsl of
    Left err -> Left (show err)
    Right txt ->
      case Parsec.parse parseJSONValue "<input>" (TL.toStrict txt) of
        Left parsecError -> Left (show parsecError)
        Right json ->
          case decoder json of
            Success a -> Right a
            Error err -> Left err

eitherDecode :: FromJSON a => BSL.ByteString -> Either String a
eitherDecode = eitherDecodeWith fromJSON

decodeFile :: FromJSON a => FilePath -> IO (Maybe a)
decodeFile filePath = do
  bsl <- BSL.readFile filePath
  pure (decodeWith fromJSON bsl)

eitherDecodeFile :: FromJSON a => FilePath -> IO (Either String a)
eitherDecodeFile filePath = do
  bsl <- BSL.readFile filePath
  pure (eitherDecodeWith fromJSON bsl)
