{-# LANGUAGE OverloadedStrings #-}

-- | Minimal JSON / RFC 7159 support
--
-- The API is heavily inspired by @aeson@'s API but puts emphasis on
-- simplicity rather than performance. The 'ToJSON' instances are
-- intended to have an encoding compatible with @aeson@'s encoding.
--
module Distribution.Client.Utils.Json
    ( Value(..)
    , Object, object, Pair, (.=)
    , encodeToString
    , encodeToBuilder
    , ToJSON(toJSON)
    )
    where

import Data.Char
import Data.Int
import Data.String
import Data.Word
import Data.List
import Data.Monoid

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB

-- TODO: We may want to replace 'String' with 'Text' or 'ByteString'

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array  [Value]
           | String  String
           | Number !Double
           | Bool   !Bool
           | Null
           deriving (Eq, Read, Show)

-- | A key\/value pair for an 'Object'
type Pair = (String, Value)

-- | A JSON \"object\" (key/value map).
type Object = [Pair]

infixr 8 .=

-- | A key-value pair for encoding a JSON object.
(.=) :: ToJSON v => String -> v -> Pair
k .= v  = (k, toJSON v)

-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object

instance IsString Value where
  fromString = String


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
  toJSON Nothing  = Null
  toJSON (Just a) = toJSON a

instance (ToJSON a,ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = Array [toJSON a, toJSON b]

instance (ToJSON a,ToJSON b,ToJSON c) => ToJSON (a,b,c) where
  toJSON (a,b,c) = Array [toJSON a, toJSON b, toJSON c]

instance (ToJSON a,ToJSON b,ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
  toJSON (a,b,c,d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]

instance ToJSON Float where
  toJSON = Number . realToFrac

instance ToJSON Double where
  toJSON = Number

instance ToJSON Int    where  toJSON = Number . realToFrac
instance ToJSON Int8   where  toJSON = Number . realToFrac
instance ToJSON Int16  where  toJSON = Number . realToFrac
instance ToJSON Int32  where  toJSON = Number . realToFrac

instance ToJSON Word   where  toJSON = Number . realToFrac
instance ToJSON Word8  where  toJSON = Number . realToFrac
instance ToJSON Word16 where  toJSON = Number . realToFrac
instance ToJSON Word32 where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Int64  where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Word64 where  toJSON = Number . realToFrac

-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Integer where toJSON = Number . fromInteger

------------------------------------------------------------------------------
-- 'BB.Builder'-based encoding

-- | Serialise value as JSON/UTF8-encoded 'Builder'
encodeToBuilder :: ToJSON a => a -> Builder
encodeToBuilder = encodeValueBB . toJSON

encodeValueBB :: Value -> Builder
encodeValueBB jv = case jv of
  Bool True  -> "true"
  Bool False -> "false"
  Null       -> "null"
  Number n
    | isNaN n || isInfinite n   -> encodeValueBB Null
    | Just i <- doubleToInt64 n -> BB.int64Dec i
    | otherwise                 -> BB.doubleDec n
  Array a  -> encodeArrayBB a
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
    encPair (l,x) = encodeStringBB l <> BB.char8 ':' <> encodeValueBB x

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
  Bool b   -> showString (if b then "true" else "false")
  Null     -> showString "null"
  Number n
    | isNaN n || isInfinite n    -> encodeValue Null
    | Just i <- doubleToInt64 n -> shows i
    | otherwise                 -> shows n
  Array a -> encodeArray a
  String s -> encodeString s
  Object o -> encodeObject o

encodeArray :: [Value] -> ShowS
encodeArray [] = showString "[]"
encodeArray jvs = ('[':) . go jvs . (']':)
  where
    go []     = id
    go [x]    = encodeValue x
    go (x:xs) = encodeValue x . (',':) . go xs

encodeObject :: Object -> ShowS
encodeObject [] = showString "{}"
encodeObject jvs = ('{':) . go jvs . ('}':)
  where
    go []          = id
    go [(l,x)]     = encodeString l . (':':) . encodeValue x
    go ((l,x):lxs) = encodeString l . (':':) . encodeValue x . (',':) . go lxs

encodeString :: String -> ShowS
encodeString str = ('"':) . showString (escapeString str) . ('"':)

------------------------------------------------------------------------------
-- helpers

-- | Try to convert 'Double' into 'Int64', return 'Nothing' if not
-- representable loss-free as integral 'Int64' value.
doubleToInt64 :: Double -> Maybe Int64
doubleToInt64 x
  | fromInteger x' == x
  , x' <= toInteger (maxBound :: Int64)
  , x' >= toInteger (minBound :: Int64)
    = Just (fromIntegral x')
  | otherwise = Nothing
  where
    x' = round x

-- | Minimally escape a 'String' in accordance with RFC 7159, "7. Strings"
escapeString :: String -> String
escapeString s
  | not (any needsEscape s) = s
  | otherwise               = escape s
  where
    escape [] = []
    escape (x:xs) = case x of
      '\\' -> '\\':'\\':escape xs
      '"'  -> '\\':'"':escape xs
      '\b' -> '\\':'b':escape xs
      '\f' -> '\\':'f':escape xs
      '\n' -> '\\':'n':escape xs
      '\r' -> '\\':'r':escape xs
      '\t' -> '\\':'t':escape xs
      c | ord c < 0x10 -> '\\':'u':'0':'0':'0':intToDigit (ord c):escape xs
        | ord c < 0x20 -> '\\':'u':'0':'0':'1':intToDigit (ord c - 0x10):escape xs
        | otherwise    -> c : escape xs

    -- unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    needsEscape c = ord c < 0x20 || c `elem` ['\\','"']
