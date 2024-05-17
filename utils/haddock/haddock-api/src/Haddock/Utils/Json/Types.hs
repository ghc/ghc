module Haddock.Utils.Json.Types
  ( Value (..)
  , typeOf
  , Pair
  , Object
  , object
  ) where

import Data.String

-- TODO: We may want to replace 'String' with 'Text' or 'ByteString'

-- | A JSON value represented as a Haskell value.
data Value
  = Object !Object
  | Array [Value]
  | String String
  | Number !Double
  | Bool !Bool
  | Null
  deriving (Eq, Read, Show)

typeOf :: Value -> String
typeOf v = case v of
  Object _ -> "Object"
  Array _ -> "Array"
  String _ -> "String"
  Number _ -> "Number"
  Bool _ -> "Boolean"
  Null -> "Null"

-- | A key\/value pair for an 'Object'
type Pair = (String, Value)

-- | A JSON \"object\" (key/value map).
type Object = [Pair]

-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object

instance IsString Value where
  fromString = String
