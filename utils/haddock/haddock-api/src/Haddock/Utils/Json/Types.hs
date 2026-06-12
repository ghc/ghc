{-# LANGUAGE OverloadedStrings #-}

module Haddock.Utils.Json.Types
  ( Value (..)
  , typeOf
  , Pair
  , Object
  , object
  ) where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T

-- | A JSON value represented as a Haskell value.
data Value
  = Object !Object
  | Array [Value]
  | String !Text
  | Number !Double
  | Bool !Bool
  | Null
  deriving (Eq, Read, Show)

typeOf :: Value -> Text
typeOf v = case v of
  Object _ -> "Object"
  Array _ -> "Array"
  String _ -> "String"
  Number _ -> "Number"
  Bool _ -> "Boolean"
  Null -> "Null"

-- | A key\/value pair for an 'Object'
type Pair = (Text, Value)

-- | A JSON \"object\" (key/value map).
type Object = [Pair]

-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object

instance IsString Value where
  fromString = String . T.pack
