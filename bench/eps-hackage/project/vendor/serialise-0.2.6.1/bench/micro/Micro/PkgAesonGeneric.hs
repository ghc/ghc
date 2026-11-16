{-# OPTIONS_GHC -fno-warn-orphans #-}
module Micro.PkgAesonGeneric where

import Micro.Types
import Data.Aeson as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe

serialise :: Tree -> BS.ByteString
serialise = Aeson.encode

deserialise :: BS.ByteString -> Tree
deserialise = fromJust . Aeson.decode'

instance ToJSON   Tree
instance FromJSON Tree

