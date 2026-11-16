{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Micro.PkgAesonTH where

import Micro.Types
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe

deriveJSON defaultOptions ''Tree

serialise :: Tree -> BS.ByteString
serialise = Aeson.encode

deserialise :: BS.ByteString -> Tree
deserialise = fromJust . Aeson.decode'
