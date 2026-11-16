{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Micro.ReadShow where

import Micro.Types
import Data.ByteString.Lazy.Char8 as BS

serialise :: Tree -> BS.ByteString
serialise = BS.pack . show

deserialise :: BS.ByteString -> Tree
deserialise = read . BS.unpack

deriving instance Show Tree
deriving instance Read Tree

