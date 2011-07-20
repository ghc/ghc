{-# OPTIONS_GHC -fwarn-unused-imports -Werror #-}

module T2267 where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU

toString :: B.ByteString -> String
toString = BU.toString

fromString :: String -> B.ByteString
fromString = BU.fromString
