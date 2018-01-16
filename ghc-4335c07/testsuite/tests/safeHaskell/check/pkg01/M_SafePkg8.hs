{-# LANGUAGE Trustworthy #-}
module M_SafePkg8 where

import Prelude
import safe qualified Data.ByteString.Char8 as BS

s = BS.pack "Hello World"

