{-# LANGUAGE Trustworthy #-}
module M_SafePkg6 where

import safe Prelude
import safe qualified Data.ByteString.Char8 as BS

s = BS.pack "Hello World"

