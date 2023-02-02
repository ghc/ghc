module Main where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Builder

import qualified Data.ByteString.Lazy as L

main =
  L.writeFile "out"
  . toLazyByteString . foldMap byteString
  . replicate 10000000 $ pack "text"
