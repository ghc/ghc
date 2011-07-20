
module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do print $ map B.unpack $ B.lines $ B.pack "a\n\nb\n\nc"
          print $ map L.unpack $ L.lines $ L.pack "a\n\nb\n\nc"

