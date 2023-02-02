-- test for #2678
module Main (main) where

import System.IO

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          hLookAhead stdin >>= print
          hSetBuffering stdin LineBuffering
          getContents >>= print
