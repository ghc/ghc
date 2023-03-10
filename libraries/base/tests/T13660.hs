-- | This should print an InvalidArgument error complaining that
-- the file path contains a NUL octet.
module Main where

import System.IO.Error

main :: IO ()
main = do
    catchIOError
      (writeFile "hello\x00world" "hello")
      print
