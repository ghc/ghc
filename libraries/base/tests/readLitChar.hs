module Main (main)
where

import Data.Char (digitToInt, lexLitChar, readLitChar)

main :: IO ()
main =
    do putStrLn (show $ readLitChar "A")
       putStrLn (show $ readLitChar "'A'")
       putStrLn (show $ lexLitChar "A")
       putStrLn (show $ lexLitChar "'A'")

