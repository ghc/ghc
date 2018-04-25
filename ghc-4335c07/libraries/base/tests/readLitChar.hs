module Main (main)
where

import Data.Char (digitToInt, lexLitChar, readLitChar)

main :: IO ()
main =
    do putStrLn (show $ readLitChar "A")
       putStrLn (show $ readLitChar "'A'")
       putStrLn (show $ lexLitChar "A")
       putStrLn (show $ lexLitChar "'A'")
       putStrLn (show $ lexLitChar "\\243\\&1")
       putStrLn (show $ lexLitChar "a\\&1")
       putStrLn (show $ lexLitChar "a\\&\\&1")
       putStrLn (show $ lexLitChar "a\\&\\&")
