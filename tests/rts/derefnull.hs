
{-
By default, on Windows a segfault will pop up an annoying dialog box.
We want the RTS to catch it instead.
-}

module Main where

import Foreign

main :: IO ()
main = do x <- peek nullPtr
          print (x :: Int)

