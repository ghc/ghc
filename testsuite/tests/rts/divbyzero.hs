
{-
By default, on Windows a division-by-zero will pop up an annoying dialog box.
We want the RTS to catch it instead.
-}

module Main where

import GHC.Base

main :: IO ()
main = print (5 `divInt` 0)

