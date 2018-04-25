-- Test #6146 (a bug in -feager-blackholing) I couldn't reliably
-- reproduce it, but this program occasionally triggers it so it's
-- better than nothing.

module Main (main) where

import System.IO
import Control.Concurrent
import System.Mem

main :: IO ()
main = do
    putStrLn "First number?  "
    num1 <- readLn :: IO Int
    performGC -- provokes bug (sometimes)
    putStrLn "Second number? "
    num2 <- readLn :: IO Int
    let total = num1 + num2
    putStrLn $ "The sum is " ++ (show total) ++ "."

