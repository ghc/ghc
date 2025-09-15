module Main where

-- Main.hs
import GHC.Stack

main :: IO ()
main = do
  foo 23 42
  (`foo` 23) 42

foo :: HasCallStack => Int -> Int -> IO ()
foo _ _ = print (length . getCallStack $ callStack)
