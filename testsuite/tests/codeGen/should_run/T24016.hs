module Main (main) where

data Command
  = Command1
  | Command2
  | Command3
  | Command4
  | Command5
  | Command6 -- Commenting this line works with -fPIC, uncommenting leads to a crash.

main :: IO ()
main = do
  let x = case cmd of
           Command1 -> 1 :: Int
           Command2 -> 2
           Command3 -> 3
           Command4 -> 4
           Command5 -> 5
           Command6 -> 6
  putStrLn (show x)

{-# NOINLINE cmd #-}
cmd :: Command
cmd = Command6
