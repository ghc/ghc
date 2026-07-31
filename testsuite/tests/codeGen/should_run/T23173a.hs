module Main where

import T23173a_A

-- Cases on an imported evaluated constructor at -O0. With LFCon conveyed in
-- the interface the reference is tagged and never entered; without it the
-- scrutinee is entered and --fatal-enter-taggable aborts.
main :: IO ()
main = case x of
  Just b  -> print b
  Nothing -> putStrLn "nothing"
