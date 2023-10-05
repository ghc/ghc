module InlinedBreakpoint2 where

import InlinedBreakpoint1

main :: IO ()
main = do
  putStrLn rul
  inl
  putStrLn "done"
