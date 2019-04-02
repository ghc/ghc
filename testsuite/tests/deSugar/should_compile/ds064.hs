module Ds064 where

main :: IO ()
main = {-# OPTIONS_LOCAL -Wunused-do-bind #-} do
  getLine
  return ()

main' :: IO ()
main' = do
  getLine
  return ()

