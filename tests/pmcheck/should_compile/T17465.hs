module Lib where

f :: () -> ()
f _
  | False = ()
  | otherwise = ()

g :: () -> ()
g ()
  | False = ()
  | False = ()
  | otherwise = ()

h :: () -> ()
h x
  | () <- x, False = ()
  | False          = ()
  | otherwise      = ()
