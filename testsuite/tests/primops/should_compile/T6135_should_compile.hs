-- Tests created by Jan Stolarek <jan.stolarek@p.lodz.pl>

{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts

main :: IO ()
main = do
  print (h 3.0##)
  print g
  print 3.14159

h :: Double# -> String
h n = if (n ==## 3.0##) || (n ==## 4.0##)
      then "First branch"
      else "Second branch"

g :: String
g = if ((fromInteger 1 :: Integer) == 1)
    then "First branch"
    else "Second branch"
