module Main
  ( main
  ) where

import MyInteger (MyInteger (MyInteger), toMyInteger)

main :: IO ()
main = do
  let (MyInteger i) = toMyInteger (41 :: Integer)
  print i

