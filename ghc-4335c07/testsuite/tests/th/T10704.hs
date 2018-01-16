{-# LANGUAGE MagicHash, TemplateHaskell #-}
module Main where

import GHC.Exts
import T10704a

main :: IO ()
main = do
  putStrLn $(fixityExp ''(->))
  putStrLn $(fixityExp ''Show)
  putStrLn $(fixityExp 'show)
  putStrLn $(fixityExp '(+))
  putStrLn $(fixityExp ''Int)
  putStrLn $(fixityExp ''Item)
  putStrLn $(fixityExp ''Char#)
  putStrLn $(fixityExp 'Just)
  putStrLn $(fixityExp 'seq)
  putStrLn $(fixityExp '($))
  putStrLn $(fixityExp ''(:=>))
  putStrLn $(fixityExp ''(:+:))
  putStrLn $(fixityExp ''(:*:))
  putStrLn $(fixityExp ''(:%:))
  putStrLn $(fixityExp ''(:?:))
  putStrLn $(fixityExp ''(:@:))
