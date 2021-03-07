{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Monoid (Sum)
import Language.Haskell.TH

dec :: DecsQ
dec = [d| default (Int, Double)
        |]

$([d| default (Integer, Sum Integer)
        |])

main = runQ dec >>= putStrLn . pprint
       >> print (4 <> 8)
