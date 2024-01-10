{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

main = runQ t1 >>= (putStrLn . pprint)

t1 = [d|
      fac n = go n
       where go 0 = 1
             go x = x * go (x - 1)
     |]
