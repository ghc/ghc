{-# Language TemplateHaskell #-}
{-# Language RecordWildCards #-}
module Main where

import Language.Haskell.TH.Ppr

data G = H { field0 :: Int, field1 :: String }

main :: IO ()
main = do
  let pr mq = do
        q <- mq
        print q
        print . pprint $ q
  pr [e|let field0 = 3 in H {field0,..}|]
  pr [e|let { field0 = 3; field1 = "a" } in H {field0,..}|]
