{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

foo :: String -> $(tupleT 1 `appT` conT ''String)
foo x = $(tupE [[| x |]])

bar :: $(tupleT 1 `appT` conT ''String) -> String
bar $(tupP [[p| x |]]) = x

main :: IO ()
main = do
  foo undefined `seq` putStrLn "hello"
  putStrLn $ bar $ foo "world"
