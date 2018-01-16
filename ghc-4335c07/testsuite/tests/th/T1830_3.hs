{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH.Syntax (lift)
import T1830_3a

main :: IO ()
main = do
  print ($(lift algDT1) == algDT1)
  print ($(lift algDT2) == algDT2)
  print ($(lift algDT3) == algDT3)
  print ($(lift prim)   == prim)
  print ($(lift df1)    == df1)
  print ($(lift df2)    == df2)
  print ($(lift df3)    == df3)
