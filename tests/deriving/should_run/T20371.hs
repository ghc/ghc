module Main where

import T20371A

import Data.Data
import Data.Monoid
import Data.Functor.Identity

data C = C deriving Data

main = do
  print (dataTypeName $ dataTypeOf A)
  print (dataTypeName $ dataTypeOf (A :.: A))
  print (dataTypeName $ dataTypeOf C)

  print (dataTypeName $ dataTypeOf (All True))
  print (dataTypeName $ dataTypeOf (Identity True))
  print (dataTypeName $ dataTypeOf (Just True))

