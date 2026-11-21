{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import NativeTypeCaseRetainedConTh

copied :: Copied Int -> (Int, Char, Int)
copied (Copied _ True n False c v) = (n, c, v)
copied _ = error "unexpected copied constructor"

quoted :: Quoted Int -> (Int, Char, Int)
quoted (Quoted _ True n False c v) = (n, c, v)
quoted _ = error "unexpected quoted constructor"

curried :: CurriedPacked -> (Bool, String, Int, Char)
curried (CurriedPacked x y select) = (x == y, show y, select True, select False)

main :: IO ()
main = do
  print (copied (Copied (error "lazy prefix") True 11 False 'x' 27))
  print (quoted (Quoted (error "lazy prefix") True 11 False 'x' 27))
  print (case CopiedVacuous (error "unused index") 29 of CopiedVacuous _ n -> n)
  print (curried curriedSample)
