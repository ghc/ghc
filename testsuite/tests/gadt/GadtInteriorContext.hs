{-# LANGUAGE Haskell2010, ExplicitNamespaces, GADTs, ImpredicativeTypes, RequiredTypeArguments, TypeApplications, TypeOperators #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import GadtInteriorContextProvider

right :: Two a -> Bool -> Int
right (Two _ (Tag (type Int))) flag | flag = 7
right (Two _ _) _ = 13
right (Strict _ (Tag (type Int))) _ = 19

hidden :: Hidden -> Bool
hidden (Hidden _ x) = x == x

throws :: a -> IO Bool
throws x = either (const True) (const False) <$> try @SomeException (evaluate x)

main :: IO ()
main = do
  let unused = Two @Int (error "unused prefix") (Tag (type Int))
      bottom = error "strict prefix" :: Tag Int
  print (right unused True, right unused False)
  partial <- throws (seq @(Int ~ Int => Tag Int -> Two Int) (Strict @Int bottom) True)
  afterContext <- throws (Strict @Int bottom :: Tag Int -> Two Int)
  full <- throws (Strict @Int bottom (Tag (type Int)))
  print (partial, afterContext, full)
  print (hidden (Hidden True False), hidden (Hidden @Double 0 (0/0)))
