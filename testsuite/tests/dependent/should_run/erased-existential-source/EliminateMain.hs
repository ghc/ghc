{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import Eliminate
import Provider

main :: IO ()
main = do
  print (inspect supplied, inspect synthesized, inspectBare kindOnly,
         inspect kindAndSchema, inspect wildcardWitness)
  print (ignoreBottom, ignoreCall, unpackLocal)
  print (readPair (transportPair Refl (hidePair (7 :: Int))))
  print (inlineArgument True 1)
  print (render (shown True), render (shown (7 :: Int)))
  result <- try @SomeException (evaluate forceBottom)
  case result of
    Left ex | "packed payload forced" `isInfixOf` displayException ex ->
      putStrLn "strict payload pattern forced the package"
    _ -> fail "strict payload pattern did not demand the underlying payload"
