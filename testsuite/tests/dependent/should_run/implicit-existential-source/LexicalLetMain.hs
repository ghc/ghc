{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Data.IORef
import LexicalLetProvider

readPayload :: HiddenPayload -> Int
readPayload package = let (value, reader) = package in reader value

readTwice :: Bool -> (Int, Int)
readTwice flag =
  let (value, reader) = producePayload flag
  in (reader value, reader value)

ignorePayload :: HiddenPayload -> Bool
ignorePayload package = let (value, reader) = package in True

localPolymorphism :: HiddenPayload -> (Bool, Int)
localPolymorphism package =
  let (value, reader) = package
  in let identity x = x
     in (identity True, identity (reader value))

ordinaryTuple :: (Bool, Char)
ordinaryTuple =
  let (identity, _) = (id, ())
  in (identity True, identity 'x')

main :: IO ()
main = do
  flag <- newIORef True
  first <- readIORef flag
  writeIORef flag False
  second <- readIORef flag
  print (readTwice first, readTwice second, readPayload lazyPayload)
  print (ignorePayload (error "unused package"), localPolymorphism (producePayload second), ordinaryTuple)
