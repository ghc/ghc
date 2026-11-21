{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef
import LexicalBangLet
import LexicalStrictLet
import LexicalLetProvider
import LexicalLinearLet

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

observeDemand :: Bool -> IO String
observeDemand value = do
  result <- try (evaluate value)
  case result of
    Left (ErrorCall problem) -> pure problem
    Right answer -> pure (show answer)

main :: IO ()
main = do
  flag <- newIORef True
  first <- readIORef flag
  writeIORef flag False
  second <- readIORef flag
  print (readTwice first, readTwice second, readPayload lazyPayload)
  print (ignorePayload (error "unused package"), localPolymorphism (producePayload second), ordinaryTuple)
  demand <- traverse observeDemand [ignoreTilde first, ignoreBang first,
                                   ignoreStrict first, ignoreStrictTilde first]
  print (demand, readBangField, readStrictField,
         readPayload (repackStrict (producePayload second)))
  print (readLinear (linearPayload first), readLinearStrict (linearPayload second))
