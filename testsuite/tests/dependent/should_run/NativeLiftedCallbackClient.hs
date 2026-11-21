{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import NativeLiftedCallbackProvider (Decision(..), decide)
import System.Environment (getArgs)

newtype Flag = Flag Bool

observe :: forall a. (a -> Bool) -> a -> Bool
observe f x = case decide @a f x of
  IsTrue -> True
  IsFalse -> False

observeInferred :: (a -> Bool) -> a -> Bool
observeInferred f x = case decide f x of
  IsTrue -> True
  IsFalse -> False

forceFailure :: String -> Bool -> IO Bool
forceFailure message value = do
  result <- try (evaluate value)
  pure $ case result of
    Left (ErrorCall actual) -> actual == message
    Right _ -> False

main :: IO ()
main = do
  [input] <- getArgs
  let truth = read input
  print (observe not truth, observe null [truth], observeInferred (\(Flag value) -> value) (Flag truth))
  print (observe (const True) (error "unused argument" :: Flag))
  print =<< forceFailure "used argument" (observe (\flag -> flag `seq` True) (error "used argument" :: Flag))
  print =<< forceFailure "used callback" (observe (error "used callback" :: Flag -> Bool) (Flag truth))
