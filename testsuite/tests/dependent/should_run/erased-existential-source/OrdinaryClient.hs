{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.List (isInfixOf)
import Provider

main :: IO ()
main = do
  let ready = supplied `seq` synthesized `seq` kindOnly `seq`
              kindAndSchema `seq` wildcardWitness `seq` written `seq` "constructed"
  putStrLn ready
  result <- try @SomeException (evaluate (lazyPack `seq` ()))
  case result of
    Left ex | "packed payload forced" `isInfixOf` displayException ex ->
      putStrLn "Pack has the demand of its payload"
    _ -> fail "forcing Pack did not force the underlying payload"
