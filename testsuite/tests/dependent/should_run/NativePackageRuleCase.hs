{-# LANGUAGE DataKinds, ImpredicativeTypes, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Proxy (Proxy(..))
import LexicalPackageRules (readHidden)
import WitnessSchema (Payload(..), RankedBuilder, configured)

ranked :: RankedBuilder
ranked = \b -> configured @True @Bool Proxy id b

fieldsBottom :: RankedBuilder
fieldsBottom = \b -> prepare b
  where
    prepare :: Bool -> Payload Int
    prepare b = Payload (error "callback value") (\value -> (value, b))

constructorBottom :: RankedBuilder
constructorBottom = \_ -> payload
  where
    payload :: Payload Int
    payload = error "callback constructor"

{-# NOINLINE run #-}
run :: RankedBuilder -> Bool -> (Int -> Int) -> Int
run builder b readInt = case builder b of
    Payload value call ->
        let (n, tag) = call value
        in readHidden (if tag then n else n + 1, readInt)

expectError :: String -> Int -> IO Bool
expectError message value = do
    result <- try @ErrorCall (evaluate value)
    pure $ case result of
        Left err -> takeWhile (/= '\n') (displayException err) == message
        Right _ -> False

main :: IO ()
main = do
    fieldForced <- expectError "callback value" (run fieldsBottom True id)
    constructorForced <- expectError "callback constructor" (run constructorBottom True (const 17))
    print (run ranked False (+ 1), run ranked True (+ 2),
           run fieldsBottom True (const 17), fieldForced, constructorForced)
