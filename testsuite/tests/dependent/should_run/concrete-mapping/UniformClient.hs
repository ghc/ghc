{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import Row

main :: IO ()
main = do
    let discard :: Row Bool Bool Int -> Row Bool Bool Bool
        discard = fmap (error "forced callback")
        cell = error "forced cell"
    case discard (Both cell cell cell) of
        Empty -> pure ()
        _ -> fail "expected Empty"
    result <- try (evaluate (discard (error "forced receiver")))
              :: IO (Either ErrorCall (Row Bool Bool Bool))
    print (either (isInfixOf "forced receiver" . displayException) (const False) result)
