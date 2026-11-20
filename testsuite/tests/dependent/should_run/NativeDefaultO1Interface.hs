{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf)
import NativeDefaultO1InterfaceProducer
import System.Environment (getArgs)

main :: IO ()
main = do
    [arg] <- getArgs
    let n = read arg
    print (exposure (error "type-only index") (error "unused retained argument") n,
           ordinaryWorker n (error "unused ordinary argument"))
    bad <- try (evaluate (ordinaryWorker (error "strict ordinary argument") False))
        :: IO (Either ErrorCall Int)
    case bad of
        Left err | "strict ordinary argument" `isInfixOf` show err -> putStrLn "strict argument forced"
        _ -> fail "strict argument should be forced"
