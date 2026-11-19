{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE RankNTypes, TypeApplications, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (transportElement, transportElementEta, transportElementWith)
import PackedVecLibrary (Nat(..), Vec, replicateNative, toList)

main :: IO ()
main = do
    input <- newIORef (S (S Z)) >>= readIORef
    bad <- newIORef (error "length bottom" :: Nat) >>= readIORef
    let worker :: foreach (n :: Nat) -> Vec Int n
        worker n = replicateNative n 7
        bottomWorker :: foreach (n :: Nat) -> Vec Int n
        bottomWorker = error "worker bottom"
        bottomProof :: Int :~: Int
        bottomProof = error "proof bottom"
    print (toList (transportElement @Int @Int worker input),
           toList (transportElementEta @Int @Int worker input),
           toList (transportElementWith Refl worker input))
    expect "worker bottom" $ evaluate $
        case transportElement @Int @Int bottomWorker of !_ -> ()
    print $ case transportElementEta @Int @Int bottomWorker of !_ -> "eta function stays lazy"
    expect "worker bottom" $ evaluate (transportElementEta @Int @Int bottomWorker input)
    expect "proof bottom" $ evaluate $
        case transportElementWith bottomProof bottomWorker of !_ -> ()
    expect "worker bottom" $ evaluate $
        case transportElementWith Refl bottomWorker of !_ -> ()
    expect "length bottom" $ evaluate $
        length (toList (transportElement @Int @Int worker bad))

expect :: String -> IO a -> IO ()
expect label action = do
    result <- try action
    case result of
        Left e | label `isInfixOf` displayException (e :: ErrorCall) ->
            putStrLn (label ++ ": forced")
        _ -> fail ("Unexpected demand of " ++ label)
