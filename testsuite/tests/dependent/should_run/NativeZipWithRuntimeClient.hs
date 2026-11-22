{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import PreludeZipRuntime (lengthZip, lengthZipMap, lengthZipWith)

-- | Force the pair-length proof from an ordinary caller.
paired :: [Bool] -> [Int] -> Bool
paired xs ys = case lengthZip xs ys of Refl -> True

-- | Force the proof from an ordinary caller.
proved :: (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> Bool
proved f xs ys = case lengthZipWith f xs ys of Refl -> True

-- | Check the composite proof with four distinct element types.
mapped :: (Bool -> Char) -> (Char -> Int -> Ordering) -> IO Bool
mapped pre combine = do
    xs <- readIORef =<< newIORef [True]
    ys <- readIORef =<< newIORef [1]
    pure (case lengthZipMap pre combine xs ys of Refl -> True)

main :: IO ()
main = do
    let bad = error "forced callback"
    xs <- readIORef =<< newIORef [True, False]
    ys <- readIORef =<< newIORef [1]
    composite <- mapped (error "forced map callback") (error "forced zip callback")
    print (paired xs ys,
           proved bad [] (error "forced right spine"),
           proved bad [error "forced left element"] [error "forced right element"],
           composite)
