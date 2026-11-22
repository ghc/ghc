{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import PreludeZipRuntime (lengthZipWith)

-- | Force the proof from an ordinary caller.
proved :: (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> Bool
proved f xs ys = case lengthZipWith f xs ys of Refl -> True

main :: IO ()
main = do
    let bad = error "forced callback"
    print (proved bad [] (error "forced right spine"),
           proved bad [error "forced left element"] [error "forced right element"])
