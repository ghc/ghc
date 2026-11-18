{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main (main) where

import Prelude
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedSumLib

total :: Prelude.sum '[2, 3, 5] :~: 10
total = Refl

other :: NativeClosedSumLib.weighted '[2, 3, 5] :~: 20
other = Refl

contains :: NativeClosedSumLib.containsAny '[2, 3, 5] :~: True
contains = Refl

char :: NativeClosedSumLib.identity 'x' :~: 'x'
char = Refl

main :: IO ()
main = case (total, other, contains, char) of
    (Refl, Refl, Refl, Refl) -> putStrLn "closed overloaded calls"
