{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import qualified IndexedMark as P (Mark(Count, Pair), sample, readMark)

count :: P.sample 0 :~: 'P.Count 4
count = Refl

pair :: P.sample 6 :~: 'P.Pair 6 7
pair = Refl

nested :: P.readMark (P.sample 6) :~: 67
nested = Refl

selected :: P.readMark ('P.Count 9) :~: 9
selected = Refl

main :: IO ()
main = case (count, pair, nested, selected) of
  (Refl, Refl, Refl, Refl) -> putStrLn "closed indexed constructors"
