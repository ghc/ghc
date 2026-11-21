{-# LANGUAGE Haskell2010, DataKinds, GADTs #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import RetainedTuple
import TupleProvider

seen :: Tuple Bool Bool -> (Bool, Bool)
seen t = case observe t of (At, pair) -> pair

lazy :: Tuple Bool Bool -> Bool
lazy t = case keep t of At -> case reflexive t of Refl -> True

main :: IO ()
main = print (seen (Tup2 True False), seen (Tup2 False True), lazy (error "opaque tuple was forced"))
