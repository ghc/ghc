{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import qualified LazyParcel as P

proof :: P.first (P.make 7) :~: 7
proof = Refl

main :: IO ()
main = case proof of
  Refl -> print (P.first (P.make 7))
