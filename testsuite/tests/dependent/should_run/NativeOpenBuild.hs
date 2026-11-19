{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import OpenBuildSource (Make, exact)
import qualified OpenBuildWorker as B

proof :: Make Bool Char :~: 'B.Shell Bool Char
proof = exact (Proxy @Bool) (Proxy @Char)

main :: IO ()
main = case proof of
  Refl -> putStrLn "constructed open result"
