{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import OpenBuildSource (Make, Swap, exact)
import qualified OpenBuildWorker as B

proof :: Make Bool Char :~: 'B.Shell Bool Char
proof = exact (Proxy @Bool) (Proxy @Char)

-- | Supplying the constructor determines the result in this ordinary module.
late :: Swap '( 'True, 'x') :~: '( 'x', 'True)
late = Refl

main :: IO ()
main = case (proof, late) of
  (Refl, Refl) -> putStrLn "constructed open result"
