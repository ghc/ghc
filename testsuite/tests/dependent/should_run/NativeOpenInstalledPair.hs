{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
import PreludePairExport
  (first, second, swapped, nestedFirst, nestedSecond,
   abstractFirst, abstractSecond, get, swapIndex, secondIndex)

-- This composition is exposed by instantiation in this module. The exporter
-- elaborates get, swapIndex and secondIndex separately.
viaImport :: forall (p :: (Natural, Natural)). Proxy p -> ()
viaImport p = sameIndex (get (swapIndex p)) (secondIndex p)
  where
    sameIndex :: Proxy (n :: Natural) -> Proxy n -> ()
    sameIndex _ _ = ()

main :: IO ()
main =
  case ( first (Proxy @'(Int, Bool)) (Proxy @Int)
       , second (Proxy @'(Int, Bool)) (Proxy @Bool)
       , first (Proxy @'( 'True, 'x')) (Proxy @'True)
       , second (Proxy @'( 'True, 'x')) (Proxy @'x')
       , swapped (Proxy @'( 'True, 'x')) (Proxy @'( 'x', 'True))
       , nestedFirst (Proxy @'False)
       , nestedSecond (Proxy @'y')
       , abstractFirst (Proxy @'( 'True, 'x'))
       , abstractSecond (Proxy @'( 'True, 'x'))
       , viaImport (Proxy @'(17, 5))
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, ()) ->
      putStrLn "Type, Bool, Char, and Natural operands"
