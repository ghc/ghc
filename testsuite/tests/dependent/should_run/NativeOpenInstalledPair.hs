{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludePairExport (first, second, swapped, nestedFirst, nestedSecond)

main :: IO ()
main =
  case ( first (Proxy @'(Int, Bool)) (Proxy @Int)
       , second (Proxy @'(Int, Bool)) (Proxy @Bool)
       , first (Proxy @'( 'True, 'x')) (Proxy @'True)
       , second (Proxy @'( 'True, 'x')) (Proxy @'x')
       , swapped (Proxy @'( 'True, 'x')) (Proxy @'( 'x', 'True))
       , nestedFirst (Proxy @'False)
       , nestedSecond (Proxy @'y')
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> putStrLn "Type, Bool, and Char operands"
