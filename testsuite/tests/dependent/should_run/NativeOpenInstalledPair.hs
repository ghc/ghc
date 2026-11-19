{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludePairExport (first, second)

main :: IO ()
main =
  case ( first (Proxy @'(Int, Bool)) (Proxy @Int)
       , second (Proxy @'(Int, Bool)) (Proxy @Bool)
       , first (Proxy @'( 'True, 'x')) (Proxy @'True)
       , second (Proxy @'( 'True, 'x')) (Proxy @'x')
       ) of
    (Refl, Refl, Refl, Refl) -> putStrLn "Type, Bool, and Char operands"
