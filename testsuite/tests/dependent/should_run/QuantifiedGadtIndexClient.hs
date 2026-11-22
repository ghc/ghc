{-# LANGUAGE GADTs, NoExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import QuantifiedGadtIndex

clientTransport :: forall k (f :: forall (a :: k). Proxy a -> Type).
  Pick @k f -> Carrier @k f -> Carrier @Bool Slot
clientTransport PickBool x = x

observe :: Carrier @Bool Slot -> Int
observe (Carrier n) = n

main :: IO ()
main = print (observe (transport PickBool (Carrier 41)),
              observe (clientTransport PickBool (Carrier 43)))
