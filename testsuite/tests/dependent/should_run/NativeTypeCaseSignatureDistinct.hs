{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, RankNTypes #-}
module NativeTypeCaseSignatureDistinct where

import Data.Proxy (Proxy)

differentOrder :: forall (d :: Maybe Bool). Proxy d ->
  Proxy (case d of { Just True -> Int; Just _ -> Char; Nothing -> Char }) ->
  Proxy (case d of { Just _ -> Char; Just True -> Int; Nothing -> Char })
differentOrder _ input = input

wrongShadow :: forall (b :: Bool). Proxy b
            -> (case b of { True -> Int; False -> Char })
            -> (forall (b :: Bool). Proxy b
                -> (case b of { True -> Int; False -> Char }))
wrongShadow _ input _ = input

differentRetained :: foreach (left :: Bool) -> foreach (right :: Bool)
                  -> (case left of { True -> Int; False -> Char })
                  -> (case right of { True -> Int; False -> Char })
differentRetained _ _ input = input
