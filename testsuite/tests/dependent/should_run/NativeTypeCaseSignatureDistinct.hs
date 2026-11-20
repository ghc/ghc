{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll #-}
module NativeTypeCaseSignatureDistinct where

import Data.Proxy (Proxy)

differentOrder :: forall (d :: Maybe Bool). Proxy d ->
  Proxy (case d of { Just True -> Int; Just _ -> Char; Nothing -> Char }) ->
  Proxy (case d of { Just _ -> Char; Just True -> Int; Nothing -> Char })
differentOrder _ input = input
