{-# LANGUAGE BangPatterns, DataKinds, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module LexicalLinearLet (readLinear, readPlain, readForwarded, readForwardedInferred) where

import GHC.Types (Multiplicity)
import LexicalLetProvider

readLinear :: LinearPayload %1 -> Int
readLinear package = let !(value, reader) = package in reader value

readPlain :: (Bool, Bool %1 -> Int) %1 -> Int
readPlain pair = let !(value, reader) = pair in reader value

readForwarded :: forall (p :: Multiplicity). LinearPayload %p -> Int
readForwarded package = let !(value, reader) = forwardLinear @p package in reader value

readForwardedInferred :: LinearPayload %1 -> Int
readForwardedInferred package = let !(value, reader) = forwardLinear package in reader value
