{-# LANGUAGE BangPatterns, DataKinds, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module LexicalLinearLet
  ( readLinear, readPlain, readForwarded, readForwardedInferred
  , readOpen, readOuter, ignoreOpen, forceOpen
  ) where

import GHC.Types (Multiplicity(..))
import LexicalLetProvider

readLinear :: LinearPayload %1 -> Int
readLinear package = let !(value, reader) = package in reader value

readPlain :: (Bool, Bool %1 -> Int) %1 -> Int
readPlain pair = let !(value, reader) = pair in reader value

readForwarded :: forall (p :: Multiplicity). LinearPayload %p -> Int
readForwarded package = let !(value, reader) = forwardLinear @p package in reader value

readForwardedInferred :: LinearPayload %1 -> Int
readForwardedInferred package = let !(value, reader) = forwardLinear package in reader value

readOpen :: forall (p :: Multiplicity) a. OpenPayload a %p -> a
readOpen package =
  let !(value, reader) = relayLinear @p (forwardLinear @p) package
  in reader value

readOuter :: OuterPayload -> Int
readOuter outer =
  let (finish, package) = outer
  in let !(value, reader) = relayLinear @Many (forwardLinear @Many) package
     in finish (reader value)

ignoreOpen :: forall a. OpenPayload a -> Bool
ignoreOpen package = let ~(value, reader) = relayLinear @Many (forwardLinear @Many) package in True

forceOpen :: forall a. OpenPayload a -> Bool
forceOpen package = let !(value, reader) = relayLinear @Many (forwardLinear @Many) package in True
