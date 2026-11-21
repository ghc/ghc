{-# LANGUAGE BangPatterns, DataKinds, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module LexicalLinearLetReject where

import GHC.Types (Multiplicity(..))
import LexicalLetProvider

readLazy :: LinearPayload %1 -> Int
readLazy package = let ~(value, reader) = package in reader value

readTwice :: LinearPayload %1 -> (Int, Int)
readTwice package = let !(value, reader) = package in (reader value, reader value)

badMultiplicity :: forall (p :: Multiplicity). LinearPayload %p -> Int
badMultiplicity package = let !(value, reader) = forwardLinear @Many package in reader value

badSibling :: forall (p :: Multiplicity). LinearPayload %p -> Int
badSibling package = let !(value, reader) = forwardLinearIf @p () package in reader value

badKind :: forall (p :: Multiplicity). LinearPayload %p -> Int
badKind package = let !(value, reader) = forwardLinear @Bool package in reader value
