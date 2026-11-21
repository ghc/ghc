{-# LANGUAGE BangPatterns, DataKinds, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module LexicalOpenReject where

import Data.Proxy (Proxy)
import GHC.Types (Multiplicity(..))
import LexicalLetProvider

wrongResult :: forall (p :: Multiplicity) a b. OpenPayload a %p -> b
wrongResult package =
  let !(value, reader) = relayLinear @p (forwardLinear @p) package
  in reader value

escapeWitness :: forall a b. OpenPayload a -> b
escapeWitness package =
  let !(value, reader) = relayLinear @Many (forwardLinear @Many) package
  in value

escapeOuter :: forall a. OuterPayload -> a
escapeOuter outer =
  let (finish, package) = outer
  in let !(value, reader) = relayLinear @Many (forwardLinear @Many) package
     in reader value

unselectedFresh :: forall a. Proxy a -> a
unselectedFresh _ =
  let !(value, reader) = relayLinear @Many (forwardLinear @Many) (freshOpenPayload True)
  in reader value
