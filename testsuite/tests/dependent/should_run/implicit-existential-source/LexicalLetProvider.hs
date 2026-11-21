{-# LANGUAGE DataKinds, ExperimentalDependentTypes, LinearTypes, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module LexicalLetProvider
  ( HiddenPayload, producePayload, lazyPayload, bottomPayload
  , LinearPayload, linearPayload, forwardLinear, forwardLinearIf
  , OpenPayload, OuterPayload, makeOpenPayload, makeOuterPayload, freshOpenPayload, relayLinear
  ) where

import GHC.Types (Multiplicity)

type HiddenPayload = exists a. (a, a -> Int)

{-# NOINLINE producePayload #-}
producePayload :: Bool -> HiddenPayload
producePayload True = (True, fromEnum)
producePayload False = ('b', fromEnum)

lazyPayload :: HiddenPayload
lazyPayload = (error "unused payload field" :: Bool, const 77)

bottomPayload :: Bool -> HiddenPayload
bottomPayload _ = error "bottom package"

type LinearPayload = exists a. (a, a %1 -> Int)

linearPayload :: Bool -> LinearPayload
linearPayload flag = (flag, readFlag)
  where
    readFlag :: Bool %1 -> Int
    readFlag True = 5
    readFlag False = 9

{-# NOINLINE forwardLinear #-}
forwardLinear :: forall (p :: Multiplicity) a. a %p -> a
forwardLinear value = value

forwardLinearIf :: forall (p :: Multiplicity) a. Bool -> a %p -> a
forwardLinearIf _ value = value

type OpenPayload a = exists b. (b, b %1 -> a)
type OuterPayload = exists a. (a -> Int, OpenPayload a)

makeOpenPayload :: forall a. Bool -> a -> a -> OpenPayload a
makeOpenPayload flag yes no = (flag, readFlag)
  where
    readFlag :: Bool %1 -> a
    readFlag True = yes
    readFlag False = no

makeOuterPayload :: Bool -> OuterPayload
makeOuterPayload flag = (fromEnum @Char, makeOpenPayload flag 'x' 'y')

freshOpenPayload :: forall a. Bool -> OpenPayload a
freshOpenPayload _ = error "any result type"

{-# NOINLINE relayLinear #-}
relayLinear :: forall (p :: Multiplicity) a. (forall x. x %p -> x) -> a %p -> a
relayLinear transform value = transform value
