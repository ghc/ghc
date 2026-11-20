{-# LANGUAGE KindSignatures, Modifiers, PolyKinds, StandaloneKindSignatures, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module LaterMatchability where

import AnchorReceiver
import Data.Kind (Type)
import GHC.Matchability (U)

type MatchResult = Receiver (Select 'True) 'Match
type UnmatchResult = Receiver (Select 'True) 'Unmatch

type DefaultPrefix = Select 'True

signed :: Receiver (Select 'True) 'Match
signed = Just True

type family Map f xs where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family ForwardMap f xs where
  ForwardMap _ '[] = '[]
  ForwardMap f (x ': xs) = f x ': Map f xs

type family ListOf (enabled :: Bool) a where
  ListOf 'True a = [a]

type ListCallback :: Bool -> Type %U -> Type
type ListCallback enabled = ListOf enabled

-- | Name the partially applied list family for ordinary clients.
type KnownListCallback :: Type %U -> Type
type KnownListCallback = ListCallback 'True
