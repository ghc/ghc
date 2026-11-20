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

-- | Apply an unmatchable callback at Bool.
type family ApplyBool (f :: Type %U -> Type) where
  ApplyBool f = f Bool

type AtBool :: (Type %U -> Type) %U -> Type
type AtBool = ApplyBool

-- | Store the result of a callback whose kind is unmatchable.
data CallbackResult (result :: Type) (f :: Type %U -> Type) = CallbackResult result

-- | Store the result of either a constructor or a type family.
data FamilyBox f = FamilyBox (f Bool)

-- | Forward a callback to the box.
type family ForwardBox f where
  ForwardBox f = FamilyBox f

type ListBox = ForwardBox (WrapOpen 'True)
