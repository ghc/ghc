{-# LANGUAGE KindSignatures, Modifiers, PolyKinds, StandaloneKindSignatures, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs, RankNTypes, TypeAbstractions, TypeApplications #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module LaterMatchability where

import AnchorReceiver
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Matchability (M, Matchability, U)

type MatchResult = Receiver (Select 'True) 'Match
type UnmatchResult = Receiver (Select 'True) 'Unmatch

type DefaultPrefix = Select 'True

signed :: Receiver (Select 'True) 'Match
signed = Just True

type family Map f xs where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type Mapper = forall (m :: Matchability) {b} {a}. @U
              (a %m -> b) %U -> [a] %U -> [b]

type family Only (xs :: [Type]) :: Type where
  Only '[x] = x

-- | Store results from the same mapper at the two callback kinds.
type Recipe :: Mapper -> Type
data Recipe mapper where
  Recipe :: forall (mapper :: Mapper).
            Only (mapper @M Maybe '[Bool]) ->
            Only (mapper @U KnownListCallback '[Bool]) ->
            Recipe mapper

type ListRecipe = Recipe Map

type family Flip (m :: Matchability) :: Matchability where
  Flip M = U
  Flip U = M

-- | Apply a callback whose matchability is computed from a named selector.
type ApplyFlipped :: forall m. (Type %(Flip m) -> Type) -> Type -> Type
type family ApplyFlipped @m f a where
  ApplyFlipped @m f a = f a

type FlippedMatch = ApplyFlipped @U Maybe Bool
type FlippedUnmatch = ApplyFlipped @M KnownListCallback Bool

type family InitialSelector (flag :: Bool) :: Matchability where
  InitialSelector flag = U

class ComputedTag (flag :: Bool) a where
  computedTag :: Proxy flag -> Proxy a -> Bool

instance forall flag (f :: Type %(Flip (InitialSelector flag)) -> Type) x.
         ComputedTag flag (f x) where
  computedTag _ _ = True

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
