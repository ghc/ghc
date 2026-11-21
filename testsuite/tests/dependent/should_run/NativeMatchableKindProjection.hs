{-# LANGUAGE DataKinds, GADTs, Modifiers, PolyKinds, ScopedTypeVariables, StandaloneKindSignatures, TypeApplications #-}
module NativeMatchableKindProjection (observe) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Matchability (M, Matchability)

import MatchableView (Known(..))

-- | Carry a value whose constructor identifies the kind of the index.
type Index :: forall k. k -> Type
data Index x where
  Flag :: Bool -> Index (b :: Bool)
  Letter :: Char -> Index (a :: Type)

-- | Hide a function head together with the kind of its argument.
data App t where
  App :: forall k (m :: Matchability) (f :: k %m -> Type) (x :: k).
         Proxy f -> Known m -> Index x -> App (f x)

-- | Expose a matchable head after checking its witness.
data View t where
  View :: forall k (f :: k %M -> Type) (x :: k). Proxy f -> Index x -> View (f x)

{-# INLINE view #-}
view :: App t -> View t
view (App f KnownM x) = View f x

atFlagIndex :: App (Proxy 'True) -> Index 'True
atFlagIndex app = case view app of View _ index -> index
{-# NOINLINE atFlagIndex #-}

atTypeIndex :: App (Maybe Int) -> Index Int
atTypeIndex app = case view app of View _ index -> index
{-# NOINLINE atTypeIndex #-}

atFlag :: App (Proxy 'True) -> Bool
atFlag app = case atFlagIndex app of Flag b -> b

atType :: App (Maybe Int) -> Char
atType app = case atTypeIndex app of Letter c -> c

-- | Use the same projection at Bool and at Type.
observe :: (Bool, Char)
observe = (atFlag (App @Bool @M @(Proxy :: Bool -> Type) @'True Proxy KnownM (Flag True)),
           atType (App @Type @M @Maybe @Int Proxy KnownM (Letter 'k')))
