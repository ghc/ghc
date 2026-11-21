{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeApplications #-}
module MatchableView (Applied(..), Known(..), viewResult, betaResult) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M, Matchability)
import MatchableEquality (project)

-- | Supply the matchability needed to decompose an application.
data Known (m :: Matchability) where
    KnownM :: Known M

-- | Store the argument and an operation on applications of the same head.
data Applied t where
    Applied :: forall (m :: Matchability) (f :: Type %m -> Type) x.
               Proxy f -> Known m -> x -> (f Int -> f Bool) -> Applied (f x)

data View t where
    View :: forall (f :: Type -> Type) x.
            Proxy f -> x -> (f Int -> f Bool) -> View (f x)

view :: Applied t -> View t
view (Applied f KnownM x run) = View f x run
{-# INLINE view #-}

-- | Project both components after inlining the constructor view.
viewResult :: Applied (Maybe Char) -> (Char, Maybe Int -> Maybe Bool)
viewResult a = case view a of View _ x run -> (x, run)

-- | Project the head after substituting the refined type into an imported function.
betaResult :: Applied (Maybe Char) -> Maybe Int -> Maybe Bool
betaResult (Applied (_ :: Proxy f) KnownM (_ :: x) run) = project @f @x Refl run
