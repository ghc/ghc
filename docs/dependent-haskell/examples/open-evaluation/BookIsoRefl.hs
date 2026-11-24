{-# LANGUAGE ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
-- | Reflexivity of isomorphism from the PLFH Isomorphism chapter.
module BookIsoRefl (isoRefl) where

import BookIsomorphism (Iso(..))
import Data.Type.Equality ((:~:)(Refl))

-- | Applying the Prelude identity twice returns its input.
identityRoundTrip :: foreach (x :: a) -> id (id x) :~: x
identityRoundTrip _ = Refl

-- | Every type is isomorphic to itself.
isoRefl :: Iso a a
isoRefl = MkIso id id identityRoundTrip identityRoundTrip
