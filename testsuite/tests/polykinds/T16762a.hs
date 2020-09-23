{-# LANGUAGE TypeFamilies, GADTs, DataKinds, PolyKinds, ExplicitForAll #-}

module T16762a where

import Data.Kind

data SameKind :: k -> k -> *

type family F a

-- This should jolly well be rejected!
type instance forall a k (b::k). F (SameKind a b) = Int

