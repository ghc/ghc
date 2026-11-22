{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module ParametricFilter (FilterKind, FilterNaturality, filterNaturality) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified ParametricFilterAdapters as A

-- | The kind of an erased polymorphic filter-shaped function.
type FilterKind = forall a. (a %U -> Bool) %U -> [a] %U -> [a]

-- | State the naturality law at Bool and Ordering. The free theorem applies
-- to parametric implementations. This alias supplies no proof.
type FilterNaturality = forall (r :: FilterKind). foreach (xs :: [Bool])
                      -> r A.isGT (map A.boolToOrdering xs)
                         :~: map A.boolToOrdering (r A.isTrue xs)

-- | Prove the law for the installed filter by induction on a finite list
-- with defined Boolean elements.
filterNaturality :: foreach (xs :: [Bool])
                 -> filter A.isGT (map A.boolToOrdering xs)
                    :~: map A.boolToOrdering (filter A.isTrue xs)
filterNaturality [] = Refl
filterNaturality (x : xs) = case x of
    False -> filterNaturality xs
    True -> case filterNaturality xs of Refl -> Refl
