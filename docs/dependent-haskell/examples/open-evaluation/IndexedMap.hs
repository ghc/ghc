{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleInstances, Modifiers, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeFamilies #-}
module IndexedMap where

import Data.Kind (Constraint, Type)
import GHC.Matchability (Matchability, M, U)

type Batch :: forall (m :: Matchability). (Type %m -> Type) -> Type
data Batch @m f = Batch [f Bool]

type IndexedMap :: forall (m :: Matchability). (Type %m -> Type) -> Constraint
class IndexedMap @m f where
    mapIndexed :: (f Bool -> f Bool) -> Batch @m f -> Batch @m f

instance IndexedMap @m f where
    mapIndexed h (Batch xs) = Batch (map h xs)

type Pick :: Bool -> Type %U -> Type
type family Pick choice a where
    Pick 'True a = Maybe a
    Pick 'False a = [a]

type Forward :: forall (m :: Matchability). (Type %m -> Type) -> Type
type family Forward @m f where
    Forward @m f = Batch @m f

type FamilyBatch = Forward (Pick 'True)
type MatchableBatch = Forward @M Maybe

familyInput :: FamilyBatch
familyInput = Batch [Just False, Nothing]

matchableInput :: MatchableBatch
matchableInput = Batch [Just False, Nothing]

getCells :: Batch @m f -> [f Bool]
getCells (Batch xs) = xs
