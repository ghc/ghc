{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module CastedConstructorGivens where

import Data.Kind (Type)

-- | Describe the branching structure of a tree.
data Shape = Leaf | Branch Shape Shape

-- | Index each tree by its shape.
data Tree :: Shape -> Type where
    Tip :: Tree 'Leaf
    Fork :: Tree l -> Tree r -> Tree ('Branch l r)

-- | Provide a witness for every leaf and branch.
type Present :: forall s. Tree s -> Type
data Present tree where
    Here :: Present 'Tip
    Both :: Present l -> Present r -> Present ('Fork l r)

-- | Relate trees of the same shape.
type Rel :: forall s. Tree s -> Tree s -> Type
data Rel from to where
    Same :: Rel t t
    Branches :: Rel l l' -> Rel r r' -> Rel ('Fork l r) ('Fork l' r')

-- | Both matches refine the shape of the same tree.
preserve :: Rel t u -> Present t -> Present u
preserve Same witness = witness
preserve (Branches left right) (Both l r) =
    Both (preserve left l) (preserve right r)

data Depth = Stop | Next Depth

data Env (n :: Depth) where
    Empty :: Env 'Stop
    Push :: Env n -> Bool -> Env ('Next n)

type Insert :: forall n. Env n -> Env ('Next n) -> Type
data Insert e e' where
    First :: Insert e ('Push e 'False)
    Later :: Insert e e' -> Insert ('Push e a) ('Push e' a)

type Den :: forall n. Env n -> Type
data Den e where
    Intro :: Den ('Push e a) -> Den e

type Packed :: forall n. Env n -> Type
data Packed e where
    Pack :: Insert e e' -> Den e' -> Packed e

-- The recursive call fixes the kinds of the existential match through
-- unification. The casted Givens still refer to those metavariables.
nested :: Insert e e' -> Den e -> Packed e
nested ins (Intro body) =
    case nested (Later ins) body of
        Pack (Later rest) den -> Pack rest (Intro den)
