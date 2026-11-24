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
