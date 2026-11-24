{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

-- | Two computations from the Lists and Denotational chapters of PLFA.
module BookCalculations (downFrom, lengthDownFrom, Value(..), dom, domJoin) where

import HaskLib (rewrite, size, (:~:)(Refl))
import HaskLib.Nat (Nat(..))

-- | List each smaller natural in descending order.
downFrom :: Nat -> [Nat]
downFrom Zero     = []
downFrom (Succ n) = n : downFrom n

-- | The countdown has one element for each successor.
lengthDownFrom :: foreach (n :: Nat) -> size (downFrom n) :~: n
lengthDownFrom Zero     = Refl
lengthDownFrom (Succ n) = rewrite (lengthDownFrom n) Refl

-- | A finite approximation of a function in the book's denotational semantics.
data Value where
    Bottom :: Value
    Arrow, Join :: Value -> Value -> Value
    deriving Show

-- | Collect the inputs of the arrows in a value.
dom :: Value -> Value
dom Bottom      = Bottom
dom (Arrow v _) = v
dom (Join l r)  = Join (dom l) (dom r)

-- | Taking the domain distributes over a join.
domJoin :: foreach (l :: Value) -> foreach (r :: Value)
        -> dom ('Join l r) :~: 'Join (dom l) (dom r)
domJoin _ _ = Refl
