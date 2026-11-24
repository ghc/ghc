{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, GADTs #-}
-- | The fixed-point reduction from the PLFH Lambda chapter.
module BookLambdaUnfold (Term(..), substTerm, Step(..), unwrapVariable) where

import HaskLib.Nat (Nat(..))

-- | A term in the variable and fixed-point fragment.
data Term = Var Nat | TMu Nat Term

-- | Compare the variable names in this fragment.
same :: Nat -> Nat -> Bool
same Zero Zero = True
same Zero (Succ _) = False
same (Succ _) Zero = False
same (Succ x) (Succ y) = same x y

-- | Substitute under a binder when it has a different name.
substTerm :: Term -> Nat -> Term -> Term
substTerm (Var x) y v = case same x y of
    True -> v
    False -> Var x
substTerm (TMu x m) y v = case same x y of
    True -> TMu x m
    False -> TMu x (substTerm m y v)

-- | A fixed point unfolds to its body with the fixed point substituted.
data Step (m :: Term) (n :: Term) where
    BetaMu :: forall x m. Step (TMu x m) (substTerm m x (TMu x m))

-- | Unfolding the fixed point of the identity returns the original term.
unwrapVariable :: Step (TMu Zero (Var Zero)) (TMu Zero (Var Zero))
unwrapVariable = BetaMu
