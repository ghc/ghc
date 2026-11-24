{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications #-}
-- | Capture-avoiding beta reduction for the PLFH DeBruijn terms.
module BookBetaReduction (Step(..), captureAvoided, freeIndexAfterBeta) where

import BookDeBruijn
import Data.Kind (Type)

-- | The beta step substitutes the argument only after it becomes a value.
type Step :: forall g a. Term g a -> Term g a -> Type
data Step m n where
    BetaLam :: forall g a b (body :: Term (CExt g a) b) (w :: Term g a).
               Value w -> Step (App (Lam body) w) (substSingle body w)

type Outer = CExt CEmpty TNat
type Fn = TArr TNat TNat
type Argument = Lam (Var (S Z)) :: Term Outer Fn
type Body = Lam (Var (S Z)) :: Term (CExt Outer Fn) (TArr TNat Fn)

-- | The free variable in the argument crosses one extra binder.
captureAvoided :: Step (App (Lam Body) Argument) (Lam (Lam (Var (S (S Z)))))
captureAvoided = BetaLam @Outer @Fn @(TArr TNat Fn) @Body @Argument VLam

-- | Read the index of the variable in the ordinary substitution result.
freeIndexAfterBeta :: Maybe Int
freeIndexAfterBeta = case substSingle body argument of
    Lam (Lam (Var x)) -> Just (distance x)
    _ -> Nothing
  where
    body :: Term (CExt Outer Fn) (TArr TNat Fn)
    body = Lam (Var (S Z))
    argument :: Term Outer Fn
    argument = Lam (Var (S Z))
    distance :: Lookup g a -> Int
    distance Z = 0
    distance (S i) = 1 + distance i
