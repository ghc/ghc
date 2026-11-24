{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase #-}
{-# LANGUAGE PolyKinds, RankNTypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
-- | Typed terms and substitution from the PLFH DeBruijn chapter.
-- Adapted from PLFH batch 21 under CC BY 4.0. See REFERENCES.md.
module BookDeBruijn (Ty(..), Ctx(..), Lookup(..), Term(..), Value(..),
                     ext, rename, weakenLookup, exts, subst, substSingle, substDouble) where

import Data.Kind (Type)

-- | The object language has natural numbers and function types.
data Ty = TNat | TArr Ty Ty

-- | Nameless typing contexts.
data Ctx = CEmpty | CExt Ctx Ty

-- | Intrinsically typed de Bruijn indices (Γ ∋ A).
data Lookup (g :: Ctx) (a :: Ty) where
  Z :: Lookup (CExt g a) a
  S :: Lookup g a -> Lookup (CExt g b) a

-- | Intrinsically typed STLC terms with naturals, case, and μ-recursion.
data Term (g :: Ctx) (a :: Ty) where
  Var  :: Lookup g a -> Term g a
  Lam  :: Term (CExt g a) b -> Term g (TArr a b)
  App  :: Term g (TArr a b) -> Term g a -> Term g b
  Zero :: Term g TNat
  Suc  :: Term g TNat -> Term g TNat
  Case :: Term g TNat -> Term g a -> Term (CExt g TNat) a -> Term g a
  Mu   :: Term (CExt g a) a -> Term g a

-- | Extend a variable renaming under a binder.
ext :: (forall a. Lookup g a -> Lookup d a)
    -> (forall a. Lookup (CExt g b) a -> Lookup (CExt d b) a)
ext _   Z     = Z
ext rho (S x) = S (rho x)

-- | Simultaneous renaming on intrinsically typed terms.
rename :: (forall a. Lookup g a -> Lookup d a) -> Term g b -> Term d b
rename rho (Var x)      = Var (rho x)
rename rho (Lam n)      = Lam (rename (ext rho) n)
rename rho (App l m)    = App (rename rho l) (rename rho m)
rename _   Zero         = Zero
rename rho (Suc m)      = Suc (rename rho m)
rename rho (Case l m n) = Case (rename rho l) (rename rho m) (rename (ext rho) n)
rename rho (Mu n)       = Mu (rename (ext rho) n)

-- | Shift a typed variable into an extended context.
{-# NOINLINE weakenLookup #-}
weakenLookup :: forall g b a. Lookup g a -> Lookup (CExt g b) a
weakenLookup x = S x

-- | Extend a simultaneous substitution under a binder.
exts :: (forall a. Lookup g a -> Term d a)
     -> (forall a. Lookup (CExt g b) a -> Term (CExt d b) a)
exts _     Z     = Var Z
exts sigma (S x) = rename weakenLookup (sigma x)

-- | Simultaneous substitution on intrinsically typed terms.
subst :: (forall a. Lookup g a -> Term d a) -> Term g b -> Term d b
subst sigma (Var x)      = sigma x
subst sigma (Lam n)      = Lam (subst (exts sigma) n)
subst sigma (App l m)    = App (subst sigma l) (subst sigma m)
subst _     Zero         = Zero
subst sigma (Suc m)      = Suc (subst sigma m)
subst sigma (Case l m n) = Case (subst sigma l) (subst sigma m) (subst (exts sigma) n)
subst sigma (Mu n)       = Mu (subst (exts sigma) n)

-- | Substitute a single closed/open term `w` for variable `Z` in `n` (N [ W ]).
substSingle :: Term (CExt g a) b -> Term g a -> Term g b
substSingle n w = subst (\case { Z -> w; S x -> Var x }) n

-- | Replace the two newest variables and carry outer variables through.
substDouble :: Term (CExt (CExt g a) b) c -> Term g a -> Term g b -> Term g c
substDouble n older newer = subst (\case { Z -> newer; S Z -> older; S (S x) -> Var x }) n

-- | Evidence that a typed term is a value.
type Value :: forall (g :: Ctx) (a :: Ty). Term g a -> Type
data Value m where
  VLam  :: Value (Lam n)
  VZero :: Value Zero
  VSuc  :: Value v -> Value (Suc v)
