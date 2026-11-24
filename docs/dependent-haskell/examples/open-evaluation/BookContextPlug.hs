{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
-- | Untyped one-hole contexts from PLFH ContextualEquivalence.
-- Adapted from PLFH under CC BY 4.0. See REFERENCES.md.
module BookContextPlug (UCtx(..), plug) where

import BookParallel (UTerm(..))
import HaskLib.Nat (Nat(..))

-- | One-hole contexts linking hole scope @m@ to outer scope @n@.
data UCtx (m :: Nat) (n :: Nat) where
    CHole :: UCtx n n
    CLam  :: UCtx m (Succ n) -> UCtx m n
    CAppL :: UCtx m n -> UTerm n -> UCtx m n
    CAppR :: UTerm n -> UCtx m n -> UCtx m n

-- | Fill the hole of a context with a term in the hole's scope.
plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole       t = t
plug (CLam c)    t = ULam (plug c t)
plug (CAppL c r) t = UApp (plug c t) r
plug (CAppR l c) t = UApp l (plug c t)
