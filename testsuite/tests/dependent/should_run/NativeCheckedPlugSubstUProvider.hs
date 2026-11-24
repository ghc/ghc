{-# LANGUAGE CPP, DataKinds, EmptyCase, ExperimentalDependentTypes, GADTs, LambdaCase, PolyKinds, RankNTypes #-}
module NativeCheckedPlugSubstUProvider
#ifdef HIDE_CTX
  (Nat(..), Fin(..), UTerm(..), UCtx(CHole, CAppL, CAppR), SCtx(..),
   plug, plugS, extU, renameU, extsU, substU, substSingleU)
#endif
where

data Nat = Zero | Succ Nat

data Fin (n :: Nat) where
  FZ :: Fin ('Succ n)
  FS :: Fin n -> Fin ('Succ n)

data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm ('Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m ('Succ n) -> UCtx m n
  CAppL :: UCtx m n -> UTerm n -> UCtx m n
  CAppR :: UTerm n -> UCtx m n -> UCtx m n

data SCtx (m :: Nat) (n :: Nat) where
  SHole :: SCtx n n
  SLam  :: SCtx ('Succ m) ('Succ n) -> SCtx ('Succ m) n
  SAppL :: SCtx m n -> UTerm n -> SCtx m n
  SAppR :: UTerm n -> SCtx m n -> SCtx m n

plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole       t =
#ifdef STRICT_PLUG
  t `seq` t
#else
  t
#endif
plug (CLam c)    t =
#ifdef CHANGED_PLUG
  ULam (plug c (UApp t t))
#else
  ULam (plug c t)
#endif
plug (CAppL c r) t = UApp (plug c t) r
plug (CAppR l c) t = UApp l (plug c t)

plugS :: SCtx m n -> UTerm m -> UTerm n
plugS SHole       t = t
plugS (SLam c)    t = ULam (plugS c t)
plugS (SAppL c r) t = UApp (plugS c t) r
plugS (SAppR l c) t = UApp l (plugS c t)

extU :: (Fin m -> Fin n) -> Fin ('Succ m) -> Fin ('Succ n)
extU _   FZ     = FZ
extU rho (FS x) = FS (rho x)

renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU rho (UVar x)   = UVar (rho x)
renameU rho (ULam b)   = ULam (renameU (extU rho) b)
renameU rho (UApp l m) = UApp (renameU rho l) (renameU rho m)

extsU :: (Fin m -> UTerm n) -> Fin ('Succ m) -> UTerm ('Succ n)
extsU _     FZ     = UVar FZ
extsU sigma (FS x) = renameU FS (sigma x)

substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU sigma (UVar x)   = sigma x
substU sigma (ULam b)   = ULam (substU (extsU sigma) b)
substU sigma (UApp l m) = UApp (substU sigma l) (substU sigma m)

substSingleU :: UTerm ('Succ n) -> UTerm n -> UTerm n
substSingleU b w = substU (\case
  FZ -> w
#ifdef STRICT_SUBST
  FS x -> w `seq` UVar x
#else
  FS x -> UVar x
#endif
  ) b
