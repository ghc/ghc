{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, EmptyCase, ExperimentalDependentTypes, GADTs #-}
{-# LANGUAGE LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module Main where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedPlugSubstUProvider as P

congLam :: a :~: b -> P.ULam a :~: P.ULam b
congLam Refl = Refl

plugLamEq
  :: forall (m :: P.Nat) (n :: P.Nat) (t :: P.UTerm m).
     foreach (c :: P.UCtx m ('P.Succ n)) ->
     P.plug (P.CLam c) t :~: P.ULam (P.plug c t)
plugLamEq _ = Refl

plugAppREq
  :: forall (m :: P.Nat) (n :: P.Nat) (l :: P.UTerm n) (t :: P.UTerm m).
     foreach (c :: P.UCtx m n) ->
     P.plug (P.CAppR l c) t :~: P.UApp l (P.plug c t)
plugAppREq _ = Refl

plugCong
  :: forall (m :: P.Nat) (n :: P.Nat) (t :: P.UTerm m) (t' :: P.UTerm m).
     foreach (c :: P.UCtx m n) -> t :~: t' -> P.plug c t :~: P.plug c t'
plugCong c eq = case c of
  P.CHole      -> eq
  P.CLam c'    -> congLam (plugCong c' eq)
  P.CAppL c' _ -> case plugCong c' eq of Refl -> Refl
  P.CAppR _ c' -> case plugCong c' eq of Refl -> Refl

plugSCong
  :: forall (m :: P.Nat) (n :: P.Nat) (t :: P.UTerm m) (t' :: P.UTerm m).
     foreach (c :: P.SCtx m n) -> t :~: t' -> P.plugS c t :~: P.plugS c t'
plugSCong c eq = case c of
  P.SHole      -> eq
  P.SLam c'    -> congLam (plugSCong c' eq)
  P.SAppL c' _ -> case plugSCong c' eq of Refl -> Refl
  P.SAppR _ c' -> case plugSCong c' eq of Refl -> Refl

type BetaStep :: forall (n :: P.Nat). P.UTerm n -> P.UTerm n -> Type
data BetaStep t t' where
  Beta :: forall (n :: P.Nat) (b :: P.UTerm ('P.Succ n)) (w :: P.UTerm n).
          BetaStep (P.UApp (P.ULam b) w) (P.substSingleU b w)

captureAvoidU
  :: BetaStep
       (P.UApp (P.ULam (P.ULam (P.UVar (P.FS P.FZ)))) (P.UVar P.FZ) :: P.UTerm ('P.Succ 'P.Zero))
       (P.ULam (P.UVar (P.FS P.FZ)))
captureAvoidU = Beta

substVarFZ :: forall (n :: P.Nat) (w :: P.UTerm n). P.substSingleU (P.UVar P.FZ) w :~: w
substVarFZ = Refl

substVarFS :: forall (n :: P.Nat) (x :: P.Fin n) (w :: P.UTerm n).
              P.substSingleU (P.UVar (P.FS x)) w :~: P.UVar x
substVarFS = Refl

substAppEq :: forall (n :: P.Nat) (l :: P.UTerm ('P.Succ n)) (m :: P.UTerm ('P.Succ n)) (w :: P.UTerm n).
              P.substSingleU (P.UApp l m) w :~: P.UApp (P.substSingleU l w) (P.substSingleU m w)
substAppEq = Refl

type SimRel :: forall (n :: P.Nat). P.UTerm n -> P.UTerm n -> Type
data SimRel l r where
  SimVar :: SimRel (P.UVar x) (P.UVar x)
  SimLam :: SimRel b b' -> SimRel (P.ULam b) (P.ULam b')
  SimApp :: SimRel l l' -> SimRel m m' -> SimRel (P.UApp l m) (P.UApp l' m')

simRenameU
  :: forall (m :: P.Nat) (n :: P.Nat) (r :: P.Fin m -> P.Fin n)
            (t :: P.UTerm m) (t' :: P.UTerm m).
     SimRel t t' -> SimRel (P.renameU r t) (P.renameU r t')
simRenameU SimVar       = SimVar
simRenameU (SimLam b)   = SimLam (simRenameU @_ @_ @(P.extU r) b)
simRenameU (SimApp l m) = SimApp (simRenameU @_ @_ @r l) (simRenameU @_ @_ @r m)

-- Same-module partial-known open evaluation of untyped substitution (without -fwrite-source-equations)
localExtU :: (P.Fin m -> P.Fin n) -> P.Fin ('P.Succ m) -> P.Fin ('P.Succ n)
localExtU _   P.FZ     = P.FZ
localExtU rho (P.FS x) = P.FS (rho x)

localRenameU :: (P.Fin m -> P.Fin n) -> P.UTerm m -> P.UTerm n
localRenameU rho (P.UVar x)   = P.UVar (rho x)
localRenameU rho (P.ULam b)   = P.ULam (localRenameU (localExtU rho) b)
localRenameU rho (P.UApp l m) = P.UApp (localRenameU rho l) (localRenameU rho m)

localExtsU :: (P.Fin m -> P.UTerm n) -> P.Fin ('P.Succ m) -> P.UTerm ('P.Succ n)
localExtsU _     P.FZ     = P.UVar P.FZ
localExtsU sigma (P.FS x) = localRenameU P.FS (sigma x)

localSubstU :: (P.Fin m -> P.UTerm n) -> P.UTerm m -> P.UTerm n
localSubstU sigma (P.UVar x)   = sigma x
localSubstU sigma (P.ULam b)   = P.ULam (localSubstU (localExtsU sigma) b)
localSubstU sigma (P.UApp l m) = P.UApp (localSubstU sigma l) (localSubstU sigma m)

localSubstSingleU :: P.UTerm ('P.Succ n) -> P.UTerm n -> P.UTerm n
localSubstSingleU b w = localSubstU (\case { P.FZ -> w; P.FS x -> P.UVar x }) b

localSubstId :: forall (n :: P.Nat) (w :: P.UTerm n). localSubstSingleU ('P.UVar 'P.FZ) w :~: w
localSubstId = Refl

#ifdef WRONG_HOLE
wrongHole :: forall (n :: P.Nat) (m :: P.UTerm n). P.plug P.CHole m :~: P.ULam (P.UVar P.FZ)
wrongHole = Refl
#endif

#ifdef WRONG_HOLE_SELF
wrongHoleSelf :: forall (n :: P.Nat) (m :: P.UTerm n). P.plug P.CHole m :~: P.UApp m m
wrongHoleSelf = Refl
#endif

#ifdef WRONG_LAM
wrongLam :: forall (m :: P.Nat) (n :: P.Nat) (t :: P.UTerm m).
            foreach (c :: P.UCtx m ('P.Succ n)) ->
            P.plug (P.CLam c) t :~: P.plug c t
wrongLam _ = Refl
#endif

#ifdef WRONG_APPR
wrongAppR :: forall (m :: P.Nat) (n :: P.Nat) (l :: P.UTerm n) (t :: P.UTerm m).
             foreach (c :: P.UCtx m n) ->
             P.plug (P.CAppR l c) t :~: P.UApp (P.plug c t) l
wrongAppR _ = Refl
#endif

#ifdef WRONG_CAPTURE_U
wrongCaptureU
  :: BetaStep
       (P.UApp (P.ULam (P.ULam (P.UVar (P.FS P.FZ)))) (P.UVar P.FZ) :: P.UTerm ('P.Succ 'P.Zero))
       (P.ULam (P.UVar P.FZ))
wrongCaptureU = Beta
#endif

#ifdef WRONG_LOCAL_SUBST
wrongLocalSubst :: forall (n :: P.Nat) (x :: P.Fin n) (w :: P.UTerm n).
                   localSubstSingleU ('P.UVar ('P.FS x)) w :~: w
wrongLocalSubst = Refl
#endif

#ifdef RULE_PLUG
{-# RULES "override plug" forall (c :: P.UCtx n n) (t :: P.UTerm n). P.plug c t = t #-}
#endif

#ifdef RULE_SUBST_U
{-# RULES "override substSingleU" forall b (w :: P.UTerm n). P.substSingleU b w = w #-}
#endif

runCong :: P.UCtx ('P.Succ 'P.Zero) 'P.Zero -> Bool
runCong ctx =
  case plugCong @('P.Succ 'P.Zero) @'P.Zero @(P.UVar P.FZ) @(P.UVar P.FZ) ctx Refl of
    Refl -> True

main :: IO ()
main = do
  let r1 = plugLamEq @('P.Succ 'P.Zero) @'P.Zero @(P.UVar P.FZ) P.CHole
      r2 = runCong (P.CLam (P.CAppL (P.CAppR (error "unused left") P.CHole) (error "unused right")))
      r3 = localSubstId @('P.Succ 'P.Zero) @(P.UVar P.FZ)
  case P.plug (P.CAppL P.CHole (error "unused right")) (error "unused hole" :: P.UTerm 'P.Zero) of
    P.UApp _ _ | r2 -> print (r1, r3)
    _               -> error "unreachable"
