{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeOperators #-}

module NativeCheckedClosureDouble where

#ifdef ABSTRACT_SUBST
import Data.Kind (Type)
#endif
import Data.Type.Equality ((:~:) (Refl))
#if defined(ABSTRACT_RENAMING) || defined(ABSTRACT_SUBST) || defined(ABSTRACT_SUBST_EQ)
import GHC.Exts (U)
#endif
#ifdef HELPER_RULE
import NativeCheckedClosureDoubleRule ()
#endif
import qualified NativeCheckedClosureDoubleProvider as P

#if defined(ABSTRACT_SUBST) || defined(ABSTRACT_SUBST_EQ)
type Renaming g d = forall a. P.Slot g a %U -> P.Slot d a
type SubstFn g d = forall a. P.Slot g a %U -> P.Expr d a

walkVariable :: forall g d a (s :: SubstFn g d) (t :: SubstFn g d) (x :: P.Slot g a).
                P.walk s (P.At x) :~:
#ifdef WRONG_SUBST
                t x
#else
                s x
#endif
walkVariable = Refl

walkBinders :: forall g d (s :: SubstFn g d)
                      (n :: P.Expr (P.Push (P.Push g P.Nat) P.FlagSort) P.Nat).
               P.walk s (P.BindBoth n)
               :~: P.BindBoth (P.walk (P.extendExpr (P.extendExpr s)) n)
walkBinders = Refl

extFresh :: forall g d b (s :: SubstFn g d).
            P.extendExpr @g @d @b s (P.New :: P.Slot (P.Push g b) b)
            :~: P.At P.New
extFresh = Refl

#ifdef LOCAL_EXT_RULE
{-# RULES "local-extend-expr"
    P.extendExpr (P.At :: forall a. P.Slot P.Empty a -> P.Expr P.Empty a)
                 (P.New :: P.Slot (P.Push P.Empty P.Nat) P.Nat) = P.Lit #-}
#endif
#ifdef LOCAL_WALK_RULE
{-# RULES "local-walk-expr"
    P.walk (P.At :: forall a. P.Slot P.Empty a -> P.Expr P.Empty a)
           (P.Lit :: P.Expr P.Empty P.Nat) = P.Incr P.Lit #-}
#endif

#ifdef ABSTRACT_SUBST
type Rel :: forall (g :: P.Scope) (a :: P.Sort). P.Expr g a -> P.Expr g a -> Type
data Rel m n where
    RelAt       :: Rel (P.At x) (P.At x)
    RelLit      :: Rel P.Lit P.Lit
    RelIncr     :: Rel m n -> Rel (P.Incr m) (P.Incr n)
    RelFlag     :: Rel P.Flag P.Flag
    RelBoth     :: Rel l l' -> Rel r r' -> Rel (P.Both l r) (P.Both l' r')
    RelBind     :: Rel m n -> Rel (P.Bind m) (P.Bind n)
    RelBindBoth :: Rel m n -> Rel (P.BindBoth m) (P.BindBoth n)

relRename :: forall g d a (r :: Renaming g d) (m :: P.Expr g a) (n :: P.Expr g a).
             Rel m n -> Rel (P.renameExpr r m) (P.renameExpr r n)
relRename RelAt = RelAt
relRename RelLit = RelLit
relRename (RelIncr s) = RelIncr (relRename @_ @_ @_ @r s)
relRename RelFlag = RelFlag
relRename (RelBoth l r') = RelBoth (relRename @_ @_ @_ @r l) (relRename @_ @_ @_ @r r')
relRename (RelBind s) = RelBind (relRename @_ @_ @_ @(P.extendName r) s)
relRename (RelBindBoth s) = RelBindBoth (relRename @_ @_ @_ @(P.extendName (P.extendName r)) s)

simExt :: forall g d b (s :: SubstFn g d) (t :: SubstFn g d).
          (forall a. foreach (x :: P.Slot g a) -> Rel (s x) (t x)) ->
          forall a. foreach (x :: P.Slot (P.Push g b) a) ->
#ifdef WRONG_SUBST_EXT
                    Rel (P.extendExpr @g @d @b s x) (P.extendExpr @g @d @b s x)
#else
                    Rel (P.extendExpr @g @d @b s x) (P.extendExpr @g @d @b t x)
#endif
simExt step slot = case slot of
    P.New -> RelAt
    P.Old x -> relRename @d @(P.Push d b) @_ @(P.weakenSlot @d @b) @(s x) @(t x) (step x)
#endif
#elif defined(ABSTRACT_RENAMING)
type Renaming g d = forall a. P.Slot g a %U -> P.Slot d a

renameVariable :: forall g d a (r :: Renaming g d) (s :: Renaming g d) (x :: P.Slot g a).
                  P.renameExpr r (P.At x) :~:
#ifdef WRONG_RENAMING
                  P.At (s x)
#else
                  P.At (r x)
#endif
renameVariable = Refl

renameBinders :: forall g d (r :: Renaming g d)
                        (n :: P.Expr (P.Push (P.Push g P.Nat) P.FlagSort) P.Nat).
                 P.renameExpr r (P.BindBoth n)
                 :~: P.BindBoth (P.renameExpr (P.extendName (P.extendName r)) n)
renameBinders = Refl
#else
type Outer = P.Push P.Empty P.Nat
type BodyScope = P.Push (P.Push Outer P.Nat) P.FlagSort

type Body = P.Both (P.At (P.Old P.New)) (P.At P.New) :: P.Expr BodyScope P.Nat
type Under = P.BindBoth
    (P.Both (P.At (P.Old (P.Old (P.Old P.New)))) (P.At (P.Old (P.Old P.New))))
      :: P.Expr BodyScope P.Nat

twoReplacements :: P.replaceBoth Body (P.Lit :: P.Expr Outer P.Nat) P.Flag
                 :~: P.Both P.Lit P.Flag
twoReplacements = Refl

underBoth :: P.replaceBoth Under (P.At P.New :: P.Expr Outer P.Nat) P.Flag
          :~: P.BindBoth (P.Both (P.At (P.Old (P.Old P.New))) P.Flag)
underBoth = Refl

#ifdef WRONG_DOUBLE_CAPTURE
captured :: P.replaceBoth Under (P.At P.New :: P.Expr Outer P.Nat) P.Flag
         :~: P.BindBoth (P.Both (P.At (P.Old P.New)) P.Flag)
captured = Refl
#endif
#endif
