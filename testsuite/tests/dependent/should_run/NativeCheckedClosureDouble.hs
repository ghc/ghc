{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs, Modifiers, PolyKinds, RankNTypes, TypeOperators #-}

module NativeCheckedClosureDouble where

import Data.Type.Equality ((:~:) (Refl))
#ifdef ABSTRACT_RENAMING
import GHC.Exts (U)
#endif
#ifdef HELPER_RULE
import NativeCheckedClosureDoubleRule ()
#endif
import qualified NativeCheckedClosureDoubleProvider as P

#ifdef ABSTRACT_RENAMING
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
