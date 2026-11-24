{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

module NativeCheckedClosureDouble where

import Data.Type.Equality ((:~:) (Refl))
#ifdef HELPER_RULE
import NativeCheckedClosureDoubleRule ()
#endif
import qualified NativeCheckedClosureDoubleProvider as P

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
