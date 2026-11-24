{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeOperators #-}
module NativeCheckedClosure where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeCheckedClosureProvider as P

type Step :: forall g a. P.Expr g a -> P.Expr g a -> Type
data Step m n where
    Beta :: forall g a b (n :: P.Expr (P.Push g a) b) (w :: P.Expr g a).
            Step (P.Call (P.Bind n) w) (P.replace n w)
    Unroll :: forall g a (n :: P.Expr (P.Push g a) a).
              Step (P.Loop n) (P.replace n (P.Loop n))

type NatScope = P.Push P.Empty P.Nat
type Fn = P.Arrow P.Nat P.Nat
type Inserted = P.Bind (P.At (P.Old P.New)) :: P.Expr NatScope Fn
type Body = P.Bind (P.At (P.Old P.New)) :: P.Expr (P.Push NatScope Fn) (P.Arrow P.Nat Fn)

captureAvoided :: Step (P.Call (P.Bind Body) Inserted)
                        (P.Bind (P.Bind (P.At (P.Old (P.Old P.New)))))
captureAvoided = Beta @NatScope @Fn @(P.Arrow P.Nat Fn) @Body @Inserted

caseBinder :: P.replace
                (P.Test P.Lit P.Lit (P.At P.New)
                  :: P.Expr (P.Push P.Empty P.Nat) P.Nat)
                P.Lit :~: P.Test P.Lit P.Lit (P.At P.New)
caseBinder = Refl

#ifdef WRONG_CAPTURE
captured :: Step (P.Call (P.Bind Body) Inserted)
                 (P.Bind (P.Bind (P.At P.New)))
captured = captureAvoided
#endif
