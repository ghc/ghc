{-# LANGUAGE CPP, DataKinds, GADTs, RankNTypes #-}

module NativeCheckedClosureDoubleRule () where

import qualified NativeCheckedClosureDoubleProvider as P

#ifdef ABSTRACT_RENAMING
{-# RULES "extend-name"
    P.extendName (P.Old :: forall a. P.Slot P.Empty a -> P.Slot (P.Push P.Empty P.Nat) a)
                 (P.New :: P.Slot (P.Push P.Empty P.Nat) P.Nat) = P.Old P.New #-}
#else
{-# RULES "rename-literal"
    P.renameExpr (P.Old :: forall a. P.Slot P.Empty a -> P.Slot (P.Push P.Empty P.Nat) a)
                 (P.Lit :: P.Expr P.Empty P.Nat) = P.Incr P.Lit #-}
#endif
