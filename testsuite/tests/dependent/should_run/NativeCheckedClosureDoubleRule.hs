{-# LANGUAGE DataKinds, GADTs, RankNTypes #-}

module NativeCheckedClosureDoubleRule () where

import qualified NativeCheckedClosureDoubleProvider as P

{-# RULES "rename-literal"
    P.renameExpr (P.Old :: forall a. P.Slot P.Empty a -> P.Slot (P.Push P.Empty P.Nat) a)
                 (P.Lit :: P.Expr P.Empty P.Nat) = P.Incr P.Lit #-}
