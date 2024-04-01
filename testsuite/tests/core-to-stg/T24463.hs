{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module T24463 where

import Unsafe.Coerce (unsafeCoerce)

data Term where
  BinaryTerm :: !arg1 -> !arg2 -> Term

f :: Term -> (b, c)
f (BinaryTerm t1 t2) = (unsafeCoerce t1, unsafeCoerce t2)

pattern P :: b -> c -> Term
pattern P t1 t2 <- (f -> (t1, t2))
