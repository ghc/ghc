{-# LANGUAGE CPP #-}
module NativeOpenMonusSource
  ( monus
#ifdef MIXED_PROVIDER
  , reflexivity
#endif
#ifdef LOCAL_PROOF
  , monusZero
#endif
  ) where

#ifdef MIXED_PROVIDER
import Data.Type.Equality ((:~:) (Refl))
#endif
import PreludeMapLengthSize (Nat (..))

monus :: Nat -> Nat -> Nat
monus m        Zero     = m
monus Zero     (Succ _) = Zero
monus (Succ m) (Succ n) = monus m n

#ifdef MIXED_PROVIDER
reflexivity :: foreach (n :: Nat) -> n :~: n
reflexivity _ = Refl
#endif

#ifdef LOCAL_PROOF
monusZero :: foreach (n :: Nat) -> monus n 'Zero :~: n
monusZero _ = Refl
#endif
