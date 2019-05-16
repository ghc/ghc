{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE EmptyCase #-}

module Fin (Nat (..), Fin (FZ, FS)) where
import Numeric.Natural
import Unsafe.Coerce

data Nat = Z | S Nat

-- Fin *must* be exported abstractly (or placed in an Unsafe
-- module) to maintain type safety.
newtype Fin (n :: Nat) = Fin Natural

data FinView n where
  VZ :: FinView ('S n)
  VS :: !(Fin n) -> FinView ('S n)

viewFin :: Fin n -> FinView n
viewFin (Fin 0) = unsafeCoerce VZ
viewFin (Fin n) = unsafeCoerce (VS (Fin (n - 1)))

pattern FZ :: () => n ~ 'S m => Fin n
pattern FZ <- (viewFin -> VZ) where
  FZ = Fin 0

pattern FS :: () => n ~ 'S m => Fin m -> Fin n
pattern FS m <- (viewFin -> VS m) where
  FS (Fin m) = Fin (1 + m)

{-# COMPLETE FZ, FS #-}

finZAbsurd :: Fin 'Z -> a
finZAbsurd x = case x of

