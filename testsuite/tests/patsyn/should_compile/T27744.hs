{-# LANGUAGE CPP #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

-- | A facility for faking GADTs that work sufficiently similarly
-- to unary natural numbers.
module T27744
  ( Nattish (Zeroy, Succy)
  )
  where
import Unsafe.Coerce (unsafeCoerce)
import Data.Kind (Type)

type Nattish :: forall k. k -> (k -> k) -> k -> Type
newtype Nattish zero succ n = Nattish Word
type role Nattish nominal nominal nominal

data Res zero succ n where
  ResZero :: Res zero succ zero
  ResSucc :: !(Nattish zero succ n) -> Res zero succ (succ n)

check :: Nattish zero succ n -> Res zero succ n
check (Nattish 0) = unsafeCoerce ResZero
check (Nattish n) = unsafeCoerce $ ResSucc (Nattish (n - 1))

pattern Zeroy :: forall {k} zero succ (n :: k). () => n ~ zero => Nattish zero succ n
pattern Zeroy <- (check -> ResZero)
  where
    Zeroy = Nattish 0
{-# INLINE Zeroy #-}

pattern Succy :: forall {k} zero succ (n :: k). () => forall (n' :: k). n ~ succ n' => Nattish zero succ n' -> Nattish zero succ n
pattern Succy n <- (check -> ResSucc n)
  where
    Succy (Nattish n) = Nattish (n + 1)
{-# INLINE Succy #-}

{-# COMPLETE Zeroy, Succy #-}
