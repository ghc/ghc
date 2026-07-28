{-# LANGUAGE DataKinds, MagicHash, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}

-- Reduced reproducer from https://github.com/kleinreact/clash-crypto-etfr

module T27336 where

import Data.Proxy ( Proxy(..) )
import GHC.TypeNats ( type (+), CmpNat, Nat, Natural, natVal )

type Max :: Nat -> Nat -> Nat
type family Max m n where
  Max m n = OrdCond (CmpNat m n) n n m

type OrdCond :: Ordering -> Nat -> Nat -> Nat -> Nat
type family OrdCond o lt eq gt where
  OrdCond LT lt eq gt = lt
  OrdCond EQ lt eq gt = eq
  OrdCond GT lt eq gt = gt

data Instruction routine = OP | RUN routine

type Instructions :: forall routine. routine -> [Instruction routine]
type family Instructions r

type CallDepth :: forall routine. routine -> Nat
type CallDepth r = 1 + CallDepth# (Instructions r)

type CallDepth# :: forall routine. [Instruction routine] -> Nat
type family CallDepth# is where
  CallDepth# (RUN r : is) =
    -- NB: here we triplicate the redex 'CallDepth# is'; see 'Max'.
    Max (CallDepth r) (CallDepth# is)
  CallDepth# (_     : is) = CallDepth# is
  CallDepth# '[]          = 0

data Routine = L0 | L1 | L2 | L3 | L4 | L5 | L6

type instance Instructions L0 = '[ OP ]
type instance Instructions L1 = '[ RUN L0, RUN L0 ]
type instance Instructions L2 = '[ RUN L1, RUN L1 ]
type instance Instructions L3 = '[ RUN L2, RUN L2 ]
type instance Instructions L4 = '[ RUN L3, RUN L3 ]
type instance Instructions L5 = '[ RUN L4, RUN L4 ]
type instance Instructions L6 = '[ RUN L5, RUN L5 ]
-- NB: each extra level costs roughly 7x

callDepth :: Natural
callDepth = natVal ( Proxy :: Proxy (CallDepth L6) )
