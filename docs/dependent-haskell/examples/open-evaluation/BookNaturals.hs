{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

-- | Natural number operations used by the PLFH Induction chapter.
module BookNaturals (monus, times, pow, threePowFour, threeMonusTwo, twoMonusThree) where

import Data.Type.Equality ((:~:)(Refl))
import HaskLib.Nat (Nat(..), plus)

-- | Subtract natural numbers, stopping at zero.
monus :: Nat -> Nat -> Nat
monus m        Zero     = m
monus Zero     (Succ n) = Zero
monus (Succ m) (Succ n) = monus m n

-- | Multiply by recursion on the first operand.
times :: Nat -> Nat -> Nat
times Zero     n = Zero
times (Succ m) n = plus n (times m n)

-- | Raise the base to the natural exponent.
pow :: Nat -> Nat -> Nat
pow m Zero     = Succ Zero
pow m (Succ n) = times m (pow m n)

-- | Three to the fourth power is eighty-one.
threePowFour :: pow 3 4 :~: 81
threePowFour = Refl

-- | Truncated subtraction returns the difference when it is positive.
threeMonusTwo :: monus 3 2 :~: 1
threeMonusTwo = Refl

-- | Truncated subtraction stops at zero.
twoMonusThree :: monus 2 3 :~: 0
twoMonusThree = Refl
