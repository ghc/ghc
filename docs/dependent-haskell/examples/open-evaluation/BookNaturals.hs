-- | Natural number operations used by the PLFH Induction chapter.
module BookNaturals (monus) where

import HaskLib.Nat (Nat(..))

-- | Subtract natural numbers, stopping at zero.
monus :: Nat -> Nat -> Nat
monus m        Zero     = m
monus Zero     (Succ n) = Zero
monus (Succ m) (Succ n) = monus m n
