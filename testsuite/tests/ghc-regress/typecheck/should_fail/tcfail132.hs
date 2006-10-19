-- Fails with a kind error.
-- The current error message was rather horrible (trac bug #312):
--
--    Kind error: Expecting kind `k_a1JA -> k_a1JE -> k_a1JI -> *',
--	          but `DUnit t' has kind `k_a1JA -> k_a1JE -> *'
--
-- as we couldn't tidy kinds, becuase they didn't have OccNames.
-- This test recalls the bad error message.

module ShouldFail where

newtype Object f' f t o1 o2  = Object (f' t o1 -> f t o2)
type DUnit t o1 o2       = ()

type T f g t o1 o2  = Either (f t o1 o2) (g t o1 o2)

type LiftObject t f' f         = T (Object f' f t) (DUnit t)
 

