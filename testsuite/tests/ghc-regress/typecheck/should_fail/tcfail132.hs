-- Fails with a kind error.
-- The current error message is rather horrible:
--
--    Kind error: Expecting kind `k_a1JA -> k_a1JE -> k_a1JI -> *',
--	          but `DUnit t' has kind `k_a1JA -> k_a1JE -> *'
--
-- but we can't tidy kinds at the moment, becuase they don't have OccNames.
-- This test recalls the bad error message.
--
-- One way to improve matters would be to compile type defns in dependency order
-- I'm not sure when we stopped doing so; and stopping doing so at least means that
-- uses can influence kinds...

module ShouldFail where

newtype Object f' f t o1 o2  = Object (f' t o1 -> f t o2)
type DUnit t o1 o2       = ()

type T f g t o1 o2  = Either (f t o1 o2) (g t o1 o2)

type LiftObject t f' f         = T (Object f' f t) (DUnit t)
 

