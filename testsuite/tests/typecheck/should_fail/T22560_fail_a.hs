module T22560_fail_a where

import Data.Kind (Type)

-- NB: the inferred forall is always skipped
type P :: forall {k}. k -> Type
data P @k a = MkP k