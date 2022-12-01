module T22560_fail_b where

import Data.Kind (Type)

type P :: forall a -> Type
data P @a = MkP