module A where

-- An ambiguity error whose message lists potential instances, both in-scope
-- and (via the instance-only imports) involving out-of-scope types.

import Data.Functor.Const ()
import Data.Functor.Identity ()
import Data.Monoid ()
import Data.Proxy ()
import Data.Ord ()

v :: Int
v = foldr (+) 0 (pure 21)
