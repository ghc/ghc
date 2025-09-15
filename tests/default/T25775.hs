

module T25775 where

import Data.Kind

default (Int)

type NonStd :: Type -> Constraint
class NonStd a where

f :: (Num a, NonStd a) => a -> a
f = (+1)

x :: String
x = show (f 0)
  -- We should NOT default 0 to type Int, despite the top-level default
  -- declaration in this module, because of the presence of the
  -- non-standard class 'NonStd'.
