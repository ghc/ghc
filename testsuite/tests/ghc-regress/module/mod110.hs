-- !!! Re-defining Prelude class
module M where

import Prelude
--import qualified Prelude

class Eq a where
 equal  :: a -> a -> Prelude.Bool
 negate :: a -> a

instance Eq Prelude.Int where
  equal x y = x Prelude.== y
  negate x  = Prelude.negate x
x = M.negate (2 :: Prelude.Int)

