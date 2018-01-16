module T9117_3 where

import Data.Type.Coercion
import Data.Coerce

eta :: Coercible f g => Coercion (f a) (g a)
eta = Coercion
