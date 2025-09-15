{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
module T25127_data_inst where

import Data.Kind
import GHC.TypeLits

data family T a
data instance T a where
  Typed :: forall a -> a -> T a

t1 = Typed Int 42
t2 = Typed String "hello"
t3 = Typed (Int -> Bool) even

f1 (Typed a x) = x :: a
f2 (Typed Int n) = n*2
f3 (Typed ((->) w Bool) g) = not . g
