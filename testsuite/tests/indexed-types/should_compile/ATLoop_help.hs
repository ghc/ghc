{-# LANGUAGE TypeFamilies #-}
module ATLoop_help where

import Data.Kind (Type)

class Foo a where
   data FooT a :: Type
   int :: FooT a -> Int

instance Foo Int where
    data FooT Int = FooInt !Int
    int (FooInt n) = n
