{-# OPTIONS -findexed-types #-}
module ATLoop_help where

class Foo a where
   data FooT a :: *
   int :: FooT a -> Int

instance Foo Int where
    data FooT Int = FooInt !Int
    int (FooInt n) = n
