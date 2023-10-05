{-# LANGUAGE RankNTypes #-}
module T7264 where

data Foo = Foo (forall r . r -> String)

mmap :: (a->b) -> Maybe a -> Maybe b
mmap f (Just x) = Just (f x)
mmap f Nothing  = Nothing

-- mkFoo2 :: (forall r. r -> String) -> Maybe Foo
-- Should be rejected because it requires instantiating
-- mmap at a polymorphic type
mkFoo2 val = mmap Foo (Just val)
