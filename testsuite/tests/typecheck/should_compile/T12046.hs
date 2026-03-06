-- Test that AllowAmbiguousTypes works with UndecidableSuperClasses.
-- Both test1 and test2 should be accepted.
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T12046 where

class A (T a) => A a where
    type T a

test1 :: forall a. A a => ()
test1 = ()

test2 :: A a => proxy a -> ()
test2 _ = ()

