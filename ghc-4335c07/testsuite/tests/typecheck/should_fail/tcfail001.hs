-- !!! This should fail with a type error: the instance method
-- !!! has a function type when it should have the type [a].
module ShouldFail where

class A a where
 op :: a

instance (A a, A a) => A [a] where
 op [] = []
