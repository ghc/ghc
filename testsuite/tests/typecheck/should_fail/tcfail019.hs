module ShouldFail where

class A a where
 p1 :: a -> a
 p2 :: a -> a -> a

class (A b) => B b where
 p3 :: b
 p4 :: b -> b

class (A c) => C c where
 p5 :: c -> c
 p6 :: c -> Int

class (B d,C d) => D d where
 p7 :: d -> d

instance D [a] where
 p7 l = []

