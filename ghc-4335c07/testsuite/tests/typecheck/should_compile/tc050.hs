module ShouldSucceed where

class Foo a where
 o_and :: a -> a -> a


instance Foo Bool where
 o_and False x = False
 o_and x False = False
 o_and True True = True


instance Foo Int where
 o_and x 0 = 0
 o_and 0 x = 0
 o_and 1 1 = 1


f x y = o_and x False

g x y = o_and x 1


