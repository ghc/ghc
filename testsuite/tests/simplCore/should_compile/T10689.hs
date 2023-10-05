module T10694 where

f :: Eq a => a -> Bool
{-# NOINLINE f #-}
f x = x==x

type Foo a b = b

{-# RULES "foo" forall (x :: Foo a Char). f x = True #-}

finkle = f 'c'
