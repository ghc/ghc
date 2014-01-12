{-# LANGUAGE OverlappingInstances, UndecidableInstances, MultiParamTypeClasses, 
             FunctionalDependencies, FlexibleInstances #-}

module T3108 where

-- Direct recursion terminates (typechecking-wise)

class C0 x
 where
 m0 :: x -> ()
 m0 = const undefined

instance (C0 x, C0 y) => C0 (x,y)
instance C0 Bool
instance C0 (x,Bool) => C0 x

foo :: ()
foo = m0 (1::Int)


-- Indirect recursion does not terminate (typechecking-wise)

class C1 x
 where
 m1 :: x -> ()
 m1 = const undefined

instance (C1 x, C1 y) => C1 (x,y)
instance C1 Bool
instance (C2 x y, C1 (y,Bool)) => C1 x

class C2 x y | x -> y
instance C2 Int Int

-- It is this declaration that causes nontermination of typechecking.
bar :: ()
bar = m1 (1::Int)
