{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- UndecidableInstances now needed because the Coverage Condition fails

-- !!! Functional dependency test. Hugs [Apr 2001] fails to typecheck this
-- Rather bizarre example submitted by Jonathon Bell

module ShouldCompile where

-- module Foo where

class Bug f a r | f a -> r where
   bug::f->a->r

instance                Bug (Int->r) Int      r
instance (Bug f a r) => Bug f        (c a)    (c r) 

f:: Bug(Int->Int) a r => a->r
f = bug (id::Int->Int)

g1 = f (f [0::Int])
-- Inner f gives result type 
--	f [0::Int] :: Bug (Int->Int) [Int] r => r
-- Which matches the second instance declaration, giving r = [r']
--	f [0::Int] :: Bug (Int->Int) Int r' => r'
-- Wwich matches the first instance decl giving r' = Int
--	f [0::Int] :: Int
-- The outer f now has constraint
--	Bug (Int->Int) Int r
-- which makes r=Int
-- So g1::Int

g2 = f (f (f [0::Int]))
-- The outer f repeats the exercise, so g2::Int
-- This is the definition that Hugs rejects

