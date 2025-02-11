{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
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
--      f [0::Int] :: Bug (Int->Int) [Int] r => r
-- Which matches the second instance declaration, giving r = [r']
--      f [0::Int] :: Bug (Int->Int) Int r' => r'
-- Wwich matches the first instance decl giving r' = Int
--      f [0::Int] :: Int
-- The outer f now has constraint
--      Bug (Int->Int) Int r
-- which makes r=Int
-- So g1::Int

g2 = f (f (f [0::Int]))
-- The outer f repeats the exercise, so g2::Int
-- This is the definition that Hugs rejects


{-
start iteration=0
        [W] $dBug_aET {0} :: Bug (Int -> Int) a_aER[tau:1] r_aES[tau:1] (CDictCan)
        [W] $dBug_aEW {0} :: Bug (Int -> Int) [r_aFd[tau:1]] a_aER[tau:1] (CDictCan)
        [W] $dBug_aF0 {0} :: Bug (Int -> Int) [Int] [r_aFd[tau:1]] (CDictCan)}
-->
        [W] $dBug_aET {0} :: Bug (Int -> Int) a_aER[tau:1] r_aES[tau:1] (CDictCan)
        [W] $dBug_aEW {0} :: Bug (Int -> Int) [r_aFd[tau:1]] a_aER[tau:1] (CDictCan)
        [W] $dBug_aF0 {0} :: Bug (Int -> Int) Int r_aFd[tau:1] (CDictCan)}
--> fds
  a_aER := [r_aFf]
  r_fD  := Int

start iteration=1
        [W] $dBug_aET {0} :: Bug (Int -> Int) [r_aFf[tau:1]] r_aES[tau:1] (CDictCan)
        [W] $dBug_aEW {0} :: Bug (Int -> Int) [r_aFd[tau:1]] [r_aFf][tau:1]] (CDictCan)
        [W] $dBug_aF0 {0} :: Bug (Int -> Int) Int Int (CDictCan)}
--> (solve aF0, instance for aEW)
        [W] $dBug_aET {0} :: Bug (Int -> Int) [r_aFf[tau:1]] r_aES[tau:1] (CDictCan)
        [W] $dBug_aFg {1} :: Bug (Int -> Int) Int r_aFf[tau:1] (CDictCan)}
--> fds
  r_aES := [r_aFh]    -- fresh r_aFh
  r_aFf := Int

start iteration=2
        [W] $dBug_aET {0} :: Bug (Int -> Int) [Int] [r_afH[tau:1]] (CDictCan)
        [W] $dBug_aFg {1} :: Bug (Int -> Int) Int Int  (CDictCan)}
--> (solve afG instance for aET
        [W] $dBug_aFi {1} :: Bug (Int -> Int) Int r_aFh[tau:1] (CDictCan)}
--> fds
   f_afH := Int

start iteration 3
        [W] $dBug_aFi {1} :: Bug (Int -> Int) Int Int (CDictCan)}
--> solve



                   [W] $dBug_aCv {0} :: Bug (Int -> Int) [r_aCL[tau:1]] r_aCu[tau:1] (CDictCan)
                   [W] $dBug_aEI {1} :: Bug (Int -> Int) Int            r_aCL[tau:1] (CDictCan)}
-->
  r_aCL := Int
  r_aCu := [ r_new ]   -- r_aEJ

  [W] Bug (Int -> Int) Int r_new
-}
