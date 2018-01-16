{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             ExistentialQuantification #-}

-- !!! Functional dependencies and existentials

-- Hugs (February 2000) doesn't like it. It says
--  Variable "e" in constraint is not locally bound

module ShouldCompile where

class Collection c e | c -> e where
   empty :: c
   put   :: c -> e -> c

data SomeCollection e = forall c . Collection c e => MakeSomeCollection c
