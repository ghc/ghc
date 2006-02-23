{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-- -fallow-undecidable-instanced now needed because the Coverage Condition fails

-- Hugs failed this functional-dependency test
-- Reported by Iavor Diatchki Feb 05

module ShouldCompile where

data N0
newtype Succ n    = Succ n

class Plus a b c | a b -> c
instance Plus N0 n n
instance Plus a b c => Plus (Succ a) b (Succ c)

( # )              :: Plus x y z => x -> y -> z
( # )               = undefined

class BitRep t n | t -> n where
  toBits         :: t -> n

instance BitRep Bool (Succ N0) where
  toBits = error "urk"

instance BitRep (Bool,Bool,Bool) (Succ (Succ (Succ N0))) where
  toBits (x,y,z) = toBits x # toBits y # toBits z

-- Hugs complains that it cannot solve the constraint:
-- Plus (Succ N0) (Succ N0) (Succ (Succ N0))

