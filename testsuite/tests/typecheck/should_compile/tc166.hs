{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             ExistentialQuantification, RankNTypes,
             FlexibleInstances #-}

-- Arguably, the type signature for f1 should be enough to make
-- this program compile, but in 5.04 it wasn't; the
-- extra sig in f2 was needed.
--
-- This is a pretty borderline case.

module ShouldCompile where

    class C t a b | t a -> b
    instance C Char a Bool

    data P t a = forall b. (C t a b) => MkP b
    
    data Q t = MkQ (forall a. P t a)
    
    f1 :: Q Char
    f1 = MkQ (MkP True)

    f2 :: Q Char
    f2 = MkQ (MkP True :: forall a. P Char a)

