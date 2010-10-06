{-# LANGUAGE GADTs #-}

-- This one requires careful handling in 
-- TcUnify.unifyTyConApp, to preserve rigidity.

module ShouldCompile where

data X a b where
    X :: X a a

data Y x a b where
    Y :: x a b -> x b c -> Y x a c

doy :: Y X a b -> Y X a b
doy (Y X X) = Y X X
