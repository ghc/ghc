{-# LANGUAGE ScopedTypeVariables #-}

-- !!! scoped type variables
-- this test failed in pre-release GHC 5.02

module ShouldCompile where

f :: forall x. x -> x -> x
f (x::x) (y::x) = x
-- Two points: (a) we are using x as a term variable and as a type variable
--	       (b) the type variable appears twice, but that is OK
