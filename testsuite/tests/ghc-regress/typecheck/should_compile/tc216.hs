{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}

-- Test for trac #816
-- GHC's typechecker loops when trying to type this, resulting in a
-- context stack overflow.

module ShouldCompile where

class Foo x y | x -> y where
 foo :: x -> y

class Bar x y where
 bar :: x -> y -> Int

instance (Foo x y, Bar y z) => Bar x z where
 bar x z = bar (foo x) z

