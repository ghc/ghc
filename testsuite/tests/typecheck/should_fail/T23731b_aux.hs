{-# LANGUAGE RoleAnnotations #-}
module T23731b_aux (Foo(..)) where


newtype Foo a = MkFoo a
type role Foo nominal
