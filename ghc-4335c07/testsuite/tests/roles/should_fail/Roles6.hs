{-# LANGUAGE RoleAnnotations #-}

module Roles6 where

data Foo a b = MkFoo (a b)

type role Foo nominal representational phantom

