{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

-- Trac #1470

module Foo where

class Sat a
class Data ctx a
instance  Sat (ctx Char)             => Data ctx Char
instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]

class Data FooD a => Foo a

data FooD a = FooD

instance Foo t => Sat (FooD t)

instance Data FooD a => Foo a
instance Foo a       => Foo [a]
instance                Foo [Char]
